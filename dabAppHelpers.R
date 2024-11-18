readDabAgg <- function(){
  aggList <- list()
  
  if (file.exists("alldata.csv")){
    alldata <- read_csv("alldata.csv")
    if (!exists("Region", alldata) && consortiumName == "MWCCS"){
      alldata$Region <- alldata$site
      alldata$Sex <- alldata$sex
      alldata$Variable <- alldata$variable
      alldata$Patients <- alldata$patients
      #alldata$numobserv <- alldata$patients
      alldata$Value <- alldata$value
      alldata$dabAge <- alldata$age
      alldata$Age <- alldata$age
      alldata$Year <- alldata$visit
      alldata$PercSites <- 100
      alldata$PROGRAM <- "ALL"
      alldata$centers <- 1
      alldata$numcenters <- 1
      mainvars <- c("Region", "Year", "Sex", "Variable", "Value", "Patients", 
                    "dabAge", "Age", "centers", "numcenters",
                    "PercSites", "PROGRAM")
      alldata <- alldata[, mainvars] %>% filter(Region != "ALL")
    }
  
  } else {
    for (varName in names(varInfo)){
      #if (varName == "CD4_V") browser()
      for (region in regions){
        if (file.exists(paste0(
          "~/R/dashboard aggregation/shiny version/dabaggoutput/", 
          region, "_", varName, ".csv"))){
          df <- read_csv(paste0(
            "~/R/dashboard aggregation/shiny version/dabaggoutput/", 
            region, "_", varName, ".csv"))

          if ("aggnames" %in% names(df)){
            df <- df %>% select(-aggnames)
          }

          if (exists(varName, df)){
            df <- df %>% 
              mutate(Variable = varName) %>% 
              rename(Value = !! rlang::sym(varName)) 
          } else {
            # this isn't a factor, it's a simple count
            df <- df %>% 
              mutate(Variable = varName) %>% 
              mutate(Value = "Count") 
          }
          
          if ("PROGRAM" %in% names(df)){
            df <- df %>% 
              filter(PROGRAM == "ALL")
          } else {
            df <- df %>% 
              mutate(PROGRAM = "ALL") %>% 
              mutate(Age = "ALL",
                     #TreatNaive = "ALL",
                     sites = 1,
                     PercSites = 1
              )
            if (exists("numobserv", df)){
              browser()
              df <- df %>% mutate(Patients = numobserv) %>% select(-numobserv)
            } 
 
          }
          
          aggList[[varName]][[region]] <- df
        } else {

          aggList[[varName]][[region]] <- tibble(
            Region = region,
            Year = c(c(minyear:maxyear), "ALL"),
            Age = "ALL",
            #TreatNaive = "ALL",
            PROGRAM = "ALL",
            Value = "ALL",
            Patients = 0,
            sites = 0,
            PercSites = 0,
            Variable = varName
          )
        }
      }

      aggList[[varName]] <- rbindlist(aggList[[varName]], use.names = TRUE, fill = TRUE)
    }

    alldata <- rbindlist(aggList, use.names = TRUE, fill = TRUE)

 #   if (!exists("dabAge", alldata)){
      alldata <- alldata %>% 
        mutate(dabAge = Age) %>% 
        mutate(centers = sites) %>% 
        rename(numcenters = sites) #%>% 
        #mutate(numobserv = Patients)
 #   } else {
 #     alldata <- alldata %>% 
#        mutate(centers = sites) %>% 
#        rename(numcenters = sites) %>% 
#        mutate(numobserv = Patients)
#    }
    
    
    write_csv(alldata, "alldata.csv", na = "")
  }

  return(alldata)
}

orderCodesByFreq <- function(varNames, alldata){

  if (file.exists("codesInOrder.json")){
    codesInOrder <- rjson::fromJSON(file = "codesInOrder.json")
  } else {
    codeFreq <- list()
    additionalMessages <- list()
    for (varName in varNames){
      varDetails <- varInfo[[varName]]
      additionalMessages[[varName]] <- NULL
      if (exists("allcodes", varDetails) && varDetails$allcodes) {
        codeLevels <- alldata %>% filter(Variable == varName) %>% 
          filter(Value != "ALL") %>% 
          distinct(Value) %>% pull(Value)
        
        valuesWithNoObs <- alldata %>% filter(Variable == varName) %>% 
          filter(Year == "ALL", dabAge == "ALL", Sex == "ALL") %>%  #, TreatNaive == "ALL") %>% 
          filter(Value != "ALL") %>% 
          group_by(Value) %>% 
          summarise(totalcount = sum(Patients)) %>% 
          filter(totalcount == 0) %>% 
          pull(Value)
        
        if (!is_empty(valuesWithNoObs)){
          codeLevels <- codeLevels[!codeLevels %in% valuesWithNoObs]
          additionalMessages[[varName]] <- paste0("The following codes had zero observations in any region:",
                                                  paste(valuesWithNoObs, collapse = ", "))
        }
      } else if (!is.null(varDetails$codes)) {
        codeLevels <- varDetails$codes
      }
      if (consortiumName == "MCCS/WHS" && varName == "SEX"){
        codeLevels <- c("ALL", codeLevels)
      }
      ##################################################################
      # if user selected one age group
      ##################################################################
      
      counting <- alldata %>%
        filter(Variable == varName) %>% 
        filter(Year == "ALL", Value %in% codeLevels) %>% 
        group_by(Value, dabAge, Sex) %>% 
        summarise(count = sum(Patients), .groups = 'drop') %>% 
        pivot_wider(names_from = c(dabAge, Sex), values_from = count) 
      # now make data frame with columns labeled dabAge_Sex, e.g., 0-17_ALL
      combonames <- names(counting)
      combonames <- combonames[!combonames == "Value"]
      for (comboname in combonames){
        codeFreq[[varName]][[comboname]] <- counting[, c("Value", comboname)] %>% 
          arrange(desc(!!rlang::sym(comboname))) %>% 
          mutate(codeLabel = paste0(Value,  " (", !!rlang::sym(comboname), ")"))
      }
      ###########################################################################
      
      
      
      for (index in 1:length(agecombos)){
        
        agecombo <- agecombos[[index]]
        agegroupname <- names(agecombos)[[index]]
        
        counting <- alldata %>%
          filter(Variable == varName) %>% 
          filter(Year == "ALL", Value %in% codeLevels) %>% 
          filter(dabAge %in% agecombo) %>% 
          # mutate(dabAge 
          group_by(Value, Sex) %>% 
          summarise(count = sum(Patients), .groups = 'drop') %>% 
          mutate(dabAge = agegroupname) %>% 
          pivot_wider(names_from = c(dabAge, Sex), values_from = count)
        
        combonames <- names(counting)
        combonames <- combonames[!combonames == "Value"]
        
        for (comboname in combonames){
          codeFreq[[varName]][[comboname]] <- counting[, c("Value", comboname)] %>% 
            arrange(desc(!!rlang::sym(comboname))) %>% 
            mutate(codeLabel = paste0(Value,  " (", !!rlang::sym(comboname), ")"))
        }
      }
    }
    codesInOrder <- list(
      codeFreq = codeFreq, 
      # codeFreqTwo = codeFreqTwo,
      additionalMessages = additionalMessages
    )
    
    sink(file = "codesInOrder.json")
    print(toJSON(codesInOrder, dataframe = "columns"))
    sink()

  }

  return(codesInOrder)
}

makeAgeGroupCombos <- function(ageGroups, ageGroupDetails){
  agecombos <- list()
  numberOfAgeGroups <- length(ageGroups)
  
  for (ageGroupIndex in 1:(numberOfAgeGroups-1)){

    ageGroup <- ageGroups[[ageGroupIndex]]
    minAge <- ageGroupDetails[[ageGroupIndex]]$min
    
    agesInGroup <- ageGroup
    for (upperIndex in (ageGroupIndex+1):numberOfAgeGroups){
      upperAgeGroup <- ageGroups[[upperIndex]]
      agesInGroup <- c(agesInGroup, upperAgeGroup)
      maxAge <- ageGroupDetails[[upperIndex]]$max
      if (upperIndex < numberOfAgeGroups){
        nameOfCombo <- paste0(minAge, "-", maxAge)
      } else {
        nameOfCombo <- paste0(minAge, "+")
      }
      agecombos[[nameOfCombo]] <- agesInGroup
    }
  }
  return(agecombos)
}

createTooltipInfo <- function(summary_df, region_detail_df){
  df_for_displaycol <- summary_df %>% rename(RegionLineBreaks = Region) %>% 
    left_join(region_detail_df %>% select(RegionLineBreaks, TotalPatients, RegionFull), 
              by = "RegionLineBreaks")
#browser()
  displayDetails <- df_for_displaycol %>% 
    mutate(
      fordisplay = paste0(
        "<span style='font-size:15px;'>",
        Patients, 
        " unique patients match the selected criteria out of all ",
        TotalPatients,
        " patients in ",
        RegionFull,
        "<span>")
    ) %>% pull(fordisplay)
  return(displayDetails)
}