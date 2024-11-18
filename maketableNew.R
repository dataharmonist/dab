makeSureEveryRegionEveryLevel <- function(df, all_levels){
  region_level_combos <- df %>% distinct(Region, Value)
  count_levels <- region_level_combos %>% 
    group_by(Region) %>% 
    summarise(num_levels = n()) %>% 
    filter(num_levels < length(all_levels))
  if (nrow(count_levels) > 0){
    problem_regions <- count_levels$Region
    for (region_name in problem_regions){
      levels_found <- region_level_combos %>% 
        filter(Region == region_name) %>% pull(Value)
      missing_levels <- all_levels[!all_levels %in% levels_found]
      for (level in missing_levels){
        new_row <- tibble(
          Region = region_name,
          Year = as.character(minyear),
          Age = df$Age[[1]],
          PROGRAM = "ALL",
          Sex = df$Sex[[1]],
          Value = level,
          Patients = 0,
          numcenters = 0,
          PercSites = 0,
          Variable = df$Variable[[1]],
          dabAge = df$dabAge[[1]],
          centers = 0#,
          #numobserv = 0
        ) 
        new_row$Value <- factor(new_row$Value, levels = all_levels)
        df <- bind_rows(df, new_row)
      }
    }
  }
  return(df)
}

makeSureEveryRegionEverySex <- function(df, all_levels = sexGroups){
  region_level_combos <- df %>% distinct(Region, Sex)
  count_levels <- region_level_combos %>% 
    group_by(Region) %>% 
    summarise(num_levels = n()) %>% 
    filter(num_levels < length(all_levels))
  if (nrow(count_levels) > 0){
    problem_regions <- count_levels$Region
    for (region_name in problem_regions){
      levels_found <- region_level_combos %>% 
        filter(Region == region_name) %>% pull(Sex)
      if (is.na(levels_found)) next
      missing_levels <- all_levels[!all_levels %in% levels_found]
      for (level in missing_levels){
        new_row <- tibble(
          Region = region_name,
          Year = as.character(minyear),
          Age = df$Age[[1]],
          PROGRAM = "ALL",
          Sex = level,
          Value = df$Value[[1]],
          Patients = 0,
          numcenters = 0,
          PercSites = 0,
          Variable = df$Variable[[1]],
          dabAge = df$dabAge[[1]],
          centers = 0#,
          #numobserv = 0
        ) 
        new_row$Sex <- factor(new_row$Sex, levels = all_levels)
        df <- bind_rows(df, new_row)
      }
    }
  }
  return(df) 
}

makeSureEveryRegionEveryAge <- function(df, all_levels){
  region_level_combos <- df %>% distinct(Region, dabAge)
  count_levels <- region_level_combos %>% 
    group_by(Region) %>% 
    summarise(num_levels = n()) %>% 
    filter(num_levels < length(all_levels))
  if (nrow(count_levels) > 0){
    problem_regions <- count_levels$Region
    for (region_name in problem_regions){
      levels_found <- region_level_combos %>% 
        filter(Region == region_name) %>% pull(dabAge)
      if (is_empty(levels_found)) next
      if (length(levels_found) == 1 && is.na(levels_found)) next
      missing_levels <- all_levels[!all_levels %in% levels_found]
      for (level in missing_levels){
        new_row <- tibble(
          Region = region_name,
          Year = as.character(minyear),
          Age = level,
          PROGRAM = "ALL",
          Sex = df$Sex[[1]],
          Value = df$Value[[1]],
          Patients = 0,
          numcenters = 0,
          PercSites = 0,
          Variable = df$Variable[[1]],
          dabAge = level,
          centers = 0#,
          #numobserv = 0
        ) 
        new_row$dabAge <- factor(new_row$dabAge, levels = all_levels, ordered = TRUE)
        df <- bind_rows(df, new_row)
      }
    }
  }
  return(df)
}


create_plot <- function(x){
 # if (x$firstregion[[1]]){
 #   addlegend <- TRUE
 #   height <- 140
 # } else {
    addlegend <- FALSE
    height <- 120
  #}
  # if this is a count, don't display legend
  if (x$Value[[1]] == "Count"){
    addlegend <- FALSE
    height <- 120
  }
  x <- x %>% select(-firstregion)
  x$Year <- as.integer(x$Year)

  p <- hchart(x %>% rename({{ timeVar }} := Year), 
              "column", 
              hcaes(x = .data[[timeVar]], y = Patients, group = Value), stacking = "normal") %>% 
    hc_title(text = "") %>% 
    hc_yAxis(title = list(text = "")) %>% # or maybe y axis should have a label?
    hc_xAxis(title = list(text = ""),
             min = minyear,
             max = maxyear)  %>% 
    hc_size(height = height) %>% 
    hc_tooltip(pointFormat = "{point.tooltip}") 
  if (addlegend){
    p <- p %>% hc_legend(verticalAlign = "top")
  } else {
    p <- p %>% hc_legend(element_blank())
  }
  return(p)
}

create_plot_sex <- function(x){
  
  # if (x$firstregion[[1]]){
  #   addlegend <- TRUE
  #   height <- 140
  # } else {
  addlegend <- FALSE # legend will be by sex
  height <- 120
  #}
  # if this is a count, don't display legend
  # if (x$Value[[1]] == "Count"){
  #   addlegend <- FALSE
  #   height <- 120
  # }
  x <- x %>% select(-firstregion)
  x$Year <- as.integer(x$Year)
  p <- hchart(x %>% rename({{ timeVar }} := Year), "column", 
              hcaes(x = .data[[timeVar]], y = Patients, group = Sex), stacking = "normal") %>% 
    hc_title(text = "") %>% 
    hc_yAxis(title = list(text = "")) %>% # or maybe y axis should have a label?
    hc_xAxis(title = list(text = ""),
             min = minyear,
             max = maxyear)  %>% 
    hc_size(height = height) %>% 
    hc_tooltip(pointFormat = "{point.tooltip}") 
  if (addlegend){
    p <- p %>% hc_legend(verticalAlign = "top")
  } else {
    p <- p %>% hc_legend(element_blank())
  }
  return(p)
}

create_plot_age <- function(x){
  # if (x$firstregion[[1]]){
  #   addlegend <- TRUE
  #   height <- 140
  # } else {
  addlegend <- FALSE
  height <- 120
  #}
  # if this is a count, don't display legend
  # if (x$Value[[1]] == "Count"){
  #   addlegend <- FALSE
  #   height <- 120
  # }
  x <- x %>% select(-firstregion)
  x$Year <- as.integer(x$Year) #integer
  p <- hchart(x %>% rename({{ timeVar }} := Year), "column", 
              hcaes(x = .data[[timeVar]], y = Patients, group = Age), stacking = "normal") %>% 
    hc_title(text = "") %>% 
    hc_yAxis(title = list(text = "")) %>% # or maybe y axis should have a label?
    hc_xAxis(title = list(text = ""),
             min = minyear,
             max = maxyear)  %>% 
    hc_size(height = height) %>% 
    hc_tooltip(pointFormat = "{point.tooltip}") 
  if (addlegend){
    p <- p %>% hc_legend(verticalAlign = "top")
  } else {
    p <- p %>% hc_legend(element_blank())
  }
  return(p)
}

# this creates the region-specific detail plot that opens on expand
create_plot_value <- function(x){

  varDetails <- varInfo[[input$var]]
  description <- paste0(varDetails$displayName)

  regionName <- gsub("<br>.*$", "", x$Region[[1]])
  if (!regionName %in% unlist(fullRegionNames)){
    regionName <- fullRegionNames[[regionName]]
  }

 # regionName <- str_split(x$Region[[1]], "<br>")[[1]]
  x$Year <- as.integer(x$Year)
  hc <- hchart(x %>% rename({{ timeVar }} := Year), "column", 
               hcaes(x = .data[[timeVar]], y = Patients, group = Value), stacking = "normal") %>% 
    hc_title(text = "") %>% 
    hc_yAxis(title = list(text = "Number of observations")) %>% 
    hc_xAxis(title = list(text = timeVar)) %>% #,
           #  min = minyear,
           #  max = maxyear)  %>% 
    hc_size(height = 300) %>% 
    hc_tooltip(pointFormat = "{point.tooltip}") %>% 
    hc_title(text = paste0("Observations of ", description, " from ", regionName))
  if (length(levels(x$Value)) == 1 &&
      levels(x$Value) == "Count"){
    hc <- hc %>% 
      hc_legend(element_blank())
  }
  return(hc)
}
# this creates the region-specific detail plot that opens on expand when grouping by sex
create_plot_value_sex <- function(x){
  varDetails <- varInfo[[input$var]]
  description <- paste0(varDetails$displayName)
  
  regionName <- gsub("<br>.*$", "", x$Region[[1]])
  if (!regionName %in% unlist(fullRegionNames)){
    regionName <- fullRegionNames[[regionName]]
  }
  
  # regionName <- str_split(x$Region[[1]], "<br>")[[1]]

  x$Year <- as.integer(x$Year)
  
  hc <- hchart(x %>% rename({{ timeVar }} := Year), "column", 
               hcaes(x = .data[[timeVar]], y = Patients, group = Sex), stacking = "normal") %>% 
    hc_title(text = "") %>% 
    hc_yAxis(title = list(text = "Number of observations")) %>% 
    hc_xAxis(title = list(text = timeVar),
             min = minyear,
             max = maxyear)  %>% 
    hc_size(height = 300) %>% 
    hc_tooltip(pointFormat = "{point.tooltip}") %>% 
    hc_title(text = paste0("Observations of ", description, " from ", regionName))
  # if (length(levels(x$Value)) == 1 &&
  #     levels(x$Value) == "Count"){
  #   hc <- hc %>% 
  #     hc_legend(element_blank())
  # }
  return(hc)
}

# this creates the region-specific detail plot that opens on expand when grouping by age
create_plot_value_age <- function(x){

  varDetails <- varInfo[[input$var]]
  description <- paste0(varDetails$displayName)
  
  regionName <- gsub("<br>.*$", "", x$Region[[1]])
  if (!regionName %in% unlist(fullRegionNames)){
    regionName <- fullRegionNames[[regionName]]
  }
  
  # regionName <- str_split(x$Region[[1]], "<br>")[[1]]

  x$Year <- as.integer(x$Year)
  
  hc <- hchart(x %>% rename({{ timeVar }} := Year), "column", 
               hcaes(x = .data[[timeVar]], y = Patients, group = Age), stacking = "normal") %>% 
    hc_title(text = "") %>% 
    hc_yAxis(title = list(text = "Number of observations")) %>% 
    hc_xAxis(title = list(text = timeVar),
             min = minyear,
             max = maxyear)  %>% 
    hc_size(height = 300) %>% 
    hc_tooltip(pointFormat = "{point.tooltip}") %>% 
    hc_title(text = paste0("Observations of ", description, " from ", regionName))
  # if (length(levels(x$Value)) == 1 &&
  #     levels(x$Value) == "Count"){
  #   hc <- hc %>% 
  #     hc_legend(element_blank())
  # }
  return(hc)
}

# create_plot_byage <- function(x){
#   hchart(x, "column", hcaes(x = Year, y = numobserv, group = dabAge), stacking = "normal") %>% 
#     hc_title(text = "") %>% 
#     hc_yAxis(title = list(text = "Number of observations")) %>% #,
#     hc_xAxis(title = list(text = "Year"))  %>% 
#     hc_size(height = 499) %>% 
#     hc_tooltip(pointFormat = "{point.tooltip}") %>% 
#     hc_title(text = paste0("Observations of ", input$var)) #,
#   # margin = 20,
#   # align = "left",
#   # style = list(color = "#22A884", useHTML = TRUE)
#   #)
# }

observeEvent(input$var,{
  print("newvar")
  tableReady(FALSE)
  byAgeTableReady(FALSE)
  bySexTableReady(FALSE)
})

observeEvent(input$codes,{
  tableReady(FALSE)
  byAgeTableReady(FALSE)
  bySexTableReady(FALSE)
})

observeEvent(input$filterAge,{
  tableReady(FALSE)
  byAgeTableReady(FALSE)
  bySexTableReady(FALSE)
})

observeEvent(input$filterSex,{
  tableReady(FALSE)
  byAgeTableReady(FALSE)
  bySexTableReady(FALSE)
})

observeEvent(input$sexGroup,{
  tableReady(FALSE)
  byAgeTableReady(FALSE)
  bySexTableReady(FALSE)
})

observeEvent(input$range,{
  tableReady(FALSE)
  byAgeTableReady(FALSE)
  bySexTableReady(FALSE)
})

# for all tabs, initial filtering by variable and code level choice necessary
initialTable <- eventReactive(input$makeTable,{
  if (authRequired && !authenticatedUser()) return(NULL)
  
  #bySexTable(NULL)
 # byAgeTable(NULL)
  noData(FALSE)
  missingRegions(NULL)

  varChoice <- input$var
  varDetails <- varInfo[[input$var]]
  
  
  # 1: filter just this variable
  df <- alldata %>%
    filter(Variable == varChoice)
  
  # fix in aggregations JUDY
  if (input$var == "MOTHER_ID"){
    df <- df %>% 
      filter(Value != "ALL")
  }
  
  
  # 2: if coded, filter just the codes chosen
  #is this an observation variable, not a multiple level variable, is it coded?
  if (all(df$Value == "Count")){
    hasCodes <- FALSE
    valMatch <- "Count"
    codeVals <- "Count"
  } else {
    hasCodes <- TRUE
    codeVals <- input$codes # be careful. this could be from last variable
    valMatch <- input$codes
  }
  
  df <- df %>% 
    filter(Value %in% valMatch) %>% 
    mutate(Value = factor(Value, levels = valMatch))
  # end of step 2
  
  return(df)

})


overviewTable <- eventReactive(input$makeTable,{ #initialTable(),{
  noData(FALSE)
  df <- initialTable()
  varChoice <- input$var
  varDetails <- varInfo[[input$var]]
  variableDesc <- varInfo[[input$var]]$displayName
  
  sexChoice <- ifelse(!input$filterSex || is.null(input$sexGroup), "ALL", input$sexGroup)
  ageChoice <- ifelse(is.null(ageGroupChosen()), "ALL", ageGroupChosen())
  
  if (tolower(ageChoice) == "all") ageChoice <- "ALL"
  
  # FILTERING by SEX
  df <- df %>%
    filter(Sex == sexChoice)
  
  # FILTERING by AGE
  if (ageChoice %in% ageGroups || tolower(ageChoice) == "all"){ # this is a single age group, including ALL
    df <- df %>% 
      filter(dabAge == ageChoice)
  } else {
    # this is a combo group
    df <- df %>% 
      filter(dabAge %in% agecombos[[ageChoice]]) %>% 
      group_by(Region, Year, PROGRAM, Sex, Value, Variable) %>% 
      summarise(
        Patients = sum(Patients),
        numcenters = max(numcenters),
        PercSites = max(PercSites),
        centers = max(centers)#,
        #numobserv = sum(numobserv)
      ) %>%
      ungroup() %>% 
      mutate(
        dabAge = ageChoice,
        Age = ageChoice
      )
  }

  # if no matching data, return null
  if (nrow(df) == 0) {
    noData(TRUE)
    return(NULL)
  }
  
  if (all(df$Value == "Count")){
    valMatch <- "Count"
    codeVals <- "Count"
  } else {
    codeVals <- input$codes # be careful. this could be from last variable
    valMatch <- input$codes
  }
  
  # if no matching data, return null
  if (nrow(df) == 0) {
    noData(TRUE)
    return(NULL)
  }
  
  # detect regions missing data for this variable/value
  if (any(!regions %in% unique(df$Region))){
    missingones <- regions[!regions %in% unique(df$Region)]
    missingRegions(missingones)
  }
  
  df <- makeSureEveryRegionEveryLevel(df, valMatch)
  
  df <- left_join(df, regiondatawithprogs) %>% 
    mutate(Regioncode = Region) %>% 
    mutate(Region = RegionLineBreaks) %>% #RegionWithNum) %>% 
    select(-RegionWithNum) %>% 
    mutate(PercPatients = round(100*Patients/TotalPatients, digits =2),
           PercSites = round(100*centers/totalprogs, digits = 1))
  
  # clean up (revisit with respect to region code, etc)
  if (exists("NumPatients", df)) df <- df %>% select(-NumPatients)
  extracols <- c("TotalPatients", "RegionLineBreaks", "totalprogs")
  for (colname in extracols){
    if (exists(colname, df)) df <- df %>% select(- !! rlang::sym(colname))
  }
  
  # here multiple values should be handled - sum by region
  df_summary <- df %>% 
    filter(Year == "ALL") %>% 
    select(-Year, -Variable, -dabAge, -Sex, -RegionFull) #-TreatNaive, 
  
  if (exists("PROGRAM", df_summary)) df_summary <- df_summary %>% select(-PROGRAM)
  
  if (nrow(df_summary) == 0 || sum(df_summary$Patients) == 0) {
    noData(TRUE)
    return(NULL)
  }
  
  if (any(!regions %in% unique(df_summary$Regioncode))){
    
    missingones <- regions[!regions %in% unique(df_summary$Regioncode)]
    missingRegions(missingones)
  }
  
  df_summary <- df_summary %>% select(-Regioncode)
  df <- df %>% select(-Regioncode)
  
  # for the summary plot, summarize by region
  if (nrow(df_summary) > length(regions)){
    
    df_summary <- df_summary %>% 
      group_by(Region) %>% 
      summarise(Patients = sum(Patients),
                numcenters = max(numcenters),
                PercSites = max(PercSites),
                centers = max(centers),
                #numobserv = sum(numobserv),
                PercPatients = sum(PercPatients))
  }
  
  
  ########## CREATE PLOTS FOR TABLE OF REGIONS ####################
  #trend_google:
  df_summ_hist <- df %>% # this is the small plot in the table
    filter(Year != "ALL")
  
  if (length(valMatch) == 1 && valMatch == "Count"){
    df_summ_hist <- df_summ_hist %>% 
      mutate(tooltip = paste0("Number of patients: ", Patients,", ",
                             " <br>", variableDesc))
  } else {
    df_summ_hist <- df_summ_hist %>% 
      mutate(tooltip = paste0("Number of patients: ", Patients,", ",
                              " <br>",variableDesc, ": ", Value))
  }
  # for summary plot1
  allregionstable <- df_summ_hist
  
  #nest_trend - this is the small plot in the table
  df_summ_hist_nest <- df_summ_hist %>% 
    select(Region, Year, Value, Patients, numcenters,  centers, tooltip) %>% 
    mutate(firstregion = ifelse(startsWith(Region, "AP"), TRUE, FALSE)) %>% 
    nest(data = c(Year, Value, Patients, numcenters, centers, tooltip, firstregion))    
  
  df_for_summary <- df_summary %>% 
    left_join(df_summ_hist_nest)
  
  # is df_summary_detail_hist_nest the same as df_summ_hist
  df_summary_detail_hist_nest <- df %>% 
    filter(Year != "ALL") #%>%  
  # filter(Value %in% codeVals)
  
  # for summary plot2
  allregionsbyleveltable <- df_summary_detail_hist_nest
  
  # this is the plot that opens for inspection:
  if (length(valMatch) == 1 && valMatch == "Count"){
    df_summary_detail_hist_nest <- df_summary_detail_hist_nest %>% 
      mutate(tooltip = paste0("Number of patients: ", Patients,", ",
                             " <br>", variableDesc))
  } else {
    df_summary_detail_hist_nest <- df_summary_detail_hist_nest %>% 
      mutate(tooltip = paste0("Number of patients: ", Patients,", ",
                              " <br>",variableDesc, ": ", Value))
  }
  
  df_summary_detail_hist_nest <- df_summary_detail_hist_nest %>% 
    # mutate(tooltip = paste0("Number of patients: ", Patients,", ",
    #                         " <br>",variableDesc, ": ", Value)) %>% 
    #mutate(tooltip = paste0("Number of patients: ", Patients,", ", Value)) %>% 
    select(Region, Year, Value, Patients, numcenters, centers, tooltip) %>% 
    nest(data = c(Year, Value, Patients, numcenters, centers, tooltip))
  
  if (exists("Age", df_for_summary)) df_for_summary <- df_for_summary %>% select(-Age)
  if (exists("Value", df_for_summary)) df_for_summary <- df_for_summary %>% select(-Value)
  
  displayDetails <- createTooltipInfo(df_for_summary, regionPatientsWithNum)
  summaryTable <- reactable(
    df_for_summary %>% 
      # mutate(displaynum = ifelse(
      #   PercPatients < 0.5 & numobserv > 0,
      #   paste0(numobserv,"<br>(<1%)"),
      #   paste0(numobserv,"<br>(", round(PercPatients, 0), "%)"))) %>% 
      select(Region, Patients, centers, everything()),
    # searchable = TRUE,
    highlight = TRUE,
    # theme = reactableTheme(
    #   # Vertically center cells
    #   cellStyle = list(display = "flex", 
    #                    flexDirection = "column", 
    #                    justifyContent = "center"),
    #   style = list(fontFamily="Brandon Grotesque, Gill Sans")
    # ),
    bordered = TRUE,
    columns = list(
      Region = colDef(name = paste0(groupVar, " <br> (Total Patients)"),
                      maxWidth = 150,
                      align = "center",
                      minWidth = 70,
                      html = TRUE),
      #   
      # Age = colDef(show = FALSE),
      #   #PercPatients = colDef(show = FALSE),
      #Patients = colDef(show = FALSE),
     
      numcenters = colDef(show = FALSE),
      PercSites = colDef(show = FALSE),
      PercPatients = colDef(show = FALSE),
      Patients = colDef(name = "Patients with Data",
                          html = TRUE,
                          headerStyle = list(display = "flex", justifyContent = "center"),
                          align = "right",
                          minWidth = 40,
                           cell = function(value ,index) {
                             div(style = "text-decoration: underline;cursor: help;height: 100%;",
                                 tippy(value, tooltip = displayDetails[[index]]))
                           }, 
                         style = list(padding = 0)
                          # style = color_scales(df_summary,
                          #                      colors = greenCellColors,
                          #                      color_by = "PercPatients",
                          #                      bold_text = TRUE)
      ),
      
      centers = colDef(show = FALSE),
        # name = "Programs with Data",
        #                align = "right",
        #                minWidth = 42),
    #  displaynum = colDef(show = FALSE),
      # style = color_scales(df_summary,
      #                      colors = blueCellColors,
      #                      color_by = "PercSites",
      #                      bold_text = TRUE)),
      data = colDef(name = paste0("Observations per ", timeVar),
                    minWidth = 300,
                    align = "center",
                    cell = function(value){
                      create_plot(value)
                    })
    ),
    details = function(index) {
      regionClick <- df_for_summary$Region[index]
      data_sub <- df_summary_detail_hist_nest[df_summary_detail_hist_nest$Region == regionClick, ]
      data_sub$data[[1]]$Region = regionClick
      reactable(data_sub %>%  mutate(buffer = "  ") %>% select(buffer, everything()),
                columns = list(
                  buffer = colDef(
                    name = "  ",
                    minWidth = 80),
                  Region = colDef(show = FALSE),
                  data = colDef(
                    name = "",
                    minWidth = 200,
                    align = "center",
                    cell = function(value){
                      create_plot_value(value)
                    })
                )
      )
    }
  )
  
  tableReady(TRUE)
  return(list(summaryTable = summaryTable, # reactable tables
              #allregionstable = allregionstable, 
              allregionsbyleveltable = allregionsbyleveltable, #overview
              allregionsbysex = NULL))
})

# GROUP BY SEX for PANEL 2 -> create bySexTable()$overview and $reactable
bySexTable <- eventReactive(input$makeTable, { #initialTable(), {#observeEvent(input$overviewtabs, {
  if (is.null(initialTable())) return(NULL)
# observeEvent(input$overviewtabs, {
  # if not by sex tab, return. if grouped by sex table already complete, return
  # if (!is.null(input$overviewtabs) && 
  #     input$overviewtabs != "bySex") return(NULL)
  # if (input$overviewtabs == "bySex" && !is.null(bySexTable())) return(NULL)
  
  # we know that there's only one code level and we know that there are multiple
  # sex levels. We need to group by sex group level for summary and for 
  # reactable detail

  if (!canGroup()$bySex) return(NULL)
  if (input$var == "MOTHER_ID") return(NULL)
  bySexTableReady(FALSE)
  toShow <- list()

  df <- initialTable()
  varChoice <- input$var
  varDetails <- varInfo[[input$var]]
  variableDesc <- varInfo[[input$var]]$displayName
  
  ageChoice <- ifelse(is.null(ageGroupChosen()), "ALL", ageGroupChosen())
  if (tolower(ageChoice) == "all") ageChoice <- "ALL"
  
  if (ageChoice %in% ageGroups){ # this is a single age group
    df <- df %>% 
      filter(dabAge == ageChoice)
  } else if (tolower(ageChoice) == "all") {
    # include all age groups
    df <- df %>% 
      filter(dabAge == "ALL")
  } else {
    # this is a combo group
    df <- df %>% 
      filter(dabAge %in% agecombos[[ageChoice]]) %>% 
      group_by(Region, Year, PROGRAM, Sex, Value, Variable) %>% 
      summarise(
        Patients = sum(Patients),
        numcenters = max(numcenters),
        PercSites = max(PercSites),
        centers = max(centers)#,
        #numobserv = sum(numobserv)
      ) %>%
      ungroup() %>% 
      mutate(
        dabAge = ageChoice,
        Age = ageChoice
      )
  }
  
  if (nrow(df) == 0) {
    noData(TRUE)
    return(NULL)
  }
  
  df <- df %>% 
    filter(Sex %in% sexGroups)
  
  if (nrow(df) == 0) {
    noData(TRUE)
    return(NULL)
  }
  #################################
  # return this df for overview plot 
  toShow <- list("overview" = df)
  #################################
  

  if (all(df$Value == "Count")){
    valMatch <- "Count"
    codeVals <- "Count"
  } else {
    codeVals <- input$codes # be careful. this could be from last variable
    # first check to see if it's an allcodes variable (no code list in vardetails)
    valMatch <- input$codes
  }
  
  # detect regions missing data for this variable/value
  if (any(!regions %in% unique(df$Region))){
    missingones <- regions[!regions %in% unique(df$Region)]
    missingRegions(missingones)
  }
  
  df <- makeSureEveryRegionEverySex(df)

  df <- left_join(df, regiondatawithprogs) %>% 
    mutate(Regioncode = Region) %>% 
    mutate(Region = RegionLineBreaks) %>% #RegionWithNum) %>% 
    select(-RegionWithNum) %>% 
    mutate(PercPatients = round(100*Patients/TotalPatients, digits =2),
           PercSites = round(100*centers/totalprogs, digits = 1))
  
  # clean up (revisit with respect to region code, etc)
  if (exists("NumPatients", df)) df <- df %>% select(-NumPatients)
  extracols <- c("TotalPatients", "RegionLineBreaks", "totalprogs")
  for (colname in extracols){
    if (exists(colname, df)) df <- df %>% select(- !! rlang::sym(colname))
  }

  # here multiple values should be handled - sum by region
  df_summary <- df %>% 
    filter(Year == "ALL") %>% 
    select(-Year, -Variable, -dabAge, -PROGRAM, #-Sex, 
           -RegionFull) #-TreatNaive, 
  
  if (nrow(df_summary) == 0 || sum(df_summary$Patients) == 0) {
    noData(TRUE)
    return(NULL)
  }
  
  if (any(!regions %in% unique(df_summary$Regioncode))){
    missingones <- regions[!regions %in% unique(df_summary$Regioncode)]
    missingRegions(missingones)
  }
  
  df_summary <- df_summary %>% select(-Regioncode)
  df <- df %>% select(-Regioncode)
  
  # for the summary plot, summarize by region
  if (nrow(df_summary) > length(regions)){
    
    df_summary <- df_summary %>% 
      group_by(Region) %>% 
      summarise(Patients = sum(Patients),
                numcenters = max(numcenters),
                PercSites = max(PercSites),
                centers = max(centers),
                #numobserv = sum(numobserv),
                PercPatients = sum(PercPatients))
  }
  
  
  ########## CREATE PLOTS FOR TABLE OF REGIONS ####################
  #trend_google:
  df_summ_hist <- df %>% 
    filter(Year != "ALL")
  
  if (length(valMatch) == 1 && valMatch == "Count"){
    df_summ_hist <- df_summ_hist %>% 
      mutate(tooltip = paste0("Number of patients: ", Patients,", ",
                             " <br> Sex: ", Sex,
                             " <br>", variableDesc))
  } else {
    df_summ_hist <- df_summ_hist %>% 
      mutate(tooltip = paste0("Number of patients: ", Patients,", ",
                              " <br> Sex: ", Sex,
                              " <br>",variableDesc, ": ", Value))
  }
  # this is the small plot in the table

  # for summary plot1
  allregionstable <- df_summ_hist
  
  #nest_trend - this is the small plot in the table
  df_summ_hist_nest <- df_summ_hist %>% 
    select(Region, Year, Sex, Value, Patients, numcenters,  centers, tooltip) %>% 
    mutate(firstregion = ifelse(startsWith(Region, "AP"), TRUE, FALSE)) %>% 
    nest(data = c(Year, Sex, Value, Patients, numcenters, centers, tooltip, firstregion))    
  
  df_for_summary <- df_summary %>% 
    left_join(df_summ_hist_nest)
  
  # is df_summary_detail_hist_nest the same as df_summ_hist
  df_summary_detail_hist_nest <- df %>% 
    filter(Year != "ALL") #%>%  
  # filter(Value %in% codeVals)
  
  # for summary plot2
  allregionsbyleveltable <- df_summary_detail_hist_nest
  
  if (length(valMatch) == 1 && valMatch == "Count"){
    df_summary_detail_hist_nest <- df_summary_detail_hist_nest %>% 
      mutate(tooltip = paste0("Number of patients: ", Patients,", ",
                             " <br> Sex: ", Sex,
                             " <br>", variableDesc))
  } else {
    df_summary_detail_hist_nest <- df_summary_detail_hist_nest %>% 
      mutate(tooltip = paste0("Number of patients: ", Patients,", ",
                              " <br> Sex: ", Sex,
                              " <br>",variableDesc, ": ", Value))
  }
  
  # this is the plot that opens for inspection:
  df_summary_detail_hist_nest <- df_summary_detail_hist_nest %>% 

    select(Region, Year, Value, Sex, Patients, numcenters, centers, tooltip) %>% 
    nest(data = c(Year, Value, Sex, Patients, numcenters, centers, tooltip))
  
  if (exists("Age", df_for_summary)) df_for_summary <- df_for_summary %>% select(-Age)
  if (exists("Value", df_for_summary)) df_for_summary <- df_for_summary %>% select(-Value)
  displayDetails <- createTooltipInfo(df_for_summary, regionPatientsWithNum)
  
  summaryTable <- reactable(
    df_for_summary %>% 
      # mutate(displaynum = ifelse(
      #   PercPatients < 0.5 & numobserv > 0,
      #   paste0(numobserv,"<br>(<1%)"),
      #   paste0(numobserv,"<br>(", round(PercPatients, 0), "%)"))) %>% 
      select(Region, Patients, centers, everything()),
    # searchable = TRUE,
    highlight = TRUE,
    # theme = reactableTheme(
    #   # Vertically center cells
    #   cellStyle = list(display = "flex", 
    #                    flexDirection = "column", 
    #                    justifyContent = "center"),
    #   style = list(fontFamily="Brandon Grotesque, Gill Sans")
    # ),
    bordered = TRUE,
    columns = list(
      Region = colDef(name = paste0(groupVar, " <br> (Total Patients)"),
                      maxWidth = 150,
                      align = "center",
                      minWidth = 70,
                      html = TRUE),
      #   
      # Age = colDef(show = FALSE),
      #   #PercPatients = colDef(show = FALSE),
     # Patients = colDef(show = FALSE),
      
      numcenters = colDef(show = FALSE),
      PercSites = colDef(show = FALSE),
      PercPatients = colDef(show = FALSE),
      Patients = colDef(name = "Patients with Data",
                         html = TRUE,
                         headerStyle = list(display = "flex", justifyContent = "center"),
                         align = "right",
                         minWidth = 40,
                         cell = function(value ,index) {
                           div(style = "text-decoration: underline;cursor: help;height: 100%;",
                               tippy(value, tooltip = displayDetails[[index]]))
                         }, 
                         style = list(padding = 0)
                         # style = color_scales(df_summary,
                         #                      colors = greenCellColors,
                         #                      color_by = "PercPatients",
                         #                      bold_text = TRUE)
      ),
  # summaryTable <- reactable(
  #   df_for_summary %>%  
  #     mutate(displaynum = ifelse(
  #       PercPatients < 0.5 & numobserv > 0,
  #       paste0(numobserv,"<br>(<1%)"),
  #       paste0(numobserv,"<br>(", round(PercPatients, 0), "%)"))) %>% 
  #     select(Region, displaynum, centers, everything()),
  #   # searchable = TRUE,
  #   highlight = TRUE,
  #   # theme = reactableTheme(
  #   #   # Vertically center cells
  #   #   cellStyle = list(display = "flex", 
  #   #                    flexDirection = "column", 
  #   #                    justifyContent = "center"),
  #   #   style = list(fontFamily="Brandon Grotesque, Gill Sans")
  #   # ),
  #   bordered = TRUE,
  #   columns = list(
  #     Region = colDef(name = "Region <br> (Total Patients)",
  #                     maxWidth = 150,
  #                     align = "center",
  #                     minWidth = 70,
  #                     html = TRUE),
  #     #   
  #     # Age = colDef(show = FALSE),
  #     #   #PercPatients = colDef(show = FALSE),
  #     Patients = colDef(show = FALSE),
  #     numobserv = colDef(show = FALSE),
  #     numcenters = colDef(show = FALSE),
  #     PercSites = colDef(show = FALSE),
  #     PercPatients = colDef(show = FALSE),
  #     displaynum = colDef(name = "Patients with Data",
  #                         html = TRUE,
  #                         headerStyle = list(display = "flex", justifyContent = "center"),
  #                         align = "right",
  #                         minWidth = 40,
  #                         style = color_scales(df_summary,
  #                                              colors = greenCellColors,
  #                                              color_by = "PercPatients",
  #                                              bold_text = TRUE)
  #     ),
      
  centers = colDef(show = FALSE),
  # name = "Programs with Data",
  #                align = "right",
  #                minWidth = 42),
      # style = color_scales(df_summary,
      #                      colors = blueCellColors,
      #                      color_by = "PercSites",
      #                      bold_text = TRUE)),
      data = colDef(name = paste0("Observations per ", timeVar),
                    minWidth = 300,
                    align = "center",
                    cell = function(value){
                      create_plot_sex(value)
                    })
    ),
    details = function(index) {
      regionClick <- df_for_summary$Region[index]
      data_sub <- df_summary_detail_hist_nest[df_summary_detail_hist_nest$Region == regionClick, ]
      data_sub$data[[1]]$Region = regionClick
      reactable(data_sub %>%  mutate(buffer = "  ") %>% select(buffer, everything()),
                columns = list(
                  buffer = colDef(
                    name = "  ",
                    minWidth = 80),
                  Region = colDef(show = FALSE),
                  data = colDef(
                    name = "",
                    minWidth = 200,
                    align = "center",
                    cell = function(value){
                      create_plot_value_sex(value)
                    })
                )
      )
    }
  )
  toShow$reactableTable <- summaryTable
#  bySexTable(toShow)
  bySexTableReady(TRUE)
  return(toShow)
})


#GROUP BY AGE for PANEL 3 -> create byAgeTable()$overview and $reactable
byAgeTable <- eventReactive(input$makeTable, { #initialTable(), {#observeEvent(input$overviewtabs, {
  if (is.null(initialTable())) return(NULL)
print("calculating by age")
  # if not by age tab, return. if grouped by age table already complete, return
  # if (!is.null(input$overviewtabs) &&
  #     input$overviewtabs != "byAge") return(NULL)
  # if (input$overviewtabs == "byAge" && !is.null(byAgeTable())) return(NULL)
  # we know now that the tab choice is byAge

  if (!canGroup()$byAge) return(NULL)
# if filtering by age but choosing a single age group, return NULL (no grouping by age)
  if (input$filterAge && ageGroupChosen() %in% ageGroups) return(NULL)
 # ARE there multiple age groups? What if one?
toShow <- list()
  # we know that there's only one code level and we know that there are multiple
  # age levels. We need to group by age group level for summary and for
  # reactable detail

  df <- initialTable()
  varChoice <- input$var
  varDetails <- varInfo[[input$var]]
  variableDesc <- varInfo[[input$var]]$displayName

  sexChoice <- ifelse(!input$filterSex || is.null(input$sexGroup), "ALL", input$sexGroup)
  ageChoice <- ifelse(is.null(ageGroupChosen()), "ALL", ageGroupChosen())
  if (tolower(ageChoice) == "all") ageChoice <- "ALL"

  # filter by chosen sex group
  df <- df %>%
    filter(Sex == sexChoice)

  # Now we need individual age groups, not ALL
  if (ageChoice == "ALL"){
    df <- df %>%
      filter(dabAge != "ALL")
    agelevels <- ageGroups

  } else {
    # we know it's not a single age group so proceed with assuming combo
      # this is a combo group
    agelevels <- agecombos[[ageChoice]]
      df <- df %>%
        filter(dabAge %in% agelevels)
  }
  df$dabAge <- factor(df$dabAge, levels = agelevels, ordered = TRUE)
  df$Age <- factor(df$Age, levels = agelevels, ordered = TRUE)
  # not this code below because we want to retain individual age levels
  # df <- df %>%
  #       group_by(Region, Year, PROGRAM, Age, Value, Variable) %>%
  #       summarise(
  #         Patients = sum(Patients),
  #         numcenters = max(numcenters),
  #         PercSites = max(PercSites),
  #         centers = max(centers),
  #         numobserv = sum(numobserv)
  #       ) %>%
  #       ungroup() %>%
  #       mutate(
  #         dabAge = ageChoice,
  #         Age = ageChoice
  #       )
    
  if (nrow(df) == 0) {
    noData(TRUE)
    return(NULL)
  }

  #################################
  # return this df for overview plot
  toShow <- list("overview" = df)
  #################################


  if (all(df$Value == "Count")){
    valMatch <- "Count"
    codeVals <- "Count"
  } else {
    codeVals <- input$codes # be careful. this could be from last variable
    # first check to see if it's an allcodes variable (no code list in vardetails)
    valMatch <- input$codes
  }

  # detect regions missing data for this variable/value
  if (any(!regions %in% unique(df$Region))){
    missingones <- regions[!regions %in% unique(df$Region)]
    missingRegions(missingones)
  }

  df <- makeSureEveryRegionEveryAge(df, agelevels)

  df <- left_join(df, regiondatawithprogs) %>%
    mutate(Regioncode = Region) %>%
    mutate(Region = RegionLineBreaks) %>% #RegionWithNum) %>%
    select(-RegionWithNum) %>%
    mutate(PercPatients = round(100*Patients/TotalPatients, digits =2),
           PercSites = round(100*centers/totalprogs, digits = 1))

  # clean up (revisit with respect to region code, etc)
  if (exists("NumPatients", df)) df <- df %>% select(-NumPatients)
  extracols <- c("TotalPatients", "RegionLineBreaks", "totalprogs")
  for (colname in extracols){
    if (exists(colname, df)) df <- df %>% select(- !! rlang::sym(colname))
  }

  # here multiple values should be handled - sum by region
  df_summary <- df %>%
    filter(Year == "ALL") %>%
    select(-Year, -Variable, #-dabAge, 
           -PROGRAM, -Sex,
           -RegionFull) #-TreatNaive,

  if (nrow(df_summary) == 0 || sum(df_summary$Patients) == 0) {
    noData(TRUE)
    return(NULL)
  }

  if (any(!regions %in% unique(df_summary$Regioncode))){
    missingones <- regions[!regions %in% unique(df_summary$Regioncode)]
    missingRegions(missingones)
  }

  df_summary <- df_summary %>% select(-Regioncode)
  df <- df %>% select(-Regioncode)

  # for the summary plot, summarize by region
  if (nrow(df_summary) > length(regions)){

    df_summary <- df_summary %>%
      group_by(Region) %>%
      summarise(Patients = sum(Patients),
                numcenters = max(numcenters),
                PercSites = max(PercSites),
                centers = max(centers),
                #numobserv = sum(numobserv),
                PercPatients = sum(PercPatients))
  }


  ########## CREATE PLOTS FOR TABLE OF REGIONS ####################
  #trend_google:
  df_summ_hist <- df %>%
    filter(Year != "ALL")
  
  if (length(valMatch) == 1 && valMatch == "Count"){
    df_summ_hist <- df_summ_hist %>% 
      mutate(tooltip =paste0("Number of patients: ", Patients,", ",
                             " <br> Age: ", Age,
                             " <br>", variableDesc))
  } else {
    df_summ_hist <- df_summ_hist %>% 
      mutate(tooltip = paste0("Number of patients: ", Patients,", ",
                              " <br> Age: ", Age,
                              " <br>",variableDesc, ": ", Value))
  }
  
  # this is the small plot in the table

  # for summary plot1
  allregionstable <- df_summ_hist

  #nest_trend - this is the small plot in the table
  df_summ_hist_nest <- df_summ_hist %>%
    select(Region, Year, Age, Value, Patients, numcenters,  centers, tooltip) %>%
    mutate(firstregion = ifelse(startsWith(Region, "AP"), TRUE, FALSE)) %>%
    nest(data = c(Year, Age, Value, Patients, numcenters, centers, tooltip, firstregion))

  df_for_summary <- df_summary %>%
    left_join(df_summ_hist_nest)

  # is df_summary_detail_hist_nest the same as df_summ_hist
  df_summary_detail_hist_nest <- df %>%
    filter(Year != "ALL") #%>%
  # filter(Value %in% codeVals)

  # for summary plot2
  allregionsbyleveltable <- df_summary_detail_hist_nest

  # this is the plot that opens for inspection:
  if (length(valMatch) == 1 && valMatch == "Count"){
    df_summary_detail_hist_nest <- df_summary_detail_hist_nest %>% 
      mutate(tooltip =paste0("Number of patients: ", Patients,", ",
                             " <br> Age: ", Age,
                             " <br>", variableDesc))
  } else {
    df_summary_detail_hist_nest <- df_summary_detail_hist_nest %>% 
      mutate(tooltip = paste0("Number of patients: ", Patients,", ",
                              " <br> Age: ", Age,
                              " <br>",variableDesc, ": ", Value))
  }
  
  df_summary_detail_hist_nest <- df_summary_detail_hist_nest %>%
    select(Region, Year, Value, Age, Patients, numcenters, centers, tooltip) %>%
    nest(data = c(Year, Value, Age, Patients, numcenters, centers, tooltip))


  if (exists("Sex", df_for_summary)) df_for_summary <- df_for_summary %>% select(-Sex)
  if (exists("Value", df_for_summary)) df_for_summary <- df_for_summary %>% select(-Value)
  
  displayDetails <- createTooltipInfo(df_for_summary, regionPatientsWithNum)
  
  summaryTable <- reactable(
    df_for_summary %>% 
      # mutate(displaynum = ifelse(
      #   PercPatients < 0.5 & numobserv > 0,
      #   paste0(numobserv,"<br>(<1%)"),
      #   paste0(numobserv,"<br>(", round(PercPatients, 0), "%)"))) %>% 
      select(Region, Patients, centers, everything()),
    # searchable = TRUE,
    highlight = TRUE,
    # theme = reactableTheme(
    #   # Vertically center cells
    #   cellStyle = list(display = "flex", 
    #                    flexDirection = "column", 
    #                    justifyContent = "center"),
    #   style = list(fontFamily="Brandon Grotesque, Gill Sans")
    # ),
    bordered = TRUE,
    columns = list(
      Region = colDef(name = paste0(groupVar, " <br> (Total Patients)"),
                      maxWidth = 150,
                      align = "center",
                      minWidth = 70,
                      html = TRUE),
      #   
      # Age = colDef(show = FALSE),
      #   #PercPatients = colDef(show = FALSE),
      #Patients = colDef(show = FALSE),
      
      numcenters = colDef(show = FALSE),
      PercSites = colDef(show = FALSE),
      PercPatients = colDef(show = FALSE),
      Patients = colDef(name = "Patients with Data",
                         html = TRUE,
                         headerStyle = list(display = "flex", justifyContent = "center"),
                         align = "right",
                         minWidth = 40,
                         cell = function(value ,index) {
                           div(style = "text-decoration: underline;cursor: help;height: 100%;",
                               tippy(value, tooltip = displayDetails[[index]]))
                         }, 
                         style = list(padding = 0)
                         # style = color_scales(df_summary,
                         #                      colors = greenCellColors,
                         #                      color_by = "PercPatients",
                         #                      bold_text = TRUE)
      ),

  # summaryTable <- reactable(
  #   df_for_summary %>%
  #     mutate(displaynum = ifelse(
  #       PercPatients < 0.5 & numobserv > 0,
  #       paste0(numobserv,"<br>(<1%)"),
  #       paste0(numobserv,"<br>(", round(PercPatients, 0), "%)"))) %>%
  #     select(Region, displaynum, centers, everything()),
  #   # searchable = TRUE,
  #   highlight = TRUE,
  #   # theme = reactableTheme(
  #   #   # Vertically center cells
  #   #   cellStyle = list(display = "flex",
  #   #                    flexDirection = "column",
  #   #                    justifyContent = "center"),
  #   #   style = list(fontFamily="Brandon Grotesque, Gill Sans")
  #   # ),
  #   bordered = TRUE,
  #   columns = list(
  #     Region = colDef(name = "Region <br> (Total Patients)",
  #                     maxWidth = 150,
  #                     align = "center",
  #                     minWidth = 70,
  #                     html = TRUE),
  #     #
  #     # Age = colDef(show = FALSE),
  #     #   #PercPatients = colDef(show = FALSE),
  #     Patients = colDef(show = FALSE),
  #     numobserv = colDef(show = FALSE),
  #     numcenters = colDef(show = FALSE),
  #     PercSites = colDef(show = FALSE),
  #     PercPatients = colDef(show = FALSE),
  #     displaynum = colDef(name = "Patients with Data",
  #                         html = TRUE,
  #                         headerStyle = list(display = "flex", justifyContent = "center"),
  #                         align = "right",
  #                         minWidth = 40,
  #                         style = color_scales(df_summary,
  #                                              colors = greenCellColors,
  #                                              color_by = "PercPatients",
  #                                              bold_text = TRUE)
  #     ),

  centers = colDef(show = FALSE),
  # name = "Programs with Data",
  #                align = "right",
  #                minWidth = 42),
      # style = color_scales(df_summary,
      #                      colors = blueCellColors,
      #                      color_by = "PercSites",
      #                      bold_text = TRUE)),
      data = colDef(name = paste0("Observations per ", timeVar),
                    minWidth = 300,
                    align = "center",
                    cell = function(value){
                      create_plot_age(value)
                    })
    ),
    details = function(index) {
      regionClick <- df_for_summary$Region[index]
      data_sub <- df_summary_detail_hist_nest[df_summary_detail_hist_nest$Region == regionClick, ]
      data_sub$data[[1]]$Region = regionClick
      reactable(data_sub %>%  mutate(buffer = "  ") %>% select(buffer, everything()),
                columns = list(
                  buffer = colDef(
                    name = "  ",
                    minWidth = 80),
                  Region = colDef(show = FALSE),
                  data = colDef(
                    name = "",
                    minWidth = 200,
                    align = "center",
                    cell = function(value){
                      create_plot_value_age(value)
                    })
                )
      )
    }
  )
  toShow$reactableTable <- summaryTable
  #byAgeTable(toShow)
  byAgeTableReady(TRUE)

  return(toShow)
})


# eventReactive(input$makeTable,{
#   # if (is.null(input$var)) return(NULL)
#   #if (is.null(input$ageGroups)) return(NULL)
# 
#   if (authRequired && !authenticatedUser()) return(NULL)
# 
#   print("in table to show all")
#   dfByAge <- NULL
#   groupByAge <- FALSE
#   dfBySex <- NULL
#   groupBySex <- FALSE
# 
#   noData(FALSE)
#   missingRegions(NULL)
#   varChoice <- input$var
#   sexChoice <- ifelse(!input$filterSex || is.null(input$sexGroup), "ALL", input$sexGroup)
#   ageChoice <- ifelse(is.null(ageGroupChosen()), "ALL", ageGroupChosen())
#   if (tolower(ageChoice) == "all") ageChoice <- "ALL"
#   
#   varDetails <- varInfo[[input$var]]
#   
# 
#   # 1: filter just this variable
#   df <- alldata %>%
#     filter(Variable == varChoice)
#   
#   ##############
#   # 2: if coded, filter just the codes chosen
#   #is this an observation variable, not a multiple level variable, is it coded?
#   if (all(df$Value == "Count")){
#     hasCodes <- FALSE
#     valMatch <- "Count"
#     codeVals <- "Count"
#   } else {
#     hasCodes <- TRUE
#     codeVals <- input$codes # be careful. this could be from last variable
#     # first check to see if it's an allcodes variable (no code list in vardetails)
#     if (exists("allcodes", varDetails) && varDetails$allcodes){
#       codeList <- allcodeslist()
#       ### JUDY START HERE NOW THAT YOU"RE NOT SELECTING EVERY CODE INITIALLY THIS FALLS APART
#     } else {
#       codeList <- varDetails$codes
#     }
#     valMatch <- input$codes
#   }
#   
#   df <- df %>% 
#     filter(Value %in% valMatch) %>% 
#     mutate(Value = factor(Value, levels = valMatch))
#   # end of step 2
#   ###############
#   
#   dfALL <- df
#   
#   # FILTERING/GROUPING by AGE
#   # need to filter to chosen ages, but preserve individual levels for dfByAge 
#   # First, single age group (then dfByAge is NULL)
#   if (ageChoice %in% ageGroups){ # this is a single age group
#     groupByAge <- FALSE
#     dfOverview <- df %>% 
#       filter(dabAge == ageChoice)
#     
#   } else if (tolower(ageChoice) == "all"){ # this is ALL, which is mult age groups
#     groupByAge <- TRUE
#     # for overview, use ALL
#     dfOverview <- df %>% 
#       filter(dabAge == ageChoice)
#     # for by age, need individual levels
#     dfByAge <- df %>% 
#       filter(dabAge != "ALL")
#   } else { # this is a combo group, filter and group by age
#     groupByAge <- TRUE
#     temp <- df %>% 
#       filter(dabAge %in% agecombos[[ageChoice]])
#  
#     dfByAge <- temp %>% 
#   }
#   
#   
#   # create a data frame that can be grouped by sex at birth
#   if (!input$filterSex){
#     dfBySex <- df %>% 
#       filter(Sex != "ALL") %>% 
#       filter(Sex %in% c("Male", "Female"))
#   } else {
#     dfBySex <- NULL
#   }
#   
#   df <- df %>%
#     filter(Sex == sexChoice)
#   
#   # fix in aggregations JUDY
#   if (input$var == "MOTHER_ID"){
#     df <- df %>% 
#       filter(Value != "ALL")
#   }
# 
#   if (nrow(df) == 0) {
#     noData(TRUE)
#     return(NULL)
#   }
#   
#   variableDesc <- varInfo[[input$var]]$displayName
#   
#   
# 
# 
#   # if this is a Count or one code chosen, option to view by Sex will be available
#   if (!input$filterSex && (!hasCodes || length(codeVals) == 1)){
#     dfBySex <- dfBySex %>% 
#       filter(Value %in% valMatch) %>% 
#       mutate(Value = factor(Value, levels = valMatch))
#   } else {
#     dfBySex <- NULL
#   }
#   
#     
#   # detect regions missing data for this variable/value
#   if (any(!regions %in% unique(df$Region))){
#     missingones <- regions[!regions %in% unique(df$Region)]
#     missingRegions(missingones)
#   }
#   
#   dfOverview <- temp %>% 
#     group_by(Region, Year, PROGRAM, Sex, Value, Variable) %>% 
#     summarise(
#       Patients = sum(Patients),
#       numcenters = max(numcenters),
#       PercSites = max(PercSites),
#       centers = max(centers),
#       numobserv = sum(numobserv)
#     ) %>%
#     ungroup() %>% 
#     mutate(
#       dabAge = ageChoice,
#       Age = ageChoice
#     )
#   
#   df <- makeSureEveryRegionEveryLevel(df, valMatch)
#   
#   df <- left_join(df, regiondatawithprogs) %>% 
#     mutate(Regioncode = Region) %>% 
#     mutate(Region = RegionLineBreaks) %>% 
#     select(-RegionWithNum) %>% 
#     mutate(PercPatients = round(100*Patients/TotalPatients, digits =2),
#            PercSites = round(100*centers/totalprogs, digits = 1))
#   
#   # clean up (revisit with respect to region code, etc)
#   if (exists("NumPatients", df)) df <- df %>% select(-NumPatients)
#   extracols <- c("TotalPatients", "RegionLineBreaks", "totalprogs")
#   for (colname in extracols){
#     if (exists(colname, df)) df <- df %>% select(- !! rlang::sym(colname))
#   }
# 
#  # here multiple values should be handled - sum by region
#   df_summary <- df %>% 
#     filter(Year == "ALL") %>% 
#     select(-Year, -Variable, -dabAge, -PROGRAM, -Sex, -RegionFull) #-TreatNaive, 
#   
#   if (nrow(df_summary) == 0 || sum(df_summary$Patients) == 0) {
#     noData(TRUE)
#     return(NULL)
#   }
# 
#   if (any(!regions %in% unique(df_summary$Regioncode))){
# 
#     missingones <- regions[!regions %in% unique(df_summary$Regioncode)]
#     missingRegions(missingones)
#   }
#   
#   df_summary <- df_summary %>% select(-Regioncode)
#   df <- df %>% select(-Regioncode)
# 
#   # for the summary plot, summarize by region
#   if (nrow(df_summary) > length(regions)){
# 
#     df_summary <- df_summary %>% 
#       group_by(Region) %>% 
#       summarise(Patients = sum(Patients),
#                 numcenters = max(numcenters),
#                 PercSites = max(PercSites),
#                 centers = max(centers),
#                 numobserv = sum(numobserv),
#                 PercPatients = sum(PercPatients))
#   }
#   
# 
#   if (hasCodes && length(input$codes) == 0){
# 
#   } else {
# 
#     #trend_google:
#     df_summ_hist <- df %>% 
#       filter(Year != "ALL") %>% # this is the small plot in the table
#       mutate(tooltip = paste0("Number of patients: ", Patients,", ",
#                               " <br>",variableDesc, ": ", Value))
#     # for summary plot1
#     allregionstable <- df_summ_hist
# 
#     #nest_trend - this is the small plot in the table
#     df_summ_hist_nest <- df_summ_hist %>% 
#       select(Region, Year, Value, numobserv, numcenters,  centers, tooltip) %>% 
#       mutate(firstregion = ifelse(startsWith(Region, "AP"), TRUE, FALSE)) %>% 
#       nest(data = c(Year, Value, numobserv, numcenters, centers, tooltip, firstregion))    
# 
#     df_for_summary <- df_summary %>% 
#       left_join(df_summ_hist_nest)
#     
#     ##### now create tables for detail - I don't think this is used
#     # df_summary_for_detail <- df %>% 
#     #   filter(Year == "ALL") %>% 
#     #  # filter(Value %in% valMatch) %>% 
#     #   select(-Year, -dabAge, -Variable, -Value, -Sex) #-TreatNaive,
#     
#     # is df_summary_detail_hist_nest the same as df_summ_hist
#     df_summary_detail_hist_nest <- df %>% 
#       filter(Year != "ALL") #%>%  
#      # filter(Value %in% codeVals)
#     
#     # for summary plot2
#     allregionsbyleveltable <- df_summary_detail_hist_nest
#     
#     # this is the plot that opens for inspection:
#     df_summary_detail_hist_nest <- df_summary_detail_hist_nest %>% 
#       mutate(tooltip = paste0("Number of patients: ", Patients,", ", Value)) %>% 
#       select(Region, Year, Value, numobserv, numcenters, centers, tooltip) %>% 
#       nest(data = c(Year, Value, numobserv, numcenters, centers, tooltip))
#     
#     if (exists("Age", df_for_summary)) df_for_summary <- df_for_summary %>% select(-Age)
#     if (exists("Value", df_for_summary)) df_for_summary <- df_for_summary %>% select(-Value)
# 
#     summaryTable <- reactable(
#       df_for_summary %>%  
#       mutate(displaynum = ifelse(
#         PercPatients < 0.5 & numobserv > 0,
#         paste0(numobserv,"<br>(<1%)"),
#         paste0(numobserv,"<br>(", round(PercPatients, 0), "%)"))) %>% 
#       select(Region, displaynum, centers, everything()),
#       # searchable = TRUE,
#       highlight = TRUE,
#       # theme = reactableTheme(
#       #   # Vertically center cells
#       #   cellStyle = list(display = "flex", 
#       #                    flexDirection = "column", 
#       #                    justifyContent = "center"),
#       #   style = list(fontFamily="Brandon Grotesque, Gill Sans")
#       # ),
#       bordered = TRUE,
#       columns = list(
#         Region = colDef(name = "Region <br> (Total Patients)",
#                         maxWidth = 150,
#                         align = "center",
#                         minWidth = 70,
#                         html = TRUE),
#         #   
#         # Age = colDef(show = FALSE),
#         #   #PercPatients = colDef(show = FALSE),
#         Patients = colDef(show = FALSE),
#         numobserv = colDef(show = FALSE),
#         numcenters = colDef(show = FALSE),
#         PercSites = colDef(show = FALSE),
#         PercPatients = colDef(show = FALSE),
#         displaynum = colDef(name = "Patients with Data",
#                             html = TRUE,
#                             headerStyle = list(display = "flex", justifyContent = "center"),
#                             align = "right",
#                             minWidth = 40,
#                             style = color_scales(df_summary,
#                                                  colors = greenCellColors,
#                                                  color_by = "PercPatients",
#                                                  bold_text = TRUE)
#         ),
#         
#         centers = colDef(name = "Programs with Data",
#                          align = "right",
#                          minWidth = 42),
#         # style = color_scales(df_summary,
#         #                      colors = blueCellColors,
#         #                      color_by = "PercSites",
#         #                      bold_text = TRUE)),
#         data = colDef(name = "Observations per Year",
#                       minWidth = 300,
#                       align = "center",
#                       cell = function(value){
#                         create_plot(value)
#                       })
#       ),
#       details = function(index) {
#         regionClick <- df_for_summary$Region[index]
#         data_sub <- df_summary_detail_hist_nest[df_summary_detail_hist_nest$Region == regionClick, ]
#         data_sub$data[[1]]$Region = regionClick
#         reactable(data_sub %>%  mutate(buffer = "  ") %>% select(buffer, everything()),
#                   columns = list(
#                     buffer = colDef(
#                       name = "  ",
#                       minWidth = 80),
#                     Region = colDef(show = FALSE),
#                     data = colDef(
#                       name = "",
#                       minWidth = 200,
#                       align = "center",
#                       cell = function(value){
#                         create_plot_value(value)
#                       })
#                   )
#         )
#       }
#     )
#   }
#   tableReady(TRUE)
#   return(list(summaryTable = summaryTable, # reactable tables
#               allregionstable = allregionstable, 
#               allregionsbyleveltable = allregionsbyleveltable, # overview
#               allregionsbysex = dfBySex)) # overview panel 2
# })

