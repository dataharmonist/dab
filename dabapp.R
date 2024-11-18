#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(bslib)
library(tidyverse)
library(ggplot2)
library(data.table)
library(DT)
library(gt)
library(reactable)
library(reactablefmtr)
library(highcharter)
library(plotly)
library(shinyWidgets)
library(httr)
library(rjson)
library(tools)
library(jsonlite)
library(openssl)
library(tippy)
options(scipen = 999)

redcap_url <- "https://redcap.vanderbilt.edu/api/"
authRequired <- FALSE #TRUE # set to FALSE to remove authentication requirement

source("redcapTokens.R", local = TRUE)
source("REDCapCore.R", local = TRUE)
source("helpers.R", local = TRUE)
source("dabAppHelpers.R", local = TRUE)

blueCellColors <- c("#dbe3ed", "#9ab1cc", "#597fab")
greenCellColors <- c("#E9F8CB", "#A4DAAD", "#4AB8AB")

highchartcolors <- c(
  "#7cb5ec", #blue
  
  "#434348", #dark gray
  
  "#90ed7d", #green
  
  "#f7a35c", #orange
  
  "#8085e9", #purple
  
  "#f15c80", #pink
  
  "#e4d354", #gold
  
  "#2b908f", #dark green
  
  "#f45b5b", #red
  
  "#91e8e1", #minty blue/green
  
  "#E0A4B3", #blossom
  
  "#CA21BC", #deep magenta
  
  "#967969"  #mocha
)

source("consortiaDetails.R", local = TRUE)

varInfo <- rjson::fromJSON(file = "varChoices.json")
ageGroups <- names(rjson::fromJSON(file = "dabAgeGroups.json"))
ageGroupDetails <- rjson::fromJSON(file = "dabAgeGroups.json")
numberAgeGroups <- length(ageGroups)
maxLowerAge <- ageGroupDetails[[numberAgeGroups]]$min
minUpperAge <- ageGroupDetails[[1]]$max + 1
sliderPoints <- c(unlist(lapply(ageGroupDetails, function(x){return(x$min)})),
                  100)

agecombos <- makeAgeGroupCombos(ageGroups, ageGroupDetails)

# define UI
ui <- fluidPage(
  
  titlePanel("Data Availability Browser"),
  
  # Sidebar
  # no sidebar for this application since it runs in an iframe
  
  # body 
  uiOutput("tableHeader"),
  uiOutput("sidebarContent"),
  #uiOutput("banner"), # hide for now
  uiOutput("noDataMessage"),
  
  uiOutput("overviewUI"), #panel1: allregionsbylevel
  uiOutput("missingDataAlert"),
  #uiOutput("regionTableHeader"), 
  #reactableOutput("allRegionsTable"), # summaryTable 
  htmlOutput("linebreaks"),
  #uiOutput("allregionsoverviewUI"),
  
  tags$style(HTML("
    .irs--shiny .irs-shadow{
      height: 0px;
    }"
  )),
  tags$head(
    #   #tags$link(rel = "stylesheet", type = "text/css", href = "AdminLTE.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  )
)


# Define server logic 
server <- function(input, output, session) {
  testUser <- reactiveVal(FALSE) # set to TRUE if you don't want records added to H17
  
  sessionID <- reactive(session$token)
  # since modalFunctions references sessionID() object, include here
  source("modalFunctions.R", local = TRUE)
  source("REDCapHelpers.R", local = TRUE)
  source("maketableNew.R", local = TRUE)
  
  # authenticatedUser will be TRUE once user token has been confirmed in REDCap
  # Once session ends, set as FALSE
  authenticatedUser <- reactiveVal(FALSE)
  
  # recordDeleted reactive variable to indicate that record in Harmonist 18 security has been deleted
  # if the user has entered with an encrypted token from the Hub
  recordDeleted <- reactiveVal(FALSE)
  
  # bySexTable <- reactiveVal(NULL)
 # byAgeTable <- reactiveVal(NULL)
  
  getUserInfo <- reactive({
    if (!authRequired) return(NULL)
    invalidToken <- TRUE
    authenticatedUser(FALSE)
    
    query <- parseQueryString(session$clientData$url_search)
    encryptedToken <- query[["tokendab"]]
    # case 1: no token in dab url - returning NULL will trigger error modal
    if (is.null(encryptedToken)){
      return(NULL)
    }

    # case 2: token exists, decrypt it
    decryptedToken <- getCrypt(encryptedToken,"d")
    
    # case 2a: unable to decrypt token
    if (is.null(decryptedToken)){
      authenticatedUser(FALSE)
      return(NULL)
    } 
    
    # now use token to get user info
    userInfo <- getOneRecord(tokenForHarmonist18, decryptedToken,
                             projectName = "Harmonist 18")
    if (length(userInfo) == 1 && userInfo == ""){
      errorMessageModalDAB(messageHeader = "REDCap Error",
                           message = "Inability to access REDCap for authentication at this time",
                           secondaryMessage = "Please contact judy.lewis@vumc.org if this error persists")

      return("")
    }
    # case 2b: token doesn't work/redcap not responding
    if (inherits(userInfo, "postFailure")) {
      errorMessageModalDAB(messageHeader = "REDCap Error",
                           message = "Inability to access REDCap for authentication at this time",
                           secondaryMessage = "Please contact judy.lewis@vumc.org if this error persists")

      return(NULL)
    }
    # check once more - if valid token then userInfo will be list of 14 elements
    if (is_empty(userInfo) || length(userInfo) < 2){
      invalidToken <-  TRUE
    } else {
      # valid token, check to make sure hasn't expired
      expiration <- userInfo$tokenexpiration_ts
      # if no expiration date in record OR if expiration date has passed, invalid token
      if (is_blank(expiration) || (Sys.Date() - as.Date(expiration) > 1)){
        invalidToken <- TRUE
      } else {
        #success! token is valid
        invalidToken <- FALSE
        # Case 2d: valid token! retrieve info 
        cat("Session:", isolate(sessionID())," valid token","\n", file = stderr())  
        authenticatedUser(TRUE)
        # JUDY delete token here?
      }
    }
    return(userInfo)
  })
  
  
  #  At beginning of session this will be triggered and will obtain user info and determine
  #  if the user entered from the hub or not
  observeEvent(session$token,{
    authenticatedUser(FALSE)
    print(paste("session =", session$token))
    
    userInfo <- getUserInfo()
    if (is.null(userInfo) && authRequired && !authenticatedUser()){
      print("not authenticated user")
      errorMessageModalDAB(messageHeader = "Valid authorization required to view",
                           secondaryMessage = "Follow up instructions...")
      
    } else if (is.null(userInfo)){
      print("auth not required and unknown user")
    } else {
      print("we do have user info")
      if (length(userInfo) == 1 && userInfo == ""){
        errorMessageModalDAB(messageHeader = "REDCap Error",
                             message = "Inability to access REDCap for authentication at this time",
                             secondaryMessage = "Please contact judy.lewis@vumc.org if this error persists")
        
      } else {
        regionID <- as.numeric(userInfo[["uploadregion_id"]])
        print(regionID)
        postProgressDAB(list(action_step = "start", 
                             userregion_id = regionID,
                             toolkituser_id = as.numeric(userInfo[["uploaduser_id"]])))
      }
    }
  })
  
  alldata <- readDabAgg()

  allcodeslist <- reactiveVal(NULL)
  missingRegions <- reactiveVal(NULL)
  allregionstable <- reactiveVal(NULL)
  allregionsbyleveltable <- reactiveVal(NULL)
  tableReady <- reactiveVal(FALSE)
  noData <- reactiveVal(FALSE)
  byAgeTableReady <- reactiveVal(FALSE)
  bySexTableReady <- reactiveVal(FALSE)
  
  output$sidebarContent <- renderUI({
    fluidRow(
      column(
        12,
        wellPanel(
          fluidRow(
            column(
              6,
              uiOutput("chooseVar"),
              uiOutput("chooseCodes")
            ),
            column(
              6, 
              uiOutput("filterAgeUI"),
              textOutput("informAgeGroup"),
              uiOutput("chooseAgeGroup"),
              uiOutput("filterSexUI"),
              uiOutput("chooseSex"),
              htmlOutput("linebreaks2"),
              uiOutput("tableButton")
            )
          )
        )
      )
    )
    
  })

  ageGroupChosen <- reactive({
    if (is.null(input$range)) return(NULL)
    if (!input$filterAge) return(NULL)
    
    lower <- input$range[[1]]
    upper <- input$range[[2]]
    newRange <- "All"
    
    if (lower == upper){
      return(NULL)
    }
    numpoints <- length(sliderPoints)
 
    # case 1: entire range
    if (lower == sliderPoints[[1]] && 
        upper == sliderPoints[[numpoints]]){
      newRange <- "All"
    } else if (upper == sliderPoints[[numpoints]]){
      # case 2: if upper selection is 100, use +
      newRange <- paste0(lower, "+")
    } else {
      newRange <- paste0(lower, "-", (upper-1))
    }


    return(newRange)
  })
  
  observeEvent(input$range,{
    if (input$range[[1]] != input$range[[2]]) return(NULL)
    lower <- input$range[[1]]
    index <- which(sliderPoints == lower)
    updateSliderTextInput(
      session = session,
      inputId = "range",
      choices = sliderPoints,
      selected = c(lower,
                   sliderPoints[[index + 1]])
    )
  },
  ignoreNULL = TRUE,
  ignoreInit = TRUE
  )
  
  textDescribingAgeSex <- reactive({

    if (!input$filterAge){
      ageText <- ""
    } else {
      ageGroup <- ageGroupChosen()
      if (is.null(ageGroup)) return(NULL)
      if (!is.null(ageGroup) && ageGroup == "All"){
        ageText <- ""
      } else{
        ageText <- paste0("ages ",
                          ageGroup,
                          " years")
      }
    }
    if (!input$filterSex){
      sexText <- ""
    } else {
      sexText <- paste0(tolower(input$sexGroup), " at birth")
    }
    connector <- ""
    if (ageText == "" && sexText == ""){
      textOut = ""
    } else {
      if (ageText != "" && sexText != ""){
        connector <- " and "
      }
      textOut <- paste0(" (",ageText, connector, sexText, ")")
    } 
    
    return(textOut)
  })
  if (consortiumName == "MWCCS"){
    overview <- alldata %>%
      filter(Variable == "SEX") %>% 
      filter(Value == "ALL")
  } else {
  overview <- alldata %>%
    filter(Variable == "SEX") %>% 
    filter(Value == "ALL")
  }
  
  regionsInData <- sort(unique(overview$Region))
  regionPatients <- list()
  templist <- list()

  for (region in regionsInData){
    regionPatients[[region]] <- alldata %>% 
      filter(Region == region, 
             Variable == "SEX", 
             Year == "ALL",
             #TreatNaive == "ALL",
             Sex == "ALL",
             dabAge == "ALL",
             Value == "ALL") %>% 
      pull(Patients)
    
    templist[[region]] <- tibble(Region = region, 
                                 TotalPatients = regionPatients[[region]],
                                 RegionWithNum = paste0(region, " (",regionPatients[[region]]," Patients)"),
                                 RegionLineBreaks = paste0(fullRegionNames[[region]], 
                                                           "<br>(",regionPatients[[region]],
                                                           " Patients)"),
                                 RegionFull = fullRegionNames[[region]]
                                 )
  }
  regionPatientsWithNum <- rbindlist(templist, use.names = TRUE)
  regiondatawithprogs <- regionPatientsWithNum
  regiondatawithprogs$totalprogs <- totalProgsPerRegion
  regiondatawithprogs$Regioncode <- regiondatawithprogs$Region
  regionLineBreakAndCode <- regionPatientsWithNum %>% 
    mutate(Regioncode = Region,
           Region = RegionLineBreaks) %>% 
    select(Region, Regioncode)
  
  # figure out how to calculate global variable of num pts in each region and
  # number of PROGRAMS in each region
  
  variables <- names(varInfo)
  displayNames <- sapply(varInfo, function(x){return(x$displayName)})
  names(variables) <- displayNames
  
  # make a list of all coded variables
  codedVars <- names(unlist(sapply(varInfo, function(x){
    if (!is.null(x$codes) || (exists("allcodes", x) && x$allcodes) ) return(TRUE)
  })))

  codesInOrder <- orderCodesByFreq(codedVars, alldata)

  output$chooseVar <- renderUI({
    if (authRequired && !authenticatedUser()) return("User not authenticated")
    # restrict choices to variables in aggregate data
    
    varsInData <- unique(alldata$Variable)
    varsToChoose <- variables[variables %in% varsInData]
    if (consortiumName == "MWCCS"){
      varsToChoose <- varsToChoose[varsToChoose != "SEX"]
    }
    chooseone <- ""
    names(chooseone) <- "Variable options"
    varChoices <- c(chooseone, varsToChoose)
    
    selectInput("var", 
                "Choose one variable to explore",
                choices = varChoices,
                selected = NULL)
  })
  
  # output$chooseNaive <- renderUI({
  #   req(input$var)
  #   checkboxInput("naive",
  #                 label = "Restrict results to patients who were ART-Naive at enrollment"
  #   )
  # })
  
  output$filterAgeUI <- renderUI({
    req(input$var)
    ageAtValue <- varInfo[[input$var]][["dateWord"]]
    checkboxInput(
      "filterAge",
      label = paste0("Filter results by patient age at ", ageAtValue),
      value = FALSE
      )
  })
  
  
  output$chooseAgeGroup <- renderUI({
    req(input$var)
    req(input$filterAge)
    if (input$var == "") return(NULL)
    if (!input$filterAge) return(NULL)

    sliderTextInput(
      inputId = "range", 
      label = NULL,  
      choices = sliderPoints, 
      selected = range(sliderPoints), 
      from_max = maxLowerAge,
      to_min = minUpperAge,
      grid = FALSE,
      width = "80%"
    )
  })

  output$informAgeGroup <- renderText({
    req(input$filterAge)
    req(input$range)
    if (!input$filterAge) return(NULL)

    return(paste0(ageGroupChosen(), " years"))
  })

  output$filterSexUI <- renderUI({
    req(input$var)

    checkboxInput(
      "filterSex",
      "Filter results by patient sex at birth",
      value = FALSE
    )
    
  })
  
  output$chooseSex <- renderUI({
    req(input$var)
    if (input$var == "") return(NULL)
    req(input$filterSex)
    if (!input$filterSex) return(NULL)
    radioButtons("sexGroup",
                       label = NULL, #"Select sex at birth group(s) of interest",
                       choices = sexGroups, #defined in consortiaDetails.R
                       selected = NULL,
                       inline = TRUE)
  })
  
  output$chooseCodes <- renderUI({
    req(input$var)
    if (input$var == "") return(NULL)

    varDetails <- varInfo[[input$var]]
    listOfRegions <- unique(alldata$Region) ## why
    numRegions <- length(listOfRegions)
    additionalMessage <- ""
    
    varName <- input$var
    if (is.null(input$range) || is.null(input$filterAge) ||
        !input$filterAge || is.null(ageGroupChosen()) || 
        tolower(ageGroupChosen()) == "all"){
      ageGroupChoice <- "ALL"
    } else {
      ageGroupChoice <- ageGroupChosen()
    }
    
    if (is.null(input$filterSex) || !input$filterSex) {
      sexChoice <- "ALL"
    } else {
      sexChoice <- ifelse(is.null(input$sexGroup), "ALL", input$sexGroup)
    }
    
    if (varName %in% codedVars){
      # for coded variables
      varchoices <- codesInOrder$codeFreq[[varName]][[paste0(ageGroupChoice,"_", sexChoice)]][["Value"]]
      names(varchoices) <- codesInOrder$codeFreq[[varName]][[paste0(ageGroupChoice,"_", sexChoice)]][["codeLabel"]]
      
    } else { # not a coded variable
      varchoices <- character(0)
      selection <- character(0)
      return(checkboxGroupInput("codes",
                                "",
                                choices = varchoices,
                                selected = selection))
    }
    
    if (length(varchoices) >= 6){
      if (!is.null(input$codes) &&
          all(input$codes %in% varchoices)){
        selection <- input$codes
      } else {
        selection <- character(0)
      }
      
      inputUI <- tagList(
        selectizeInput("codes", 
                       label = "Select values of interest (Number of patients with data)", 
                       choices = varchoices,
                       selected = selection, 
                       multiple = TRUE,
                       options = list(maxItems = 10))
      )
    } else {
      if (length(varchoices) == 1){
        selection <- varchoices
      } else {
        if (!is.null(input$codes) &&
            all(input$codes %in% varchoices)){
          selection <- input$codes
        } else {
          selection <- character(0)
        }
      }
      inputUI <- checkboxGroupInput("codes",
                                    "Select values of interest (Number of patients with data)",
                                    choices = varchoices,
                                    selected = selection)
    }
    
    allcodeslist(varchoices) 
    return(
      tagList(
        inputUI,
        
        tags$h6(codesInOrder$additionalMessages[[varName]])
      ))
  })
  
  output$tableButton <- renderUI({
    req(input$var)
    #req(input$ageGroups)
    varDetails <- varInfo[[input$var]]

    if (!is.null(varDetails$codes)){
      req(input$codes)
    } 
    if (input$var %in% codedVars){
      req(input$codes)
    }
    if (tableReady()) return(NULL)
    actionButton("makeTable", "Explore Availability", class="btn-primary")
  })
  
  output$noDataMessage <- renderUI({
    if (!noData()) return(NULL)
    return(
      shinyWidgets::alert(
        #tags$h3(class = "row_text title", 
                "No data for this selection from any region",#),
        status = "warning"
    )
    )
  })
  
  canGroup <- reactive({
    bySex <- FALSE
    byAge <- FALSE
    if ( ((!is.null(varInfo[[input$var]]$allcodes) && varInfo[[input$var]]$allcodes) ||
          !is.null(varInfo[[input$var]]$codes)) && (length(input$codes) > 1) ){
      # if more than one code is selected, no option for by sex or by age
      bySex <- FALSE
      byAge <- FALSE
    } else {
      # we know there's just one code. 
      if (!input$filterSex){
        bySex <- TRUE
      }
      # if not filtering by age or if multiple age groups chosen, allow grouping
      # by age and enable Panel 3
      if (!input$filterAge ||
          (input$filterAge && !ageGroupChosen() %in% ageGroups)){
        byAge <- TRUE
      }
    }
    return(list(bySex = bySex, byAge = byAge))
  })
  
  output$overviewUI <- renderUI({
    if (is.null(overviewTable())) return(NULL)
    if (!tableReady()) return(NULL)
    info <- textDescribingAgeSex()
    byAgeTable()
    bySexTable()
    headertext <- paste0(
      "Overview: Number of Patients from ",
      consortiumName,
      " with Data",
      info
    )
    panel2 <- NULL
    panel3 <- NULL
    
    # panel 1 will always be populated
    
    if ( ((!is.null(varInfo[[input$var]]$allcodes) && varInfo[[input$var]]$allcodes) ||
        !is.null(varInfo[[input$var]]$codes)) && (length(input$codes) > 1) ){
      # if more than one code is selected, no option for by sex or by age
      panel2 <- NULL
      panel3 <- NULL
    } else {
      if (input$var == "MOTHER_ID"){
        # dont provide grouping by Sex
        panel2 <- NULL
      } else{
        # we know there's just one code. 
        if (!input$filterSex){
          
          sexheadertext <- paste0(
            "Number of Patients with Data",
            info, ", Grouped by Sex at Birth"
          )
          ps <- plotlyOutput("overviewbysex", height = "350px")
          panel2 <- tabPanel(
            title = "By Sex",
            value = "bySex",
            panel(
              heading = sexheadertext,
              ps,
              style = "padding: 5px; border-color: darkgray",
              reactableTableHeader(),
              reactableOutput("bySexReactable")
            )
          )
      }
      
      }
      # if not filtering by age or if multiple age groups chosen, allow grouping
      # by age and enable Panel 3
      if (!input$filterAge ||
           (input$filterAge && !ageGroupChosen() %in% ageGroups)){
        ageheadertext <- paste0(
          "Number of Patients with Data",
          info, ", Grouped by Age"
        )
        pa <- plotlyOutput("overviewbyage", height = "350px")
        panel3 <- tabPanel(
          title = "By Age",
          value = "byAge",
          panel(
            heading = ageheadertext,
            pa,
            style = "padding: 5px; border-color: darkgray",
            reactableTableHeader(),
            reactableOutput("byAgeReactable")
          )
        )
      }
    }

    
    tabsetPanel(
      id = "overviewtabs",
      tabPanel(
        title = "Overview",
        value = "overview",
        panel(
          heading = headertext,
          plotlyOutput("allregionsbylevel", height = "350px"),
          style = "padding: 5px; border-color: darkgray",
          reactableTableHeader(),
          reactableOutput("allRegionsTable"), # summaryTable 
        )
      ),
      panel2,
      panel3
    )
  })
  
  # this is in the Overview panel 1
  output$allregionsbylevel <- renderPlotly({
    if (is.null(overviewTable())) return(NULL)
    if (!tableReady()) return(NULL)
    input$makeTable
    
    tableData <- overviewTable()$allregionsbyleveltable
    extratext2 <- ""#textDescribingAgeSex()

    # first if this is not coded variable:
    if ( is.null(varInfo[[input$var]]$codes) &&
         is.null(varInfo[[input$var]]$allcodes)) { #is.null(input$codes)){
      browser()
      tableData <- tableData %>% 
        group_by(Year) %>% 
        summarise(Patients = sum(Patients),
                  numcenters = max(numcenters),
                  PercSites = max(PercSites),
                  centers = max(centers),
                  #numobserv = max(numobserv),
                  PercPatients = sum(PercPatients)) %>% 
        rename(Percent = PercPatients)   %>% 
        ungroup() %>% 
        mutate(Year = as.numeric(Year))
      p <- ggplot(tableData %>% rename({{ timeVar }} := Year), 
                  aes(x = .data[[timeVar]], 
                      y = Patients, 
                      name = Patients, 
                      group = 1,
                      text = paste0(varInfo[[input$var]]$displayName))) +
        geom_line(color = highchartcolors[[1]]) +
        geom_point(color = highchartcolors[[1]]) + ylab("Number of Patients") +
        xlim(minyear, maxyear) +
        # labs(col = input$var,
        #      shape = input$var) +
        ggtitle(paste0(#"Number of Patients from All IeDEA Regions with Data: ",
          varInfo[[input$var]][["displayName"]], 
          " ", extratext2)) +
        theme_bw()
    } else {
      # then we know this is a coded variable
      extratext <- ""
      if ((!is.null(varInfo[[input$var]]$allcodes) && varInfo[[input$var]]$allcodes) ||
          !is.null(varInfo[[input$var]]$codes)){
        if (length(input$codes) > 1){
          extratext <- " = Selected values"
        } else if (length(input$codes == 1)){
          extratext <- paste0(" = ", input$codes)
        }
      }
      
      tableData <- tableData %>% 
        mutate(Value = factor(Value)) %>% 
        group_by(Year, Value) %>% 
        summarise(Patients = sum(Patients),
                  numcenters = max(numcenters),
                  PercSites = max(PercSites),
                  centers = max(centers),
                  #numobserv = max(numobserv),
                  PercPatients = sum(PercPatients)) %>% 
        rename(Percent = PercPatients) %>% 
        ungroup() %>% 
        mutate(Year = as.numeric(Year))
      
      p <- ggplot(tableData %>% rename({{ timeVar }} := Year), 
                  aes(x = .data[[timeVar]],  y = Patients, 
                                 group = Value,
                                 name = Patients, 
                                 col = Value,
                                 shape = Value,
                                 text = paste0(
                                   varInfo[[input$var]]$displayName,
                                   ": ", Value))) +
        geom_line() +
        geom_point() + ylab("Number of Patients") +
        labs(col = input$var,
             shape = input$var) +
        scale_color_manual(values=highchartcolors) +
        xlim(minyear, maxyear) +
        ggtitle(paste0(#"Number of Patients from All IeDEA Regions with Data: ",
          varInfo[[input$var]][["displayName"]],
          extratext, " ", extratext2)) +
        theme_bw()
    }
    # here, add tooltip content JUDY

    return(ggplotly(p, tooltip = c("y", "x", "text")) %>% 
             plotly::layout(
               xaxis = list(
                 title = list(text = timeVar, font = list(size = 14, family = "Arial black")),
                 tickfont = list(family = "Arial black")#,
                 # tickangle = -45
               ),
               yaxis = list(
                 title = list(text = 
                                "Number of Patients", font = list(size = 14, family = "Arial black")),
                 tickfont = list(family = "Arial black")
               ),
               legend = list(
                 # font = list(
                 #          family = "Arial",
                 #          size = 8
                 #          ),
                 orientation = "h",
                 y = -.25
               ),
               autosize = TRUE,
               margin = list(
                 l = 50, # adjust the left margin
                 r = 50, # adjust the right margin
                 t = 50,  # adjust the top margin
                 b = 50   # adjust the bottom margin
               ) ) %>% 
             plotly::config(displaylogo = FALSE,
                            modeBarButtonsToRemove = list('hoverClosestCartesian',
                                                          'hoverCompareCartesian',
                                                          'autoScale2d',
                                                          'lasso2d',
                                                          'select2d',
                                                          'zoom2d',
                                                          'toImage'
                            ))
    )
  })
  
  
  # this is tab panel 2 if count or one level chosen
  output$overviewbysex <- renderPlotly({
    #req(bySexTable())
    if (!tableReady()) return(NULL)
    input$makeTable
#browser()
    #if (!bySexTableReady()) return(NULL)
    if (is.null(bySexTable())) return(NULL)
    tableData <- bySexTable()$overview %>% filter(Year != "ALL")
    extratext <-  ""
    extratext2 <- ""

    # first if this is not coded variable:
    if ( is.null(varInfo[[input$var]]$codes) &&
         is.null(varInfo[[input$var]]$allcodes)) { #is.null(input$codes)){
      tableData <- tableData %>% 
        group_by(Year, Sex) %>% 
        summarise(Patients = sum(Patients)) %>% 
        ungroup() %>% 
        mutate(Year = as.numeric(Year))

    } else {
      # then we know this is a coded variable
      extratext <- ""
      if ((!is.null(varInfo[[input$var]]$allcodes) && varInfo[[input$var]]$allcodes) ||
           !is.null(varInfo[[input$var]]$codes)){
        if (length(input$codes) > 1){
          #should not be possible
          extratext <- " = Selected values"
        } else if (length(input$codes == 1)){
          extratext <- paste0(" = ", input$codes)
        }
      }

      tableData <- tableData %>% 
        group_by(Year, Sex) %>% 
        summarise(Patients = sum(Patients)) %>% 
        ungroup() %>% 
        mutate(Year = as.numeric(Year))
    }
      
      p <- ggplot(tableData %>% rename({{ timeVar }} := Year), 
                  aes(x = .data[[timeVar]], y = Patients, fill = Sex,
                                 text = paste0("Sex: ", Sex))) +
                                # name = Patients, 
                                # col = Sex)) +
        geom_bar(stat = "identity") + ylab("Number of Patients") +
        scale_fill_manual(values=highchartcolors[1:2]) +
        xlim(minyear-1/2, maxyear+1) + # to include minyear and maxyear with bar plot
        ggtitle(paste0(#"Number of Patients from All IeDEA Regions with Data: ",
                       varInfo[[input$var]][["displayName"]],
                       extratext, " ", extratext2)) +
        theme_bw()

# here, add tooltip content JUDY
    return(ggplotly(p, tooltip = c("y", "x", "text")) %>% 
             plotly::layout(
               xaxis = list(
                 title = list(text = timeVar, font = list(size = 14, family = "Arial black")),
                 tickfont = list(family = "Arial black")#,
                # tickangle = -45
               ),
               yaxis = list(
                 title = list(text = 
                                "Number of Patients", font = list(size = 14, family = "Arial black")),
                 tickfont = list(family = "Arial black")
               ),
               legend = list(
                 # font = list(
                 #          family = "Arial",
                 #          size = 8
                 #          ),
                 orientation = "h",
                 y = -.25
               ),
               autosize = TRUE,
               margin = list(
                 l = 50, # adjust the left margin
                 r = 50, # adjust the right margin
                 t = 50,  # adjust the top margin
                 b = 50   # adjust the bottom margin
               ) ) %>% 
      plotly::config(displaylogo = FALSE,
                     modeBarButtonsToRemove = list('hoverClosestCartesian',
                                                   'hoverCompareCartesian',
                                                   'autoScale2d',
                                                   'lasso2d',
                                                   'select2d',
                                                   'zoom2d',
                                                   'toImage'
                     ))
    )
  })
  
  # this is tab panel 3 if count or one level chosen
  output$overviewbyage <- renderPlotly({
    if (!tableReady()) return(NULL)
    print("attemptingoverviewbyage")

    if (is.null(byAgeTable())) return(NULL)
    input$makeTable
    tableData <- byAgeTable()$overview %>% filter(Year != "ALL")
    extratext <-  ""
    extratext2 <- ""
    # first if this is not coded variable:
    if ( is.null(varInfo[[input$var]]$codes) &&
         is.null(varInfo[[input$var]]$allcodes)) { #is.null(input$codes)){
      tableData <- tableData %>% 
        group_by(Year, Age) %>% 
        summarise(Patients = sum(Patients)) %>% 
        ungroup() %>% 
        mutate(Year = as.numeric(Year))
      
    } else {
      # then we know this is a coded variable
      extratext <- ""
      if ((!is.null(varInfo[[input$var]]$allcodes) && varInfo[[input$var]]$allcodes) ||
          !is.null(varInfo[[input$var]]$codes)){
        if (length(input$codes) > 1){
          #should not be possible
          extratext <- " = Selected values"
        } else if (length(input$codes == 1)){
          extratext <- paste0(" = ", input$codes)
        }
      }
      
      tableData <- tableData %>% 
        group_by(Year, Age) %>% 
        summarise(Patients = sum(Patients)) %>% 
        ungroup() %>% 
        mutate(Year = as.numeric(Year))
    }
    
    p <- ggplot(tableData %>% rename({{ timeVar }} := Year), 
                aes(x = .data[[timeVar]], y = Patients, fill = Age,
                               text = paste0("Age: ", Age))) +
      # name = Patients, 
      # col = Sex)) +
      geom_bar(stat = "identity") + ylab("Number of Patients") +
      scale_fill_manual(values=highchartcolors) +
      xlim(minyear-1/2, maxyear+1) + # required to include minyear and maxyear in bar plot
      ggtitle(paste0(#"Number of Patients from All IeDEA Regions with Data: ",
        varInfo[[input$var]][["displayName"]],
        extratext, " ", extratext2)) +
      theme_bw()
    
    # here, add tooltip content JUDY
    return(ggplotly(p, tooltip = c("y", "x", "text")) %>% 
             plotly::layout(
               xaxis = list(
                 title = list(text = timeVar, font = list(size = 14, family = "Arial black")),
                 tickfont = list(family = "Arial black")#,
                 # tickangle = -45
               ),
               yaxis = list(
                 title = list(text = 
                                "Number of Patients", font = list(size = 14, family = "Arial black")),
                 tickfont = list(family = "Arial black")
               ),
               legend = list(
                 # font = list(
                 #          family = "Arial",
                 #          size = 8
                 #          ),
                 orientation = "h",
                 y = -.25
               ),
               autosize = TRUE,
               margin = list(
                 l = 50, # adjust the left margin
                 r = 50, # adjust the right margin
                 t = 50,  # adjust the top margin
                 b = 50   # adjust the bottom margin
               ) ) %>% 
             plotly::config(displaylogo = FALSE,
                            modeBarButtonsToRemove = list('hoverClosestCartesian',
                                                          'hoverCompareCartesian',
                                                          'autoScale2d',
                                                          'lasso2d',
                                                          'select2d',
                                                          'zoom2d',
                                                          'toImage'
                            ))
    )
  })
    
    # removeHTML removes html formatting from a character string--------------
    removeHTML <- function(htmlString) {
      return(gsub("<.*?>", "", htmlString))
    }  
    
  output$missingDataAlert <- renderUI({
    if (!tableReady()) return(NULL)
    if (is.null(missingRegions())) return(NULL)
    if (is.null(overviewTable())) return(NULL)
    missingones <- missingRegions()
    if (length(missingones) > 1){
      regionword <- "regions have"
    } else regionword <- "region has"
    missingones <-  paste0(missingones, collapse = ", ")
    missingones <- removeHTML(missingones)
    shinyWidgets::alert(paste0("The following ", regionword, 
                           " no data for this variable: ",
                          missingones), 
          status = "danger")
  })
  
  output$tableHeader <- renderUI({
    if (authRequired && !authenticatedUser()) return(NULL)
      
   # if (!(tableReady())){
      return(
        shinyWidgets::alert(
          
        tagList(
          tags$b("This application is under development. ",
           "Data are preliminary and may not represent actual data availability in each region"),
          tags$p("This dashboard provides a high-level picture of selected data across", consortiumName, "to aid investigators in project planning.")),
        tags$ul(
          tags$li(tags$b("Step 1:"), "Choose a variable and values to explore"),
          tags$li(tags$b("Step 2:"), "(Optional) Restrict to a specific age group at enrollment or sex at birth"),
          tags$li(tags$b("Step 3:"), "Click", tags$b("Explore Availability"))
        ),
        p("The resulting histograms summarize the number of patients with observations, by",
        tolower(timeVar), ". ",
        "For a complete list of", consortiumName, "variables and codes, visit", tags$a(
          websiteName, 
          href=consortiaWebsite, target="_blank"
        ))
        ))
    
  })
  
  # output$banner <- renderUI({
  #   if (!tableReady()) return(NULL)
  #   varDetails <- varInfo[[input$var]]
  #   
  #   extratext <- ""
  #   if (length(input$codes) > 1){
  #     extratext <- " (Selected values)"
  #   } else if (length(input$codes == 1)){
  #     extratext <- paste0(" = ", input$codes)
  #   }
  #   extratext2 <- ""# textDescribingAgeSex()
  #   description <- paste0(varDetails$displayName, extratext)
  #   
  #   ageGroupLabel <- ifelse(input$filterAge && length(input$ageGroups) != length(ageGroups), 
  #                           
  #                           " selected age group", " all age groups")
  #   if (extratext2 == ""){
  #     extratext2 = ""# "all patients"
  #   }
  #   tablesubtitle <- paste0(
  #     #"Including patients from ",
  #     extratext2) 
  #   
  #   shinyWidgets::alert(
  #     tags$h4("Data Available for ", 
  #             description),
  #     tags$h5(tablesubtitle)
  #   )
  # })
  
  
  reactableTableHeader <- reactive({ 
    if (is.null(overviewTable())) return(NULL)
    if (!tableReady()) return(NULL)

    extratext <- ""
    
    if ((!is.null(varInfo[[input$var]]$allcodes) && varInfo[[input$var]]$allcodes) ||
         !is.null(varInfo[[input$var]]$codes)){
      if (length(input$codes) > 1){
        extratext <- " = Selected values"
      } else if (length(input$codes == 1)){
        extratext <- paste0(" = ", input$codes)
      }
    }

    return(tagList(
      tags$h4(""),
      tags$h4(
        span("Data Availability by Region:",
             varInfo[[input$var]]$displayName,
             extratext,
             textDescribingAgeSex())
        ),
      tags$h4(" ")
    ))
  })
  
  # output$regionTableHeader <- renderUI({
  #   if (is.null(overviewTable())) return(NULL)
  #   if (!tableReady()) return(NULL)
  #   
  #   extratext <- ""
  #   
  #   if ((!is.null(varInfo[[input$var]]$allcodes) && varInfo[[input$var]]$allcodes) ||
  #       !is.null(varInfo[[input$var]]$codes)){
  #     if (length(input$codes) > 1){
  #       extratext <- " = Selected values"
  #     } else if (length(input$codes == 1)){
  #       extratext <- paste0(" = ", input$codes)
  #     }
  #   }
  #   
  #   tagList(
  #     tags$h4(""),
  #     tags$h3(
  #       span("Data Availability by Region:",
  #            varInfo[[input$var]]$displayName,
  #            extratext,
  #            textDescribingAgeSex())
  #     ),
  #     tags$h4(" ")
  #   )
  # })
  
  output$allRegionsTable <- renderReactable({
    if (!tableReady()) return(NULL)

    tablelist <- overviewTable()
    #if (is.null(tablelist)) return(NULL)
    tableSummary <- tablelist$summaryTable
    return(tableSummary)}
  )
  
  output$bySexReactable <- renderReactable({
    if (!bySexTableReady()) return(NULL)
    
    tablelist <- bySexTable()
    #if (is.null(tablelist)) return(NULL)
    tableSummary <- tablelist$reactableTable
    return(tableSummary)}
  )
  
  output$byAgeReactable <- renderReactable({
    
    tablelist <- byAgeTable()
    #if (is.null(tablelist)) return(NULL)
    tableSummary <- tablelist$reactableTable
    return(tableSummary)}
  )
  
  output$linebreaks <- renderUI({
    HTML(paste(" ", " ", "", " ", sep="<br/>"))
  })
  
  output$linebreaks2 <- renderUI({
    HTML(paste(" ", #" ", "", " ", " ", " "
               sep="<br/>"))
  })
  
}


#output$allregionsoverviewUI <- renderUI({
#   if (is.null(overviewTable())) return(NULL)
#   panel(
#     heading = ("Number of Patients from Each IeDEA Region with Data"),
#     plotlyOutput("allregionsoverview", height = "400px"),
#     style = "padding: 5px; border-color: darkgray"
#   )
# })

#   output$allregionsoverview <- renderPlotly({
#     #   if (!tableReady()) return(NULL)
#     if (is.null(overviewTable())) return(NULL)
#     if (!tableReady()) return(NULL)
# 
#     extratext <- ""
#     if ((!is.null(varInfo[[input$var]]$allcodes) && varInfo[[input$var]]$allcodes) ||
#         !is.null(varInfo[[input$var]]$codes)){ 
#       if (length(input$codes) > 1){
#         extratext <- " = Selected values"
#       } else if (length(input$codes == 1)){
#         extratext <- paste0(" = ", input$codes)
#       }
#     }
#     
#     extratext2 <- "" #textDescribingAgeSex()
#     tableData <- tableToShowALL()$allregionstable
#     if (((!is.null(varInfo[[input$var]]$allcodes) && varInfo[[input$var]]$allcodes) ||
#       !is.null(varInfo[[input$var]]$codes)) && length(input$codes) > 1){
#       if (varInfo[[input$var]][["multRecords"]]){
#         tableData <- tableData %>% 
#           group_by(Region, Year) %>% 
#           summarise(Patients = max(Patients),
#                     numcenters = max(numcenters),
#                     PercSites = max(PercSites),
#                     centers = max(centers),
#                     numobserv = max(numobserv),
#                     PercPatients = max(PercPatients)) %>% 
#           ungroup()
#       } else {
#         tableData <- tableData %>% 
#           group_by(Region, Year) %>% 
#           summarise(Patients = sum(Patients),
#                     numcenters = max(numcenters),
#                     PercSites = max(PercSites),
#                     centers = max(centers),
#                     numobserv = max(numobserv),
#                     PercPatients = sum(PercPatients)) %>% 
#           ungroup()
#       }
#     }
#     
#     tableData <- tableData %>% rename(Percent = PercPatients) %>% 
#       mutate(Year = as.numeric(Year))
#     if (!exists("Regioncode", tableData)){
#       tableData <- tableData %>% left_join(regionPatientsWithNum %>% 
#                                              mutate(Regioncode = Region,
#                                                     Region = RegionLineBreaks) %>% 
#                                              select(Region, Regioncode), by = "Region")
#     }
#     if (!exists("RegionFull", tableData)){
#       tableData <- tableData %>% left_join(regionPatientsWithNum %>% 
#                                              select(Region, RegionFull) %>% 
#                                              rename(Regioncode = Region),
#                                            by = c("Regioncode"))
#     }
# # let's just show the region name not count
#     p <- ggplot(tableData %>% mutate(Region = RegionFull), aes(x = Year, y = Patients, 
#                                group = Region,
#                                name = Patients, 
#                                col = Region,
#                                shape = Region,
#                                text = paste0("Region: ", Region))) + #Regioncode))) +
#       # geom_bar(stat = "identity") +
#       geom_line() +
#       geom_point() + 
#       ylab("Number of Patients") +
#       labs(col = "Region",
#            shape = "Region") +
#       xlim(minyear, maxyear) +
#       ggtitle(paste0(#"Number of Patients from Each IeDEA Region with Data: ",
#                      varInfo[[input$var]][["displayName"]],
#                      extratext," ", extratext2)) +
#       theme_bw() 
#     
#     return(
#       ggplotly(p, tooltip = c( "y", "x", "text")) %>% 
#              plotly::layout(
#       xaxis = list(
#         title = list(text = "Year", font = list(size = 14, family = "Arial black")),
#         tickfont = list(family = "Arial black")#,
#         #tickangle = -45
#       ),
#       yaxis = list(
#         title = list(text = 
#                        "Number of Patients", font = list(size = 14, family = "Arial black")),
#         tickfont = list(family = "Arial black")
#       ),
#       #paper_bgcolor = "#eeeeee",
#       legend = list(
#         # font = list(
#         #          family = "Arial",
#         #          size = 8
#         #          ),
#         orientation = "h",
#         y = -.25,
#         x = -.1
#       ),
#       autosize = TRUE,
#       margin = list(
#         l = 50, # adjust the left margin
#         r = 50, # adjust the right margin
#         t = 50,  # adjust the top margin
#         b = 50   # adjust the bottom margin
#       )) %>% 
#         plotly::config(displaylogo = FALSE,
#                        modeBarButtonsToRemove = list('hoverClosestCartesian',
#                                                      'hoverCompareCartesian',
#                                                      'autoScale2d',
#                                                      'lasso2d',
#                                                      'select2d',
#                                                      'zoom2d',
#                                                      'toImage'
#                        ))
#       )
#     
#   })
# Run the application 
shinyApp(ui = ui, server = server)
