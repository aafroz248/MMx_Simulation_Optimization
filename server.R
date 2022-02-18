####################################################################################
# Title : Marketing Mix Simulation and Optimization Tool


## Version History
# Created : 25th November 2020
# Author : Karthik Premanand, Mohammed Abu
# Version : 20.10.02 - 20.11.04
# Description : Add Recommendations tab and UX Revamp



# Created : 8th November 2018
# Author : Abhinav Karnwal, Sunuganty Achyut Raj, Vyshali B
# Version : 19.06.01
# Description : Upload a marketing mix model and simulate/optimize different spend scenarios
####################################################################################

sign_formatter <- formatter(
  "span",
  style = x ~ style(color = ifelse(x < 0 , "red", "green")),
  x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
sign_formatter(c(-1, 0, 1))



shinyServer(function(input, output, session) {
  
  #options(shiny.launch.browser = TRUE)
  #Setting maximum upload size of files
  options(shiny.maxRequestSize=100*1024^2)
  
  ################################# Load Utilities ##########################################
  
  source('./www/MMX_Utils/Utils/adstock_decay_utility_functions.R')
  source('./www/MMX_Utils/Utils/simulation_utils.R')
  source('./www/MMX_Utils/Utils/optimization_utils.R')
  source('./www/MMX_Utils/Utils/contrib_marg_utility_functions.R')
  source('./www/MMX_Utils/Utils/customPrecision.R')
  source('./www/MMX_Utils/Utils/isres_fn.R')
  
  ################################# Define Reactive Values ###########################################
  
  # observeEvent(input$sidebarItemExpanded, {
  #   if(input$sidebarItemExpanded == "CHARTS" || input$sidebarCollapsed == TRUE){
  #     #browser()
  #     updateTabItems(session, "sideTabs", selected = "hiddenCharts")
  #   }
  # })
  
  
  dates_df1 <- shiny::reactiveValues(
    dataset = data.frame()
  )
  reactData <- shiny::reactiveValues(
    uploadedSimScenarioList = c(),
    dataset = data.frame(),
    long_dataset = data.frame(),
    scenarioName = ""
  )
  reactDataOpt <- shiny::reactiveValues(
    dataset = data.frame()
  )
  
  combinedData<- shiny ::reactiveValues(
    final_data = data.frame(),
    df_final1 = data.frame(),
    end_data=data.frame(),
    filename = ""
    
  )
  
  extReactData <- shiny::reactiveValues(
    dataset = data.frame(),
    dataset_name = "",
    processed_dataset = data.frame(),
    numeric_cols = c(),
    cat_cols = c(),
    dataset_name = "", 
    mis_100_names = c(),
    dummy_cols = c(),
    singular_cols = c(),
    working_dir = getwd(),
    openReport = F
  )
  simExtReactData <- shiny::reactiveValues(dataset=data.frame())
  editedScenarioReactData <- shiny::reactiveValues(
    dataset = data.frame()
  )
  
  mmxModel <- shiny::reactiveValues(model=NULL,train_data=NULL,full_object=NULL)
  histReactData <- shiny::reactiveValues(
    dataset = data.frame(),
    long_dataset = data.frame()
  )
  modelClass <- shiny::reactiveValues()
  histPanelReactData <- shiny::reactiveValues(
    dataset = data.frame(),
    wide_dataset = data.frame(),
    long_dataset = data.frame())
  histAreaData <- shiny::reactiveValues(
    dataset = data.frame()
  )
  histColData <- shiny::reactiveValues(
    dataset = data.frame()
  )
  method1 <- shiny::reactiveValues(
    dataset = data.frame(),
    uploadedDataset = data.frame(),
    scenarioName = "",
    schedulingStrategy = "Custom Spend Pattern",
    simulationDataset = data.frame(),
    runSimulationDataset = data.frame()
  )
  method2 <- shiny::reactiveValues(
    dataset = data.frame(),
    uploadedDataset = data.frame(),
    scenarioName = "",
    schedulingStrategy = "Percentage change from historical pattern",
    simulationDataset = data.frame(),
    runSimulationDataset = data.frame()
  )
  method3 <- shiny::reactiveValues(
    dataset = data.frame(),
    uploadedDataset = data.frame(),
    scenarioName = "",
    schedulingStrategy = "Spend concentrated in a few months",
    simulationDataset = data.frame(),
    runSimulationDataset = data.frame()
  )
  adstock <- shiny::reactiveValues(
    dataset = data.frame(),
    uploadedDataset = data.frame(),
    transformedDataset = data.frame(),
    long_dataset = data.frame()
  )
  scenarioList <- shiny::reactiveValues(
    files=list()
  )
  optimisationScenarioList <- shiny::reactiveValues(
    files=list()
  )
  scenarioTable <- shiny::reactiveValues(
    dataset = data.frame()
  )
  compareScenarioTable <- shiny::reactiveValues(
    dataset = data.frame()
  )
  generatedScenarioTable <- shiny::reactiveValues(
    dataset = data.frame()
  )
  runPanel <- shiny::reactiveValues(
    dataset = data.frame(),
    long_dataset = data.frame()
  )
  viewScenario <- shiny::reactiveValues()
  runPanelAll <- shiny::reactiveValues(
    dataset = data.frame()
  )
  runPanelHist <- shiny::reactiveValues(
    dataset = data.frame()
  )
  runPanelAdstockHist <- shiny::reactiveValues(
    dataset = data.frame()
  )
  chartChannel <- shiny::reactiveValues(
    dataset = data.frame()
  )
  histPanelContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  histPanelNonPctContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  histPanelPctContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  histPanelChannelContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  histPanelChannelPctContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  histSummaryPanelContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  histSummaryPanelNonPctContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  histSummaryPanelPctContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  runPanelAllWide <- shiny::reactiveValues(
    dataset = data.frame()
  )
  allPanelContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  allPanelNonPctContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  allPanelPctContribution <- shiny::reactiveValues(
    dataset = data.frame()
  ) 
  runPanelContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  runPanelNonPctContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  runPanelPctContribution <- shiny::reactiveValues(
    dataset = data.frame()
  ) 
  runSummaryPanelContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  runSummaryPanelNonPctContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  runSummaryPanelPctContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  runPanelChannelContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  runPanelChannelPctContribution <- shiny::reactiveValues(
    dataset = data.frame()
  )
  contribChannel <- shiny::reactiveValues(
    dataset = data.frame()
  )
  contribPctChannel <- shiny::reactiveValues(
    dataset = data.frame()
  )
  contribSummaryPanel <- shiny::reactiveValues(
    dataset = data.frame()
  )
  contribPctSummaryPanel <- shiny::reactiveValues(
    dataset = data.frame()
  )
  compareScenario <- shiny::reactiveValues(
    # histScenario1 = data.frame(),
    # histScenario1 = data.frame(),
    # runScenario1 = data.frame(),
    # runScenario2 = data.frame()
  )
  scenarioSummaryData<- shiny::reactiveValues(
    dataset = data.frame()
  )
  
  histCompareScenario1 <- shiny::reactiveValues()
  histCompareScenario2 <- shiny::reactiveValues()
  runCompareScenario1 <- shiny::reactiveValues()
  runCompareScenario2 <- shiny::reactiveValues()
  
  
  runPanelCompareContributionScenario1 <- shiny::reactiveValues()
  histPanelCompareContributionScenario1 <- shiny::reactiveValues()
  runPanelCompareContributionScenario2 <- shiny::reactiveValues()
  histPanelCompareContributionScenario2 <- shiny::reactiveValues()
  
  runPanelCompareNonPctContributionScenario1 <- shiny::reactiveValues()
  runPanelCompareNonPctContributionScenario2 <- shiny::reactiveValues()
  histPanelCompareNonPctContributionScenario1 <- shiny::reactiveValues()
  histPanelCompareNonPctContributionScenario2 <- shiny::reactiveValues()
  
  runPanelComparePctContributionScenario1 <- shiny::reactiveValues()
  runPanelComparePctContributionScenario2 <- shiny::reactiveValues()
  histPanelComparePctContributionScenario1 <- shiny::reactiveValues()
  histPanelComparePctContributionScenario2 <- shiny::reactiveValues()
  
  allPctContribDataScenario2 <- shiny::reactiveValues()
  allPctContribDataScenario1 <- shiny::reactiveValues()
  allNonPctContribDataScenario2 <- shiny::reactiveValues()
  allNonPctContribDataScenario1 <- shiny::reactiveValues()
  
  allSalesDataScenario1 <- shiny::reactiveValues()
  allSalesDataScenario2 <- shiny::reactiveValues()
  
  
  
  
  
  # Custom renderer function
  color_renderer <- "
  function(instance, td) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    td.style.background = '#AEC7E8';
  }"
  
  
  # grobPlots <- shiny::reactiveValues()
  
  ############################### Define Functions ###########################################
  
  summarise_at_fun <- function(variables, func, data){
    data2 <- data %>%
      summarise_at(vars(variables), funs(get(func)(.)))
    return(data2)
  }
  

  ################### Disable tabs initially ##########################
  #js$disableTab("configureVarTab")
  #js$disableTab("createScenarioTab")
  #js$disableTab("setTimePeriodTab")
  #js$disableTab("runSimulationTab")
  #js$disableTab("selectObjective")
  #js$disableTab("addConstraintsTab")
  #js$disableTab("selectOptimizationTab")
  #js$disableMenu("tabGenerateScenarios")
  #js$disableMenu("tabOptimization")
  #js$disableMenu("tabOptimizationResults")
  #js$disableMenu("tabSimulationResults")
  #js$disableMenu("tabCompareScenario")
  #js$disableTab("viewSimulationResults")
  #js$disableTab("viewOptimizationResults")
  #js$disableMenu("tabRecommend")
  
  
  #### Display only logo on sidebar collapse
  
  # runjs({'
  #       var el2 = document.querySelector(".skin-blue");
  #       el2.className = "skin-blue sidebar-mini";
  #       var clicker = document.querySelector(".sidebar-toggle");
  #       clicker.id = "switchState";
  #   '})
  # 
  # onclick('switchState', runjs({'
  #       var title = document.querySelector(".logo")
  #       if (title.style.visibility == "hidden") {
  #         title.style.visibility = "visible";
  #       } else {
  #         title.style.visibility = "hidden";
  #       }
  # '}))
  # 
  
  
  
  ############################### Download Scenarios UI and ObserveEvents ##############################################
  
  output$downloadReportUI <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    # column(4, selectInput('simScenarioSelect','Select Simulation Scenario', choices = c(unique(names(scenarioList$files))), multiple=FALSE, selected = "")),
                    column(2, style='padding:25px', shiny::downloadButton('downloadReportButton','Download')),
    )
    
  })
  
  observe({output$downloadReportButton <- shiny::downloadHandler(
    filename = function() {
      paste(combinedData$filename,".html", sep = "")
    },
    content = function(file) {
      file.copy(paste0("./Downloads/",combinedData$filename, ".html"), file)
    },
    contentType = "application/html"
  )
  })
  
  output$downloadScenarioUI <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    column(4, selectInput('simScenarioSelect','Select Simulation Scenario', choices = c(unique(names(scenarioList$files))), multiple=FALSE, selected = "")),
                    column(2, style='padding:25px', downloadButton('simScenarioDownload', 'Download')),
                    column(4, selectInput('optiScenarioSelect','Select Optimization Scenario', choices = c(unique(names(optimisationScenarioList$files))), multiple=FALSE, selected = "")),
                    column(2, style='padding:25px', downloadButton('optiScenarioDownload', 'Download'))
    )
    
  })
  
  output$simScenarioDownload <- shiny::downloadHandler(
    filename =function(){
      paste(input$simScenarioSelect,".rds", sep = "")
    },
    content = function(file){
      saveRDS(scenarioList$files[[input$simScenarioSelect]],file)
    }
  )
  
  output$optiScenarioDownload <- shiny::downloadHandler(
    filename =function(){
      paste(input$optiScenarioSelect,".rds", sep = "")
    },
    content = function(file){
      saveRDS(optimisationScenarioList$files[[input$optiScenarioSelect]],file)
    }
    
  )
  
  
  
  
  # observeEvent(input$optiScenarioDownload,{
  #   tryCatch({
  #     saveRDS(optimisationScenarioList$files[[input$optiScenarioSelect]], file = paste0("Scenarios/", c(input$optiScenarioSelect), ".RDS"))
  #     output$ScenarioDownloadText <- shiny::renderText(paste0(
  #       input$optiScenarioSelect, ' has been run successfuly saved in the "Scenarios" folder.\n'))
  #   },error=function(e) showNotification(paste0('Error in optiScenarioDownload section:',e[1])))
  # })
  ################################################################################################
  
  
  ############################### Load Historical Data ###########################################
  
  observe({
    if(req(input$sideTabs)=='tabConfigure'){
      # switch to menu
      updateTabItems(session, "sideTabs", "tabConfigure")
      
      # enable and switch to tab
      updateNavbarPage(session, "configureNav", "uploadDataTab")
    }
    else if(req(input$sideTabs)=='tabGenerateScenarios'){
      updateTabItems(session, "sideTabs", "tabGenerateScenarios")
      updateNavbarPage(session, "scenarioNav", "createScenarioTab")
    }
    else if(req(input$sideTabs)=='tabOptimization'){
      updateTabItems(session, "sideTabs", "tabOptimization")
      updateNavbarPage(session, "optScenarioNav", "selectObjectiveTab")
    }
  })
  
  observe({
    if(length(scenarioList$files)>0 |length(optimisationScenarioList$files)>0){
      shinyjs::enable('reportDownload')
    }
    
    if(length(scenarioList$files)>0 |length(optimisationScenarioList$files)>0){
      shinyjs::enable('scenarioDownload')
    }
  })
  
  ### Conductor for loading historical CSV file
  preDataset <- shiny::reactive({
    # infile <- input$preData
    #  infile$datapath
    hisDataFlag <<- 0
    x <- input$preData
    if(x>0) 
      dataset <- data.frame(read.csv("./Data/historical_data.csv"))
    return(dataset)
    
    
  })
  ### Conductor for loading historical CSV file
  histDataset <- shiny::reactive({
    infile <- input$histDataFile
    infile$datapath
    if(is.null(infile))
    {
      # User has not uploaded a file yet
      return(data.frame())
    }
    else
    {
      if(file_ext(infile$datapath)!='csv')
      {
        shiny::showNotification('Please load only CSV files', type='error');
        return()
      }
      dataset <- data.frame(read.csv(infile$datapath))
      return(dataset)
    }
  })
  
  ## upload sample data ##
  shiny::observeEvent(
    input$preData,
    {
      tryCatch({
        uploadedDataset <- preDataset()
        nums <-  unlist(lapply(uploadedDataset, is.numeric))
        output$histHeadTable <- DT::renderDataTable(DT::datatable(roundOffDataFrame(head(uploadedDataset),TRUE,2),options=list(scrollX=T, searching=FALSE, dom = 't'))%>%
                                                      formatCurrency(colnames(uploadedDataset[nums]),currency = "", interval = 3, mark = ","))
        reactData$dataset <- uploadedDataset
      },error=function(e) showNotification(paste0('Historical Data File Upload :',e[1])))
    })
  
  ### Checks change of state for upload of historical data file
  shiny::observeEvent(
    input$histDataFile,
    {
      tryCatch({
        uploadedDataset <- histDataset()
        nums <-  unlist(lapply(uploadedDataset, is.numeric))
        output$histHeadTable <- DT::renderDataTable(DT::datatable(roundOffDataFrame(head(uploadedDataset),TRUE,2),options=list(scrollX=T, searching=FALSE, dom = 't'))%>%
                                                      formatCurrency(colnames(uploadedDataset[nums]),currency = "", interval = 3, mark = ","))
        reactData$dataset <- uploadedDataset
      },error=function(e) showNotification(paste0('Historical Data File Upload :',e[1])))
    })
  
  output$histTableText <- shiny::renderUI({
    if(nrow(reactData$dataset)){
      shiny::tags$p(shiny::HTML(paste0("The uploaded dataset ",
                                       " contains <b> ",
                                       nrow(reactData$dataset),
                                       "</b> Rows and <b>",
                                       ncol(reactData$dataset),
                                       "</b> Columns.",
                                       "</br>",
                                       "First 6 rows have been shown below in order to ensure that relevant data has been loaded for the analysis.")))
    }
    
  })
  
  
  output$loadSummaryStats <- shiny::renderPrint({
    if(nrow(reactData$dataset)){
      cat("Summary Statistics for the uploaded dataset:\n")
      summary_stats <- summary(reactData$dataset)
      summary_stats
    }
  })  
  
  output$loadDataStr <- shiny::renderPrint({
    if(nrow(reactData$dataset)){
      str_data <- str(reactData$dataset)
      str_data
    }
  })  
  
  ### Checks for click of help icon
  shiny::observeEvent(
    input$historicalHelpText,
    {
      tryCatch({
        showModal(modalDialog(
          title = "Load Historical Data - Help",
          p(HTML(loadHistoricalDataText)),
          footer = tagList(
            modalButton("OK")
          )
        ))
      },error=function(e) showNotification(paste0('Load Historical Data - Help Text :',e[1])))
    }
  )
  
  ############################### Load MMX Model ###########################################
  
  ## for sample model ##
  preModelFile <- shiny :: reactive({
    infile <- input$preModel
    objectList <- readRDS("./Models/Fractional_Root.RDS")
    return(objectList)
  })
  
  
  ### Conductor for loading MMX model object as an RDS file
  modelObject <- shiny::reactive({
    infile <- input$modelFile
    if(is.null(infile))
    {
      # User has not uploaded a file yet
      return()
    }
    else
    {
      if(file_ext(infile$datapath)!='RDS')
      {
        shiny::showNotification('Please load only RDS files', type='error');
        return()
      }
      objectList <- readRDS(infile$datapath)
      return(objectList)
    }
  })
  
  ### Checks change of state for upload of model object as RDS file
  shiny::observeEvent(
    input$modelFile,{
      tryCatch({
        objectList <- modelObject()
        mmxModel$model <- objectList$model
        mmxModel$predFuns <- objectList$predFuns
        if(!is.null(objectList$train_data))
          mmxModel$train_data <- objectList$train_data
        mmxModel$full_object <- objectList
        if(!is.null(mmxModel$predFuns)){
          modelClass <- class(mmxModel$model)[1]
          assign(paste0("predict.",modelClass), mmxModel$predFuns, envir = .GlobalEnv)
        }
      },error=function(e) showNotification(paste0('Load Model Object :',e[1])))
    })
  
  ## for pre loaded model ##
  shiny::observeEvent(
    input$preModel,{
      tryCatch({
        objectList <- preModelFile()
        mmxModel$model <- objectList$model
        mmxModel$predFuns <- objectList$predFuns
        if(!is.null(objectList$train_data))
          mmxModel$train_data <- objectList$train_data
        mmxModel$full_object <- objectList
        if(!is.null(mmxModel$predFuns)){
          modelClass <- class(mmxModel$model)[1]
          assign(paste0("predict.",modelClass), mmxModel$predFuns, envir = .GlobalEnv)
        }
      },error=function(e) showNotification(paste0('Load Model Object :',e[1])))
    })
  
  output$loadModelText <- shiny::renderUI({
    if(!is.null(mmxModel$model)){
      
      formula_value <- mmxModel$formula
      if(is.null(formula_value)) 
        formula_value <- tryCatch(formula(mmxModel$model),error=function(e) return('')) else formula_value <- ''
      reactData$formula_report <- formula_value
      list(
      shiny::tags$p(shiny::HTML(paste0("<strong>Train data, spend, panel and other variables will be retrieved from the model object if these are saved as specfied in the help text.</strong>"))),
      renderPrint({
        if(formula_value!=''){
        cat("Formula of the uploaded model object :\n")
        print(formula_value)
        }
        }))
    }
    
  })
  
  ### Checks for click of help icon
  shiny::observeEvent(
    input$loadModelHelpText,
    {
      showModal(modalDialog(
        title = "Load MMx Model - Help",
        p(HTML(loadModelTextData)),
        footer = tagList(
          modalButton("OK")
        )
        ))
    }
      )
  
  ############# Checks if train data is already present in supplied model object  ###############
  train_data_from_object <- reactive({
    if(is.null(mmxModel$full_object))
    {
      return(NULL)
    }
    train_data <- mmxModel$full_object$train_data
    nums <-  unlist(lapply(train_data, is.numeric))
    # trainDataTakenFromModel <- ""
    if(!is.null(train_data)){
      output$trainHeadTable <- DT::renderDataTable({
        if(is.null(train_data)) return()
        DT::datatable(roundOffDataFrame(head(train_data), TRUE,2),options=list(scrollX=T, searching=FALSE, dom = 't')) %>%
          formatCurrency(colnames(train_data[nums]),currency = "", interval = 3, mark = ",")})
      # trainDataTakenFromModel <- "Train data has been taken from the uploaded model object using the variable train_data"
    }
    else{
      output$trainHeadTable <- DT::renderDataTable(DT::datatable(data.frame()))
    }
    return(train_data)
  })
  
  # output$trainDataTakenFromModelText <- shiny::renderUI({
  #   shiny::tags$p(shiny::HTML(trainDataTakenFromModel))
  # })
  
  
  
  output$loadTrainData <- renderUI({
    
    if(!is.null(mmxModel$model) & is.null(train_data_from_object())){
      tagList(
        shiny::tags$h3('Load training dataset'),
        shiny::tags$p(shiny::HTML(loadTrainDataText)),
        ### Load File Widget - Source for getting user uploaded MMX model object as an RDS file
        shiny::inputPanel(shiny::fileInput('trainDataFile', 'Choose RDS file'))
        
      )}
  })
  
  ### Conductor for loading train data as a CSV file
  trainDataObject <- shiny::reactive({
    infile <- input$trainDataFile
    if(is.null(infile))
    {
      # User has not uploaded a file yet
      return(data.frame())
    }
    else
    {
      if(file_ext(infile$datapath)!='RDS')
      {
        shiny::showNotification('Please load only RDS files', type='error');
        return()
      }
      trainDataObjectList <- readRDS(infile$datapath)
      print(names(trainDataObjectList))
      return(trainDataObjectList)
    }
  })
  
  ### Checks change of state for upload of train data as CSV file
  shiny::observeEvent(input$trainDataFile,{
      tryCatch({
        trainDataObjectList <- trainDataObject()
        mmxModel$train_data <- trainDataObjectList$train_data
        nums <-  unlist(lapply(mmxModel$train_data, is.numeric))
        if(!is.null(mmxModel$train_data))
          output$trainHeadTable <- DT::renderDataTable(DT::datatable(
            roundOffDataFrame(head(mmxModel$train_data), TRUE,2),options=list(scrollX=T, searching=FALSE, dom = 't'))%>%
              formatCurrency(colnames(mmxModel$train_data[nums]),currency = "", interval = 3, mark = ","))
      },error=function(e) showNotification(paste0('Load Train Data :',e[1])))
    })
  
  ### Displays output of uploaded training data file
  output$loadTrainDataOutput <- shiny::renderUI({
    if(is.null(mmxModel$train_data)) return()
    if(nrow(mmxModel$train_data)){
      list(
        shiny::HTML(paste0("The uploaded training dataset ",
                           " contains <b> ",
                           nrow(mmxModel$train_data),
                           "</b> Rows and <b>",
                           ncol(mmxModel$train_data),
                           "</b> Columns.")),
        shiny::renderText("First 6 rows have been shown below in order to ensure that relevant data has been loaded for the analysis."),
        DT::dataTableOutput("trainHeadTable")
      )
    }
    else
      DT::dataTableOutput("trainHeadTable")
  })
  
  ### Section to check if datetime variable and format are already specified in the uploaded model object
  
  ### Checks for click of help icon
  shiny::observeEvent(input$loadDateHelpText,{
      showModal(modalDialog(
        title = "Select Date Columns - Help",
        shiny::p(shiny::HTML(dateConfigureText)),
        footer = tagList(
          modalButton("OK")
        )
      ))
    }
  )
  
  output$uploadDataTabUI <- shiny::renderUI({
    shiny::column(12, shiny::actionButton('uploadDataTabButton', 'Next', class='next_button', icon = icon("check-circle")))
  })
  
  
  ### Switch to Configuration Tab from Landing page
  
  shiny::observeEvent(input$beginButton,{
    #browser()
    # switch to tab
    js$enableTab("uploadDataTab")
    
    
    ### Switch sidebar menu
    updateTabItems(session, "sideTabs", "tabConfigure")
    
    ### Switch to Upload Tab
    updateNavbarPage(session, "configureNav", "uploadDataTab")
    
  })
  
  
  
  ### Scenario Creation History interactions
  
  myValue <- reactiveValues(employee = '')
  viewValue <- reactiveValues(selected = '')
  viewValueSim <- reactiveValues(selected = '')
  
  shinyInput <- function(FUN, len, id, ...) {
    #browser()
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  shinyInput_custom <- function(FUN, len, id, ...) {
    #browser()
    inputs <- ""
    inputs <- as.character(FUN(paste0(id, len), ...))
    inputs
  }
  
  
  scenario_history_df <- reactiveValues(data = data.frame(
    'Scenario Name' = character(0),
    'Scenario Description' = character(0),
    'Scenario Type' = character(0),
    'View Results' = character(0),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    row.names = NULL
    
    #row.names = 1:5
  ))
  
  
  output$scenario_history <- shiny::renderUI({
    if (length(scenarioList$files) == 0   && length(optimisationScenarioList$files) ==0  ){
      #browser()
      div(
        div(column(12,
             includeHTML("./www/Logos/scenario_history.html"),
             includeCSS("./www/Logos/welcome.css")
      )
      ),
      
      shiny::column(8,style="padding-right: -200px;height: 60px;", 
                    shiny::actionButton('createLandingOptimizationButton', 
                                        'Create Optimization Scenario', width = '220px',
                                        #height = '60px',
                                        class='begin_button'),
                    shiny::actionButton('createLandingSimulationButton', 
                                        'Create Simulation Scenario', width = '220px',
                                        #height = '60px',
                                        class='begin_button')
      )
      
      
      )
      


    } else {
      div(
      div(DT::renderDataTable(
        scenario_history_df$data, options = list(pageLength = 15, lengthChange = FALSE, dom ='t', columnDefs = list(list(className = 'dt-center', targets = 0:3)))
        , server = FALSE
        , rownames= FALSE
        , escape = FALSE, selection = 'none'),
        br(),
        br(),
        br(),
        br(),
        br()
      ),
      
      shiny::column(10,style="padding-right: -200px;height: 60px;", 
                    shiny::actionButton('addLandingOptimizationButton', 
                                        'Add Optimization Scenario', width = '220px',
                                        #height = '60px',
                                        class='begin_button'),
                    shiny::actionButton('addLandingSimulationButton', 
                                        'Add Simulation Scenario', width = '220px',
                                        #height = '60px',
                                        class='begin_button'),
                    shiny::actionButton('simulationResultsCompareButtonHistory', 'Compare Scenarios', width = '220px', class='second_button'),
                    shiny::actionButton('recommendationOptButtonHistory', 'Recommendations', width = '220px', class='second_button')
      )
      
      )
      }
    
    })
  
  
  shiny::observeEvent(input$simulationResultsCompareButtonHistory, {
    tryCatch({
      # Error handling
      if(nrow(compareScenarioTable$dataset)<2 | is.null(compareScenarioTable$dataset)){
        shiny::showNotification("Please generate a minimum of two scenarios before proceeding to the compare scenario section", type = c("error")); return()
      }else{
        # enable tab when clicking the button
        js$enableMenu("tabCompareScenario")
        # enable tab when clicking the button
        js$enableTab("selectObjectiveTab")
        # Change status to complete by highlighting
        #js$greenMenu("tabSimulationResults")
        
        # switch to tab
        updateTabItems(session, "sideTabs", "tabCompareScenario")
      }
    },error= function(e) showNotification(paste0('Simulation Results Next Button :',e[1],type='error')))
  })
  
  
  shiny::observeEvent(input$recommendationOptButtonHistory, {
    tryCatch({
      # Error handling
      if (is.null(reactData$dataset) | is.null(mmxModel$model) | is.null(mmxModel$train_data)){
        shiny::showNotification("Please complete the Compare Scenario Comparison", type = c("error")); return()
      }else{
        # enable tab when clicking the button
        js$enableMenu("tabRecommend")
        # enable tab when clicking the button
        # js$enableTab("tabCompareScenario")
        # Change status to complete by highlighting
        #js$greenMenu("tabCompareScenario")
        
        # switch to tab
        updateTabItems(session, "sideTabs", "tabRecommend")
      }
    },error= function(e) showNotification(paste0('Simulation Results Next Button :',e[1])))
  })
  
  
  shiny::observeEvent(input$createLandingSimulationButton, {
    # Error handling
    if (is.null(reactData$simStartDateVar) | is.null(reactData$simEndDateVar) | is.null(reactData$dateFrequency)){
      shiny::showNotification("Please configure simulation/optimization start and end dates before proceeding to the next section", type = c("error")); return()
    }else{
      # Change status to complete by highlighting
      # js$greenTab("viewHistPerformance")
      #js$greenMenu("tabConfigure")
      
      # enable tab when clicking the button
      js$enableMenu("tabGenerateScenarios")
      js$enableMenu("tabOptimization")
      
      # switch to menu
      updateTabItems(session, "sideTabs", "tabGenerateScenarios")
      
      # enable and switch to tab
      js$enableTab("createScenarioTab")
      updateNavbarPage(session, "scenarioNav", "createScenarioTab")
    }
  })
  
  
  shiny::observeEvent(input$addLandingSimulationButton, {
    # Error handling
    if (is.null(reactData$simStartDateVar) | is.null(reactData$simEndDateVar) | is.null(reactData$dateFrequency)){
      shiny::showNotification("Please configure simulation/optimization start and end dates before proceeding to the next section", type = c("error")); return()
    }else{
      # Change status to complete by highlighting
      # js$greenTab("viewHistPerformance")
      #js$greenMenu("tabConfigure")
      
      # enable tab when clicking the button
      js$enableMenu("tabGenerateScenarios")
      js$enableMenu("tabOptimization")
      
      # switch to menu
      updateTabItems(session, "sideTabs", "tabGenerateScenarios")
      
      # enable and switch to tab
      js$enableTab("createScenarioTab")
      updateNavbarPage(session, "scenarioNav", "createScenarioTab")
    }
  })
  
  shiny::observeEvent(input$createLandingOptimizationButton, {
    # Error handling
    if (is.null(reactData$simStartDateVar) | is.null(reactData$simEndDateVar) | is.null(reactData$dateFrequency)){
      shiny::showNotification("Please configure simulation/optimization start and end dates before proceeding to the next section", type = c("error")); return()
    }else{
      # Change status to complete by highlighting
      js$greenTab("setTimePeriodTab")
      #js$greenMenu("tabConfigure")
      
      # enable tab when clicking the button
      js$enableMenu("tabGenerateScenarios")
      js$enableTab("createScenarioTab")
      js$enableMenu("tabOptimization")
      
      # switch to menu
      updateTabItems(session, "sideTabs", "tabOptimization")
      
      # switch to tab
      # updateNavbarPage(session, "scenarioNav", "viewHistPerformance")
    }
  })
  
  shiny::observeEvent(input$addLandingOptimizationButton, {
    # Error handling
    if (is.null(reactData$simStartDateVar) | is.null(reactData$simEndDateVar) | is.null(reactData$dateFrequency)){
      shiny::showNotification("Please configure simulation/optimization start and end dates before proceeding to the next section", type = c("error")); return()
    }else{
      # Change status to complete by highlighting
      js$greenTab("setTimePeriodTab")
      #js$greenMenu("tabConfigure")
      
      # enable tab when clicking the button
      js$enableMenu("tabGenerateScenarios")
      js$enableTab("createScenarioTab")
      js$enableMenu("tabOptimization")
      
      # switch to menu
      updateTabItems(session, "sideTabs", "tabOptimization")
      
      # switch to tab
      # updateNavbarPage(session, "scenarioNav", "viewHistPerformance")
    }
  })
  
  
  observeEvent(input$chill_button, {
    #scenarioTable$dataset$Scenario_Name
    #browser()
    selectedRow <- as.numeric(strsplit(input$chill_button, "_")[[1]][2])
    myValue$employee <<- paste('click on ',scenario_history_df$data[selectedRow,1])
    
    viewValue$selected <- scenario_history_df$data[selectedRow,1]
    
    updateTabItems(session, "sideTabs", "tabSimulationResults")
    updateNavbarPage(session, "resultsNav", "viewOptimizationResults")
    
    
    
    
  })
  
  
  observeEvent(input$chill_button_1, {
    #scenarioTable$dataset$Scenario_Name
    #browser()
    selectedRow <- as.numeric(strsplit(input$chill_button_1, "_")[[1]][2])
    myValue$employee <<- paste('click on ',scenario_history_df$data[selectedRow,1])
    
    viewValueSim$selected <- scenario_history_df$data[selectedRow,1]
    
    updateTabItems(session, "sideTabs", "tabSimulationResults")
    updateNavbarPage(session, "resultsNav", "viewSimulationResults")
    
    
    
    
  })
  
  
  
  output$myText <- renderText({
    
    myValue$employee
    
  })
  
  
  
  
  
  ################# Observe event to check if step 1 - upload historical data and model has been completed ###################
  shiny::observeEvent(input$uploadDataTabButton,{
    # Error handling
    if (is.null(reactData$dataset) | is.null(mmxModel$model) | is.null(mmxModel$train_data)){
      shiny::showNotification("Please upload the historical dataset, model object and train data (if not already saved in the model RDS file) before proceeding to the next section", type = c("error")); return()
    }else{
      # enable tab when clicking the button
      js$enableTab("configureVarTab")
      # Change status to complete by highlighting
      js$greenTab("uploadDataTab")
      # switch to tab
      updateNavbarPage(session, "configureNav", "configureVarTab")
    }
    
  })
  
  ################# Observe event to check if step 2 - Configure Variables has been completed ###################
  
  output$configureVarTabUI <- shiny::renderUI({
    shiny::column(12, shiny::actionButton('configureVarTabButton', 'Next', class='next_button', icon = icon("check-circle")))
  })
  
  shiny::observeEvent(input$configureVarTabButton, {
    # Error handling
    if (is.null(reactData$DateVar) | is.null(reactData$DTformat) | is.null(reactData$TimeZone) | is.null(reactData$spendVar) | is.null(reactData$panelVar) | is.null(reactData$targetVar) | is.null(reactData$adstock_plot_dataset)){
      shiny::showNotification("Please configure spend, panel, target, date and adstock variables before proceeding to the next section", type = c("error")); return()
    }else{
      # enable tab when clicking the button
      js$enableTab("setTimePeriodTab")
      # Change status to complete by highlighting
      js$greenTab("configureVarTab")
      # switch to tab
      updateNavbarPage(session, "configureNav", "setTimePeriodTab")
    }
    
  })
  
  ################# Observe event to check if step 3 - Define Simulation/Optimization Period and Load Non-Spend Variables has been completed ###################
  ##### The below UI is to give option to the user to move to the next section to create Simulation Scenarios 
  output$gotoSimulation <- shiny::renderUI({
    shiny::column(12, shiny::actionButton('nowSimulate', 'Create Simulation Scenarios', class='next_button', icon = icon("check-circle")))
  })
  
  shiny::observeEvent(input$nowSimulate, {
    # Error handling
    if (is.null(reactData$simStartDateVar) | is.null(reactData$simEndDateVar) | is.null(reactData$dateFrequency)){
      shiny::showNotification("Please configure simulation/optimization start and end dates before proceeding to the next section", type = c("error")); return()
    }else{
      # Change status to complete by highlighting
      # js$greenTab("viewHistPerformance")
      #js$greenMenu("tabConfigure")
      
      # enable tab when clicking the button
      js$enableMenu("tabGenerateScenarios")
      js$enableMenu("tabOptimization")
      
      # switch to menu
      updateTabItems(session, "sideTabs", "tabGenerateScenarios")
      
      # enable and switch to tab
      js$enableTab("createScenarioTab")
      updateNavbarPage(session, "scenarioNav", "createScenarioTab")
    }
  })
  
  ##### The below UI is to give option to the user to move to the next section to create Optimization Scenarios 
  output$gotoOptimization <- shiny::renderUI({
    shiny::column(12, shiny::actionButton('nowOptimize', 'Create Optimization Scenarios', class='next_button', icon = icon("check-circle")))
  })
  
  shiny::observeEvent(input$nowOptimize, {
    # Error handling
    if (is.null(reactData$simStartDateVar) | is.null(reactData$simEndDateVar) | is.null(reactData$dateFrequency)){
      shiny::showNotification("Please configure simulation/optimization start and end dates before proceeding to the next section", type = c("error")); return()
    }else{
      # Change status to complete by highlighting
      js$greenTab("setTimePeriodTab")
      #js$greenMenu("tabConfigure")
      
      # enable tab when clicking the button
      js$enableMenu("tabGenerateScenarios")
      js$enableTab("createScenarioTab")
      js$enableMenu("tabOptimization")
      
      # switch to menu
      updateTabItems(session, "sideTabs", "tabOptimization")
      
      # switch to tab
      # updateNavbarPage(session, "scenarioNav", "viewHistPerformance")
    }
  })
  
  ##############################################################################################################
  
  shiny::observeEvent(input$historicalPerformanceNextButton, {
    # Error handling
    if (is.null(reactData$simStartDateVar) | is.null(reactData$simEndDateVar) | is.null(reactData$dateFrequency)){
      shiny::showNotification("Please configure simulation/optimization start and end dates before proceeding to the next section", type = c("error")); return()
    }else{
      # enable tab when clicking the button
      js$enableTab("viewHistPerformance")
      # change status to complete by highlighting
      js$greenTab("setTimePeriodTab")
      # switch to tab
      updateNavbarPage(session, "configureNav", "viewHistPerformance")
    }
    
    
  })
  
  shiny::observeEvent(input$beginScenarioPlanningButton, {
    # Error handling
    if (is.null(reactData$simStartDateVar) | is.null(reactData$simEndDateVar) | is.null(reactData$dateFrequency)){
      shiny::showNotification("Please configure simulation/optimization start and end dates before proceeding to the next section", type = c("error")); return()
    }else{
      # enable tab when clicking the button
      js$enableTab("viewHistPerformance")
      # change status to complete by highlighting
      js$greenTab("setTimePeriodTab")
      # switch to tab
      updateTabItems(session, "sideTabs", "hiddenCharts")
    }
    
    
  })
  
  
  shiny::observeEvent(input$beginScenarioPlanningButtonSummary, {
    # Error handling
    if (is.null(reactData$simStartDateVar) | is.null(reactData$simEndDateVar) | is.null(reactData$dateFrequency)){
      shiny::showNotification("Please configure simulation/optimization start and end dates before proceeding to the next section", type = c("error")); return()
    }else{
      # enable tab when clicking the button
      js$enableTab("viewHistPerformance")
      # change status to complete by highlighting
      js$greenTab("setTimePeriodTab")
      # switch to tab
      updateTabItems(session, "sideTabs", "hiddenCharts")
    }
    
    
  })
  
  
  ################# Observe event to check if step 2 - Define Scenarios has been completed ###################

  output$createScenarioTabUI <- shiny::renderUI({
    shiny::column(12, shiny::actionButton('createScenarioTabButton', 'Next', class='next_button', icon = icon("check-circle")))
  })
  
  shiny::observeEvent(input$createScenarioTabButton, {
    # Error handling
    if (is.null(scenarioTable$dataset) | nrow(scenarioTable$dataset)<=0){
      shiny::showNotification("Please create simulation scenarios before proceeding to the next section", type = c("error")); return()
    }else{
      # enable tab when clicking the button
      js$enableTab("runSimulationTab")
      # Change status to complete by highlighting
      js$greenTab("createScenarioTab")
      # switch to tab
      updateNavbarPage(session, "scenarioNav", "runSimulationTab")
    }
    
  })
  
  output$runSimulationTabUI <- shiny::renderUI({
    #shiny::column(12, shiny::actionButton('runSimulationTabButton', 'View Results', class='next_button', icon = icon("check-circle")))
    shiny::column(8,style="padding-right: -200px;height: 60px;",
                  shiny::actionButton('runSimulationTabButton', 'View Results', width = '220px', class='begin_button'),
                  shiny::actionButton('beginScenarioPlanningButtonSim', 
                                      'View Created Scenarios', width = '220px',
                                      #height = '60px',
                                      class='begin_button')
    )
  })
  
  shiny::observeEvent(input$runSimulationTabButton, {
    # Error handling
    if(is.null(input$runScenarioSelected)){
      shiny::showNotification("Please generate and run the simulation scenarios before proceeding to the next section", type = c("error")); return()} 
    if(nrow(scenarioList$files[[input$runScenarioSelected]]$scheduling_dataset$runSimulationDataset)<=0 | is.null(scenarioList$files[[input$runScenarioSelected]]$scheduling_dataset$runSimulationDataset)){shiny::showNotification("Please generate and run the simulation scenarios before proceeding to the next section", type = c("error")); return()}
    else{
      # enable tab when clicking the button
      js$enableMenu("tabSimulationResults")
      js$enableTab("viewSimulationResults")
      # js$enableMenu("tabCompareScenario")
      # Change status to complete by highlighting
      js$greenTab("runSimulationTab")
      #js$greenMenu("tabGenerateScenarios")
      # switch to tab
      updateTabItems(session, "sideTabs", "tabSimulationResults")
      updateNavbarPage(session, "resultsNav", "viewSimulationResults")
    }
  
  })
  
  
  shiny::observeEvent(input$beginScenarioPlanningButtonSim, {
    # Error handling
    if (is.null(reactData$simStartDateVar) | is.null(reactData$simEndDateVar) | is.null(reactData$dateFrequency)){
      shiny::showNotification("Please configure simulation/optimization start and end dates before proceeding to the next section", type = c("error")); return()
    }else{
      # enable tab when clicking the button
      js$enableTab("viewHistPerformance")
      # change status to complete by highlighting
      js$greenTab("setTimePeriodTab")
      # switch to tab
      updateTabItems(session, "sideTabs", "hiddenCharts")
    }
    
    
  })
  
  
  shiny::observeEvent(input$beginScenarioPlanningButtonCompare, {
    # Error handling
    if (is.null(reactData$simStartDateVar) | is.null(reactData$simEndDateVar) | is.null(reactData$dateFrequency)){
      shiny::showNotification("Please configure simulation/optimization start and end dates before proceeding to the next section", type = c("error")); return()
    }else{
      # enable tab when clicking the button
      js$enableTab("viewHistPerformance")
      # change status to complete by highlighting
      js$greenTab("setTimePeriodTab")
      # switch to tab
      updateTabItems(session, "sideTabs", "hiddenCharts")
    }
    
    
  })
  
  
  shiny::observeEvent(input$beginScenarioPlanningButtonOpti, {
    # Error handling
    if (is.null(reactData$simStartDateVar) | is.null(reactData$simEndDateVar) | is.null(reactData$dateFrequency)){
      shiny::showNotification("Please configure simulation/optimization start and end dates before proceeding to the next section", type = c("error")); return()
    }else{
      # enable tab when clicking the button
      js$enableTab("viewHistPerformance")
      # change status to complete by highlighting
      js$greenTab("setTimePeriodTab")
      # switch to tab
      updateTabItems(session, "sideTabs", "hiddenCharts")
    }
    
    
  })
  
  shiny::observeEvent(input$simulationResultsOptButton, {
    tryCatch({
      # Error handling
      if (is.null(reactData$dataset) | is.null(mmxModel$model) | is.null(mmxModel$train_data)){
        shiny::showNotification("Please complete the Configuration section before proceeding to generate optimization scenarios", type = c("error")); return()
      }else{
        # enable tab when clicking the button
        js$enableMenu("tabOptimization")
        # enable tab when clicking the button
        js$enableTab("selectObjectiveTab")
        # Change status to complete by highlighting
        #js$greenMenu("tabSimulationResults")
        
        # switch to tab
        updateTabItems(session, "sideTabs", "tabOptimization")
        updateNavbarPage(session, "optScenarioNav", "selectObjectiveTab")
      }
    },error= function(e) showNotification(paste0('Simulation Results Next Button :',e[1])))
  })
  
  
  shiny::observeEvent(input$simulationResultsOptButtonSecond, {
    tryCatch({
      # Error handling
      if (is.null(reactData$dataset) | is.null(mmxModel$model) | is.null(mmxModel$train_data)){
        shiny::showNotification("Please complete the Configuration section before proceeding to generate optimization scenarios", type = c("error")); return()
      }else{
        # enable tab when clicking the button
        js$enableMenu("tabOptimization")
        # enable tab when clicking the button
        js$enableTab("selectObjectiveTab")
        # Change status to complete by highlighting
        #js$greenMenu("tabSimulationResults")
        
        # switch to tab
        updateTabItems(session, "sideTabs", "tabOptimization")
        updateNavbarPage(session, "optScenarioNav", "selectObjectiveTab")
      }
    },error= function(e) showNotification(paste0('Simulation Results Next Button :',e[1])))
  })
  
  
  shiny::observeEvent(input$simulationResultsOptButtonCompare, {
    tryCatch({
      # Error handling
      if (is.null(reactData$dataset) | is.null(mmxModel$model) | is.null(mmxModel$train_data)){
        shiny::showNotification("Please complete the Configuration section before proceeding to generate optimization scenarios", type = c("error")); return()
      }else{
        # enable tab when clicking the button
        js$enableMenu("tabOptimization")
        # enable tab when clicking the button
        js$enableTab("selectObjectiveTab")
        # Change status to complete by highlighting
        #js$greenMenu("tabSimulationResults")
        
        # switch to tab
        updateTabItems(session, "sideTabs", "tabOptimization")
        updateNavbarPage(session, "optScenarioNav", "selectObjectiveTab")
      }
    },error= function(e) showNotification(paste0('Simulation Results Next Button :',e[1])))
  })
  
  shiny::observeEvent(input$simulationResultsSimButton, {
    tryCatch({
      # Error handling
      if (is.null(reactData$dataset) | is.null(mmxModel$model) | is.null(mmxModel$train_data)){
        shiny::showNotification("Please complete the Configuration section before proceeding to generate optimization scenarios", type = c("error")); return()
      }else{
        # enable tab when clicking the button
        #js$enableMenu("tabOptimization")
        # enable tab when clicking the button
        #js$enableTab("selectObjectiveTab")
        # Change status to complete by highlighting
        #js$greenMenu("tabSimulationResults")
        
        # switch to tab
        updateTabItems(session, "sideTabs", "tabGenerateScenarios")
        updateNavbarPage(session, "scenarioNav", "createScenarioTab")
      }
    },error= function(e) showNotification(paste0('Simulation Results Next Button :',e[1])))
  })
  
  
  shiny::observeEvent(input$simulationResultsSimButtonSecond, {
    tryCatch({
      # Error handling
      if (is.null(reactData$dataset) | is.null(mmxModel$model) | is.null(mmxModel$train_data)){
        shiny::showNotification("Please complete the Configuration section before proceeding to generate optimization scenarios", type = c("error")); return()
      }else{
        # enable tab when clicking the button
        #js$enableMenu("tabOptimization")
        # enable tab when clicking the button
        #js$enableTab("selectObjectiveTab")
        # Change status to complete by highlighting
        #js$greenMenu("tabSimulationResults")
        
        # switch to tab
        updateTabItems(session, "sideTabs", "tabGenerateScenarios")
        updateNavbarPage(session, "scenarioNav", "createScenarioTab")
      }
    },error= function(e) showNotification(paste0('Simulation Results Next Button :',e[1])))
  })
  
  
  shiny::observeEvent(input$simulationResultsSimButtonCompare, {
    tryCatch({
      # Error handling
      if (is.null(reactData$dataset) | is.null(mmxModel$model) | is.null(mmxModel$train_data)){
        shiny::showNotification("Please complete the Configuration section before proceeding to generate optimization scenarios", type = c("error")); return()
      }else{
        # enable tab when clicking the button
        #js$enableMenu("tabOptimization")
        # enable tab when clicking the button
        #js$enableTab("selectObjectiveTab")
        # Change status to complete by highlighting
        #js$greenMenu("tabSimulationResults")
        
        # switch to tab
        updateTabItems(session, "sideTabs", "tabGenerateScenarios")
        updateNavbarPage(session, "scenarioNav", "createScenarioTab")
      }
    },error= function(e) showNotification(paste0('Simulation Results Next Button :',e[1])))
  })
  
  
  shiny::observeEvent(input$simulationResultsCompareButton, {
    tryCatch({
      # Error handling
      if(nrow(compareScenarioTable$dataset)<2 | is.null(compareScenarioTable$dataset)){
        shiny::showNotification("Please generate a minimum of two scenarios before proceeding to the compare scenario section", type = c("error")); return()
      }else{
        # enable tab when clicking the button
        js$enableMenu("tabCompareScenario")
        # enable tab when clicking the button
        js$enableTab("selectObjectiveTab")
        # Change status to complete by highlighting
        #js$greenMenu("tabSimulationResults")
        
        # switch to tab
        updateTabItems(session, "sideTabs", "tabCompareScenario")
      }
    },error= function(e) showNotification(paste0('Simulation Results Next Button :',e[1],type='error')))
  })
  
  shiny::observeEvent(input$optimizationResultsCompareButton, {
    tryCatch({
      # Error handling
      if(nrow(compareScenarioTable$dataset)<2 | is.null(compareScenarioTable$dataset)){
        shiny::showNotification("Please generate a minimum of two scenarios before proceeding to the compare scenario section", type = c("error")); return()
      } else {
        # enable tab when clicking the button
        js$enableMenu("tabCompareScenario")
        # Change status to complete by highlighting
        #js$greenMenu("tabOptimizationResults")
        
        # switch to tab
        updateTabItems(session, "sideTabs", "tabCompareScenario")
      }
    },error= function(e) showNotification(paste0('Simulation Results Next Button :',e[1],type='error')))
  })
  
  
  output$addConstraintsTabUI <- shiny::renderUI({
    shiny::column(12, shiny::actionButton('addConstraintsTabButton', 'Next', class='next_button', icon = icon("check-circle")))
  })
  
  shiny::observeEvent(input$addConstraintsTabButton, {
    # Error handling
    if(is.null(reactData$optimisedBudget) | is.null(reactData$bound)){
      shiny::showNotification("Please set a budget and define optimization constraints before proceeding to the next section.", type = c("error")); return()
    }else{
      # enable tab when clicking the button
      js$enableTab("selectOptimizationTab")
      # Change status to complete by highlighting
      js$greenTab("selectObjectiveTab")
      
      # switch to tab
      updateNavbarPage(session, "optScenarioNav", "selectOptimizationTab")
    }
    
  })
  
  output$selectOptimizationTabUI <- shiny::renderUI({
    #shiny::column(12, shiny::actionButton('selectOptimizationTabButton', 'Next', class='next_button', icon = icon("check-circle")))
    
    shiny::column(8,style="padding-right: -200px;height: 60px;",
                  shiny::actionButton('selectOptimizationTabButton', 'View Results', width = '220px', class='begin_button'),
                  shiny::actionButton('beginScenarioPlanningButtonOpti', 
                                      'View Created Scenarios', width = '220px',
                                      #height = '60px',
                                      class='begin_button')
    )
  })
  
  shiny::observeEvent(input$selectOptimizationTabButton, {
    # Error handling
    if(is.null(optimisationScenarioList$files) | length(optimisationScenarioList$files)<=0){
      shiny::showNotification("Please name and save the optimization scenario(s) before proceeding to the next section", type = c("error")); return()}
    else{
      # enable tab when clicking the button
      js$enableMenu("tabSimulationResults")
      js$enableTab("viewOptimizationResults")
      # Change status to complete by highlighting
      js$greenTab("selectOptimizationTab")
      #js$greenMenu("tabOptimization")
      # switch to tab
      updateTabItems(session, "sideTabs", "tabSimulationResults")
      updateNavbarPage(session, "resultsNav", "viewOptimizationResults")
    }
    
  })
  
  
  
  
  output$loadDateTime <- shiny::renderUI({
    object <- mmxModel$full_object[c('date_column','date_format')]
    if(any(sapply(object,is.null))){
      list(HTML(date_time_section_header),
           shiny::fluidRow(class='box1',
                           shiny::column(4,shiny::selectInput('DateVar','Select DateTime Column',
                                                              choices = c("", colnames(reactData$dataset)),multiple=FALSE, selected = "")),
                           shiny::column(4,shiny::textInput("DTformat", "DateTime format (in data)",
                                                            value = "y-m-d")),
                           shiny::column(4,shiny::selectInput("TimeZone", "Select Time Zone",
                                                              choices = OlsonNames(), 
                                                              multiple = FALSE, 
                                                              selected = NULL)),
                           shiny::column(class='button_pad',2,
                                         shiny::actionButton('variable_select_submit','Submit')))
      )}else{
        shiny::tags$p(shiny::HTML('<strong>The date column and its format have been retrieved from the model object</strong>'))
      }
    
  })
  
  DateVar <<- DTformat <<- TimeZone <<- NULL
  reactData$date_time_selected <- FALSE
  reactData$prev_data_level <- NULL
  
  observeEvent(mmxModel$full_object,{
    tryCatch({
      object <- mmxModel$full_object
      object <- object[c('date_column','date_format')]
      
      if(!any(sapply(object,is.null))){
        reactData$DateVar <- object$date_column
        reactData$DTformat <- object$date_format
        reactData$TimeZone <- 'GMT'
        
        reactData$dataset[[reactData$DateVar]] <- lubridate::parse_date_time(reactData$dataset[[reactData$DateVar]],orders = reactData$DTformat)
        # reactData$dataset[[reactData$DateVar]] <- as.Date(reactData$dataset[[reactData$DateVar]])
        
        reactData$min_date <- min(reactData$dataset[[reactData$DateVar]])
        reactData$max_date <- max(reactData$dataset[[reactData$DateVar]])
        
        reactData$preprocessing_text <- paste0(
          "Date Column Selected: ", reactData$DateVar, "\n"
          ,"Start Date of uploaded historical dataset: ", reactData$min_date, "\n"
          ,"End Date of uploaded historical dataset: ", reactData$max_date, "\n"
          ,"Time Zone: ", reactData$TimeZone, "\n"
        )
      }
      
      return(object)
    },error=function(e) showNotification(paste0('Configure Date Variable :',e[1])))
  })
  
  
  shiny::observeEvent(input$variable_select_submit, {
    tryCatch({
      
      varsList <- c(input$DateVar, input$DTformat, input$TimeZone)
      DateVar <<- input$DateVar
      DTformat <<- input$DTformat
      TimeZone <<- input$TimeZone
      
      reactData$DateVar <- input$DateVar
      reactData$DTformat <- input$DTformat
      reactData$TimeZone <- input$TimeZone
      # reactData$dataset <- merge(extReactData$dataset, reactData$dataset, by = reactData$mergeVar)
      
      # Check if the selected column in already in date time format
      if(class(reactData$dataset[,DateVar])[1] %in% c("POSIXct","POSIXt"))
      {
        shiny::showNotification('Selected column is already in Date-Time format',type='warning')
        return()
      }
      
      output$datetime_warning_text <- shiny::renderText("")
      output$datetime_error_text <- shiny::renderText("")
      Sys.setenv(TZ = TimeZone)
      
      reactData$dataset[[reactData$DateVar]] <- lubridate::parse_date_time(reactData$dataset[[reactData$DateVar]],orders = reactData$DTformat)
      reactData$min_date <- min(reactData$dataset[[reactData$DateVar]])
      reactData$max_date <- max(reactData$dataset[[reactData$DateVar]])
      
      
      reactData$preprocessing_text <- paste0(
        "Date Column Selected: ", reactData$DateVar, "\n"
        ,"Start Date: ", reactData$min_date, "\n"
        ,"End Date: ", reactData$max_date, "\n"
        ,"Time Zone: ", reactData$TimeZone, "\n"
        
      )
      
      # Update the select inputs to the original selection.
      # Original selection goes away due to reactivity dependency on dataset which changes on convering to time column
      shiny::updateSelectInput(session,'DateVar',selected=reactData$DateVar)
      shiny::updateSelectInput(session,'DTformat',selected=gsub(",","",reactData$DTformat))
      shiny::updateSelectInput(session,'TimeZone',selected=reactData$TimeZone)
      reactData$missing_datasets <- reactData$dataset
      
    },error = function(e) {
      output$datetime_error_text <- shiny::renderText(e$message)
    },warning = function(w){
      output$datetime_warning_text <- shiny::renderText(w$message)
    }
    )
  })
  
  output$preprocessing <- shiny::renderText({
    reactData$preprocessing_text
  })
  
  #### Checks if spend, trend, panel and target columns are already defined in uploaded model object ####
  
  # Reactive function that returns info for the relevant columns required in this section i.e. spend, panel and y variables
  
  observeEvent(mmxModel$full_object,{
    tryCatch({
      object <- mmxModel$full_object
      col_info_obj <- object[c('spend_variables','y_variable','panel_column','trend_column')]
      reactData$spendVar <- col_info_obj$spend_variables
      reactData$panelVar <- col_info_obj$panel_column
      reactData$targetVar <- col_info_obj$y_variable
      reactData$timeOrderVar <- col_info_obj$trend_column
    },error=function(e) showNotification(paste0('Check variables defined in model object :',e[1])))
  })
  
  ### Checks for click of help icon
  shiny::observeEvent(
    input$loadVariableHelpText,
    {
      showModal(modalDialog(
        title = "Select Spend, Panel and Target Columns - Help",
        shiny::p(shiny::HTML(variableConfigureText)),
        footer = tagList(
          modalButton("OK")
        )
      ))
    }
  )
  
  output$loadVariables <- renderUI({
    mmxObj <- mmxModel$full_object
    
    if((!is.null(mmxObj)))
      if( (is.null(mmxObj$spend_variables)) | (is.null(mmxObj$panel_column)) | (is.null(mmxObj$y_variable))){
        list(
          
          shiny::renderUI({
            list(shiny::fluidRow(class='box1',
                                 shiny::column(3,shiny::selectInput('spendVar','Select Spend Variables',
                                                                    choices = c("",colnames(reactData$dataset)),multiple=TRUE, selected = "")),
                                 shiny::column(3,shiny::selectInput('panelVar','Select Panel Variable',
                                                                    choices = c("", colnames(reactData$dataset)),multiple=FALSE, selected = "")),
                                 shiny::column(3,shiny::selectInput('targetVar','Select Target Variable',
                                                                    choices = c("", colnames(reactData$dataset)), selected = ""))),
                 shiny::fluidRow(style='padding-left:15px',
                                 shiny::column(12,shiny::checkboxInput('trend_column_checkbox','Do you want to specify a trend variable?')),
                                 conditionalPanel('input.trend_column_checkbox',
                                                  shiny::column(3,shiny::selectInput('timeOrderVar','Select Trend Variable',
                                                                                     choices = c("", colnames(reactData$dataset)),multiple=FALSE, selected = "")))),
                 shiny::fluidRow(style='padding-left:15px',
                                 shiny::column(12,shiny::checkboxInput('seasonal_column_checkbox','Do you want to specify a seasonal variable?')),
                                 conditionalPanel('input.seasonal_column_checkbox',
                                                  shiny::column(3,shiny::selectInput('seasonalVar','Select Seasonal Variable',
                                                                                     choices = c("", colnames(reactData$dataset)),multiple=FALSE, selected = "")))),
                 
                 
                 shiny::fluidRow(class='box1',
                                 shiny::column(class='button_pad',4,
                                               shiny::actionButton('spendVarSelection','Submit'))))
            
          }))
      }else{
        shiny::tags$p(shiny::HTML('<strong>Spend, panel and other columns have been retrieved from the model object</strong>'))
      }
  })
  shiny::br()
  
  
  
  
  shiny::observeEvent(input$spendVarSelection, {
    tryCatch({
    spendVar <- list(input$spendVar)
    reactData$spendVar <- input$spendVar
    panelVar <- input$panelVar
    reactData$panelVar <- input$panelVar
    targetVar <- input$targetVar
    reactData$targetVar <- input$targetVar
    
    
    if(is.null(reactData$spendVar)){shiny::showNotification('Please select spend variables', type='error')}
    
    shiny::updateSelectInput(session,'spendVar',selected=reactData$spendVar)
    shiny::updateSelectInput(session,'panelVar',selected=reactData$panelVar)
    shiny::updateSelectInput(session,'targetVar',selected=reactData$targetVar)
    
    if(input$trend_column_checkbox==1){
      timeOrderVar <- input$timeOrderVar
      reactData$timeOrderVar <- input$timeOrderVar
      shiny::updateSelectInput(session,'timeOrderVar',selected=reactData$timeOrderVar)
    }
    
    if(input$seasonal_column_checkbox==1){
      seasonalVar <- input$seasonalVar
      reactData$seasonalVar <- input$seasonalVar
      shiny::updateSelectInput(session,'seasonalVar',selected=reactData$seasonalVar)
    }
    
    },error=function(e) showNotification(paste0('Select Spend, Panel and Target Variables :',e[1])))
  })
  
  output$spend_preprocessing <- shiny::renderText({
    
    reactData$spend_preprocessing_text <- paste0(
      " Spend Column(s) Selected: ", paste(reactData$spendVar,collapse=', '), "\n"
      ," Panel Column Selected: ", paste(reactData$panelVar,collapse=', '), "\n"
      ," Target Column Selected: ", paste(reactData$targetVar,collapse=', '), "\n"
      ," Time Trend Column Selected: ", reactData$timeOrderVar, "\n"
      ," Seasonal Column Selected: ", reactData$seasonalVar, "\n"
    )
  })
  
  
  
  ############################### Historical Data Plot ########################################### 
  
  output$histPlotSelectInput <- shiny::renderUI({
    if(is.null(reactData$dataset) | is.null(reactData$panelVar)) return(shiny::HTML(''))
    shiny::fluidRow(class='box1',
                    column(4,selectInput('loadDataPanelVar','Select Panel', choices = c("", "Total", unique(reactData$dataset[reactData$panelVar])), multiple=TRUE, selected = "Total")),
                    column(2, style='padding:25px', actionButton('viewLoadDataSalesPlot', 'Submit')),
                    shiny::conditionalPanel("input.viewLoadDataSalesPlot>0", column(2, style='padding:25px', downloadButton("downloadLoadDataSalesPlot", "Download Plot(s)"))),
                    shiny::conditionalPanel("input.viewLoadDataSalesPlot>0", column(2, style='padding:25px', downloadButton("downloadLoadDataSalesData", "Download Data"))),
                    # shiny::fluidRow(class='box1', style='padding:30px', shiny::conditionalPanel("input.loadDataPanelVar=='Total'",
                                                                                                shiny::fluidRow(shiny::column(12, shiny::radioButtons("loadSalesChartType", "Panel Type", choices=c("Aggregate", "All"), inline = TRUE)))
                                                                                                # ))
    )
  })
  
  
  shiny::observeEvent(input$viewLoadDataSalesPlot, {
    tryCatch({
    if(nrow(reactData$dataset)<=0) {shiny::showNotification('Please load historical data as described in the Configuration section', type='error'); return()}
    if(is.null(reactData$panelVar) | is.null(reactData$spendVar) | is.null(reactData$targetVar) | is.null(reactData$DateVar)) {shiny::showNotification("Please select Spend, Panel, Seasonal and Trend Columns", type='error'); return()}
    if(input$loadDataPanelVar ==''){
      shiny::showNotification('Please select a valid panel from the list', type='error'); return()
    }
    if("Total" %in% input$loadDataPanelVar && length(input$loadDataPanelVar)>1){
      shiny::showNotification('Multiple selection not supported with "Total" option', type='error'); return()
    }
    # browser()
    panelSelected <- input$loadDataPanelVar
    
    if ("Total" %in% panelSelected | length(panelSelected)>1) {
      wide_dataset = reactData$dataset
      wide_dataset <- fn_filter_dataset(dataset = wide_dataset, filter_col = reactData$panelVar, filter_value = panelSelected, flag = 1)
      ############### Code to generate chart data for "All" panels option #################
      histAllPanelSalesData <- getAllPanelsDataset(dataset=wide_dataset,
                                                   lvl_of_data=c(reactData$DateVar, reactData$panelVar),
                                                   melt_data=c(reactData$spendVar),
                                                   y_variable=reactData$targetVar)
      setnames(histAllPanelSalesData, old=c('variable', reactData$DateVar, reactData$targetVar, 'value'), new=c('Spend_Channel', 'Time_Period', 'Total_Sales', 'Total_Spend'))
    }

    else{
      wide_dataset = dplyr::filter(reactData$dataset, reactData$dataset[reactData$panelVar] == panelSelected)
    }
    
    histSalesData <- getAllPanelsDataset(dataset=wide_dataset,
                                        lvl_of_data=c(reactData$DateVar),
                                        melt_data=c(reactData$spendVar),
                                        y_variable=reactData$targetVar)
    setnames(histSalesData, old=c('variable', reactData$DateVar, reactData$targetVar, 'value'), new=c('Spend_Channel', 'Time_Period', 'Total_Sales', 'Total_Spend'))
    
    loadDataSalesggPlot <- ggplot(histSalesData, aes(x= as.Date(Time_Period), y= Total_Sales)) +
      geom_line(color='#1F77B4', size=1.5) + geom_point(color='#1F77B4', size=4) + theme_light() + theme(plot.title = element_text(hjust = 0.5, size = 14), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=14), legend.spacing.x = unit(0.2, 'cm'))+ scale_x_date(labels = scales::date_format("%Y-%b"), expand = c(0.04,0)) + labs(title = paste0("Time Series Plot of Historical Sales - Panel: ", paste(panelSelected, collapse = ', ')), x = "Time", y = "Historical Sales") + scale_y_continuous(labels=dollar_format(prefix="$"))
    
    if("Total" %in% panelSelected | length(panelSelected)>1){
      
      allLoadDataSalesggPlot <- ggplot(histAllPanelSalesData, aes(x=as.Date(Time_Period), y = Total_Sales)) +
        geom_line(aes(color=histAllPanelSalesData[[reactData$panelVar]]), size=1.5) +  theme_light() + theme(plot.title = element_text(hjust = 0.5, size = 14), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=14)) + labs(title = paste0("Time Series Plot of Historical Sales - Panel: ", paste(panelSelected, collapse = ', ')), x = "Time", y = "Historical Sales") + scale_y_continuous(labels=dollar_format(prefix="$")) + geom_point(aes(color=histAllPanelSalesData[[reactData$panelVar]]), size=4) + scale_x_date(limits = c(min=as.Date(min(histAllPanelSalesData$Time_Period)), max=as.Date(max(histAllPanelSalesData$Time_Period))), expand = c(0.04,0))
      
      loadSalesPlotType <- function(type) 
      {
        switch(type,
               Aggregate = loadDataSalesggPlot,
               All = allLoadDataSalesggPlot
        )
      }  
      
      output$loadDataSalesPlot <- renderPlot({ 
        loadSalesPlotType(input$loadSalesChartType)
      })
    }else{
      output$loadDataSalesPlot <- renderPlot({loadDataSalesggPlot})
    }
    
    output$downloadLoadDataSalesPlot <- shiny::downloadHandler(
      filename = function(){'Historical_Data_Plot.png'},
      content = function(file) {
        ###### Using function loadSalesPlotType() to switch download plot based on Panel Type : Aggregate or All
        ggsave(file, plot = loadSalesPlotType(input$loadSalesChartType), device = "png", width = 45, height = 25, units = 'cm')
      },
      shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
    )
    
    output$downloadLoadDataSalesData <- shiny::downloadHandler(
      filename = function() {'Historical_Data.csv'},
      content = function(file) {
        write.csv(histSalesData, file, row.names = FALSE)
      }
    )
    
    #### Plot for Report 
    reactData$loadDataSalesPlot<- loadDataSalesggPlot
    
    },error=function(e) showNotification(paste0('Load Historical Sales Plot - Help Text :',e[1])))
  })
  
  output$histPlotsUI <- shiny::renderUI({
    shiny::fluidRow(class='box1', shiny::br(), shiny::column(12, shiny::plotOutput('loadDataSalesPlot')))
  })
  
  
  # ----------------------------------------------------------------------------------------------
  # Configure adstock decay rates
  #  ---------------------------------------------------------------------------------------------
  observeEvent(mmxModel$full_object,{
    tryCatch({
      # browser()
      adstock_values <- mmxModel$full_object$adstock_decay_rates
      # print(mmxModel$full_object)
      if(!is.null(adstock_values)){
        reactData$adstockDecayValues <- adstock_values
        output$adstockDecayTextUI <- renderUI({HTML('<strong>Adstock decay values have been retrieved from the model object uploaded and are shown in the table below.</strong>')})
      } else {
        reactData$adstockDecayValues <- NULL
        output$adstockDecayTextUI <- renderUI({HTML('<strong>Adstock decay values not specified in model object. Please configure the table below.</strong>')})
      }
    },error=function(e) showNotification(paste0('Check variables defined in model object :',e[1])))
  })
  
  output$downloadAdstockData <- shiny::downloadHandler(
    filename = function() {
      paste("Adstock_Decay", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(adstock$dataset, file, row.names = FALSE)
    },
    shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
  )
  
  
  output$adstockTable <-  renderRHandsontable({
    tryCatch({
      
      adstockSampleData <- data.frame()
      list_of_samples <- 0
      
      if(!is.null(reactData$spendVar)){
        adstockSampleData <- as.data.frame(list(Spend_Channel = reactData$spendVar))
        
        if(!is.null(reactData$adstockDecayValues)){
          decay_df <- data.frame(Adstock_Decay=(data.frame(reactData$adstockDecayValues,stringsAsFactors = FALSE)))
          # decay_df$Spend_Channel <- row.names(decay_df)
          adstockSampleData <- data.frame(cbind(adstockSampleData, decay_df), stringsAsFactors = FALSE)
          colnames(adstockSampleData) <- c("Spend_Channel", "Adstock_Decay")
          # merge(adstockSampleData,decay_df,by='Spend_Channel',all.x=TRUE)
        } else {
          adstockSampleData$Adstock_Decay  <- rep(0,nrow(adstockSampleData))
        }
        # reactData$adstockDecayData <- adstockSampleData
        
      }
    
    
    # Check if data changed 
    # if(identical(reactData$adstockDecayData,adstockSampleData))
    #   newDataflag <- 1
    # else newDataflag <- 0
    # reactData$adstockDecayData <- adstockSampleData
    
    if ((!is.null(input$adstockTable)) ) { # if there is an rhot user input...
      adstock$dataset <- hot_to_r(input$adstockTable) # convert rhandsontable data to R object and store in data frame
    }
    else {
      adstock$dataset <- adstockSampleData
    }
    if(nrow(adstock$dataset)==0){
      shiny::p(shiny::HTML("Please configure date, spend, panel and target variables before configuring adstock decay rates."))
    }else{
      rhandsontable(adstock$dataset) %>% # actual rhandsontable object
        hot_table(highlightCol = TRUE, highlightRow = TRUE, readOnly = TRUE) %>%
        hot_col("Adstock_Decay", readOnly = FALSE, renderer = color_renderer)
    }
    
    })
  })
  
  output$adstockSubmitUI <- shiny::renderUI({
    # browesr()
    list(
      shiny::br(),
      shiny::fluidRow(shiny::column(2, shiny::actionButton('adstockSubmit', 'Apply Adstock'))),
      shiny::br(),   
      shiny::fluidRow(shiny::column(12, shiny::verbatimTextOutput('adstockSubmissionText'))))
  })
  
  shiny::observeEvent(input$adstockSubmit, {
    tryCatch({
      
    ######## Error handling ########
    if(is.null(mmxModel$train_data) | is.null(reactData$spendVar) | is.null(reactData$DateVar) | is.null(reactData$panelVar)){shiny::showNotification('Please load model object and configure spend,date and panel variables', type='error'); return()}
    
    if(nrow(reactData$dataset)<=0){shiny::showNotification('Please load historical data as described in the Upload ADS and Model tab')}
    
    ######## Converting data type of historical data based on train data
    reactData$dataset <- convertBackFactors_2(mmxModel$train_data, reactData$dataset)
    
    ################# Creating adstock transformed dataset #####################
    reactData$adstockSpendValues <- adstock$dataset$Adstock_Decay
    adstock$dataset$Adstock_variable <- paste0(reactData$spendVar, "_adstock")
    reactData$adstockSpendVar <- c(adstock$dataset$Adstock_variable)
    
    ### Adstock transforming the spends
    adstock_data <- apply_adstock(dataset=reactData$dataset,
                                  spend_variables=as.character(adstock$dataset$Spend_Channel),
                                  decay_rate=adstock$dataset$Adstock_Decay,
                                  time_variable=reactData$DateVar,
                                  adstock_panel_variables=reactData$panelVar)
    keeps <- c(reactData$DateVar, reactData$panelVar, reactData$spendVar)
    adstock_data <- adstock_data[keeps]
    
    #### Creating dataframe with adstock and non-adstock spends in wide format
    reactData$all_adstock_data <- merge(reactData$dataset, adstock_data, by=c(reactData$DateVar, reactData$panelVar))
    names(reactData$all_adstock_data) = gsub(pattern = ".x", replacement = "", x = names(reactData$all_adstock_data))
    names(reactData$all_adstock_data) = gsub(pattern = ".y", replacement = "_adstock", x = names(reactData$all_adstock_data))
    
    #### Creating dataframe with non-adstocked spends
    long_dataset_1 <- melt(reactData$all_adstock_data, measure.vars=reactData$spendVar)
    setnames(long_dataset_1, old=c("variable", "value", reactData$DateVar), new=c("Spend_Channel", "Spends", "Time_Period"))
    
    #### Creating dataframe with adstocked spends
    long_dataset_2 <- melt(reactData$all_adstock_data, measure.vars=reactData$adstockSpendVar)
    setnames(long_dataset_2, old=c("variable", "value", reactData$DateVar), new=c("Adstock_Spend_Channel", "Adstock_Spends", "Time_Period"))
    long_dataset_2 <- merge(long_dataset_2, adstock$dataset, by.x=c('Adstock_Spend_Channel'), by.y=c('Adstock_variable'))
    
    #### Keeping only required columns
    
    keeps_1 <- c('Time_Period', reactData$panelVar, 'Spend_Channel', 'Spends')
    keeps_2 <- c('Time_Period', reactData$panelVar, 'Spend_Channel', 'Adstock_Spends')
    
    long_dataset_1 <- long_dataset_1[keeps_1]
    long_dataset_2 <- long_dataset_2[keeps_2]
    
    #### Merging above dataframes to get both adstocked and non-adstocked spends by channel, panel and time
    reactData$adstock_plot_dataset <- merge(long_dataset_1, long_dataset_2, by=c('Time_Period', reactData$panelVar, 'Spend_Channel'))
    
    ###### Output to indicate successful submission of adstock values #######
    output$adstockSubmissionText <- shiny::renderText('Spends have been adstock transformed successfully')
  },error=function(e) showNotification(paste0('Apply Adstock :',e[1])))
  })
  
  
  
  
  output$adstockPanelUI <- shiny::renderUI({
    shiny::conditionalPanel("input.adstockSubmit>0", shiny::fluidRow(class='box1',
                                                                     column(12, h4('View Adstock Spend Comparison Plot')),
                                                                     shiny::br(),
                                                                     shiny::column(3, shiny::selectInput('viewAdstockPanelVar','Select Panel', choices = c("", "Total", unique(reactData$dataset[reactData$panelVar])), multiple=FALSE, selected = "Total")),
                                                                     shiny::column(3, shiny::selectInput('viewAdstockChannelVar','Select Channel', choices = c("", "Total", reactData$spendVar), multiple=FALSE, selected = "Total")),
                                                                     shiny::column(style='padding-top:25px', class='button_pad',2,
                                                                                   shiny::actionButton('viewAdstockPanelChannelSelected','Submit')),
                                                                     shiny::conditionalPanel("input.viewAdstockPanelChannelSelected>0",shiny::column(2, style='padding-top:25px', shiny::downloadButton('downloadAdstockPlots','Download Plot(s)'))),
                                                                     shiny::conditionalPanel("input.viewAdstockPanelChannelSelected>0",shiny::column(2, style='padding-top:25px', shiny::downloadButton('downloadAdstockData','Download Data')))
    ))
  })
  
  shiny::observeEvent(input$viewAdstockPanelChannelSelected, {
    tryCatch({
    if(nrow(reactData$dataset)<=0) {shiny::showNotification('Please load historical data as described in the Configuration section', type='error'); return()}
    if(is.null(reactData$panelVar) | is.null(reactData$spendVar) | is.null(reactData$DateVar)) {shiny::showNotification("Please select Spend, Panel, Adstock and Target Columns", type='error'); return()}
      if(input$viewAdstockPanelVar==""){
        shiny::showNotification('Please select a valid panel from the list', type='error'); return()
      }
      if(input$viewAdstockChannelVar==""){
        shiny::showNotification('Please select a valid channel from the list', type='error'); return()
      }
      
    panelSelected <- input$viewAdstockPanelVar
    channelSelected <- input$viewAdstockChannelVar
    
    
    if(!("Total" %in% panelSelected) & !("Total" %in% channelSelected)){
      
      adstock_plot_dataset <- dplyr::filter(reactData$adstock_plot_dataset, reactData$adstock_plot_dataset[reactData$panelVar] == panelSelected & reactData$adstock_plot_dataset["Spend_Channel"] == channelSelected)
    }
    if(!("Total" %in% channelSelected) & ("Total" %in% panelSelected)){
      
      adstock_plot_dataset <- dplyr::filter(reactData$adstock_plot_dataset, reactData$adstock_plot_dataset["Spend_Channel"] == channelSelected)
    }
    if (!("Total" %in% panelSelected) & "Total" %in% channelSelected){
      
      adstock_plot_dataset <- dplyr::filter(reactData$adstock_plot_dataset, reactData$adstock_plot_dataset[reactData$panelVar] == panelSelected)
    }
    if(("Total" %in% panelSelected) & ("Total" %in% channelSelected)){
      
      adstock_plot_dataset <- reactData$adstock_plot_dataset
    }
    
    # Group the data
    
    adstock_plot_dataset <- dplyr::group_by_at(adstock_plot_dataset, "Time_Period")
    adstock_plot_dataset <- dplyr::summarise(adstock_plot_dataset, Total_Adstock_Spends = sum(Adstock_Spends, na.rm = TRUE),
                                             Total_Spends = sum(Spends, na.rm=TRUE))
    
    adstockggPlot <- ggplot(adstock_plot_dataset, aes(x=as.Date(Time_Period))) +
      geom_line(aes(y=Total_Spends, color='Total_Spends'), size=1.5) + geom_point(aes(y=adstock_plot_dataset$Total_Spends, color='Total_Spends'), size=4) +
      geom_line(aes(y=Total_Adstock_Spends, color='Total_Adstock_Spends'), size=1.5) + geom_point(aes(y=adstock_plot_dataset$Total_Adstock_Spends, color='Total_Adstock_Spends'), size=4) + 
      labs(title = paste0("Adstock Comparison Plots - Panel: ", panelSelected, ", Channel: ", channelSelected), x = "Time", y = "Spends") +  theme_light() + theme(plot.title = element_text(hjust = 0.5, size = 14), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=14), legend.spacing.x = unit(0.2, 'cm')) +
      scale_color_manual(values = c(
        'Total_Spends' = '#1F77B4',
        'Total_Adstock_Spends' = '#AEC7E8')) + scale_y_continuous(labels=dollar_format(prefix="$")) + scale_x_date(expand = c(0.04,0))
    
    output$adstockPlot = renderPlot({adstockggPlot})
    
    output$downloadAdstockData <- shiny::downloadHandler(
      filename = function() {'Adstock_Data.csv'},
      content = function(file) {
        write.csv(adstock_plot_dataset, file, row.names = FALSE)
      },
      shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
    )
    
    output$downloadAdstockPlots <- shiny::downloadHandler(
      filename = function(){'Adstock_Plots.png'},
      content = function(file) {
        ggsave(file, plot=adstockggPlot, device='png', width = 45, height = 25, units = 'cm')
      }
    )
    
    #### Plot for report
    reactData$reportAdstockPlot <- adstockggPlot
      
    },error=function(e) showNotification(paste0('Load Adstock Plots :',e[1])))
  })
  
  
  # ----------------------------------------------------------------------------------------------
  # Define Simulation Period
  #  ---------------------------------------------------------------------------------------------
  
  ### Checks for click of help icon
  shiny::observeEvent(
    input$simulationPeriodHelpText,
    {
      showModal(modalDialog(
        title = "Define Simulation/Optimization Period - Help",
        shiny::p(shiny::HTML(timePeriodConfigText)),
        footer = tagList(
          modalButton("OK")
        )
      ))
    }
  )
  
  output$simulationPeriodDatesText <- shiny::renderUI({
    list(shiny::tags$h5(shiny::HTML(paste0('<b>Uploaded Historical Period: </b>', reactData$min_date,' to ', reactData$max_date))))
  })
  
  
  output$simulationPeriodSelectionUI <- shiny::renderUI({
    
    list(shiny::fluidRow(class='box1',
                         # shiny::column(12, shiny::tags$span(style = "color:red", shiny::HTML('Below selection will define the simulation/optimization period'))),
                         shiny::column(4,shiny::dateInput('simStartDateVar',label='Select Simulation/Optimization Start Date', value="2017-01-01")),
                         shiny::column(4,shiny::dateInput('simEndDateVar',label='Select Simulation/Optimization End Date', value="2017-06-01")),
                         shiny::column(4, shiny::selectInput("dateFrequency", "Select Frequency of Time Series", choices = c('','Monthly', 'Weekly', 'Daily'), selected = "")),
                         shiny::column(class='button_pad',2,shiny::actionButton('dateVarSelection','Submit')),
                         shiny::br()),
         shiny::fluidRow(shiny::column(12,shiny::verbatimTextOutput("timePeriodPreprocessing"))))
  })
  
  shiny::observeEvent(input$dateVarSelection, {
    tryCatch({
    simStartDateVar <<- input$simStartDateVar
    reactData$simStartDateVar <- input$simStartDateVar
    simEndDateVar <<- input$simEndDateVar
    reactData$simEndDateVar <- input$simEndDateVar
    dateFrequency <<- input$dateFrequency
    reactData$dateFrequency <- input$dateFrequency
    
    ######## Progress bar #########
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Processing...")
    
    ######## Error handling #########
    if(is.null(simStartDateVar)){shiny::showNotification("Please select simulation start date", type='error'); return()}
    if(is.null(simEndDateVar)){shiny::showNotification("Please select simulation end date", type='error'); return()}
    if(is.null(dateFrequency)){shiny::showNotification("Please select frequency of time series", type='error'); return()}
    if(reactData$dateFrequency == '') {shiny::showNotification('Please select a valid time series frequency from the options provided', type='error'); return()
    }
    if(nrow(reactData$dataset)<=0) {shiny::showNotification('Please load historical data as described in the Configuration section', type='error'); return()}
    if(is.null(reactData$DateVar) | nrow(adstock$dataset)<=0) {shiny::showNotification("Please select Spend, Panel, Adstock and Target Columns", type='error'); return()}
    
    
    ####### Assigning numeric values to date frequency selected #######
    if(reactData$dateFrequency == 'Monthly'){
      reactData$numDateFrequency <- 12
      reactData$typeDateFrequency <- 'month'
    }
    
    if(reactData$dateFrequency == 'Weekly'){
      reactData$numDateFrequency <- 52
      reactData$typeDateFrequency <- 'week'
    }
    
    if(reactData$dateFrequency == 'Daily'){
      reactData$numDateFrequency <- 365
      reactData$typeDateFrequency <- 'days'
    }
    
 
    
    reactData$long_dataset <- melt(reactData$dataset, measure.vars=reactData$spendVar)
    
    ###### Converting simulation start and end dates to POSIXlt format
    reactData$simStartDateVar <- as.POSIXlt(reactData$simStartDateVar)
    reactData$simEndDateVar <- as.POSIXlt(reactData$simEndDateVar)
    
    ####### Assigning initial values to historical comparison start and end dates
    reactData$histStartDateVar <- reactData$simStartDateVar
    reactData$histEndDateVar <- reactData$simEndDateVar
    
    #### Extracting historical start and end dates corresponding to simulation period selected ###
    reactData$histStartDateVar$year <- reactData$simStartDateVar$year-1
    reactData$histStartDateVar<- as.Date(reactData$histStartDateVar)
    
    reactData$histEndDateVar$year <- reactData$simEndDateVar$year-1
    reactData$histEndDateVar <- as.Date(reactData$histEndDateVar)
    # browser()
    # Error handling for Simulation/ Optimization Time Period
    if(reactData$histEndDateVar > reactData$max_date){shiny::showNotification("Error Message: Please select valid simulation/ optimization time period", type='error'); return()}
    
    #### Filtering uploaded dataset for historical comparable period ####
    histReactData$dataset <- filter_at(reactData$dataset, reactData$DateVar, any_vars(. >= reactData$histStartDateVar & . <= reactData$histEndDateVar))
    
    # Historical data from which the spends range is to be considered
    histReactData$long_dataset <- melt(histReactData$dataset, measure.vars=reactData$spendVar)
    setnames(histReactData$long_dataset, old=c("variable", "value"), new=c("Spend_Channel", "Spends"))
    
    drops <- c(reactData$adstockSpendVar, reactData$adstockSpendValues)
    histReactData$long_dataset <- histReactData$long_dataset[ , !names(histReactData$long_dataset) %in% drops]
    # browser()
    # Getting adstock data
    
    drops <- c(reactData$spendVar)
    adstock$allTransformedDataset <- reactData$all_adstock_data[ , !(names(reactData$all_adstock_data) %in% drops)]
    names(adstock$allTransformedDataset) = gsub(pattern = "_adstock*", replacement = "", x = names(adstock$allTransformedDataset))
    
    adstock$transformedDataset <- filter_at(adstock$allTransformedDataset, reactData$DateVar, any_vars(. >= reactData$histStartDateVar & . <= reactData$histEndDateVar))
    
    # adstock$transformedDataset <- melt(reactData$adstock_plot_dataset, measure.vars = )
    
    # adstock$dataset$Spend_Channel <- gsub("\\s", "", adstock$dataset$Spend_Channel)
    # adstock$dataset$Adstock_Variable_Replace <- paste0(adstock$dataset$Spend_Channel,"_adstock")
    # adstock$dataset$Adstock_Variable_Replace <-  as.character(adstock$dataset$Adstock_Variable_Replace)
    # adstock$dataset$Adstock_Variable <- as.character(adstock$dataset$Adstock_Variable)
    # 
    # new_names = list(n=adstock$dataset$Adstock_Variable_Replace)
    # old_names = list(o=adstock$dataset$Adstock_Variable)
    # setnames(adstock$transformedDataset, old=old_names$o, new=new_names$n)
    # names(adstock$transformedDataset) = gsub(pattern = "_adstock*", replacement = "", x = names(adstock$transformedDataset))
    
    adstock$long_dataset <- melt(adstock$transformedDataset, measure.vars=reactData$spendVar)
    setnames(adstock$long_dataset, old = c('variable', 'value'), new = c('Spend_Channel', 'Spends'))
    keeps <- c("Spend_Channel", "Adstock_Decay")
    adstock$value_dataset <- adstock$dataset[keeps]
    
    reactData$optiDateSequence <- generateDatesDataframe(startDate = as.Date(reactData$simStartDateVar), endDate = as.Date(reactData$simEndDateVar), dateFrequency = reactData$dateFrequency, dateVar = reactData$DateVar)
    reactData$histDateSequence <- generateDatesDataframe(startDate = reactData$histStartDateVar, endDate = reactData$histEndDateVar, dateFrequency = reactData$dateFrequency, dateVar = reactData$DateVar)
    
    reactData$allDateSequence <- merge(reactData$histDateSequence, reactData$optiDateSequence, by=c('time'))
    #setnames(reactData$allDateSequence, old=c('Month_Date.x', 'Month_Date.y'), new=c(paste0('Hist_', reactData$DateVar), paste0('Opti_', reactData$DateVar)))
    
    setnames(reactData$allDateSequence, old=c(paste(reactData$DateVar,".x",sep=""), paste(reactData$DateVar,".y",sep="")), new=c(paste0('Hist_', reactData$DateVar), paste0('Opti_', reactData$DateVar)))
    
    reactData$histMeanDateVar <- mean(as.Date(reactData$histDateSequence[[reactData$DateVar]]))
    
    reactData$timePeriodPreprocessingText <- paste0(
      " Simulation/Optimization Period \n \t Start Date Selected: ", reactData$simStartDateVar, "\n"
      ," \t End Date Selected: ", reactData$simEndDateVar, "\n\n"
      ," Historical Comparison Period \n \t Start Date Selected: ", reactData$histStartDateVar, "\n"
      ," \t End Date Selected: ", reactData$histEndDateVar, "\n\n"
      ," Time Series Frequency: ", reactData$dateFrequency, "\n"
    )
    },error=function(e) showNotification(paste0('Select Simulation/Optimization Period :',e[1])))
  })
  
  output$timePeriodPreprocessing <- shiny::renderText(
    reactData$timePeriodPreprocessingText
  )
  

  
  
  # ----------------------------------------------------------------------------------------------
  # Load Non-Spend Variables
  #  ---------------------------------------------------------------------------------------------
  
  
  ### Checks for click of help icon
  shiny::observeEvent(
    input$extPostHelpText,
    {
      showModal(modalDialog(
        title = "Load non-spend Variables - Help",
        shiny::p(shiny::HTML(exPostText)),
        footer = tagList(
          modalButton("OK")
        )
      ))
    })
  
  
  ### Conductor for loading external variables' CSV file
  extDataset <- shiny::reactive({
    infile <- input$extDataFile
    infile$datapath
    if(is.null(infile))
    {
      # User has not uploaded a file yet
      return(data.frame())
    }
    else
    {
      if(file_ext(infile$datapath)!='csv')
      {
        shiny::showNotification('Please load only CSV files', type='error');
        return()
      }
      dataset <- data.frame(read.csv(infile$datapath))
      return(dataset)
    }
  })
  
  ### Checks change of state for upload of external variable data file
  shiny::observeEvent(
    input$extDataFile,
    {
      tryCatch({
      dataset <- extDataset()
      extReactData$dataset <- dataset
      nums <-  unlist(lapply(extReactData$dataset, is.numeric)) 
      output$extHeadTable <- DT::renderDataTable(DT::datatable(roundOffDataFrame(head(extReactData$dataset), TRUE,2),options=list(scrollX=T, searching=FALSE, dom = 't'))%>%
                                                   formatCurrency(colnames(extReactData$dataset[nums]),currency = "", interval = 3, mark = ","))
    },error=function(e) showNotification(paste0('Load External Data :',e[1])))
    })
  
  mergeText <- "Select the columns using which the historical dataset will be merged with the uploaded external dataset"
  
  output$externalDataDatesUI <- shiny::renderUI({
                         shiny::column(12, shiny::tags$p(paste0('Please load non-spend variables for the period between ', reactData$simStartDateVar, ' and ', reactData$simEndDateVar, ' i.e. Simulation/Optimization Period defined previously')))
  })
  
  ### Displays output of uploaded external factors data file
  output$externalDataInfoUI <- shiny::renderUI({
    if(nrow(extReactData$dataset)){
      list(
        shiny::HTML(paste0("The uploaded external dataset ",
                           " contains <b> ",
                           nrow(extReactData$dataset),
                           "</b> Rows and <b>",
                           ncol(extReactData$dataset),
                           "</b> Columns.")),
        shiny::renderText("First 6 rows have been shown below in order to ensure that relevant data has been loaded for the analysis."),
        DT::dataTableOutput("extHeadTable"),
        shiny::tags$p(shiny::HTML(mergeText))
      )
    }
  })
  
  output$externalDataInputUI <- shiny::renderUI({
    tagList(shiny::fluidRow(class='box1',
                            shiny::column(4, shiny::selectInput('mergeVar', 'Select Merge Columns', choices = c("", colnames(extReactData$dataset)), multiple = TRUE, selected = "")),
                            shiny::column(style='padding-top:25px', class='button_pad',2, shiny::actionButton('extVarSubmit','Submit'))),
            shiny::column(12,shiny::verbatimTextOutput("extVarConfiguration"))
    )
  })
  
  shiny::observeEvent(input$extVarSubmit, {
    tryCatch({
    ######## Progress bar #########
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Processing...")
    
    ### Error handling
    if(is.null(mmxModel$train_data) | is.null(reactData$dataset) | is.null(adstock$dataset)){shiny::showNotification('Please load and configure historical data and model object as described in the Load ADS and model tab', type='error'); return()}
    if(is.null(reactData$panelVar) | is.null(reactData$spendVar)){shiny::showNotification('Please configure spend and panel variables as described in the Configure Variables tab', type='error'); return()}
    if(is.null(reactData$DateVar) | is.null(reactData$DTformat) | is.null(reactData$TimeZone)){shiny::showNotification('Please configure date and date format as described in the Configure Variables tab', type='error'); return()}
    if(is.null(reactData$simStartDateVar) | is.null(reactData$simEndDateVar) | is.null(reactData$numDateFrequency)){shiny::showNotification('Please select simulation dates and date frequency above', type='error'); return()}

    
    extReactData$dataset[reactData$DateVar] <- lubridate::parse_date_time(extReactData$dataset[[reactData$DateVar]],orders = reactData$DTformat)
    
    extReactData$dataset <- convertBackFactors_2(mmxModel$train_data, extReactData$dataset)
    
    
    reactData$mergeVar <- input$mergeVar
    # reactData$timeOrderVar <- input$timeOrderVar
    
    extVarConfigurationText <- paste0(
      # "* External Column(s) Selected: ", list(reactData$extVar), "\n",
      "Merge Column(s) Selected: ", list(reactData$mergeVar), "\n"
      # ," Trend Column Selected: ", reactData$timeOrderVar, "\n"
    )
    output$extVarConfiguration <- shiny::renderText(
      extVarConfigurationText
    )
    
    # setnames(reactData$dataset, old=c('Time'), new=c(reactData$timeOrderVar))
    
    ########### Extracting historical comparison time period for give simulation period ###################
    
    simExtReactData$dataset <- filter_at(extReactData$dataset, reactData$DateVar, any_vars(. >= reactData$simStartDateVar & . <= reactData$simEndDateVar))
    
    #### Converting simulation dates to POSIXct format to extract historical dates #### 
    reactData$simStartDateVar <-as.POSIXlt(as.Date(reactData$simStartDateVar))
    reactData$simEndDateVar <- as.POSIXlt(as.Date(reactData$simEndDateVar))
  },error=function(e) showNotification(paste0('Submit External Data :',e[1])))
  })
  
  
  
  
  ##########################################################################################################################
  # Generate Simulation Scenarios tab
  ##########################################################################################################################
  
  #--------------------------------------------------------------------------------
  # Historical Comparison Period Performance
  #--------------------------------------------------------------------------------
  
  ### Checks for click of help icon
  shiny::observeEvent(
    input$histPerformanceHelpText,
    {
      showModal(modalDialog(
        title = "Historical Performance - Help",
        shiny::p(shiny::HTML(histPerformanceText)),
        footer = tagList(
          modalButton("OK")
        )
      ))
    }
  )
  
  output$histDatesText <- shiny::renderUI({
    if(is.null(reactData$simStartDateVar)) return(shiny::HTML(''))
    list(shiny::tags$h5(shiny::HTML(paste0('<b>Historical Comparison Period: </b>', reactData$histStartDateVar,' to ', reactData$histEndDateVar))),
         shiny::tags$h5(shiny::HTML(paste0('<b>Simulation Period: </b>', reactData$simStartDateVar,' to ', reactData$simEndDateVar))))
  })
  
  output$historicalPerformanceUI <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(4,shiny::selectInput('histPanelVar','Select Panel', choices = c("", "Total", unique(reactData$dataset[reactData$panelVar])), multiple=FALSE, selected = "Total"))
                    ,shiny::column(style='padding-top:25px', class='button_pad',2,shiny::actionButton('viewHistPanelSelected','Submit'))
                    ,shiny::conditionalPanel("input.viewHistPanelSelected>0",shiny::column(2, style='padding-top:25px', shiny::downloadButton('downloadHistComparisonPeriodPlots','Download Plot(s)')))
                    ,shiny::conditionalPanel("input.viewHistPanelSelected>0",shiny::column(2, style='padding-top:25px', shiny::downloadButton('downloadHistComparisonPeriodData','Download Data')))
    )
  })
  
  output$historicalPerformancePlotsUI <- renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(12,
                                  shiny::tabsetPanel(type = "tabs",
                                                     shiny::tabPanel("Tabular View", 
                                                                     shiny::br(), 
                                                                     shiny::fluidRow(shiny::column(12, DT::dataTableOutput('agg_data_long')))),
                                                     shiny::tabPanel("Sales", 
                                                                     shiny::br(),
                                                                     shiny::conditionalPanel("input.histPanelVar=='Total'",shiny::fluidRow(shiny::column(6, shiny::radioButtons("histSalesChartType", "Panel Type", choices=c("Aggregate", "All"), inline = TRUE)))),
                                                                     shiny::fluidRow(shiny::column(12, shiny::plotOutput('salesPlot')))),
                                                     shiny::tabPanel("Spend",
                                                                     shiny::br(),
                                                                     shiny::fluidRow(shiny::column(6, shiny::radioButtons("histSpendChartType", "Spend Type", choices=c("Absolute", "Percentage"), inline = TRUE))),
                                                                     shiny::fluidRow(shiny::column(12, shiny::plotOutput("spendAreaPlot")))),
                                                     
                                                     shiny::tabPanel("Contribution", 
                                                                     shiny::br(),
                                                                     shiny::fluidRow(shiny::column(6, shiny::radioButtons("histContributionChartType", "Contribution Type", choices=c("Absolute", "Percentage"), inline = TRUE))),
                                                                     shiny::fluidRow(shiny::column(12, shiny::plotOutput("contributionAreaPlot")))),
                                                     shiny::tabPanel("ROI",
                                                                     shiny::br(),
                                                                     shiny::fluidRow(shiny::column(12, shiny::plotOutput('roiPlot')))))
                    ))
  })
  
  
  shiny::observeEvent(input$viewHistPanelSelected, {
    tryCatch({
      #### Error handling #####
      if(nrow(histReactData$dataset)<=0) {shiny::showNotification('Please load historical data as described in the Configuration section', type='error'); return()}
      
      if(is.null(reactData$spendVar) | is.null(reactData$panelVar) | is.null(reactData$targetVar) | is.null(reactData$DateVar)) {shiny::showNotification('Please configure spend, panel and date variables as described in the Configuration section', type='error'); return()}
      
      if(nrow(adstock$transformedDataset)<=0) {shiny::showNotification('Please adstock transform spends in Configuration section', type='error'); return()}
      
      if(input$histPanelVar==""){
        shiny::showNotification('Please select a valid panel from the list', type='error'); return()
      }  
      
      histReactData$histPanelVar <- input$histPanelVar
      
      ######## Progress bar #########
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Processing...")
      
      
      
      if ("Total" %in% histReactData$histPanelVar) {
        all_data = reactData$dataset
        all_adstock_data = adstock$allTransformedDataset
        ############### Code to generate chart data for "All" panels option #################
        histAllPanelSalesData <- getAllPanelsDataset(dataset=all_data,
                                                     lvl_of_data=c(reactData$DateVar, reactData$panelVar),
                                                     melt_data=c(reactData$spendVar),
                                                     y_variable=reactData$targetVar)
        setnames(histAllPanelSalesData, old=c('variable', reactData$DateVar, reactData$targetVar, 'value'), new=c('Spend_Channel', 'Time_Period', 'Total_Sales', 'Total_Spend'))
        
      }
      else{
        all_data <- dplyr::filter(reactData$dataset, reactData$dataset[reactData$panelVar] == histReactData$histPanelVar)
        all_adstock_data <- dplyr::filter(adstock$allTransformedDataset, adstock$allTransformedDataset[reactData$panelVar] == histReactData$histPanelVar)
      }
      ############################## new ##############################
      histAreaData <- getAllPanelsDataset(dataset=all_data,
                                          lvl_of_data=c(reactData$DateVar),
                                          melt_data=c(reactData$spendVar),
                                          y_variable=reactData$targetVar)
      setnames(histAreaData, old=c('variable', reactData$DateVar, reactData$targetVar, 'value'), new=c('Spend_Channel', 'Time_Period', 'Total_Sales', 'Total_Spend'))
      
      ####################################### new #####################################
      histAreaData <- getSpendsDataset(dataset=histAreaData,
                                       lvl_of_data=c('Time_Period'),
                                       spend_variable='Total_Spend')
      ###########################################################
      
      
      all_adstock_data <- convertBackFactors_2(mmxModel$train_data, all_adstock_data)
      
      ############################# Calculating Contributions ############################################
      #### Get Absolute Contributions
      contributionAbsData <- fn_contrib(mmx_model = mmxModel$model
                                        , master_dataset = as.data.frame(all_adstock_data)
                                        , spendVar = reactData$spendVar
                                        , req_contrib_cols = c(reactData$DateVar)
                                        , DateVar = reactData$DateVar
                                        , panelVar = reactData$panelVar)
      
      setnames(contributionAbsData, old = c(reactData$DateVar,'variable', 'value'), new = c('Time_Period','Spend_Channel', 'Total_Contribution'))
      
      #### Get percentage contributions
      contributionAllData <- fn_pct_contrib(dataset = contributionAbsData,
                                            lvl_of_data = c('Time_Period'),
                                            contrib_col = 'Total_Contribution')
      #### Get all historical KPIs by merging contribution KPIs with spend and sales KPIs
      histAllData <- merge(contributionAllData, histAreaData, by=c('Time_Period','Spend_Channel'))
      #### Calculate RoI
      histAllData <- histAllData %>% mutate(Total_ROI = Total_Contribution/Total_Spend)
      
      ### Tabular view of historical comparison period data
      group_data_long <- histAllData
      keeps <- c('Time_Period', 'Spend_Channel', 'Total_Sales', 'Total_Spend', 'Total_Contribution', 'Total_Pct_Contribution', 'Total_ROI')
      group_data_long <- group_data_long[keeps]
      group_data_long$Time_Period <- as.Date(group_data_long$Time_Period)
      
      nums <-  unlist(lapply(group_data_long, is.numeric))
      output$agg_data_long <- DT::renderDataTable(DT::datatable(roundOffDataFrame(group_data_long, TRUE, 2), options=list(scrollX=T, searching=TRUE, paging=TRUE))%>%formatCurrency(colnames(group_data_long[nums]),currency = "", interval = 3, mark = ","))
      
      ############################ Visualizations for historical data #############################
      
      ####################################### Spend ###############################################
      
      spendAreaPctggPlot <- ggplot(histAllData, aes(x=as.Date(Time_Period), y=Total_Pct_Spend, fill=Spend_Channel)) +
        geom_area() + ggtitle(paste0("Time Series Plot of Percentage Spends - Panel: ", histReactData$histPanelVar))  + theme_light() + theme(plot.title = element_text(hjust = 0.5, size = 14), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=14), legend.spacing.x = unit(0.2, 'cm')) + scale_fill_manual(values= vcolors) + scale_x_date(labels = scales::date_format("%Y-%b")) + labs(x='Time', y='Total Spend (in %)') +
        geom_vline(color='red',linetype='dashed',xintercept = as.Date(reactData$histStartDateVar)) + 
        geom_vline(linetype='dashed',color='red',xintercept = as.Date(reactData$histEndDateVar)) + 
        annotate(color='red',"text", x = as.Date(reactData$histMeanDateVar ), y = 1, label = paste( "Historical Comparison Time Period")) + scale_x_date(expand = c(0.04,0))
      
      spendAreaAbsggPlot <- ggplot(histAllData, aes(x=as.Date(Time_Period), y=Total_Spend, fill=Spend_Channel)) +
        geom_area() + ggtitle(paste0("Time Series Plot of Absolute Spends - Panel: ", histReactData$histPanelVar))  + theme_light() + theme(plot.title = element_text(hjust = 0.5, size = 14), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=14), legend.spacing.x = unit(0.2, 'cm')) + scale_fill_manual(values= vcolors) + scale_x_date(labels = scales::date_format("%Y-%b")) + labs(x='Time', y='Total Spend') + scale_y_continuous(labels=dollar_format(prefix="$")) +
        geom_vline(color='red',linetype='dashed',xintercept = as.Date(reactData$histStartDateVar)) + 
        geom_vline(linetype='dashed',color='red',xintercept = as.Date(reactData$histEndDateVar)) + 
        annotate(color='red',"text", x = as.Date(reactData$histMeanDateVar), y = 1, label = paste( "Historical Comparison Time Period")) + scale_x_date(expand = c(0.04,0))
      
      spendPlotType <- function(type) 
      {
        switch(type,
               Percentage = spendAreaPctggPlot,
               Absolute = spendAreaAbsggPlot
        )
      }  
      
      output$spendAreaPlot <- renderPlot({ 
        spendPlotType(input$histSpendChartType)
      })
      
      ################################### Contribution ############################################
      
      contributionAreaPctggPlot <- ggplot(contributionAllData, aes(x=as.Date(Time_Period), y=Total_Pct_Contribution, fill=Spend_Channel)) +
        geom_area() + ggtitle(paste0("Time Series Plot of Percentage Contributions - Panel: ", histReactData$histPanelVar))  + theme_light() + theme(plot.title = element_text(hjust = 0.5, size = 14), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=14), legend.spacing.x = unit(0.2, 'cm')) + scale_fill_manual(values=vcolors) + scale_x_date(labels = scales::date_format("%Y-%b")) + labs(x='Time', y='Total Contribution (in %)')+
        geom_vline(color='red',linetype='dashed',xintercept = as.Date(reactData$histStartDateVar)) + 
        geom_vline(linetype='dashed',color='red',xintercept = as.Date(reactData$histEndDateVar)) + 
        annotate(color='red',"text", x = as.Date(reactData$histMeanDateVar), y = 1, label = paste( "Historical Comparison Time Period")) + scale_x_date(expand = c(0.04,0))
      
      contributionAreaAbsggPlot <- ggplot(contributionAllData, aes(x=as.Date(Time_Period), y=Total_Contribution, fill=Spend_Channel)) +
        geom_area() + ggtitle(paste0("Time Series Plot of Absolute Contributions - Panel: ", histReactData$histPanelVar))  + theme_light() + theme(plot.title = element_text(hjust = 0.5, size = 14), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=14), legend.spacing.x = unit(0.2, 'cm')) + scale_fill_manual(values=vcolors) + scale_x_date(labels = scales::date_format("%Y-%b")) + labs(x='Time', y='Total Contribution') + scale_y_continuous(labels=dollar_format(prefix="$")) +
        geom_vline(color='red',linetype='dashed',xintercept = as.Date(reactData$histStartDateVar)) + 
        geom_vline(linetype='dashed',color='red',xintercept = as.Date(reactData$histEndDateVar)) + 
        annotate(color='red',"text", x = as.Date(reactData$histMeanDateVar), y = 1, label = paste( "Historical Comparison Time Period")) + scale_x_date(expand = c(0.04,0))
      
      plotType <- function(type) 
      {
        switch(type,
               Percentage = contributionAreaPctggPlot,
               Absolute = contributionAreaAbsggPlot
        )
      }  
      
      output$contributionAreaPlot <- renderPlot({ 
        plotType(input$histContributionChartType)
      })
      
      ####################################### ROI ###############################################
      
      roiggPlot <- ggplot(histAllData, aes(color=Spend_Channel, y=Total_ROI, x=as.Date(Time_Period))) +
        geom_line(position="dodge", stat="identity", size=1.5) + geom_point(size=4) + theme_light() + theme(plot.title = element_text(hjust = 0.5, size = 14), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=14), legend.spacing.x = unit(0.2, 'cm')) + scale_x_date(labels = scales::date_format("%Y-%b")) + scale_color_manual(values=vcolors) + labs(title = paste0("Time Series Plot of ROI - Panel: ", histReactData$histPanelVar), x = "Time", y = "Total ROI") +
        geom_vline(color='red',linetype='dashed',xintercept = as.Date(reactData$histStartDateVar)) + 
        geom_vline(linetype='dashed',color='red',xintercept = as.Date(reactData$histEndDateVar)) + 
        annotate(color='red',"text", x = as.Date(reactData$histMeanDateVar), y = 1, label = paste( "Historical Comparison Time Period")) + scale_x_date(expand = c(0.04,0))
      output$roiPlot <- renderPlot({roiggPlot})
      
      ####################################### Sales ###############################################
      
      salesggPlot <- ggplot(histAllData, aes(x= as.Date(Time_Period), y= Total_Sales)) +
        geom_line(color='#1F77B4', size=1.5) + geom_point(color='#1F77B4', size=4) + theme_light() + theme(plot.title = element_text(hjust = 0.5, size = 14), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=14), legend.spacing.x = unit(0.2, 'cm'))+ scale_x_date(labels = scales::date_format("%Y-%b")) + labs(title = paste0("Time Series Plot of Sales - Panel: ", histReactData$histPanelVar), x = "Time", y = "Total Sales") + scale_y_continuous(labels=dollar_format(prefix="$")) +
        geom_vline(color='red',linetype='dashed',xintercept = as.Date(reactData$histStartDateVar)) + 
        geom_vline(linetype='dashed',color='red',xintercept = as.Date(reactData$histEndDateVar)) + 
        annotate(color='red',"text", x = as.Date(reactData$histMeanDateVar), y = 1, label = paste( "Historical Comparison Time Period")) + scale_x_date(expand = c(0.04,0))
      
      if('Total' %in% histReactData$histPanelVar){
        allSalesggPlot <- ggplot(histAllPanelSalesData, aes(x= as.Date(Time_Period), y= Total_Sales)) +
          geom_line(aes(color=histAllPanelSalesData[[reactData$panelVar]]), size=1.5) + geom_point(aes(color=histAllPanelSalesData[[reactData$panelVar]]), size=4) + theme_light() + theme(plot.title = element_text(hjust = 0.5, size = 14), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=14), legend.spacing.x = unit(0.2, 'cm'))+ scale_x_date(labels = scales::date_format("%Y-%b")) + labs(title = paste0("Time Series Plot of Sales - Panel: ", histReactData$histPanelVar), x = "Time", y = "Total Sales") + scale_y_continuous(labels=dollar_format(prefix="$")) +
          geom_vline(color='red',linetype='dashed',xintercept = as.Date(reactData$histStartDateVar)) + 
          geom_vline(linetype='dashed',color='red',xintercept = as.Date(reactData$histEndDateVar)) + 
          annotate(color='red',"text", x = as.Date(reactData$histMeanDateVar), y = 1, label = paste( "Historical Comparison Time Period")) + scale_x_date(expand = c(0.04,0))
        
        
        
        salesPlotType <- function(type) 
        {
          switch(type,
                 Aggregate = salesggPlot,
                 All = allSalesggPlot
          )
        }  
        
        output$salesPlot <- renderPlot({ 
          salesPlotType(input$histSalesChartType)
        })
      }else{
        output$salesPlot <- renderPlot({salesggPlot})
      }
      
      ##################### Arranging ggplot objects for plot downloads #######################
      
      grobHistComparisonPeriodPlots <- arrangeGrob(salesggPlot, spendAreaPctggPlot, spendAreaAbsggPlot, contributionAreaPctggPlot, contributionAreaAbsggPlot, roiggPlot, ncol=2)
      
      
      ######################### Download handler for plot download #################################
      
      output$downloadHistComparisonPeriodPlots <- shiny::downloadHandler(
        filename = function(){'Historical_Comparison_Period_Plots.png'},
        content = function(file) {
          ggsave(file, plot=grobHistComparisonPeriodPlots, device='png', width = 45, height = 25, units = 'cm')
        },
        shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
      )
      
      ######################### Download handler for data download #################################
      
      output$downloadHistComparisonPeriodData <- shiny::downloadHandler(
        filename = function() {'Historical_Comparison_Period_Data.csv'},
        content = function(file) {
          write.csv(group_data_long, file, row.names = FALSE)
        }
      )
      
      ####### Graphs for Report
      reactData$reportHistSalesSummary <- salesggPlot
      reactData$reportHistSpendsSummary <- spendAreaAbsggPlot
      reactData$reportHistPctSpendsSummary <- spendAreaPctggPlot
      reactData$reportHistContribSummary <- contributionAreaAbsggPlot
      reactData$reportHistContribPctSummary <- contributionAreaPctggPlot
      reactData$reportHistROISummary <- roiggPlot
      
    },error=function(e) showNotification(paste0('View Historical Comparison Period Plots :',e[1])))
  })
  
  output$mRoiPlotsUI <- shiny::renderUI({
    if(!is.null(mmxModel$full_object$mroi_plots)){
    shiny::fluidRow(class='box1',
                    shiny::column(4,shiny::selectInput('mroichannelvar','Select Channel', choices = c(reactData$spendVar), multiple=FALSE, selected = reactData$spendVar[[1]]))
                    ,shiny::column(style='padding-top:25px', class='button_pad',2,shiny::actionButton('viewmROIplots','Submit'))
    )}
    else{
  shiny::fluidRow(class='box1',
                      shiny::HTML('<strong>mROI plots were not included in the model object. If you wish to view the marginal ROI plots, plot the same in the modelling exercise and add the same in the model RDS.</strong>'))
    }
  })
  
  shiny::observeEvent(input$viewmROIplots, {
    
    plot_mroi_i_name<- paste0("plot_", input$mroichannelvar)
    
  output$mRoiPlots<- renderPlot({mmxModel$full_object$mroi_plots[[plot_mroi_i_name]]})
  
  output$mRoiPlots1 <- shiny::renderUI({
    #print("test mroi")
    shiny::fluidRow(class='box1', shiny::br(), shiny::column(12, shiny::plotOutput('mRoiPlots')))
  })
   })
  
  
  
  ### Checks for click of help icon
  shiny::observeEvent(
    input$simScenarioHelpText,
    {
      showModal(modalDialog(
        title = "Simulation Scenarios - Help",
        shiny::p(shiny::HTML(simScenarioText)),
        footer = tagList(
          modalButton("OK")
        )
      ))
    })
  
  output$simScenarioDatesText <- shiny::renderUI({
    if(is.null(reactData$simStartDateVar)) return(shiny::HTML(''))
    list(shiny::tags$h5(shiny::HTML(paste0('<b>Historical Comparison Period: </b>', reactData$histStartDateVar,' to ', reactData$histEndDateVar))),
         shiny::tags$h5(shiny::HTML(paste0('<b>Simulation Period: </b>', reactData$simStartDateVar,' to ', reactData$simEndDateVar))))
  })
  
  ### Checks for click of help icon
  shiny::observeEvent(
    input$schedulingMethodHelpText,
    {
      showModal(modalDialog(
        title = "Select Spend Scheduling Strategy - Help",
        shiny::p(shiny::HTML(schedulingMethodText)),
        footer = tagList(
          modalButton("OK")
        )
      ))
    })
  
  
  output$selectSchedulingStrategyUI <- shiny::renderUI({
    fluidRow(
      column(6,shiny::selectInput("selectSchedulingStrategy", "Select Strategy", choices=c("","Percentage change from historical pattern", "Flighting - Spend concentrated in a few months", "Custom Spend Pattern"), multiple=FALSE, selected="", width="70%"))
      )
  })
  
  shiny::observeEvent(input$selectSchedulingStrategy, {
    tryCatch({
    adstock$selectedSchedulingStrategy <- input$selectSchedulingStrategy
    
    ######## Progress bar #########
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Processing...")
    
    ######### Error handling ##########
    if(is.null(adstock$selectedSchedulingStrategy)) {shiny::showNotification("Please select a strategy", type='error'); return()}
    if(is.null(reactData$targetVar) | is.null(reactData$DateVar)) {shiny::showNotification("Please configure target and date variables as described in the Configuration section", type='error'); return()}
    if(nrow(histReactData$long_dataset)<=0) {shiny::showNotification("Please load historical data as described in the Configuration section", type='error'); return()}
    ########################### Error handling ends here #####################################
    
    ######## Create sample dataset based on spend scheduling strategy selected ################
    
    ######## Filter dataset for historical comparison time period ##############
    historicalComparisonData <- filter_at(histReactData$long_dataset, reactData$DateVar, any_vars(. >= reactData$histStartDateVar & . <= reactData$histEndDateVar))
    keeps <- c('Spend_Channel', 'Spends', reactData$panelVar, reactData$DateVar, reactData$targetVar)
    
    ########## Creating dataframe with future and historical comparison dates #################
    if(nrow(simExtReactData$dataset)<=0)
    {
      
      externalData <- as.data.frame(list('Simulated_Date' = seq(as.Date(reactData$simStartDateVar), as.Date(reactData$simEndDateVar), by=reactData$typeDateFrequency)))
      externalData[reactData$DateVar] <- seq(as.Date(reactData$histStartDateVar), as.Date(reactData$histEndDateVar), by=reactData$typeDateFrequency)
      reactData$mergeVar <- reactData$DateVar
      reactData$extDateVar <- 'Simulated_Date'
      simExtReactData$dataset <- externalData
      simExtReactData$dataset[[reactData$DateVar]] <<- lubridate::parse_date_time(simExtReactData$dataset[[reactData$DateVar]],orders = reactData$DTformat)
      simExtReactData$dataset[[reactData$extDateVar]] <<- lubridate::parse_date_time(simExtReactData$dataset[[reactData$extDateVar]],orders = reactData$DTformat)
      
    }
    else
    {
      
      setnames(simExtReactData$dataset, old=c(reactData$DateVar), new=c('Simulated_Date'))
      externalData <- data.frame()
      externalData <- as.data.frame(list('Time_Period' = simExtReactData$dataset$Simulated_Date))
      externalData$Time_Period <- as.POSIXlt(externalData$Time_Period)
      externalData$Time_Period$year <- externalData$Time_Period$year-1
      externalData$Time_Period<- as.Date(externalData$Time_Period)
      setnames(externalData, old=c('Time_Period'), new=c(reactData$DateVar))
      simExtReactData$dataset[reactData$DateVar] <- externalData[reactData$DateVar]
      simExtReactData$dataset[[reactData$DateVar]] <<- lubridate::parse_date_time(simExtReactData$dataset[[reactData$DateVar]],orders = reactData$DTformat)
      reactData$mergeVar <- c(reactData$mergeVar, reactData$DateVar)
      reactData$extDateVar <- 'Simulated_Date'
    }
    
    historicalComparisonData <- getFutureDataset(dataset=historicalComparisonData,
                                        external_df = simExtReactData$dataset,
                                        date_column = reactData$DateVar,
                                        merge_variables = reactData$mergeVar,
                                        external_date_variable = reactData$extDateVar)
    
    if(adstock$selectedSchedulingStrategy == "Custom Spend Pattern")
    {
      group_data_long <- dplyr::group_by_at(historicalComparisonData, c(reactData$DateVar, reactData$panelVar, "Spend_Channel"))
      agg_data_long <- dplyr::summarise(group_data_long,
                                        Total_Historical_Spend = sum(Spends, na.rm = TRUE))
      agg_data_long <- as.data.frame(agg_data_long)
      # list_of_samples <- runif(nrow(agg_data_long), min=0, max=1)
      # agg_data_long <- agg_data_long %>% mutate(Spend_Variation_Parameter=list_of_samples)
      # agg_data_long <- as.data.frame(agg_data_long)
      method1$dataset <- agg_data_long
      
      nums <-  unlist(lapply(method1$dataset, is.numeric))
      agg_data_long[[reactData$DateVar]] <- as.Date(agg_data_long[[reactData$DateVar]])
      output$selectedMethod1Table <- DT::renderDataTable(DT::datatable(roundOffDataFrame(agg_data_long, TRUE, 2), options=list(scrollX=T, searching=FALSE, dom = 't'))%>%formatCurrency(colnames(method1$dataset[nums]),currency = "", interval = 3, mark = ","))
    }
    if(adstock$selectedSchedulingStrategy == "Percentage change from historical pattern")
    {
      
      group_data_long <- dplyr::group_by(historicalComparisonData, Spend_Channel)
      
      # Create a look-up list with function names and variable to apply
      look_list <- list(sum = reactData$targetVar,
                        sum = 'Spends',
                        mean = 'Spends')
      
      # Apply the summarise_at_fun
      agg_data_long <- map2(look_list, names(look_list), summarise_at_fun, data = group_data_long) %>%
        reduce(left_join, by = c('Spend_Channel'))
      
      # Rename columns
      setnames(agg_data_long, old=c(reactData$targetVar, 'Spends.x', 'Spends.y'), new=c('Total_Historical_Sales', 'Total_Historical_Spend', 'Average_Historical_Spend'))
      
      # Add column for user to vary spends
      agg_data_long <- agg_data_long %>% mutate(Percentage_Change=0.2)
      
      # Method 2 table
      method2$dataset <- agg_data_long
      
      output$selectedMethod2Table <- renderRHandsontable({ 
        if (!is.null(input$selectedMethod2Table)) { # if there is an rhot user input...
          method2$dataset <- hot_to_r(input$selectedMethod2Table) # convert rhandsontable data to R object and store in data frame
        }
        else {
          method2$dataset <- agg_data_long
        }
        rhandsontable(method2$dataset) %>% # actual rhandsontable object
          hot_table(highlightCol = TRUE, highlightRow = TRUE, readOnly = TRUE) %>%
          hot_col("Percentage_Change", readOnly = FALSE, renderer = color_renderer) %>%
          hot_col("Total_Historical_Sales", type="numeric", format= "0,0.00", readOnly = TRUE, allowInvalid = TRUE) %>%
          hot_col("Total_Historical_Spend", type="numeric", format= "0,0.00", readOnly = TRUE, allowInvalid = TRUE) %>%
          hot_col("Average_Historical_Spend", type="numeric", format= "0,0.00", readOnly = TRUE, allowInvalid = TRUE)
      })
      
      method2$uploadedDataset <- method2$dataset
      
    }
    if(adstock$selectedSchedulingStrategy == "Flighting - Spend concentrated in a few months")
    {
      
      group_data_long <- dplyr::group_by_at(historicalComparisonData, c(reactData$DateVar, reactData$panelVar, "Spend_Channel"))
      agg_data_long <- dplyr::summarise(group_data_long,
                                        Average_Historical_Spend = mean(Spends, na.rm = TRUE))
      agg_data_long <- as.data.frame(agg_data_long)
      group_panel_channel_data <- historicalComparisonData %>% group_by_at(c(reactData$panelVar, "Spend_Channel")) %>% summarise(Spend_Per_Panel_Channel = sum(Spends, na.rm=TRUE)) %>% data.frame()
      agg_data_long <- merge(agg_data_long, group_panel_channel_data, by=c(reactData$panelVar, "Spend_Channel"), all.x=TRUE)
      # list_of_samples <- runif(nrow(agg_data_long), min=0, max=1)
      agg_data_long <- agg_data_long %>% mutate(Percentage_Contribution=Average_Historical_Spend/Spend_Per_Panel_Channel) %>% data.frame()
      method3$dataset <- agg_data_long
      
      nums <-  unlist(lapply(method3$dataset, is.numeric))
      agg_data_long[[reactData$DateVar]] <- as.Date(agg_data_long[[reactData$DateVar]])
      output$selectedMethod3Table <- DT::renderDataTable(DT::datatable(roundOffDataFrame(agg_data_long, TRUE, 2), options=list(scrollX=T, searching=FALSE, dom = 't'))%>%formatCurrency(colnames(method3$dataset[nums]),currency = "", interval = 3, mark = ","))
    }
  },error=function(e) showNotification(paste0('Select Scheduling Strategy :',e[1])))
  })
  
  
  
  output$schedulingStrategy1InfoUI <- shiny::renderUI({
    method1Text <- '<ul><li>This method allows the user to upload a custom spend pattern that will be used to simulate sales.</li> <li>The user has to download the sample file, edit the Total_Historical_Spend column and upload it back to the brick.</li></ul>'
    shiny::fluidPage(
      shiny::conditionalPanel("input.selectSchedulingStrategy=='Custom Spend Pattern'", shiny::tags$h3('Custom Spend Pattern')),
      shiny::conditionalPanel("input.selectSchedulingStrategy=='Custom Spend Pattern'", shiny::tags$p(shiny::HTML(method1Text))),
      shiny::conditionalPanel("input.selectSchedulingStrategy=='Custom Spend Pattern'", column(12,tags$p('Below is a sample of the dataset that will be available for configuring simulation spends.')), column(style='margin:20px', 12, DT::dataTableOutput("selectedMethod1Table"))),
      shiny::conditionalPanel("input.selectSchedulingStrategy=='Custom Spend Pattern'",  shiny::fluidRow(style='padding:15px'),shiny::wellPanel(
        shiny::fluidRow(class='box1',
                        shiny::column(style='padding:25px',4,shiny::downloadButton("downloadMethod1Data", "Download Sample CSV")),
                        shiny::column(6,shiny::fileInput('uploadMethod1File', 'Upload configured scenario data'))),
        shiny::fluidRow(class='box1',
                        shiny::column(4, shiny::textInput("method1ScenarioName", "Enter Scenario Name", value = "Scenario_3")),
                        shiny::column(2,style='padding-top:25px', shiny::actionButton('method1AddScenarioSubmit','Add Scenario'))))
      ))
  })
  
  output$selected_var <- renderText({ 
    length(scenarioList$files)
  })
  
  
  output$selected_optivar <- renderText({ 
    length(optimisationScenarioList$files)
  })
  
  output$selected_historyvar <- renderText({ 
    (length(scenarioList$files) + length(optimisationScenarioList$files))
  })
  
  
  output$schedulingStrategy2InfoUI <- shiny::renderUI({
    method2Text <- '<ul><li>This method involves generating simulation spend data by adding the percentage increase or decrease over the historical spend pattern.</li><li> For example, if for the month of January, in State 1, Channel 1, the spend was $100 and the increment specified is 5%, then the simulated spend for January in State 1, Channel 1 would be $100*1.05 = $105.</ul>'
    shiny::fluidPage(
      shiny::conditionalPanel("input.selectSchedulingStrategy=='Percentage change from historical pattern'", shiny::tags$h3('Percentage change from historical pattern (YoY Percentage Change)')),
      shiny::conditionalPanel("input.selectSchedulingStrategy=='Percentage change from historical pattern'",column(6,tags$img(src='Images/spend_method_2.png',width='100%')),column(6, shiny::tags$p(shiny::HTML(method2Text)))),
      shiny::conditionalPanel("input.selectSchedulingStrategy=='Percentage change from historical pattern'", fluidRow(style='margin:10px',column(12,tags$p('Please edit the Percentage_Change column in the table below to configure spend variations.')),column(style='margin:20px',12,rHandsontableOutput("selectedMethod2Table")))),
      shiny::conditionalPanel("input.selectSchedulingStrategy=='Percentage change from historical pattern'",    shiny::fluidRow(style='padding:15px'),shiny::wellPanel(shiny::fluidRow(class='box1',
                                                                                                                                                                                                                                       shiny::column(4, shiny::textInput("method2ScenarioName", "Enter Scenario Name", value = "Scenario_1")),
                                                                                                                                                                                                                                       shiny::column(2,style='padding-top:25px', shiny::actionButton('method2AddScenarioSubmit','Add Scenario'))))))
  })
  
  output$schedulingStrategy3InfoUI <- shiny::renderUI({
    method3Text <- '<ul><li>Flighting is an advertising scheduling strategy that alternates between running advertising in few months and a complete cessation of all runs in other months.</li><li> This method distributes historical spends in the proportions specified by the user(through the Percentage_Contribution column) for each Panel, Channel and Time point.</li> <li>Therefore, the sum of values in the column Percentage_Contribution must add up to 1. For this, the user has to download the sample file, edit the Percentage_Contribution column and then upload the file with the relevant constraints</li></ul></br>Below is a sample of the dataset that will be available for configuring simulation spends.'
    shiny::fluidPage(
      shiny::conditionalPanel("input.selectSchedulingStrategy=='Flighting - Spend concentrated in a few months'", shiny::tags$h3('Flighting (Spend concentrated in a few months)')),
      shiny::conditionalPanel("input.selectSchedulingStrategy=='Flighting - Spend concentrated in a few months'",fluidRow(column(6,tags$img(src='Images/spend_method_3.png',width='100%')),column(6, shiny::tags$p(shiny::HTML(method3Text))))),
      shiny::conditionalPanel("input.selectSchedulingStrategy=='Flighting - Spend concentrated in a few months'", column(12, DT::dataTableOutput("selectedMethod3Table"))),
      shiny::conditionalPanel("input.selectSchedulingStrategy=='Flighting - Spend concentrated in a few months'",  shiny::wellPanel(
        shiny::fluidRow(class='box1',
                        shiny::column(style='padding:25px',4,shiny::downloadButton("downloadMethod3Data", "Download Sample CSV")),
                        shiny::column(6,shiny::fileInput('uploadMethod3File', 'Upload configured scenario data'))),
        shiny::fluidRow(class='box1',
                        shiny::column(4, shiny::textInput("method3ScenarioName", "Enter Scenario Name", value = "Scenario_2")),
                        shiny::column(2,style='padding-top:25px', shiny::actionButton('method3AddScenarioSubmit','Add Scenario'))))))
  })
  
  ### Checks if user has clicked on Add Scenario for Method 1
  output$downloadMethod1Data <- shiny::downloadHandler(
    filename = function() {
      paste(method1$scenarioName, ".csv", sep = "")
    },
    content = function(file) {
      # browser()
      write.csv(method1$dataset, file, row.names = FALSE)
    },
    shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
  )
  
  ### Conductor for loading historical CSV file
  uploadedMethod1Dataset <- shiny::reactive({
    infile <- input$uploadMethod1File
    infile$datapath
    if(is.null(infile))
    {
      # User has not uploaded a file yet
      return(data.frame())
    }
    else
    {
      if(file_ext(infile$datapath)!='csv')
      {
        shiny::showNotification('Please load only CSV files', type='error');
        return()
      }
      dataset <- data.frame(read.csv(infile$datapath), stringsAsFactors = FALSE)
      return(dataset)
    }
  })
  
  ### Checks change of state for upload of historical data file
  shiny::observeEvent(
    input$uploadMethod1File,
    {
      tryCatch({
        # browser()
        dataset <- uploadedMethod1Dataset()
        method1$uploadedDataset <- dataset
        nums <-  unlist(lapply(method1$uploadedDataset, is.numeric))
        output$selectedMethod1Table <- DT::renderDataTable(DT::datatable(roundOffDataFrame(method1$uploadedDataset, TRUE, 2), options=list(scrollX=T, searching=FALSE, dom = 't'))%>%formatCurrency(colnames(method1$uploadedDataset[nums]),currency = "", interval = 3, mark = ","))
      },error=function(e) showNotification(paste0('Upload file for Method 1 :',e[1])))
    })
  
  ### Checks if user has clicked on Add Scenario for Method 1
  shiny::observeEvent(
    input$method1AddScenarioSubmit,{
      tryCatch({
        method1$scenarioName <- input$method1ScenarioName
        
        #### Error handling #######
        if(method1$scenarioName == ""){shiny::showNotification('Please enter a scenario name', type='error'); return()}
        if(is.null(method1$uploadedDataset) | nrow(method1$uploadedDataset)<=0){shiny::showNotification('Please upload configured scenario data as a CSV file', type='error'); return()
        }
        ### In case if the user tries to add multiple scenarios with same name
        if(method1$scenarioName %in% scenarioTable$dataset$Scenario_Name | method1$scenarioName %in% reactData$opti_scenarioNameTable$Scenario){shiny::showNotification('Scenario already exists with the same name! Please save scenario with a different name', type='error'); return()}
        ######## Progress bar #########
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Saving values")
        
        
        scenario_history_df$data[nrow(scenario_history_df$data) + 1,] = c(input$method2ScenarioName, "Percentage change from historical pattern", "Simulation Scenario",shinyInput_custom(actionButton, nrow(scenario_history_df$data)+1 , 'button_', label = "View", class = "third_button", onclick = 'Shiny.onInputChange(\"chill_button_1\",  this.id)' ))  
        
        ######## Save scenario details to list #######
        timestamp <- strftime(Sys.time(),format = "%Y-%m-%d-%H-%M-%S")
        listToWriteMethod1 = list(scheduling_dataset = shiny::reactiveValuesToList(method1, all.names = T), adstock_dataset=shiny::reactiveValuesToList(adstock, all.names=T), historical_dataset=shiny::reactiveValuesToList(histReactData, all.names=T), all_data=shiny::reactiveValuesToList(reactData, all.names=T))
        scenarioList$files[[method1$scenarioName]] <- listToWriteMethod1
        # compareScenarioList$files[[method1$scenarioName]] <- listToWriteMethod1
        
        ####### Output to indicate successful save #######
        output$method1SaveMssg <- shiny::renderText(paste0("Successfuly Saved scenario:", method1$scenarioName))
      },error=function(e) showNotification(paste0('Add Scenario for Method 1 :',e[1])))
    }
  )
  
  
  ### Checks if user has clicked on Add Scenario for Method 2
  shiny::observeEvent(
    input$method2AddScenarioSubmit,{
      tryCatch({
        
      #browser()
      
 
        
     
        
      method2$scenarioName <- input$method2ScenarioName
      
      #### Error handling #######
      if(method2$scenarioName == ""){shiny::showNotification('Please enter a scenario name', type='error'); return()}
      ### In case if the user tries to add multiple scenarios with same name
      if(method2$scenarioName %in% scenarioTable$dataset$Scenario_Name | method2$scenarioName %in% reactData$opti_scenarioNameTable$Scenario){shiny::showNotification('Scenario already exists with the same name! Please save scenario with a different name', type='error'); return()}
      ######## Progress bar #########
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Saving values")
      
      ######## Save scenario details to list #######
      
      ## waypoint 
      
      scenario_history_df$data[nrow(scenario_history_df$data) + 1,] = c(input$method2ScenarioName, "Percentage change from historical pattern", "Simulation Scenario",shinyInput_custom(actionButton, nrow(scenario_history_df$data)+1 , 'button_', label = "View", class = "third_button", onclick = 'Shiny.onInputChange(\"chill_button_1\",  this.id)' ))  
      
      
      timestamp <- strftime(Sys.time(),format = "%Y-%m-%d-%H-%M-%S")
      listToWriteMethod2 = list(scheduling_dataset = shiny::reactiveValuesToList(method2, all.names = T), adstock_dataset=shiny::reactiveValuesToList(adstock, all.names=T), historical_dataset=shiny::reactiveValuesToList(histReactData, all.names=T), all_data=shiny::reactiveValuesToList(reactData, all.names=T))
      scenarioList$files[[method2$scenarioName]] <- listToWriteMethod2
      # compareScenarioList$files[[method2$scenarioName]] <- listToWriteMethod2
      
      ####### Output to indicate successful save #######
      output$method2SaveMssg <- shiny::renderText(paste0("Successfuly Saved scenario:", method2$scenarioName))
    },error=function(e) showNotification(paste0('Add Scenario for Method 2 :',e[1])))
    }
  )
  
  output$downloadMethod3Data <- shiny::downloadHandler(
    filename = function() {
      paste(method3$scenarioName, ".csv", sep = "")
    },
    content = function(file) {
      # browser()
      write.csv(method3$dataset, file, row.names = FALSE)
    },
    shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
  )
  
  ### Conductor for loading historical CSV file
  uploadedMethod3Dataset <- shiny::reactive({
    infile <- input$uploadMethod3File
    infile$datapath
    if(is.null(infile))
    {
      # User has not uploaded a file yet
      return(data.frame())
    }
    else
    {
      if(file_ext(infile$datapath)!='csv')
      {
        shiny::showNotification('Please load only CSV files', type='error');
        return()
      }
      dataset <- data.frame(read.csv(infile$datapath), stringsAsFactors = FALSE)
      return(dataset)
    }
  })
  
  ### Checks change of state for upload of historical data file
  shiny::observeEvent(
    input$uploadMethod3File,
    {
      tryCatch({
        
        dataset <- uploadedMethod3Dataset()
        check_dataset <- dataset %>% group_by_at(c(reactData$panelVar, "Spend_Channel")) %>% summarise(Check_Pct_Contrib = sum(Percentage_Contribution, na.rm=TRUE)) %>% data.frame()
        if(sum(check_dataset$Check_Pct_Contrib)!=nrow(check_dataset)){
          shiny::showNotification('Please configure the the Percentage_Contribution column correctly. It must add up to 1 for each panel and channel combination', type=c('error'))
          return()
        }
        method3$uploadedDataset <- dataset
        nums <-  unlist(lapply(method3$uploadedDataset, is.numeric))
        output$selectedMethod3Table <- DT::renderDataTable(DT::datatable(roundOffDataFrame(method3$uploadedDataset, TRUE, 2), options=list(scrollX=T, searching=FALSE, dom = 't'))%>%formatCurrency(colnames(method3$uploadedDataset[nums]),currency = "", interval = 3, mark = ","))
    },error=function(e) showNotification(paste0('Upload file for Method 3 :',e[1])))
    })
  
  
  
  ### Checks if user has clicked on Add Scenario for Method 2
  shiny::observeEvent(
    
    input$method3AddScenarioSubmit,{
      tryCatch({
      method3$scenarioName <- input$method3ScenarioName
      
      #### Error handling #######
      if(method3$scenarioName == ""){shiny::showNotification('Please enter a scenario name', type='error'); return()}
      if(is.null(method3$uploadedDataset) | nrow(method3$uploadedDataset)<=0){shiny::showNotification('Please upload configured scenario data as a CSV file', type='error'); return()}
      ### In case if the user tries to add multiple scenarios with same name
      if(method3$scenarioName %in% scenarioTable$dataset$Scenario_Name | method3$scenarioName %in% reactData$opti_scenarioNameTable$Scenario){shiny::showNotification('Scenario already exists with the same name! Please save scenario with a different name', type='error'); return()}
      ######## Progress bar #########
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Saving values")
      
      
      
      scenario_history_df$data[nrow(scenario_history_df$data) + 1,] = c(input$method2ScenarioName, "Percentage change from historical pattern", "Simulation Scenario",shinyInput_custom(actionButton, nrow(scenario_history_df$data)+1 , 'button_', label = "View", class = "third_button", onclick = 'Shiny.onInputChange(\"chill_button_1\",  this.id)' ))  
      
      
      
      ######## Save scenario details to list #######
      timestamp <- strftime(Sys.time(),format = "%Y-%m-%d-%H-%M-%S")
      listToWriteMethod3 = list(scheduling_dataset = shiny::reactiveValuesToList(method3, all.names = T), adstock_dataset=shiny::reactiveValuesToList(adstock, all.names=T), historical_dataset=shiny::reactiveValuesToList(histReactData, all.names=T), all_data=shiny::reactiveValuesToList(reactData, all.names=T))
      scenarioList$files[[method3$scenarioName]] <- listToWriteMethod3
      # compareScenarioList$files[[method3$scenarioName]] <- listToWriteMethod3
      
      ####### Output to indicate successful save #######
      output$method3SaveMssg <- shiny::renderText(paste0("Successfuly Saved scenario:", method3$scenarioName))
      },error=function(e) showNotification(paste0('Add Scenario for Method 3 :',e[1])))
    }
  )
  
  ##################### Scenario list table  ##################### 
  
  output$scenarioListTable <- shiny::renderUI({
    #browser()
    shiny::fluidRow(
      shiny::column(12, shiny::h4('List of submitted scenarios')),
      shiny::column(12, DT::dataTableOutput("scenarioTableOutput")),
      shiny::column(12, style='padding:25px', align='right',shiny::actionButton('scenarioTableDeleteButton','Delete Selected Scenarios',icon=icon('trash')))
    )
  })
  
  
  output$scenarioTableOutput <- DT::renderDataTable({
    if(length(scenarioList$files)<=0){shiny::showNotification('No scenarios found', type='error'); return()}
    scenarioTable$dataset <- as.data.frame(scenarioList$files %>% map(~{
      c(.$scheduling_dataset$scenarioName,.$scheduling_dataset$schedulingStrategy)
    }) %>% do.call(rbind,.))
    
    
    #browser()
    
    ## waypoint

    # scenarioTable$dataset$Run_Status <- "Not Run"
    # scenarioTable$dataset$Generate_Status <- "Not Generated"
    # rownames(scenarioTable$dataset) <- NULL
    setnames(scenarioTable$dataset, old=c("V1", "V2"), new=c("Scenario_Name","Scheduling_Method"))
    
    #scenarioTable_latest <- scenarioTable$dataset[nrow(scenarioTable$dataset),]
    

    
    #browser()
    
    
    DT::datatable(roundOffDataFrame(scenarioTable$dataset, TRUE, 2)
                  , options=list(pagelength=6, scrollX=T, searching=FALSE, dom = 't'))
    
    
    })
  
  

  
  shiny::observeEvent(input$scenarioTableDeleteButton,{
    tryCatch({
    ######## Progress bar #########
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Processing...")
    
    ####### Error handling ###########
    if(nrow(scenarioTable$dataset)<=0){shiny::showNotification('No scenarios found', type='error'); return()}
    
    rows <- input$scenarioTableOutput_rows_selected
    if(length(rows)==0) return()
    dat <- scenarioTable$dataset
    rownames(dat) <- NULL
    deleted_dat <- dat[c(rows),]
    scenarioList$files[[deleted_dat$Scenario_Name]] <- NULL
    updated_dat <- dat[-c(rows),]
    scenarioTable$dataset <- updated_dat
  },error=function(e) showNotification(paste0('Delete Scenario :',e[1])))
  })
  
  ### Checks for click of help icon
  shiny::observeEvent(
    input$genSimDataHelpText,
    {
      showModal(modalDialog(
        title = "Generate Simulation Spend Data - Help",
        shiny::p(shiny::HTML(genSimDataText)),
        footer = tagList(
          modalButton("OK")
        )
      ))
    })
  
  output$genSimDatesText <- shiny::renderUI({
    if(is.null(reactData$simStartDateVar)) return(shiny::HTML(''))
    list(shiny::tags$h5(shiny::HTML(paste0('<b>Historical Comparison Period: </b>', reactData$histStartDateVar,' to ', reactData$histEndDateVar))),
         shiny::tags$h5(shiny::HTML(paste0('<b>Simulation Period: </b>', reactData$simStartDateVar,' to ', reactData$simEndDateVar))))
  })
  
  ##########################################  Generate scenario ui  ############################## 
  output$generateScenarioUI <- shiny::renderUI({
    shiny::fluidRow(class='box1',shiny::br(),
                    shiny::column(4,shiny::selectInput("generateScenarioSelected", "Select Scenario",
                                                       choices = setdiff(unique(scenarioTable$dataset$Scenario_Name), unique(reactData$uploadedSimScenarioList)),
                                                       multiple = FALSE, selected = NULL)),
                    shiny::column(style='padding-top:25px', class='button_pad',2,
                                  shiny::actionButton('generateScenarioSelectedButton','Generate')),
                    shiny::column(style='padding-top:25px;margin-right:5px',1,#align='right',
                                  shiny::downloadButton('downloadGeneratedScenarioButton','Download')))
  })
  
  shiny::observeEvent(input$generateScenarioSelectedButton, {
    tryCatch({
    adstock$generateScenarioSelected <- input$generateScenarioSelected
    
    ######## Progress bar #########
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Processing...")
    
    ############################ Error handling starts here ###################################
    
    if(adstock$generateScenarioSelected==""){shiny::showNotification('Please select a scenario', type='error'); return()}
    
    if(is.null(reactData$panelVar) | is.null(reactData$spendVar) | is.null(reactData$DateVar)) {shiny::showNotification("Please select Spend, Panel, Seasonal and Trend Columns", type='error'); return()}
    
    if(is.null(scenarioList$files[[adstock$generateScenarioSelected]]$historical_dataset$long_dataset) | is.null(scenarioList$files[[adstock$generateScenarioSelected]]$adstock$value_dataset)){shiny::showNotification('Please load and configure historical and adstock data as specified in the Configuration section', type='error'); return()}
    
    if(nrow(scenarioList$files[[adstock$generateScenarioSelected]]$historical_dataset$long_dataset)<=0| nrow(scenarioList$files[[adstock$generateScenarioSelected]]$adstock$value_dataset)<=0){shiny::showNotification('Please load and configure historical and adstock data as specified in the Configuration section', type='error'); return()}
    
    ############################ Error handling ends here ##################################
    
    ######### Calling function to generate simulated spends based on method selected ############
    tempSimData <- gen_sim_data(schedulingMethod = scenarioList$files[[adstock$generateScenarioSelected]]$scheduling_dataset$schedulingStrategy, 
                                dataframe = scenarioList$files[[adstock$generateScenarioSelected]]$all_data$long_dataset,
                                parameter_df = scenarioList$files[[adstock$generateScenarioSelected]]$scheduling_dataset$uploadedDataset, 
                                all_date_df = reactData$allDateSequence,
                                date_column = reactData$DateVar,
                                date_format = reactData$DTformat,
                                panel_column = reactData$panelVar,
                                trend_column = reactData$timeOrderVar,
                                y_variable = reactData$targetVar,
                                frequency_variable = reactData$numDateFrequency
                                )
    
    ### Get non-adstock simulated spends####
    non_adstock_df <- filter_at(tempSimData, reactData$DateVar, any_vars(. >= reactData$histStartDateVar & . <= reactData$histEndDateVar))
    
    ### Convert non-adstock simulated spends to long format ####
    non_adstock_long_df <- melt(non_adstock_df, measure.vars = reactData$spendVar)
    setnames(non_adstock_long_df, c("variable", "value"), c("Spend_Channel", "Simulated_Spend"))
    
    ### Apply adstocks to simulated spends ###
    adstock_wide_df <- apply_adstock(dataset=tempSimData,
                                     spend_variables=as.character(adstock$dataset$Spend_Channel),
                                     decay_rate=list(adstock$dataset$Adstock_Decay),
                                     time_variable=reactData$DateVar,
                                     adstock_panel_variables=reactData$panelVar)
    
    ### Convert adstock simulated spends to long format ####
    adstock_long_df <- melt(adstock_wide_df, measure.vars = reactData$spendVar)
    setnames(adstock_long_df, c("variable", "value"), c("Spend_Channel", "Simulated_Spend"))
    
    adstock_long_df <- getFutureDataset(dataset=adstock_long_df,
                                external_df = simExtReactData$dataset,
                                date_column = reactData$DateVar,
                                merge_variables = reactData$mergeVar,
                                external_date_variable = reactData$extDateVar)

    non_adstock_long_df <- getFutureDataset(dataset=non_adstock_long_df,
                                            external_df = simExtReactData$dataset,
                                            date_column = reactData$DateVar,
                                            merge_variables = reactData$mergeVar,
                                            external_date_variable = reactData$extDateVar)
    
    
    ### Save adstock simulated spends to scenario list ####
    scenarioList$files[[adstock$generateScenarioSelected]]$scheduling_dataset$generatedSimulationDataset <- adstock_long_df
    
    ### Convert non-adstock simulated spends to wide format ###
    non_adstock_wide_df <- tidyr::spread(non_adstock_long_df, key = Spend_Channel, value = Simulated_Spend)
    non_adstock_wide_df <- as.data.frame(non_adstock_wide_df)
    
    ### Save wide format of non-adstock simulated spends to scenario list ####
    scenarioList$files[[adstock$generateScenarioSelected]]$scheduling_dataset$simulationDataset <- non_adstock_wide_df
    

    
    output$generateScenarioSelectedStatusText <- shiny::renderText(paste0(
      "Simulation Spend Data for scenario: ", adstock$generateScenarioSelected, " has been generated successfuly.\n"))
    },error=function(e) showNotification(paste0('Generate Simulated Spends :',e[1])))
  })
  
  output$downloadGeneratedScenarioButton <- shiny::downloadHandler(
    filename = function() {
      paste(input$generateScenarioSelected, "_generated_simulation_data",".csv", sep = "")
    },
    content = function(file) {
      write.csv(scenarioList$files[[adstock$generateScenarioSelected]]$scheduling_dataset$generatedSimulationDataset, file, row.names = FALSE)
    },
    shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
  )
  
  # Scenario selection for viewing the simulation data
  output$editScenarioUI <- shiny::renderUI({
    tagList(shiny::fluidRow(style='padding-left:15px',shiny::checkboxInput('modify_scenario_checkbox','Do you want to customize the scenarios created in the Define Scenarios section?'),
                            conditionalPanel('input.modify_scenario_checkbox',
                                             shiny::wellPanel(
                                               shiny::tags$p(shiny::HTML(editSimDataText)),
                                               shiny::fluidRow(style='padding-top:10px',class='box1',
                                                               shiny::column(4,shiny::selectInput("editScenarioSelected", "Select Scenario"
                                                                                                  , choices = setdiff(unique(scenarioTable$dataset$Scenario_Name), unique(reactData$uploadedSimScenarioList))
                                                                                                  , multiple = FALSE, selected = NULL)),
                                                               shiny::column(6,shiny::fileInput('editedScenarioFile', 'Choose CSV file'))),
                                               shiny::fluidRow(class='box1',
                                                               ### Load File Widget - Source for getting user uploaded external data as a CSV file
                                                               shiny::column(12,shiny::actionButton(style="margin-bottom:25px", 'uploadEditedScenarioData','Upload Customized Simulation Spend Scenario Data'))),
                                               shiny::fluidRow(
                                                 shiny::column(12, shiny::verbatimTextOutput('updateScenarioMssg'))))))
    )
  })
  
  
  
  ### Conductor for loading external variables' CSV file
  editedScenarioDataset <- shiny::reactive({
    infile <- input$editedScenarioFile
    infile$datapath
    if(is.null(infile))
    {
      # User has not uploaded a file yet
      return(data.frame())
    }
    else
    {
      if(file_ext(infile$datapath)!='csv')
      {
        shiny::showNotification('Please load only CSV files', type='error');
        return()
      }
      dataset <- data.frame(read.csv(file = infile$datapath))
      return(dataset)
    }
  })
  
  ### Checks change of state for upload of edited simulation data
  shiny::observeEvent(
    input$uploadEditedScenarioData,
    {
      tryCatch({
      editedScenarioReactData$editScenarioSelected <- input$editScenarioSelected
      editedScenarioReactData$dataset <- editedScenarioDataset()
      ######## Progress bar #########
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Processing...")
      
      #### Error handling #######
      if(is.null(reactData$TimeZone) | is.null(reactData$DateVar) | is.null(reactData$DTformat)){shiny::showNotification('Please configure date and date format variables as described in the Configuration section', type='error'); return()}
      if(is.null(editedScenarioReactData$dataset) | nrow(editedScenarioReactData$dataset)<=0){shiny::showNotification('Please upload edited scenario data as a CSV file', type='error'); return()}
      if(is.null(editedScenarioReactData$editScenarioSelected)){shiny::showNotification('Please create a scenario in the Define Scenarios section and generate the simulation spend data before attempting to edit the simulated spends', type='error'); return()}
      
      scenarioList$files[[editedScenarioReactData$editScenarioSelected]]$scheduling_dataset$generatedSimulationDataset <- NULL
      Sys.setenv(TZ = reactData$TimeZone)
      editedScenarioReactData$dataset[[reactData$DateVar]] <<- lubridate::parse_date_time(editedScenarioReactData$dataset[[reactData$DateVar]],orders = reactData$DTformat)
      scenarioList$files[[editedScenarioReactData$editScenarioSelected]]$scheduling_dataset$generatedSimulationDataset <- editedScenarioReactData$dataset
      output$updateScenarioMssg <- shiny::renderText(paste0("Simulation spend data has been updated for scenario ", editedScenarioReactData$editScenarioSelected))
    },error=function(e) showNotification(paste0('Edit Simulated Spend :',e[1])))
    })
  
  
  shiny::observeEvent(input$viewGeneratedScenarioButton, {
    tryCatch({
    reactData$viewGeneratedScenarioSelected <- input$viewGeneratedScenarioSelected
    
    ######## Progress bar #########
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Processing...")
    
    #### Error handling #######
    # browser()
    if(reactData$viewGeneratedScenarioSelected==""){shiny::showNotification('Please select a scenario before attempting to view it', type='error'); return()}
    if(is.null(scenarioList$files[[reactData$viewGeneratedScenarioSelected]]$scheduling_dataset$generatedSimulationDataset)){shiny::showNotification('Please generate the scenario data before attempting to view it', type='error'); return()}
    if(nrow(scenarioList$files[[reactData$viewGeneratedScenarioSelected]]$scheduling_dataset$generatedSimulationDataset)<=0){shiny::showNotification('Please generate the scenario data before attempting to view it', type='error'); return()}
    
    reactData$generatedScenarioData <- scenarioList$files[[reactData$viewGeneratedScenarioSelected]]$scheduling_dataset$generatedSimulationDataset
    reactData$simulationScenarioData <- scenarioList$files[[reactData$viewGeneratedScenarioSelected]]$scheduling_dataset$simulationDataset
    # adstock$historicalScenarioData <- scenarioList$files[[reactData$viewGeneratedScenarioSelected]]$historical_dataset$dataset
    
    nums <-  unlist(lapply(reactData$generatedScenarioData, is.numeric))
    reactData$generatedScenarioData[[reactData$DateVar]] <- as.Date(reactData$generatedScenarioData[[reactData$DateVar]])
    output$genSimDataOutput = DT::renderDataTable(DT::datatable(roundOffDataFrame(reactData$generatedScenarioData, TRUE, 2), options=list(scrollX=T, searching=TRUE, paging=TRUE))%>%formatCurrency(colnames(reactData$generatedScenarioData[nums]),currency = "", interval = 3, mark = ","))
  },error=function(e) showNotification(paste0('View Generated Scenario :',e[1])))
  })
  
  
  output$viewGeneratedScenarioUI <- shiny::renderUI({
    list(shiny::br()
         ,shiny::h3('View simulation spend data')
         ,shiny::fluidRow(class='box1',
                          shiny::column(4,shiny::selectInput("viewGeneratedScenarioSelected", "Select Scenario",
                                                             choices = setdiff(unique(scenarioTable$dataset$Scenario_Name), unique(reactData$uploadedSimScenarioList)),
                                                             multiple = FALSE,selectize=TRUE, selected = NULL))
                          ,shiny::column(style='padding-top:25px', class='button_pad',2,shiny::actionButton('viewGeneratedScenarioButton','View Data'))
         ))
  })
  
  # UI for charts comparing the simulated and historic spends
  output$viewGeneratedScenarioChartsUI <- shiny::renderUI({
    
    tagList(shiny::fluidRow(class='box1',column(12,tabsetPanel(tabPanel('Tabular View',
                                                                        shiny::br(),
                                                                        DT::dataTableOutput("genSimDataOutput")),
                                                               tabPanel('Comparison Charts',
                                                                        shiny::br(),
                                                                        shiny::fluidRow(class='box1',
                                                                                        shiny::column(3, shiny::selectInput('viewGeneratedPanelVar','Select Panel', choices = c("", "Total", unique(reactData$generatedScenarioData[reactData$panelVar])), multiple=FALSE, selected = "Total")),
                                                                                        shiny::column(3, shiny::selectInput('viewGeneratedSpendVar','Select Channel', choices = c("", "Total", unique(reactData$generatedScenarioData["Spend_Channel"])), multiple=FALSE, selected = "Total")),
                                                                                        shiny::column(style='padding-top:25px', class='button_pad',2,
                                                                                                      shiny::actionButton('viewGeneratedPanelSpendSelected','Submit')),
                                                                                        shiny::conditionalPanel('input.viewGeneratedPanelSpendSelected>0', shiny::column(2, style='padding-top:25px', shiny::downloadButton('downloadGeneratedPanelSpendSelected', 'Download Plot(s)'))),
                                                                                        shiny::conditionalPanel('input.viewGeneratedPanelSpendSelected>0', shiny::column(2, style='padding-top:25px', shiny::downloadButton('downloadGeneratedPanelSpendData', 'Download Data')))),
                                                                        fluidRow(class='box1',
                                                                                 shiny::br(),
                                                                                 column(12,shiny::plotOutput("simulatedDataLinePlot")))
                                                               )
    ))))
  })
  
  shiny::observeEvent(input$viewGeneratedPanelSpendSelected, {
    tryCatch({
      
      #### Error handling #######
      if(nrow(reactData$generatedScenarioData)<=0){shiny::showNotification('Please generate the data and click on View Data before attempting to view panel or channel level results', type='error'); return()}
      if(is.null(reactData$panelVar) | is.null(reactData$dateFrequency) | is.null(reactData$spendVar)){shiny::showNotification('Please configure spend, panel and date frequency variables in the Configuration section', type='error'); return()}
      if(input$viewGeneratedPanelVar==""){
        shiny::showNotification('Please select a valid panel from the list', type='error'); return()
      }
      if(input$viewGeneratedSpendVar==""){
        shiny::showNotification('Please select a valid channel from the list', type='error'); return()
      }
      
      ###### Saving user selections ########
      panelSelected <- input$viewGeneratedPanelVar
      channelSelected <- input$viewGeneratedSpendVar
      
      ######## Progress bar #########
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Processing...")
      
      ###### Filtering historical data for historical comparison period ######
      historical_dataset <- filter_at(reactData$dataset, reactData$DateVar, any_vars(. >= reactData$histStartDateVar & . <= reactData$histEndDateVar))
      ######### Creating dataframe containing both simulated and historical spends #########
      
      genSimulationChartData <- fn_rbind_data(hist_dataset = historical_dataset, 
                                              opti_dataset = reactData$simulationScenarioData, 
                                              lvl_of_data = c(reactData$DateVar, reactData$panelVar), 
                                              melt_data = reactData$spendVar,
                                              type_data = 'Simulated')
      ######## Rename columns ##########
      setnames(genSimulationChartData, old=c('variable', 'value'), new=c('Spend_Channel', 'Spends'))
      
      ######## Filtering dataframe based on user selection ####################
      if (!("Total" %in% panelSelected) & !("Total" %in% channelSelected)){
        genSimulationFilteredChartData <- dplyr::filter(genSimulationChartData, genSimulationChartData[reactData$panelVar] == panelSelected & genSimulationChartData["Spend_Channel"] == channelSelected)
      }
      if (!("Total" %in% channelSelected) & ("Total" %in% panelSelected)){
        genSimulationFilteredChartData <- dplyr::filter(genSimulationChartData, genSimulationChartData["Spend_Channel"] == channelSelected)
      }
      if (!("Total" %in% panelSelected) & "Total" %in% channelSelected){
        genSimulationFilteredChartData <- dplyr::filter(genSimulationChartData, genSimulationChartData[reactData$panelVar] == panelSelected)
      }
      if(("Total" %in% panelSelected) & ("Total" %in% channelSelected)){
        genSimulationFilteredChartData <- genSimulationChartData
      }
      
      ##### Aggregate dataset to Time level ##########
      genSimulationAggChartData <- genSimulationFilteredChartData %>% group_by_at(c(reactData$DateVar, 'data_time_period')) %>% summarise(Total_Spend = sum(Spends, na.rm=TRUE)) %>% data.frame()
      
      genSimulationAggChartData <- build_timeSeriesDataset(genSimulationAggChartData, dateFrequency = reactData$dateFrequency, DateVar = reactData$DateVar)
      
      simulatedDataLineggPlot <- timeSeriesOverlayPlot(dataset = genSimulationAggChartData, chartTitle = "Spend",xvar = "Time_Num", xlab = "Time", yvar = "Total_Spend", ylab = "Total Spend", lvar = "data_time_period", panelLabel = panelSelected, channelLabel = channelSelected) + 
        scale_y_continuous(labels=scales::dollar_format(prefix="$"))
      
      output$simulatedDataLinePlot = renderPlot({simulatedDataLineggPlot})
      
      output$downloadGeneratedPanelSpendData <- shiny::downloadHandler(
        filename = function() {'Simulated_Spend_Data.csv'},
        content = function(file) {
          write.csv(genSimulationAggChartData, file, row.names = FALSE)
        },
        shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
      )
      
      output$downloadGeneratedPanelSpendSelected <- shiny::downloadHandler(
        filename = function(){'Simulated_Spend_Plot.png'},
        content = function(file) {
          ggsave(file, plot=simulatedDataLineggPlot, device='png', width = 45, height = 25, units = 'cm')
        }
      )
      
      #### Plot for Report
      reactData$simulatedDataLinePlot <- simulatedDataLineggPlot
      
    },error=function(e) showNotification(paste0('View Generated Spends :',e[1])))
    
    
    
  })
  
  
  
  
  # Run simulation ----------------------------------------------------------------------------------------
  
  
  ### Checks for click of help icon
  shiny::observeEvent(
    input$runSimulationHelpText,
    {
      showModal(modalDialog(
        title = "Run Simulation - Help",
        shiny::p(shiny::HTML(runSimulationText)),
        footer = tagList(
          modalButton("OK")
        )
      ))
    })
  
  output$runSimDatesText <- shiny::renderUI({
    if(is.null(reactData$simStartDateVar)) return(shiny::HTML(''))
    list(shiny::tags$h5(shiny::HTML(paste0('<b>Historical Comparison Period: </b>', reactData$histStartDateVar,' to ', reactData$histEndDateVar))),
         shiny::tags$h5(shiny::HTML(paste0('<b>Simulation Period: </b>', reactData$simStartDateVar,' to ', reactData$simEndDateVar))))
  })
  
  output$runSimulationUI <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(4,shiny::selectInput("runScenarioSelected", "Select Scenario",
                                                       choices = setdiff(unique(scenarioTable$dataset$Scenario_Name), unique(reactData$uploadedSimScenarioList)),
                                                       multiple = FALSE, selected = NULL)),
                    shiny::column(style='padding-top:25px', class='button_pad',2,
                                  shiny::actionButton('runScenarioSelectedButton','Run Scenario')),
                    shiny::column(style='padding-top:25px',1,
                                  shiny::downloadButton('downloadRunScenarioButton','Download Results')))
  })
  
  shiny::observeEvent(input$runScenarioSelectedButton,{
    tryCatch({
    runScenarioSelected <- input$runScenarioSelected
    
    ######## Progress bar #########
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Processing...")
    
    ####### Error handling ###########
    if(input$runScenarioSelected==""){shiny::showNotification('No scenarios found', type='error'); return()}
    if(is.null(mmxModel$model) | is.null(reactData$targetVar)) {shiny::showNotification("Please upload model object and configure variables in the Configuration section ", type="error"); return()}
    if(nrow(scenarioList$files[[input$runScenarioSelected]]$scheduling_dataset$generatedSimulationDataset)<=0){shiny::showNotification('Please generate data for the selected scenario before running simulation', type='error'); return()}
    
    simulationDatasetWide <- tidyr::spread(scenarioList$files[[runScenarioSelected]]$scheduling_dataset$generatedSimulationDataset, key = Spend_Channel, value = Simulated_Spend)
    simulationDatasetWide <- as.data.frame(simulationDatasetWide)
    train_dat <- mmxModel$train_data
    # simulationDatasetWide$Month <- factor(simulationDatasetWide$Month,levels=levels(train_dat$Month))
    
    
    
    runSimulationResults <- get_mmx_sales(mmxModel$model, simulationDatasetWide, reactData$targetVar)
    
    ######## Merging with actual spends data for getting non-adstocked spends along with simulated sales ##############
    actualSimulationDatasetWide <- scenarioList$files[[runScenarioSelected]]$scheduling_dataset$simulationDataset
    runSimulationResults <- merge(runSimulationResults, actualSimulationDatasetWide, by=c(reactData$DateVar, reactData$panelVar))
    runSimulationResults <- runSimulationResults[, -grep(".x$", colnames(runSimulationResults))]
    names(runSimulationResults) = gsub(pattern = ".y", replacement = "", x = names(runSimulationResults))
    
    scenarioList$files[[runScenarioSelected]]$scheduling_dataset$runSimulationDataset <- runSimulationResults
    output$runScenarioSelectedStatusText <- shiny::renderText(paste0(
      "Simulation Spend Data for scenario: ", runScenarioSelected, " has been run successfuly.\n"))
    if(!dir.exists("Scenarios")){
      dir.create("Scenarios")
    }
    # saveRDS(scenarioList$files[[runScenarioSelected]], file = paste0("Scenarios/", c(runScenarioSelected), ".RDS"))
    optimisationScenarioTable <- as.data.frame(optimisationScenarioList$files %>% map(~{
      c(.$optimising_dataset$scenarioName)
    }) %>% do.call(rbind,.))
    
    simulationScenarioTable <- as.data.frame(scenarioList$files %>% map(~{
      c(.$scheduling_dataset$scenarioName)
    }) %>% do.call(rbind,.))
    
    if(nrow(optimisationScenarioTable)<=0 | is.null(optimisationScenarioTable))
    {
      compareScenarioTable$dataset <- simulationScenarioTable
    }
    if(nrow(simulationScenarioTable)<=0 | is.null(simulationScenarioTable))
    {
      compareScenarioTable$dataset <- optimisationScenarioTable
    }
    
    compareScenarioTable$dataset <- rbind(optimisationScenarioTable, simulationScenarioTable)
    compareScenarioTable$dataset <- unique(compareScenarioTable$dataset)
    colnames(compareScenarioTable$dataset) <- c("Scenario_Name")
  },error=function(e) showNotification(paste0('Run Simulation :',e[1])))
  })
  
  output$downloadRunScenarioButton <- shiny::downloadHandler(
    filename = function() {
      paste("Adstock_Decay", ".csv", sep = "")
    },
    content = function(file) {
      adstock$runScenarioData <- scenarioList$files[[input$runScenarioSelected]]$scheduling_dataset$runSimulationDataset
      write.csv(adstock$runScenarioData, file, row.names = FALSE)
    },
    shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
  )
  
  
  ##############################################################################################################################################
  # Simulation results tab
  ##############################################################################################################################################
  
  ### Checks for click of help icon
  shiny::observeEvent(
    input$viewSimulationResultsHelpText,
    {
      showModal(modalDialog(
        title = "Interpret Simulation Results - Help",
        shiny::p(shiny::HTML(viewSimulationResultsText)),
        footer = tagList(
          modalButton("OK")
        )
      ))
    })
  
  output$viewSimDatesText <- shiny::renderUI({
    if(is.null(reactData$simStartDateVar)) return(shiny::HTML(''))
    list(shiny::tags$h5(shiny::HTML(paste0('<b>Historical Comparison Period: </b>', reactData$histStartDateVar,' to ', reactData$histEndDateVar))),
         shiny::tags$h5(shiny::HTML(paste0('<b>Simulation Period: </b>', reactData$simStartDateVar,' to ', reactData$simEndDateVar))))
  })
  
  output$scenarioFilePathUI <- shiny::renderUI({
    tryCatch({
    if(length(scenarioList$files)<=0){
      # browser()
      file_names <- list.files(path="Scenarios")
      for(file1 in file_names)
      { 
        scenario_name = tools::file_path_sans_ext(file1)
        scenarioList$files[[scenario_name]] <- readRDS(paste0("Scenarios/", file1))
        
        scenarioTable$dataset<-rbind(scenarioTable$dataset, data.frame(Scenario_Name=scenarioList$files[[scenario_name]]$scheduling_dataset$scenarioName, Scheduling_Method=scenarioList$files[[scenario_name]]$scheduling_dataset$schedulingStrategy))
      }
    
      optimisationScenarioTable <- as.data.frame(optimisationScenarioList$files %>% map(~{
        c(.$optimising_dataset$scenarioName)
      }) %>% do.call(rbind,.))
      
      simulationScenarioTable <- as.data.frame(scenarioList$files %>% map(~{
        c(.$scheduling_dataset$scenarioName)
      }) %>% do.call(rbind,.))
      
      if(nrow(optimisationScenarioTable)<=0 | is.null(optimisationScenarioTable))
      {
        compareScenarioTable$dataset <- simulationScenarioTable
      }
      if(nrow(simulationScenarioTable)<=0 | is.null(simulationScenarioTable))
      {
        compareScenarioTable$dataset <- optimisationScenarioTable
      }
      
      compareScenarioTable$dataset <- rbind(optimisationScenarioTable, simulationScenarioTable)
      compareScenarioTable$dataset <- unique(compareScenarioTable$dataset)
      colnames(compareScenarioTable$dataset) <- c("Scenario_Name")
      
    }
    },error=function(e) shiny::showNotification(''))
  })
  
  ### Checks for click of help icon
  shiny::observeEvent(
    input$viewSimulationTSHelpText,
    {
      showModal(modalDialog(
        title = "Interpret Simulation Results - Help",
        shiny::p(shiny::HTML(scenarioTimeSeriesText)),
        footer = tagList(
          modalButton("OK")
        )
      ))
    })
  
  output$chooseScenarioSelectionMode <- shiny::renderUI({
    
    shiny::fluidRow(class='box1',
                    shiny::column(12,shiny::radioButtons('selectMode', 'Select Scenario Mode', c("View Generated Scenarios", "Upload Scenario RDS"), inline = TRUE,  width = NULL))
    )
  })
  
  output$chooseResultsSelectionMode <- shiny::renderUI({
    
    shiny::fluidRow(class='box1',
                    shiny::column(12,shiny::radioButtons('selectResult', 'Select Results to be displayed', c("View Simulation Results", "View Optimization Results"), inline = TRUE,  width = NULL))
    )
  })
  
  output$viewSelectedScenarioUI <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(4,shiny::selectInput("viewScenarioSelected", "Select Scenario",
                                                       choices = unique(scenarioTable$dataset$Scenario_Name),
                                                       multiple = FALSE, selected = viewValueSim$selected)),
                    shiny::column(style='padding-top:25px', class='button_pad',2,
                                  shiny::actionButton('viewScenarioSelectedButton','Submit'))
    )
  })
  
  
  shiny::observeEvent(input$viewScenarioSelectedButton, {
    tryCatch({
      reactData$viewScenarioSelected <- input$viewScenarioSelected
      
      ######## Progress bar #########
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Processing...")
      
      ####### Error handling ###########
      if(reactData$viewScenarioSelected==""){shiny::showNotification('No scenarios found', type='error'); return()}
      if(nrow(scenarioList$files[[reactData$viewScenarioSelected]]$scheduling_dataset$runSimulationDataset)<=0){
        shiny::showNotification('Please run the scenario before attempting to view its results', type='error') ; return()
      }
      
      reactData$viewScenarioSelectedData <- scenarioList$files[[reactData$viewScenarioSelected]]$scheduling_dataset$runSimulationDataset
      
    },error=function(e) showNotification(paste0('View Selected Scenario :',e[1])))
  })
  
  
  ### Conductor for loading External Simulation Scenario RDS files
  scenarioObject <- shiny::reactive({
    infile <- input$scenarioFile
    if(is.null(infile))
    {
      # User has not uploaded a file yet
      return()
    }
    else
    {
      if(file_ext(infile$datapath)!='RDS')
      {
        shiny::showNotification('Please load only RDS files', type='error');
        return()
      }
      objectList <- readRDS(infile$datapath)
      return(objectList)
    }
  })
  
  ### This nugget is for when you upload an Simulation Scenario RDS file
  shiny::observeEvent(input$submitUploadedScenario,{
    tryCatch({
      # browser()
      scenarioObjectList <- scenarioObject()
      
      ## Updating Dataset variable for result processing  
      reactData$viewScenarioSelectedData <- scenarioObjectList$scheduling_dataset$runSimulationDataset
      adstock <- scenarioObjectList$adstock_dataset
      reactData <- scenarioObjectList$all_data
      
      ## Updating in the list available
      reactData$uploadedSimScenarioList <- c(reactData$uploadedSimScenarioList, scenarioObjectList$scheduling_dataset$scenarioName)
      scenarioList$files[[scenarioObjectList$scheduling_dataset$scenarioName]] <- scenarioObjectList
      scenarioTable$dataset <- rbind(scenarioTable$dataset, data.frame(Scenario_Name = c(scenarioObjectList$scheduling_dataset$scenarioName), Scheduling_Method= c("X")) )
      #############################################
      ### Updating Comapre Scenario Table #########
      #############################################
      optimisationScenarioTable <- as.data.frame(optimisationScenarioList$files %>% map(~{
        c(.$optimising_dataset$scenarioName)
      }) %>% do.call(rbind,.))
      
      simulationScenarioTable <- as.data.frame(scenarioList$files %>% map(~{
        c(.$scheduling_dataset$scenarioName)
      }) %>% do.call(rbind,.))
      
      if(nrow(optimisationScenarioTable)<=0 | is.null(optimisationScenarioTable))
      {
        compareScenarioTable$dataset <- simulationScenarioTable
      }
      if(nrow(simulationScenarioTable)<=0 | is.null(simulationScenarioTable))
      {
        compareScenarioTable$dataset <- optimisationScenarioTable
      }
      
      compareScenarioTable$dataset <- rbind(optimisationScenarioTable, simulationScenarioTable)
      compareScenarioTable$dataset <- unique(compareScenarioTable$dataset)
      colnames(compareScenarioTable$dataset) <- c("Scenario_Name")
      #######################################################################
      #######################################################################
      
      # browser()
      output$uploadedScenarioText <- shiny::renderText(paste0(
        "Scenario :  ", input$scenarioFile$name, " has been laoded successfuly.\n"))
    },error=function(e) showNotification(paste0('View input$scenarioFile :',e[1])))
  })
  
  
  
  output$scenarioTimeSeriesInfoUI <- shiny::renderUI({
    shiny::fluidRow(
      shiny::column(8, shiny::tags$h3(paste0('Scenario Summary - ', reactData$viewScenarioSelected)))
      ,shiny::conditionalPanel('input.viewPanelCompareScenarioResultsButton>0', shiny::column(2,style='padding-top:25px', shiny::downloadButton('downloadScenarioSummaryPlots', 'Download Plot(s)')))
      ,shiny::conditionalPanel('input.viewPanelCompareScenarioResultsButton>0', shiny::column(2,style='padding-top:25px', shiny::downloadButton('downloadScenarioSummaryData', 'Download Data')))
    )
  })
  
  
  output$simulationTimeSeriesUI <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(4, shiny::selectInput("selectPanelCompareScenarioResults", "Select Panel",
                                                        choices = c("", "Total", unique(reactData$viewScenarioSelectedData[reactData$panelVar])),
                                                        multiple = FALSE, selected = "Total"))
                    ,shiny::column(style='padding-top:25px', class='button_pad',2,shiny::actionButton('viewPanelCompareScenarioResultsButton','Submit'))
    )
  })
  
  output$simulationTimeSeriesPlotsUI <- shiny::renderUI({
    tagList(
      shiny::fluidRow(class='box1',column(12,shiny::tabsetPanel(type = "tabs",
                                                                shiny::tabPanel("Sales", 
                                                                                shiny::br(),
                                                                                shiny::conditionalPanel("input.selectPanelCompareScenarioResults=='Total'",shiny::fluidRow(shiny::column(6, shiny::radioButtons("simSalesChartType", "Panel Type", choices=c("Aggregate", "All"), inline = TRUE)))),
                                                                                shiny::fluidRow(shiny::column(12, shiny::plotOutput('simulationSalesPlot')))),
                                                                shiny::tabPanel("Spend", 
                                                                                shiny::br(), 
                                                                                shiny::fluidRow(shiny::column(6, shiny::radioButtons("simSpendChartType", "Spend Type", choices=c("Absolute", "Percentage"), inline = TRUE))),
                                                                                shiny::fluidRow(shiny::column(12, shiny::plotOutput("simulationSpendAreaPlot")))),
                                                                shiny::tabPanel("Contribution", 
                                                                                shiny::br(),
                                                                                shiny::fluidRow(shiny::column(6, shiny::radioButtons("simContributionChartType", "Contribution Type", choices=c("Absolute", "Percentage"), inline = TRUE))),
                                                                                shiny::fluidRow(shiny::column(12, shiny::plotOutput("simulationContributionPlot")))),
                                                                shiny::tabPanel("ROI", 
                                                                                shiny::br(), 
                                                                                shiny::fluidRow(shiny::column(12, shiny::plotOutput("simulationROIPlot"))))))))
  })
  
  
  shiny::observeEvent({
    input$viewPanelCompareScenarioResultsButton
    # !is.null(input$scenarioFile)
    }, {
    tryCatch({
    panelSelected <- input$selectPanelCompareScenarioResults
    
    ######## Progress bar #########
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Processing...")
    
    ####### Error handling ###########
    if(is.null(reactData$panelVar) | is.null(reactData$spendVar) | is.null(reactData$targetVar) | is.null(reactData$DateVar)) {shiny::showNotification("Please select Spend, Panel, Seasonal and Trend Columns as described in the Configuration section", type = 'error'); return()}
    
    if(is.null(reactData$viewScenarioSelectedData) | is.null(reactData$dataset))
    {
      shiny::showNotification('Please run the scenario before attempting to view its results', type='error'); 
      return()
    }
    
    if(nrow(reactData$viewScenarioSelectedData)<=0 | nrow(reactData$dataset)<=0){
      shiny::showNotification('Please run the scenario before attempting to view its results', type='error'); 
      return()
    }
    if(panelSelected==""){
      shiny::showNotification('Please select a valid panel from the list', type='error'); return()
    }
    ### Filter dataset based on choice of panel
    if ("Total" %in% panelSelected)
    {
      simulatedData <- reactData$viewScenarioSelectedData
      historicalData <- reactData$dataset
      
      ########### Creating data for when "All" is selected in panel ##################
      histAllPanelSalesData <- getAllPanelsDataset(dataset=simulatedData,
                                           lvl_of_data=c(reactData$DateVar, reactData$panelVar),
                                           melt_data=c(reactData$spendVar),
                                           y_variable=reactData$targetVar)
      setnames(histAllPanelSalesData, old = c(reactData$DateVar, reactData$targetVar, 'variable', 'value'), new = c('Time_Period','Total_Sales', 'Spend_Channel', 'Total_Spend'))
      
    }
    else
    {
      simulatedData <- dplyr::filter(reactData$viewScenarioSelectedData, reactData$viewScenarioSelectedData[reactData$panelVar] == panelSelected)
      historicalData <- dplyr::filter(reactData$dataset, reactData$dataset[reactData$panelVar] == panelSelected)
    }
    
    
    ############################## Creating Spends and Sales Data ##########################
    
    simAreaData <- getAllPanelsDataset(dataset=simulatedData,
                                        lvl_of_data=c(reactData$DateVar),
                                        melt_data=c(reactData$spendVar),
                                        y_variable=reactData$targetVar)
    setnames(simAreaData, old=c('variable', reactData$DateVar, reactData$targetVar, 'value'), new=c('Spend_Channel', 'Time_Period', 'Total_Sales', 'Total_Spend'))
    
    ##### Creating a dataset to calculate spends percentage #######
    simAreaData <- getSpendsDataset(dataset=simAreaData,
                                     lvl_of_data=c('Time_Period'),
                                     spend_variable=c('Total_Spend'))
    
    ############### Creating dataset containing both historical and simulated data ############
    allData <- rbind(historicalData, simulatedData)
    allData <- convertBackFactors_2(mmxModel$train_data, allData)
    
    ####################### Apply adstock to total data ###############
    allAdstockData <- apply_adstock(dataset = as.data.frame(allData), 
                                            spend_variables = as.character(adstock$dataset$Spend_Channel), 
                                            decay_rate = adstock$dataset$Adstock_Decay, 
                                            time_variable = reactData$DateVar, 
                                            adstock_panel_variables = reactData$panelVar)
    
    ############################# new ############################################
    #### Calculating absolute Contributions for total data ############
    totalContributionAbsData <- fn_contrib(mmx_model = mmxModel$model
                                      , master_dataset = as.data.frame(allAdstockData)
                                      , spendVar = reactData$spendVar
                                      , req_contrib_cols = c(reactData$DateVar)
                                      , DateVar = reactData$DateVar
                                      , panelVar = reactData$panelVar)
    
    setnames(totalContributionAbsData, old = c(reactData$DateVar,'variable', 'value'), new = c('Time_Period','Spend_Channel', 'Total_Contribution'))
    
    ############ Get percentage contributions for total data ###############
    scenarioAllContribData <- fn_pct_contrib(dataset = totalContributionAbsData,
                                          lvl_of_data = c('Time_Period'),
                                          contrib_col = 'Total_Contribution')
    
    ######## Filtering contribution for historical comparison and simulation period ###########
    scenarioSimContribData <- filter_at(scenarioAllContribData, 'Time_Period', any_vars(. >= reactData$simStartDateVar & . <= reactData$simEndDateVar))
    scenarioHistContribData <- filter_at(scenarioAllContribData, 'Time_Period', any_vars(. >= reactData$histStartDateVar & . <= reactData$histEndDateVar))
    
    ####### Creating flags for historical comparison and simulated period contribution data #####
    scenarioSimContribData$data_time_period = rep('Simulated', each=nrow(scenarioSimContribData))
    scenarioHistContribData$data_time_period = rep('Historical', each=nrow(scenarioHistContribData))
    
    ########## Creating dataframe with all calculated KPIs #####################
    simAllData <- merge(scenarioSimContribData, simAreaData, by=c('Time_Period','Spend_Channel'))
    
    ######### Create ROI KPI ###############
    simAllData <- simAllData %>% mutate(Total_ROI = Total_Contribution/Total_Spend)
    
    ### Creating reactive dataframes for section Time Series Comparison with Historical Period ###
    ########################################### start ########################################
    
    ######################## Create overlay time series dataframe ########################
    ##### Get one dataset for historical comparison and simulation period contribution data ######
    overlayContributionData <- rbind(scenarioHistContribData, scenarioSimContribData)
    
    ###### Filtering historical data for historical comparison period ######
    historicalComparisonDataset <- filter_at(historicalData, reactData$DateVar, any_vars(. >= reactData$histStartDateVar & . <= reactData$histEndDateVar))
    
    ######### Creating dataframe containing both simulated and historical spends #########
    overlayData <- fn_rbind_data(hist_dataset = historicalComparisonDataset, 
                                 opti_dataset = simulatedData, 
                                 lvl_of_data = c(reactData$DateVar), 
                                 melt_data = reactData$spendVar,
                                 type_data = 'Simulated',
                                 agg_variable = reactData$targetVar)
    
    
    ######## Rename columns ##########
    setnames(overlayData, old=c('variable', 'value', reactData$DateVar, reactData$targetVar), new=c('Spend_Channel', 'Total_Spend', 'Time_Period', 'Total_Sales'))
    
    ######## Get all KPIs for simulated and historical comparison period #########
    reactData$overlayChartData <- merge(overlayData, overlayContributionData, by=c('Time_Period', 'Spend_Channel', 'data_time_period'))
    
    ######################## Create continuous time series dataframe ########################
    
    ####### Convert total dataset into long format for filtering by channel ####### 
    allDataLong <- getAllPanelsDataset(dataset=allData,
                                       lvl_of_data=c(reactData$DateVar),
                                       melt_data=c(reactData$spendVar),
                                       y_variable=reactData$targetVar)
    
    setnames(allDataLong, old=c('variable', 'value', reactData$DateVar, reactData$targetVar), new=c('Spend_Channel', 'Total_Spend', 'Time_Period', 'Total_Sales'))
    
    ###### Get one dataset containing all KPIs in long format ############
    reactData$allDataLong <- merge(allDataLong, scenarioAllContribData, by=c('Spend_Channel', 'Time_Period'))
    
    ################################ end ######################################################
    
    ##### Display summary table ##############
    nums <-  unlist(lapply(simAllData, is.numeric))
    simAllData$Time_Period <- as.Date(simAllData$Time_Period)
    output$runSimDataOutput = DT::renderDataTable(DT::datatable(roundOffDataFrame(simAllData, TRUE, 2), options=list(scrollX=T, searching=TRUE, paging=TRUE))%>%formatCurrency(colnames(simAllData[nums]),currency = "", interval = 3, mark = ","))
    
    ################### Visualizations for Simulation Scenario Summary #####################
    
    ############################ Sales #########################################
    simulationSalesggPlot <- salesLinePlot(dataset = simAreaData, chartTitle = "Sales",xvar = "Time_Period", xlab = "Time", yvar = "Total_Sales", ylab = "Sales", lvar="#1F77B4", panelLabel = panelSelected) + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
    
    if('Total' %in% panelSelected){
      simulationAllSalesggPlot <- summarySalesPlot(dataset = histAllPanelSalesData, chartTitle = "Sales",xvar = "Time_Period", xlab = "Time", yvar = "Total_Sales", ylab = "Total Sales", lvar = reactData$panelVar, panelLabel = panelSelected, channelLabel = "All")
      
      
      simSalesPlotType <- function(type) 
      {
        switch(type,
               Aggregate = simulationSalesggPlot,
               All = simulationAllSalesggPlot
        )
      }  
      
      output$simulationSalesPlot <- renderPlot({ 
        simSalesPlotType(input$simSalesChartType)
      })
    }else{
      output$simulationSalesPlot <- renderPlot({simulationSalesggPlot})
    }
    
    
    simulationSpendAreaPctggPlot <- summaryAreaPlot(dataset = simAreaData, chartTitle = "Simulated Spend (in %)",xvar = "Time_Period", xlab = "Time", yvar = "Total_Pct_Spend", ylab = "Total Spend (in %)", lvar = "Spend_Channel", panelLabel = panelSelected)
    
    ############################ Spends #########################################
    
    simulationSpendAreaAbsggPlot <- summaryAreaPlot(dataset = simAreaData, chartTitle = "Simulated Spend",xvar = "Time_Period", xlab = "Time", yvar = "Total_Spend", ylab = "Total Spend", lvar = "Spend_Channel", panelLabel = panelSelected)
    simulationSpendAreaAbsggPlot <- simulationSpendAreaAbsggPlot + scale_y_continuous(labels=dollar_format(prefix="$"))
    
    
    simSpendPlotType <- function(type) 
    {
      switch(type,
             Percentage = simulationSpendAreaPctggPlot,
             Absolute = simulationSpendAreaAbsggPlot
      )
    }  
    
    output$simulationSpendAreaPlot <- renderPlot({ 
      simSpendPlotType(input$simSpendChartType)
    })
    
    ############################ Contribution #########################################
    
    simulationContributionPctggPlot <- summaryAreaPlot(dataset = scenarioSimContribData, chartTitle = "Simulated Contribution (in %)",xvar = "Time_Period", xlab = "Time", yvar = "Total_Pct_Contribution", ylab = "Total Contribution (in %)", lvar = "Spend_Channel", panelLabel = panelSelected)

    
    simulationContributionAbsggPlot <- summaryAreaPlot(dataset = scenarioSimContribData, chartTitle = "Simulated Contribution",xvar = "Time_Period", xlab = "Time", yvar = "Total_Contribution", ylab = "Total Contribution", lvar = "Spend_Channel", panelLabel = panelSelected) + scale_y_continuous(labels=dollar_format(prefix="$"))
    
  
    
    plotType <- function(type) 
    {
      switch(type,
             Percentage = simulationContributionPctggPlot,
             Absolute = simulationContributionAbsggPlot
      )
    }  
    
    output$simulationContributionPlot <- renderPlot({ 
      plotType(input$simContributionChartType)
    })
    
    ############################ ROI #########################################
    
    simulationROIggPlot <- summarySalesPlot(dataset = simAllData, chartTitle = "Sales",xvar = "Time_Period", xlab = "Time", yvar = "Total_ROI", ylab = "Total ROI", lvar = "Spend_Channel", panelLabel = panelSelected)
    
    output$simulationROIPlot <- renderPlot({simulationROIggPlot})
    
    ########### Save plots to object for downloading them as images ##############
    grobScenarioSummaryPlots <- arrangeGrob(simulationSalesggPlot, simulationSpendAreaPctggPlot, simulationSpendAreaAbsggPlot, simulationContributionPctggPlot, simulationContributionAbsggPlot, simulationROIggPlot, ncol=2)
    
    ########## Download handlers for plots ################
    output$downloadScenarioSummaryPlots <- shiny::downloadHandler(
      filename = function(){'Scenario_Summary_Plots.png'},
      content = function(file) {
        ggsave(file, plot=grobScenarioSummaryPlots, device='png', width = 45, height = 25, units = 'cm')
      },
      shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
    )
    
    ##### Creating scenario summary dataset that will be available for download by user #########
    keeps <- c('Time_Period', 'Spend_Channel', 'Total_Sales', 'Total_Spend', 'Total_Contribution', 'Total_Pct_Contribution', 'Total_ROI')
    scenarioSummaryData <- simAllData[keeps]
    
    ########## Download handlers for data ################
    output$downloadScenarioSummaryData <- shiny::downloadHandler(
      filename = function() {'Scenario_Summary_Data.csv'},
      content = function(file) {
        write.csv(scenarioSummaryData, file, row.names = FALSE)
      }
    )
    
    ##### Saving Plots to reactive elements for use in Report
    reactData$simulationSalesPlot <- simulationSalesggPlot
    reactData$simulationSpendAreaPctPlot <- simulationSpendAreaPctggPlot
    reactData$simulationSpendAreaAbsPlot <- simulationSpendAreaAbsggPlot
    reactData$simulationContributionPctPlot <- simulationContributionPctggPlot
    reactData$simulationContributionAbsPlot <- simulationContributionAbsggPlot
    reactData$simulationROIPlot <- simulationROIggPlot
    
    
    ################################# Code for bar plots ########################################
    histSimChartData <- overlayData %>% group_by_at(c('Spend_Channel', 'data_time_period')) %>% summarize(Total_Sales=sum(Total_Sales), Total_Spend=sum(Total_Spend)) %>% data.frame()
    scenarioChartContribData <- overlayContributionData %>% group_by_at(c('Spend_Channel', 'data_time_period')) %>% summarize(Total_Contribution=sum(Total_Contribution), Total_Pct_Contribution=sum(Total_Pct_Contribution)) %>% data.frame()
    scenarioAllData <- merge(histSimChartData, scenarioChartContribData, by=c('Spend_Channel', 'data_time_period'))
    
    ######### Create ROI KPI ###############
    scenarioAllData <- scenarioAllData %>% mutate(Total_ROI = Total_Contribution/Total_Spend)
    
    #################### Visualizations for simulated scenario comparison ######################
    
    ########################################## Sales ########################################
    salesSummaryggPlot <- sales_fn_bar_graph(dataset =  histSimChartData, chartTitle = "Sales Comparison" , xvar = "data_time_period", xlab = "", yvar = "Total_Sales", ylab = "Total Sales", lvar = "#1F77B4", panelLabel = panelSelected) +
      scale_y_continuous(labels=scales::dollar_format(prefix="$")) +
      geom_text(aes(label = dollar(Total_Sales)), position=position_dodge(width=0.9), vjust=-0.25)
    
    output$salesSummaryPlot <- renderPlot({salesSummaryggPlot})
    
    ########################################## Spend ########################################
    spendSummaryggPlot <- fn_bar_graph(dataset =  histSimChartData, chartTitle = "Spends Comparison" , xvar = "Spend_Channel", xlab = "Spend Channel", yvar = "Total_Spend", ylab = "Total Spend", lvar = "data_time_period", panelLabel = panelSelected) + 
      scale_y_continuous(labels=scales::dollar_format(prefix="$")) +
      geom_text(aes(label = dollar(Total_Spend)), position=position_dodge(width=0.9), vjust=-0.25)
    
    output$spendSummaryPlot <- renderPlot({spendSummaryggPlot})
    
    ########################################## ROI ########################################
    ROISummaryggPlot <- fn_bar_graph(dataset = scenarioAllData, chartTitle = "ROI Comparison" , xvar = "Spend_Channel", xlab = "Spend Channel", yvar = "Total_ROI", ylab = "Total ROI", lvar = "data_time_period", panelLabel = panelSelected) + 
      geom_text(aes(label = format(round(Total_ROI, 2), big.mark = ",")), position=position_dodge(width=0.9), vjust=-0.25)
    
    output$ROISummaryPlot <- renderPlot({ROISummaryggPlot})
    
    ####################################### Contribution ########################################
    contributionSummaryggPlot <- fn_bar_graph(dataset = scenarioChartContribData, chartTitle = "Contribution Comparison" , xvar = "Spend_Channel", xlab = "Spend Channel", yvar = "Total_Contribution", ylab = "Total Contribution", lvar = "data_time_period", panelLabel = panelSelected)  + geom_text(aes(label = dollar(Total_Contribution)), position=position_dodge(width=0.9), vjust=-0.25) + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
    
    output$contributionSummaryPlot <- renderPlot({contributionSummaryggPlot})
    
    grobHistSimSummaryPlots <- arrangeGrob(salesSummaryggPlot, spendSummaryggPlot, contributionSummaryggPlot, ROISummaryggPlot, ncol=2)
    
    
    
    ######################## Creating data for simulation summary cards ########################
    
    ##### Keep relevant columns #####
    keeps <- c('data_time_period', 'Spend_Channel', 'Total_Sales', 'Total_Spend', 'Total_Contribution', 'Total_Pct_Contribution', 'Total_ROI')
    histSimSummaryData <- scenarioAllData[keeps]
    
    ### Creating summary data of spend, sales, contributions and ROI data for the two periods - hist and sim ###
    scenarioSummaryData <- histSimSummaryData %>% group_by_at(c('data_time_period')) %>% summarise(Total_Sales = mean(Total_Sales), Total_Spend = sum(Total_Spend), Total_Contribution = sum(Total_Contribution), Total_Pct_Contribution = sum(Total_Pct_Contribution), Total_ROI = Total_Contribution/Total_Spend) %>% data.frame()
    
    # first remember the names
    timePeriodNameCol <- scenarioSummaryData$data_time_period
    
    # transpose all but the first column (name)
    scenarioSummaryData <- as.data.frame(t(scenarioSummaryData[,-1]))
    colnames(scenarioSummaryData) <- timePeriodNameCol
    
    setDT(scenarioSummaryData, keep.rownames = TRUE)[]
    setnames(scenarioSummaryData, old=c('rn'), new=c('KPI'))
    
    
    ##### Calculating difference in KPIs between the two scenarios #####
    scenarioSummaryData <- scenarioSummaryData %>% mutate(Difference = Simulated - Historical)
    
    ################################ Displaying card dates ################################
    
    output$hist_spend <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_Spend",
                         data_flag = "Historical",
                         currency_symbol = "$",
                         magnitude = "M"))
    })
    
    output$sim_spend <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_Spend",
                         data_flag = "Simulated",
                         currency_symbol = "$",
                         magnitude = "M"))
    })
    
    output$hist_sales <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_Sales",
                         data_flag = "Historical",
                         currency_symbol = "$",
                         magnitude = "M"))
    })
    
    output$sim_sales <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_Sales",
                         data_flag = "Simulated",
                         currency_symbol = "$",
                         magnitude = "M"))
    })
    
    output$hist_contrib <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_Contribution",
                         data_flag = "Historical",
                         currency_symbol = "$",
                         magnitude = "M"))
    })
    
    output$sim_contrib <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_Contribution",
                         data_flag = "Simulated",
                         currency_symbol = "$",
                         magnitude = "M"))
    })
    
    output$hist_roi <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_ROI",
                         data_flag = "Historical",
                         currency_symbol = "",
                         magnitude = ""))
    })
    
    output$sim_roi <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_ROI",
                         data_flag = "Simulated",
                         currency_symbol = "",
                         magnitude = ""))
    })
    
    ########### Download handlers for scenario comparison data and plots ###########
    output$downloadHistSimSummaryData <- shiny::downloadHandler(
      filename = function(){'Historical_Simulation_Summary_Data.csv'},
      content = function(file) {
        write.csv(histSimSummaryData, file, row.names = FALSE)
      },
      shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
    )
    
    output$downloadHistSimSummaryPlots <- shiny::downloadHandler(
      filename = function(){'Historical_Simulation_Summary_Plots.png'},
      content = function(file) {
        ggsave(file, plot=grobHistSimSummaryPlots, device='png', width = 45, height = 25, units = 'cm')
      }
    )
    
    ########################## Plot for report #################################
    reactData$salesSummaryPlot <- salesSummaryggPlot
    reactData$spendSummaryPlot <- spendSummaryggPlot
    reactData$contributionSummaryPlot <- contributionSummaryggPlot
    reactData$ROISummaryPlot <- ROISummaryggPlot
    
  },error=function(e) showNotification(paste0('View Scenario Summary Results :',e[1])))
  })
  
  
  ### Checks for click of help icon
  shiny::observeEvent(
    input$viewComparisonSummaryHelpText,
    {
      showModal(modalDialog(
        title = "Simulation Period vs Historical Comparison Period(Summary) - Help",
        shiny::p(shiny::HTML(scenarioSummaryText)),
        footer = tagList(
          modalButton("OK")
        )
      ))
    })
  
  output$scenarioHistoricalComparisonInfoUI <- shiny::renderUI({
    shiny::fluidRow(column(8, shiny::tags$h3(paste0('Simulation Period vs Historical Comparison Period (Summary) - ', reactData$viewScenarioSelected)))
                    ,shiny::conditionalPanel('input.viewPanelCompareScenarioResultsButton>0', column(2, style='padding-top:25px', shiny::downloadButton('downloadHistSimSummaryPlots', 'Download Plot(s)')))
                    ,shiny::conditionalPanel('input.viewPanelCompareScenarioResultsButton>0', column(2, style='padding-top:25px', shiny::downloadButton('downloadHistSimSummaryData', 'Download Data'))))
  })
  
  output$scenarioHistoricalComparisonsPlotsUI <- shiny::renderUI({
    tagList(
      shiny::fluidRow(class='box1',column(12, 
                                          shiny::tabsetPanel(type = "tabs",
                                                             shiny::tabPanel("Sales", 
                                                                             shiny::br(), 
                                                                             shiny::fluidRow(shiny::column(12, shiny::plotOutput("salesSummaryPlot")))),
                                                             shiny::tabPanel("Spend", 
                                                                             shiny::br(), 
                                                                             shiny::fluidRow(shiny::column(12, shiny::plotOutput("spendSummaryPlot")))),
                                                             shiny::tabPanel("Contribution", 
                                                                             shiny::br(), 
                                                                             shiny::fluidRow(shiny::column(12, shiny::plotOutput("contributionSummaryPlot")))),
                                                             shiny::tabPanel("ROI", 
                                                                             shiny::br(), 
                                                                             shiny::fluidRow(shiny::column(12, shiny::plotOutput("ROISummaryPlot")))))))
    )
  })

  
  output$scenarioTimeSeriesComparisonInfoUI <- shiny::renderUI({
    shiny::tags$h3(paste0('Simulation Period vs Historical Comparison Period (Time Series) - ', reactData$viewScenarioSelected))
  })
  
  output$scenarioTimeSeriesComparisonUI <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(4, shiny::selectInput("selectScenarioChannel", "Select Channel", choices = c("", "Total", unique(reactData$allDataLong["Spend_Channel"])), multiple=FALSE, selected = "Total"))
                    ,shiny::column(2,style='padding-top:25px', class='button_pad', shiny::actionButton("viewChannelScenarioResultsButton","Submit"))
                    ,shiny::conditionalPanel("input.viewChannelScenarioResultsButton>0", column(2, style='padding:25px', downloadButton("downloadChannelScenarioResultsPlots", "Download Plot(s)")))
                    ,shiny::conditionalPanel("input.viewChannelScenarioResultsButton>0", column(2, style='padding:25px', downloadButton("downloadChannelScenarioResultsData", "Download Data")))
    )
  })
  
  output$scenarioTimeSeriesComparisonPlotsUI <- shiny::renderUI({
    shiny::fluidRow(class='box1', column(12,
                                         shiny::tabsetPanel(type = "tabs",
                                                            shiny::tabPanel("Sales", 
                                                                            shiny::br(),
                                                                            shiny::fluidRow(shiny::column(6, shiny::radioButtons("salesChartType1", "Chart Type", choices=c("Sales", "Sales_Overlay"), inline = TRUE))),
                                                                            shiny::fluidRow(shiny::column(12, shiny::plotOutput("salesTSPlot")))),
                                                            shiny::tabPanel("Spend", 
                                                                            shiny::br(),
                                                                            shiny::fluidRow(shiny::column(6, shiny::radioButtons("spendChartType", "Chart Type", choices=c("Spend", "Spend_Overlay"), inline = TRUE))),
                                                                            shiny::fluidRow(shiny::column(12, shiny::plotOutput("spendTSPlot")))),
                                                            shiny::tabPanel("Contribution", 
                                                                            shiny::br(),
                                                                            shiny::fluidRow(shiny::column(6, shiny::radioButtons("contributionChartType", "Chart Type", choices=c("Contribution", "Contribution_Overlay"), inline = TRUE))),
                                                                            shiny::fluidRow(shiny::column(12, shiny::plotOutput("contributionTSPlot")))),
                                                            shiny::tabPanel("ROI", 
                                                                            shiny::br(),
                                                                            shiny::fluidRow(shiny::column(6, shiny::radioButtons("ROIChartType1", "Chart Type", choices=c("ROI", "ROI_Overlay"), inline = TRUE))),
                                                                            shiny::fluidRow(shiny::column(12, shiny::plotOutput("ROITSPlot")))))))
  })
  
  shiny::observeEvent({input$viewChannelScenarioResultsButton}, {
    tryCatch({
    channelSelected <- input$selectScenarioChannel
    
    ######## Progress bar #########
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Processing...")
    
    ####### Error handling ###########
    if(nrow(reactData$overlayChartData)<=0 | nrow(reactData$allDataLong)<=0 ){shiny::showNotification("Please view panel level results before viewing channel level results", type='error'); return()}
    if(channelSelected==""){
      shiny::showNotification('Please select a valid channel from the list', type='error'); return()
    }

    if("Total" %in% channelSelected){
      overlayChannelChartData <- reactData$overlayChartData
      continuousChannelChartData <- reactData$allDataLong
    }
    else{
      overlayChannelChartData <- dplyr::filter(reactData$overlayChartData, reactData$overlayChartData["Spend_Channel"]==channelSelected)
      continuousChannelChartData <- dplyr::filter(reactData$allDataLong, reactData$allDataLong["Spend_Channel"]==channelSelected)
    }
    
    
    continuousChannelChartData <- continuousChannelChartData %>% group_by_at('Time_Period') %>% summarize(Total_Sales = mean(Total_Sales), Total_Spend = sum(Total_Spend), Total_Contribution =sum(Total_Contribution), Total_Pct_Contribution = sum(Total_Pct_Contribution)) %>% data.frame()
    
    continuousChannelChartData <- continuousChannelChartData %>% mutate(Total_ROI = Total_Contribution/Total_Spend)
    
    
    overlayChannelChartData <- overlayChannelChartData %>% group_by_at(c('Time_Period', 'data_time_period')) %>% summarize(Total_Sales = sum(Total_Sales), Total_Spend = sum(Total_Spend), Total_Contribution =sum(Total_Contribution), Total_Pct_Contribution = sum(Total_Pct_Contribution)) %>% data.frame()
    
    overlayChannelChartData <- overlayChannelChartData %>% mutate(Total_ROI = Total_Contribution/Total_Spend)
    
    overlayChannelChartData <- build_timeSeriesDataset(overlayChannelChartData, dateFrequency = reactData$dateFrequency, DateVar = "Time_Period")
    
    
    salesTSOverlayggPlot <- timeSeriesOverlayPlot(dataset = overlayChannelChartData, chartTitle = "Sales",xvar = "Time_Num", xlab = "Time", yvar = "Total_Sales", ylab = "Total Sales", lvar = "data_time_period", panelLabel = reactData$panelSelected, channelLabel = channelSelected) + scale_y_continuous(labels=dollar_format(prefix="$"))
    
    
    salesTSggPlot <- timeSeriesPlot(dataset = continuousChannelChartData, chartTitle = "Sales", xvar = "Time_Period", xlab = "Time", ylab = "Total Sales", yvar = "Total_Sales", opti_seq = reactData$optiDateSequence, hist_seq = reactData$histDateSequence, seq_xvar = reactData$DateVar,  panelLabel = reactData$panelSelected, channelLabel = channelSelected, legendVar="Simulation") + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
    
    salesPlotType <- function(type) 
    {
      switch(type,
             Sales_Overlay = salesTSOverlayggPlot,
             Sales = salesTSggPlot
      )
    }  
    
    output$salesTSPlot <- renderPlot({ 
      salesPlotType(input$salesChartType1)
    })
    
    spendTSOverlayggPlot <- timeSeriesOverlayPlot(dataset = overlayChannelChartData, chartTitle = "Spend",xvar = "Time_Num", xlab = "Time", yvar = "Total_Spend", ylab = "Total Spend", lvar = "data_time_period", panelLabel = reactData$panelSelected, channelLabel = channelSelected) + scale_y_continuous(labels=dollar_format(prefix="$"))
    
    spendTSggPlot <- timeSeriesPlot(continuousChannelChartData, chartTitle = "Spend", xvar = "Time_Period", xlab = "Time", ylab = "Total Spend", yvar = "Total_Spend", opti_seq = reactData$optiDateSequence, hist_seq = reactData$histDateSequence, seq_xvar = reactData$DateVar, panelLabel = reactData$panelSelected, channelLabel = channelSelected, legendVar="Simulation") + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
    
    spendPlotType <- function(type) 
    {
      switch(type,
             Spend_Overlay = spendTSOverlayggPlot,
             Spend = spendTSggPlot
      )
    }  
    
    output$spendTSPlot <- renderPlot({ 
      spendPlotType(input$spendChartType)
    })
    
    ROITSOverlayggPlot <- timeSeriesOverlayPlot(dataset = overlayChannelChartData, chartTitle = "ROI",xvar = "Time_Num", xlab = "Time", yvar = "Total_ROI", ylab = "Total ROI", lvar = "data_time_period", panelLabel = reactData$panelSelected, channelLabel = channelSelected)
   
    ROITSggPlot <- timeSeriesPlot(dataset = continuousChannelChartData, chartTitle = "ROI", xvar = "Time_Period", xlab = "Time", ylab = "Total ROI", yvar = "Total_ROI", opti_seq = reactData$optiDateSequence, hist_seq = reactData$histDateSequence, seq_xvar = reactData$DateVar,  panelLabel = reactData$panelSelected, channelLabel = channelSelected, legendVar="Simulation")
    
    ROIPlotType <- function(type) 
    {
      switch(type,
             ROI_Overlay = ROITSOverlayggPlot,
             ROI = ROITSggPlot
      )
    }  
    
    output$ROITSPlot <- renderPlot({ 
      ROIPlotType(input$ROIChartType1)
    })
    
    contributionTSOverlayggPlot <- timeSeriesOverlayPlot(dataset = overlayChannelChartData, chartTitle = "Contribution",xvar = "Time_Num", xlab = "Time", yvar = "Total_Contribution", ylab = "Total Contribution", lvar = "data_time_period", panelLabel = "Total", channelLabel = channelSelected) + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
    
    contributionTSggPlot <- timeSeriesPlot(dataset = continuousChannelChartData, chartTitle = "Contribution", xvar = "Time_Period", xlab = "Time", ylab = "Total Contribution", yvar = "Total_Contribution", opti_seq = reactData$optiDateSequence, hist_seq = reactData$histDateSequence, seq_xvar = reactData$DateVar,  panelLabel = reactData$panelSelected, channelLabel = channelSelected, legendVar="Simulation") + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
    
    contributionPlotType <- function(type) 
    {
      switch(type,
             Contribution_Overlay = contributionTSOverlayggPlot,
             Contribution = contributionTSggPlot
      )
    }  
    
    output$contributionTSPlot <- renderPlot({ 
      contributionPlotType(input$contributionChartType)
    })
    
    #################### Saving plots to grob object for download ############################
    
    grobChannelScenarioResultsPlots <- arrangeGrob(salesTSOverlayggPlot, salesTSggPlot, spendTSOverlayggPlot, spendTSggPlot, contributionTSggPlot, ROITSggPlot, ncol=2)
    
    ###################### Download handlers for plots and data ############################
    
    output$downloadChannelScenarioResultsPlots <- shiny::downloadHandler(
      filename = function(){'Channel_Comparison_Plots.png'},
      content = function(file) {
        ggsave(file, plot=grobChannelScenarioResultsPlots, device='png', width = 45, height = 25, units = 'cm')
      },
      shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
    )
    
    output$downloadChannelScenarioResultsData <- shiny::downloadHandler(
      filename = function(){'Channel_Comparison_Data.csv'},
      content = function(file) {
        write.csv(continuousChannelChartData, file, row.names = FALSE)
      }
    )
    
    ######################## Saving Plots for Report ############################ 
    
    reactData$salesTSOverlayPlot <- salesTSOverlayggPlot
    reactData$salesTSPlot <- salesTSggPlot
    reactData$spendTSOverlayPlot <- spendTSOverlayggPlot
    reactData$spendTSPlot <- spendTSggPlot
    reactData$contributionTSOverlayPlot <- contributionTSOverlayggPlot
    reactData$contributionTSPlot <- contributionTSggPlot
    reactData$ROITSOverlayPlot <- ROITSOverlayggPlot
    reactData$ROITSPlot <- ROITSggPlot
    
  },error=function(e) showNotification(paste0('View Scenario Time Series Results :',e[1])))
  })
  
  
  
  #########################################################################################################################
  ## Compare Scenarios
  #########################################################################################################################
  
  ### Checks for click of help icon
  shiny::observeEvent(
    input$viewCompareScenarioHelpText,
    {
      showModal(modalDialog(
        title = "Compare Scenarios - Help",
        shiny::p(shiny::HTML(compareScenarioText)),
        footer = tagList(
          modalButton("OK")
        )
      ))
    })
  
  output$viewCompareScenarioDatesText <- shiny::renderUI({
    if(is.null(reactData$simStartDateVar)) return(shiny::HTML(''))
    
    list(shiny::tags$h5(shiny::HTML(paste0('<b>Historical Comparison Period: </b>', reactData$histStartDateVar,' to ', reactData$histEndDateVar))),
         shiny::tags$h5(shiny::HTML(paste0('<b>Simulation/Optimization Period: </b>', reactData$simStartDateVar,' to ', reactData$simEndDateVar))))
  })
  
  
  output$compareScenariosUI <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(4,shiny::selectizeInput("compareScenarioList", "Select two scenarios for comparison",
                                                          unique(compareScenarioTable$dataset$Scenario_Name),
                                                          options = list(maxItems = 2))),
                    shiny::column(style='padding-top:25px', class='button_pad',2, shiny::actionButton('compareScenarioSubmit','Submit')),
                    shiny::column(12,shiny::verbatimTextOutput("compareScenarioConfiguration")))
  })
  
  
  shiny::observeEvent(input$compareScenarioSubmit,{
    tryCatch({
    adstock$compareScenarioList <- list(scenario_list = input$compareScenarioList)
    compareScenarioConfigurationText <- paste0(
      "Scenarios Selected For Comparison: ", adstock$compareScenarioList, "\n"
    )
    
    ######## Progress bar #########
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Processing...")
    
    ####### Error handling ###########
    if(length(adstock$compareScenarioList$scenario_list)<2){shiny::showNotification('Please select two scenarios for comparison', type='error'); return()}
    
    if(is.null(scenarioList$files[[adstock$compareScenarioList$scenario_list[1]]]$scheduling_dataset$runSimulationDataset) & is.null(optimisationScenarioList$files[[adstock$compareScenarioList$scenario_list[1]]]$optimising_dataset$optimised_dataset))
    {shiny::showNotification('Please create a scenario or select a created scenario', type='error'); return()}
    if(is.null(scenarioList$files[[adstock$compareScenarioList$scenario_list[2]]]$scheduling_dataset$runSimulationDataset) & is.null(optimisationScenarioList$files[[adstock$compareScenarioList$scenario_list[2]]]$optimising_dataset$optimised_dataset))
    {shiny::showNotification('Please create a scenario or select a created scenario', type='error'); return()}
    
    ###### Get selected scenarios into two separate variables for use later ##########
    adstock$firstScenario <- as.character(adstock$compareScenarioList$scenario_list[1])
    adstock$secondScenario <- as.character(adstock$compareScenarioList$scenario_list[2])
    
    ####### Get appropriate dataset based on simulation/optimization scenario selected #########
    
    if(is.null(scenarioList$files[[adstock$compareScenarioList$scenario_list[1]]]$scheduling_dataset$runSimulationDataset))
      {
      compareScenario$histScenario1 <- optimisationScenarioList$files[[adstock$compareScenarioList$scenario_list[1]]]$optimising_dataset$scenario_histData_long
      compareScenario$histScenario1Wide <- optimisationScenarioList$files[[adstock$compareScenarioList$scenario_list[1]]]$optimising_dataset$scenario_histData_wide
      compareScenario$allHistScenario1Wide <- optimisationScenarioList$files[[adstock$compareScenarioList$scenario_list[1]]]$all_dataset$dataset
      compareScenario$runScenario1Wide <- optimisationScenarioList$files[[adstock$compareScenarioList$scenario_list[1]]]$optimising_dataset$optimised_dataset
      compareScenario$runScenario1 <- melt(compareScenario$runScenario1Wide, measure.vars=reactData$spendVar)
      setnames(compareScenario$runScenario1, old=c('variable', 'value'), new=c('Spend_Channel', 'Spends'))
    }
    if(is.null(optimisationScenarioList$files[[adstock$compareScenarioList$scenario_list[1]]]$optimising_dataset$optimised_dataset))
    {
      compareScenario$histScenario1 <- scenarioList$files[[adstock$compareScenarioList$scenario_list[1]]]$historical_dataset$long_dataset
      compareScenario$histScenario1Wide <- scenarioList$files[[adstock$compareScenarioList$scenario_list[1]]]$historical_dataset$dataset
      compareScenario$allHistScenario1Wide <- scenarioList$files[[adstock$compareScenarioList$scenario_list[1]]]$all_data$dataset
      compareScenario$runScenario1Wide <- scenarioList$files[[adstock$compareScenarioList$scenario_list[1]]]$scheduling_dataset$runSimulationDataset
      compareScenario$runScenario1 <- melt(compareScenario$runScenario1Wide, measure.vars=reactData$spendVar)
      setnames(compareScenario$runScenario1, old=c('variable', 'value'), new=c('Spend_Channel', 'Spends'))
      drops <- c('Simulated_Date.1')
      compareScenario$runScenario1Wide <- compareScenario$runScenario1Wide[ , !(names(compareScenario$runScenario1Wide) %in% drops)]
    }
    
    if(is.null(scenarioList$files[[adstock$compareScenarioList$scenario_list[2]]]$scheduling_dataset$runSimulationDataset)){
      compareScenario$histScenario2 <- optimisationScenarioList$files[[adstock$compareScenarioList$scenario_list[2]]]$optimising_dataset$scenario_histData_long
      compareScenario$histScenario2Wide <- optimisationScenarioList$files[[adstock$compareScenarioList$scenario_list[2]]]$optimising_dataset$scenario_histData_wide
      compareScenario$allHistScenario2Wide <- optimisationScenarioList$files[[adstock$compareScenarioList$scenario_list[2]]]$all_dataset$dataset
      compareScenario$runScenario2Wide <- optimisationScenarioList$files[[adstock$compareScenarioList$scenario_list[2]]]$optimising_dataset$optimised_dataset
      compareScenario$runScenario2 <- melt(compareScenario$runScenario2Wide, measure.vars=reactData$spendVar)
      setnames(compareScenario$runScenario2, old=c('variable', 'value'), new=c('Spend_Channel', 'Spends'))
      
    }
    if(is.null(optimisationScenarioList$files[[adstock$compareScenarioList$scenario_list[2]]]$optimising_dataset$optimised_dataset))
    {
      compareScenario$histScenario2 <- scenarioList$files[[adstock$compareScenarioList$scenario_list[2]]]$historical_dataset$long_dataset
      compareScenario$histScenario2Wide <- scenarioList$files[[adstock$compareScenarioList$scenario_list[2]]]$historical_dataset$dataset
      compareScenario$allHistScenario2Wide <- scenarioList$files[[adstock$compareScenarioList$scenario_list[2]]]$all_data$dataset
      compareScenario$runScenario2Wide <- scenarioList$files[[adstock$compareScenarioList$scenario_list[2]]]$scheduling_dataset$runSimulationDataset
      compareScenario$runScenario2 <- melt(compareScenario$runScenario2Wide, measure.vars=reactData$spendVar)
      setnames(compareScenario$runScenario2, old=c('variable', 'value'), new=c('Spend_Channel', 'Spends'))
      drops <- c('Simulated_Date.1')
      compareScenario$runScenario2Wide <- compareScenario$runScenario2Wide[ , !(names(compareScenario$runScenario2Wide) %in% drops)]
    }
    
    output$compareScenarioConfiguration <- shiny::renderText(compareScenarioConfigurationText)
    
    # js$greenMenu("tabCompareScenario")
    
    },error=function(e) showNotification(paste0('Select scenarios for comparison :',e[1])))
  })
  
  output$compareScenariosSummaryPanelUI <- shiny::renderUI({
    
    shiny::fluidRow(class='box1',
                    shiny::column(4,shiny::selectInput("compareScenarioPanel", "Select Panel",
                                                       choices = c("", "Total", unique(reactData$dataset[reactData$panelVar])),
                                                       multiple = FALSE, selected = "Total"))
                    ,shiny::column(2, style='padding-top:25px', class='button_pad', shiny::actionButton('compareScenarioPanelSubmit','Submit'))
    )
  })
  
  
  output$compareScenariosSummaryTablesUI <- shiny::renderUI({
    
    shiny::fluidRow(class='box1', shiny::column(12, DT::dataTableOutput('scenarioTable')))
    })
  
  
  
  output$compareScenariosSummaryInfoUI <- shiny::renderUI({
    brow
    shiny::fluidRow(
      shiny::column(8, shiny::tags$h3('Compare Scenarios (Summary)'))
      ,shiny::conditionalPanel("input.compareScenarioPanelSubmit>0",shiny::column(2, style='padding-top:25px', shiny::downloadButton('downloadCompareScenarioPlots','Download Plot(s)')))
      ,shiny::conditionalPanel("input.compareScenarioPanelSubmit>0",shiny::column(2, style='padding-top:25px', shiny::downloadButton('downloadCompareScenarioData','Download Data')))
    )
  })
  
  output$compareScenariosSummaryPlotsUI <- shiny::renderUI({
    shiny::column(12,shiny::tabsetPanel(type = "tabs",
                                        shiny::tabPanel("Sales",
                                                        shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('scenarioSalesHistPlot')),
                                                                        shiny::column(12, shiny::br()),
                                                                        shiny::column(12, shiny::plotOutput('scenarioSalesSimPlot')))),
                                        shiny::tabPanel("Spend", 
                                                        shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('scenarioSpendHistPlot')),
                                                                        shiny::column(12, shiny::br()),
                                                                        shiny::column(12, shiny::plotOutput('scenarioSpendSimPlot')))),
                                        shiny::tabPanel("Contribution", shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('scenarioContributionHistPlot')),
                                                                                        shiny::column(12, shiny::br()),
                                                                                        shiny::column(12, shiny::plotOutput('scenarioContributionSimPlot')))),
                                        shiny::tabPanel("ROI",shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('scenarioROIHistPlot')),
                                                                              shiny::column(12, shiny::br()),
                                                                              shiny::column(12, shiny::plotOutput('scenarioROISimPlot'))))))
    
  })
  
  output$compareScenariosTimeSeriesInfoUI <- shiny::renderUI({
    shiny::tags$h3('Compare Scenarios (Time Series)', reactData$viewScenarioSelected)
  })
  
  output$compareScenariosTimeSeriesUI <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(4, shiny::selectInput("selectCompareScenarioChannel", "Select Channel", choices = c("", "Total", reactData$spendVar), multiple=FALSE, selected = "Total"))
                    ,shiny::column(2,style='padding-top:25px', class='button_pad', shiny::actionButton("submitCompareScenarioChannel","Submit"))
                    ,shiny::conditionalPanel("input.submitCompareScenarioChannel>0", column(2, style='padding:25px', downloadButton("downloadCompareScenarioChannelResultsPlots", "Download Plot(s)")))
                    ,shiny::conditionalPanel("input.submitCompareScenarioChannel>0", column(2, style='padding:25px', downloadButton("downloadCompareScenarioChannelResultsData", "Download Data")))
    )
  })
  
  output$compareScenariosTimeSeriesPlotsUI <- shiny::renderUI({
    column(12,shiny::tabsetPanel(type = "tabs",
                                 shiny::tabPanel("Sales",
                                                 shiny::fluidRow(class='box1', #shiny::column(6, shiny::plotOutput('scenarioSalesHistTSPlot')),
                                                                 shiny::column(12, shiny::plotOutput('scenarioSalesSimTSPlot')))),
                                 shiny::tabPanel("Spend", 
                                                 shiny::fluidRow(class='box1', 
                                                                 shiny::column(12, shiny::plotOutput('scenarioSpendSimTSPlot')))),
                                 shiny::tabPanel("Contribution", 
                                                 shiny::fluidRow(class='box1', 
                                                                 shiny::column(12, shiny::plotOutput('scenarioContributionSimTSPlot')))),
                                 shiny::tabPanel("ROI",
                                                 shiny::fluidRow(class='box1', 
                                                                 shiny::column(12, shiny::plotOutput('scenarioROISimTSPlot'))))))
  })
  
  shiny::observeEvent(input$compareScenarioPanelSubmit, {
    
    tryCatch({
    panelSelected <- input$compareScenarioPanel
    reactData$CSPanelSelected <- input$compareScenarioPanel
    
    ######## Progress bar #########
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Processing...")
    
    ####### Error handling ###########
    if(is.null(panelSelected)){shiny::showNotification('Please select a panel', type='error'); return()}
    
    if(is.null(compareScenario$histScenario1Wide) | is.null(compareScenario$histScenario2Wide) | is.null(compareScenario$runScenario1Wide) | is.null(compareScenario$runScenario2Wide)){shiny::showNotification('Please select two scenarios for comparison', type='error');return()}
    
    if(nrow(compareScenario$histScenario1Wide)<=0 | nrow(compareScenario$histScenario2Wide)<=0 | nrow(compareScenario$runScenario1Wide)<=0 | nrow(compareScenario$runScenario2Wide)<=0){shiny::showNotification('Please select two scenarios for comparison', type='error');return()}
    
    if(is.null(reactData$panelVar) | is.null(reactData$spendVar)| is.null(reactData$targetVar)){shiny::showNotification('Please configure spend, target and panel variables as described in the Configuration Section', type='error'); return()}
    
    if(is.null(mmxModel$model)){shiny::showNotification('Please load and configure model object as described in the Configuration Section', type='error'); return()}
    
    if(panelSelected==""){
      shiny::showNotification('Please select a valid panel from the list', type='error'); return()
    }
    
    
    if("Total" %in% panelSelected){
      
      compareScenario$allHistPanelScenario1Wide <- compareScenario$allHistScenario1Wide
      compareScenario$runPanelScenario1Wide <- compareScenario$runScenario1Wide
      compareScenario$allHistPanelScenario2Wide <- compareScenario$allHistScenario2Wide
      compareScenario$runPanelScenario2Wide <- compareScenario$runScenario2Wide
      compareScenario$histPanelScenario1Wide <- compareScenario$histScenario1Wide
      compareScenario$histPanelScenario2Wide <- compareScenario$histScenario2Wide
    
    }
    else{
    
      compareScenario$allHistPanelScenario1Wide <- compareScenario$allHistScenario1Wide %>% filter(compareScenario$allHistScenario1Wide[reactData$panelVar] == panelSelected)
      compareScenario$runPanelScenario1Wide <- compareScenario$runScenario1Wide %>% filter(compareScenario$runScenario1Wide[reactData$panelVar] == panelSelected)
      compareScenario$allHistPanelScenario2Wide <- compareScenario$allHistScenario2Wide %>% filter(compareScenario$allHistScenario2Wide[reactData$panelVar] == panelSelected)
      compareScenario$runPanelScenario2Wide <- compareScenario$runScenario2Wide %>% filter(compareScenario$runScenario2Wide[reactData$panelVar] == panelSelected)
      compareScenario$histPanelScenario1Wide <- compareScenario$histScenario1Wide %>% filter(compareScenario$histScenario1Wide[reactData$panelVar] == panelSelected)
      compareScenario$histPanelScenario2Wide <- compareScenario$histScenario2Wide %>% filter(compareScenario$histScenario2Wide[reactData$panelVar] == panelSelected)
      
    }
    
    ################ Get total and summarized contributions data for scenario 1 ##################
    scenario1ContribList <- getCompareScenarioContribData(scenarioAllHistData = compareScenario$allHistPanelScenario1Wide,
                                       scenarioSimData = compareScenario$runPanelScenario1Wide,
                                       scenarioName = adstock$firstScenario,
                                       trainData = mmxModel$train_data,
                                       spendVar = reactData$spendVar,
                                       adstockSpendVar = as.character(adstock$dataset$Spend_Channel),
                                       adstockDecayRate = adstock$dataset$Adstock_Decay,
                                       dateVar = reactData$DateVar,
                                       panelVar = reactData$panelVar,
                                       modelRun = mmxModel$model,
                                       simStartDate = reactData$simStartDateVar,
                                       simEndDate = reactData$simEndDateVar,
                                       histStartDate = reactData$histStartDateVar,
                                       histEndDate = reactData$histEndDateVar)
    
    ################ Get total and summarized contributions data for scenario 2 ##################
    scenario2ContribList <- getCompareScenarioContribData(scenarioAllHistData = compareScenario$allHistPanelScenario2Wide,
                                                                 scenarioSimData = compareScenario$runPanelScenario2Wide,
                                                                 scenarioName = adstock$secondScenario,
                                                                 trainData = mmxModel$train_data,
                                                                 spendVar = reactData$spendVar,
                                                                 adstockSpendVar = as.character(adstock$dataset$Spend_Channel),
                                                                 adstockDecayRate = adstock$dataset$Adstock_Decay,
                                                                 dateVar = reactData$DateVar,
                                                                 panelVar = reactData$panelVar,
                                                                 modelRun = mmxModel$model,
                                                                 simStartDate = reactData$simStartDateVar,
                                                                 simEndDate = reactData$simEndDateVar,
                                                                 histStartDate = reactData$histStartDateVar,
                                                                 histEndDate = reactData$histEndDateVar)
    
    #### Get summarized sales and spend data at scenario, data_time_period and channel level ####
    scenario1PanelData <- getCompareScenarioSalesSummaryData(scenarioHistData = compareScenario$histPanelScenario1Wide,
                                                             scenarioSimData = compareScenario$runPanelScenario1Wide,
                                                             scenarioName = adstock$firstScenario,
                                                             dateVar = reactData$DateVar,
                                                             spendVar = reactData$spendVar,
                                                             targetVar = reactData$targetVar)
    
    scenario2PanelData <- getCompareScenarioSalesSummaryData(scenarioHistData = compareScenario$histPanelScenario2Wide,
                                                             scenarioSimData = compareScenario$runPanelScenario2Wide,
                                                             scenarioName = adstock$secondScenario,
                                                             dateVar = reactData$DateVar,
                                                             spendVar = reactData$spendVar,
                                                             targetVar = reactData$targetVar)

    scenarioContribData <- rbind(scenario1ContribList[[2]], scenario2ContribList[[2]])
    
    scenarioPanelData <- rbind(scenario1PanelData, scenario2PanelData)
    
    ### Merging summarized contributions data with sumarrized sales/spends data
    scenarioPanelData <- merge(scenarioPanelData, scenarioContribData, by=c('Spend_Channel', 'data_time_period', 'Scenario'))
    
    ### Create ROI column
    scenarioPanelData <- scenarioPanelData %>% mutate(Total_ROI = Total_Contribution/Total_Spend)
    keeps <- c('Scenario', 'data_time_period','Spend_Channel',  'Total_Sales', 'Total_Spend', 'Total_Contribution', 'Total_ROI')
    scenarioPanelData <- scenarioPanelData[keeps]
    
    ### Table to display spend, sales, contributions and ROI data for the two scenarios
    nums <-  unlist(lapply(scenarioPanelData, is.numeric))
    output$scenarioTable <- DT::renderDataTable(DT::datatable(roundOffDataFrame(scenarioPanelData,TRUE,2),options=list(scrollX=T, searching=TRUE, paging=TRUE))%>%
                                                  formatCurrency(colnames(scenarioPanelData[nums]),currency = "", interval = 3, mark = ","))
    reactData$compareScenarioTable <- scenarioPanelData
    
    ################################# Creating data for cards ##################################
    
    scenarioSummaryData <- dplyr::group_by_at(scenarioPanelData, c('data_time_period', 'Scenario'))
    
    scenarioSummaryData <- dplyr::summarise(scenarioSummaryData, Total_Sales = mean(Total_Sales), Total_Spend = sum(Total_Spend), Total_Contribution = sum(Total_Contribution), Total_ROI = (Total_Contribution/Total_Spend))
    
    scenarioSummaryData <- as.data.frame(scenarioSummaryData)
    
    scenarioSummaryData <- scenarioSummaryData %>% dplyr::filter(scenarioSummaryData$data_time_period=='Simulated')
    drops <- c('data_time_period')
    scenarioSummaryData <- scenarioSummaryData[ , !(names(scenarioSummaryData) %in% drops)]
    # first remember the names
    scenarioNameCol <- scenarioSummaryData$Scenario
    
    # transpose all but the first column (name)
    scenarioSummaryData <- as.data.frame(t(scenarioSummaryData[,-1]))
    colnames(scenarioSummaryData) <- scenarioNameCol
    
    setDT(scenarioSummaryData, keep.rownames = TRUE)[]
    setnames(scenarioSummaryData, old=c('rn'), new=c('KPI'))
    
    scenarioSummaryData <- as.data.frame(scenarioSummaryData)
    
    ##### Calculating difference in KPIs between the two scenarios #####
    
    scenarioSummaryData['Difference'] <- scenarioSummaryData[adstock$firstScenario] - scenarioSummaryData[adstock$secondScenario]
    
    scenarioSummaryData$dataset <- scenarioSummaryData
    
    ### Splitting dataframes into Historical and Simulation Periods
    scenarioPanelHistData <- dplyr::filter(scenarioPanelData, scenarioPanelData['data_time_period'] == 'Historical')
    scenarioPanelSimData <- dplyr::filter(scenarioPanelData, scenarioPanelData['data_time_period'] == 'Simulated')
    
    ################# Visualizations for compare scenario summary plots #########################
    
    ###Plotting charts for historical period - Sales Plot
    
    scenarioSalesggHistPlot <- sales_fn_bar_graph(dataset =  scenarioPanelHistData, chartTitle = "Sales Comparison for Historical Period" , xvar = "Scenario", xlab = "", yvar = "Total_Sales", ylab = "Total Sales", lvar = "#1F77B4", panelLabel = panelSelected) +
      scale_y_continuous(labels=scales::dollar_format(prefix="$")) +
      geom_text(aes(label = dollar(Total_Sales)), position=position_dodge(width=0.9), vjust=-0.25)
    
    output$scenarioSalesHistPlot <- shiny::renderPlot({scenarioSalesggHistPlot})
    
    ###Plotting charts for simulation period - Sales Plot
    
    scenarioSalesggSimPlot <- sales_fn_bar_graph(dataset =  scenarioPanelSimData, chartTitle = "Sales Comparison for Simulation/Optimization Period" , xvar = "Scenario", xlab = "", yvar = "Total_Sales", ylab = "Total Sales", lvar = "#1F77B4", panelLabel = panelSelected) +
      scale_y_continuous(labels=scales::dollar_format(prefix="$")) +
      geom_text(aes(label = dollar(Total_Sales)), position=position_dodge(width=0.9), vjust=-0.25)
    
    output$scenarioSalesSimPlot <- shiny::renderPlot({scenarioSalesggSimPlot})
    
    ###Plotting charts for simulation period - Spends Plot
    
    scenarioSpendggSimPlot <- fn_bar_graph(dataset =  scenarioPanelSimData, chartTitle = "Spend Comparison for Simulation/Optimization Period" , xvar = "Spend_Channel", xlab = "", yvar = "Total_Spend", ylab = "Total Spend", lvar = "Scenario", panelLabel = panelSelected) + 
      scale_y_continuous(labels=scales::dollar_format(prefix="$")) + 
      geom_text(aes(label = dollar(Total_Spend)), position=position_dodge(width=0.9), vjust=-0.25)
    
    output$scenarioSpendSimPlot <- shiny::renderPlot({scenarioSpendggSimPlot})
    
    ###Plotting charts for historical period - Spends Plot
    
    scenarioSpendggHistPlot <- fn_bar_graph(dataset =  scenarioPanelHistData, chartTitle = "Spend Comparison for Historical Period" , xvar = "Spend_Channel", xlab = "", yvar = "Total_Spend", ylab = "Total Spend", lvar = "Scenario", panelLabel = panelSelected) + 
      scale_y_continuous(labels=scales::dollar_format(prefix="$")) + 
      geom_text(aes(label = dollar(Total_Spend)), position=position_dodge(width=0.9), vjust=-0.25)
    
    output$scenarioSpendHistPlot <- shiny::renderPlot({scenarioSpendggHistPlot})
    
    ###Plotting charts for historical period - Contributions Plot
    
    scenarioContributionggHistPlot <- fn_bar_graph(dataset =  scenarioPanelHistData, chartTitle = "Contribution Comparison for Historical Period" , xvar = "Spend_Channel", xlab = "", yvar = "Total_Contribution", ylab = "Total Contribution", lvar = "Scenario", panelLabel = panelSelected) + 
      scale_y_continuous(labels=scales::dollar_format(prefix="$"))  + 
      geom_text(aes(label = dollar(Total_Contribution)), position=position_dodge(width=0.9), vjust=-0.25)
    
    output$scenarioContributionHistPlot <- shiny::renderPlot({scenarioContributionggHistPlot})
    
    ###Plotting charts for simulation period - Contributions Plot
    
    scenarioContributionggSimPlot <- fn_bar_graph(dataset =  scenarioPanelSimData, chartTitle = "Contribution Comparison for Simulation/Optimization Period" , xvar = "Spend_Channel", xlab = "", yvar = "Total_Contribution", ylab = "Total Contribution", lvar = "Scenario", panelLabel = panelSelected)  +
      scale_y_continuous(labels=scales::dollar_format(prefix="$")) +
      geom_text(aes(label = dollar(Total_Contribution)), position=position_dodge(width=0.9), vjust=-0.25)
    
    output$scenarioContributionSimPlot <- shiny::renderPlot({scenarioContributionggSimPlot})
    
    ###Plotting charts for historical period - ROI Plot
    
    scenarioROIggHistPlot <- fn_bar_graph(dataset =  scenarioPanelHistData, chartTitle = "ROI Comparison for Historical Period" , xvar = "Spend_Channel", xlab = "", yvar = "Total_ROI", ylab = "Total ROI", lvar = "Scenario", panelLabel = panelSelected) + 
      geom_text(aes(label = format(round(Total_ROI, 2), big.mark = ",")), position=position_dodge(width=0.9), vjust=-0.25)
    
    output$scenarioROIHistPlot <- shiny::renderPlot({scenarioROIggHistPlot})
    
    ###Plotting charts for simulation period - ROI Plot
    
    scenarioROIggSimPlot <- fn_bar_graph(dataset =  scenarioPanelSimData, chartTitle = "ROI Comparison for Simulation/Optimization Period" , xvar = "Spend_Channel", xlab = "", yvar = "Total_ROI", ylab = "Total ROI", lvar = "Scenario", panelLabel = panelSelected) + 
      geom_text(aes(label = format(round(Total_ROI, 2), big.mark = ",")), position=position_dodge(width=0.9), vjust=-0.25)
    
    output$scenarioROISimPlot <- shiny::renderPlot({scenarioROIggSimPlot})
    
    
    ###Arranging all compare scenario charts for downloading as a single image
    grobCompareScenarioPlots <- arrangeGrob(scenarioSalesggHistPlot, scenarioSalesggSimPlot, scenarioSpendggHistPlot, scenarioSpendggSimPlot, scenarioContributionggHistPlot, scenarioContributionggSimPlot, scenarioROIggHistPlot, scenarioROIggSimPlot, ncol=2)
    
    ############################ Download handler for plots ######################################
    output$downloadCompareScenarioPlots <- shiny::downloadHandler(
      filename = function(){'Compare_Scenario_Plots.png'},
      content = function(file) {
        ggsave(file, plot=grobCompareScenarioPlots, device='png', width = 45, height = 25, units = 'cm')
      },
      shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
    )
    
    ############################ Download handler for data ######################################
    output$downloadCompareScenarioData <- shiny::downloadHandler(
      filename = function(){'Compare_Scenario_Data.csv'},
      content = function(file) {
        write.csv(scenarioPanelSimData, file, row.names = FALSE)
      }
    )
    
    ##### Plot for Report
    reactData$scenarioSalesHistPlot <- scenarioSalesggHistPlot
    reactData$scenarioSalesSimPlot <- scenarioSalesggSimPlot
    reactData$scenarioSpendHistPlot <- scenarioSpendggHistPlot
    reactData$scenarioSpendSimPlot <- scenarioSpendggSimPlot
    reactData$scenarioContributionHistPlot <- scenarioContributionggHistPlot
    reactData$scenarioContributionSimPlot <- scenarioContributionggSimPlot
    reactData$scenarioROIHistPlot <- scenarioROIggHistPlot
    reactData$scenarioROISimPlot <- scenarioROIggSimPlot
    

    output$scenario2_spend <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_Spend",
                         data_flag = adstock$secondScenario,
                         currency_symbol = "$",
                         magnitude = "M"))
    })
    
    output$scenario1_spend <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_Spend",
                         data_flag = adstock$firstScenario,
                         currency_symbol = "$",
                         magnitude = "M"))
      # scenario1_spend <- scenarioSummaryData$dataset[(which(scenarioSummaryData$dataset$KPI=="Total_Spend")),adstock$firstScenario]
      # scenario1_spend_char <- shiny::tags$p(shiny::HTML(paste0("$"," ",round(scenario1_spend/1e+06,1),"M")))
      # return(scenario1_spend_char)
    })
    
    output$scenario2_spend <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_Spend",
                         data_flag = adstock$secondScenario,
                         currency_symbol = "$",
                         magnitude = "M"))
    })
    
    
    output$scenario1_sales <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_Sales",
                         data_flag = adstock$firstScenario,
                         currency_symbol = "$",
                         magnitude = "M"))
    })
    
    output$scenario2_sales <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_Sales",
                         data_flag = adstock$secondScenario,
                         currency_symbol = "$",
                         magnitude = "M"))
    })
    
    output$scenario1_contrib <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_Contribution",
                         data_flag = adstock$firstScenario,
                         currency_symbol = "$",
                         magnitude = "M"))
    })
    
    output$scenario2_contrib <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_Contribution",
                         data_flag = adstock$secondScenario,
                         currency_symbol = "$",
                         magnitude = "M"))
    })
    
    output$scenario1_roi <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_ROI",
                         data_flag = adstock$firstScenario,
                         currency_symbol = "",
                         magnitude = ""))
    })
    
    output$scenario2_roi <- shiny::renderUI({
      return(getCardData(dataset = scenarioSummaryData,
                         kpi_col = "KPI",
                         kpi = "Total_ROI",
                         data_flag = adstock$secondScenario,
                         currency_symbol = "",
                         magnitude = ""))
    })
    

    
    
    ################ Creating dataset for compare scenario time series charts ####################
    
    scenario1AllPanelData <- getCompareScenarioSalesTimeSeriesData(scenarioAllHistData = compareScenario$allHistPanelScenario1Wide,
                                                                   scenarioSimData = compareScenario$runPanelScenario1Wide,
                                                                   scenarioName = adstock$firstScenario,
                                                                   dateVar = reactData$DateVar,
                                                                   spendVar = reactData$spendVar,
                                                                   targetVar = reactData$targetVar)
    
    scenario2AllPanelData <- getCompareScenarioSalesTimeSeriesData(scenarioAllHistData = compareScenario$allHistPanelScenario2Wide,
                                                                   scenarioSimData = compareScenario$runPanelScenario2Wide,
                                                                   scenarioName = adstock$secondScenario,
                                                                   dateVar = reactData$DateVar,
                                                                   spendVar = reactData$spendVar,
                                                                   targetVar = reactData$targetVar)
    
    #### Combine total contribution data for both scenarios
    reactData$compareScenarioContributionAllData <- rbind(scenario1ContribList[[1]], scenario2ContribList[[1]])
    
    #### Combine total data for both scenarios
    compareScenarioAllData <- rbind(scenario1AllPanelData, scenario2AllPanelData)
    
    #### Drop data_time_period column ####
    drops <- c('data_time_period')
    compareScenarioAllData <- compareScenarioAllData[ , !(names(compareScenarioAllData) %in% drops)]
    
    #### Get all KPIs for compare scenario time series graphs
    reactData$compareScenarioAllData <- merge(compareScenarioAllData, reactData$compareScenarioContributionAllData, by=c('Spend_Channel', 'Time_Period', 'Scenario'))
    
    ########################### Time Series data creation ends here #############################
    
  },error=function(e) showNotification(paste0('View Compare Scenario Summary Results :',e[1])))
  })
  
  
  
  
  ### Click action for drill down table
  
  output$box_01 <- renderValueBox({
    entry_01<-20
    # box1<-valueBox(value=entry_01
    #                ,icon = icon("users",lib="font-awesome")
    #                ,width=NULL
    #                ,color = "blue"
    #                ,href="#"
    #                ,subtitle=HTML("<b>Test click on valueBox</b>")
    # )
    box1<- valueBox(subtitle = "Channel With Highest Spends"
                    , value= uiOutput("Scenario1_Spend_Channel")
                    , href = "#"
                    , icon = icon("credit-card"))
    
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_box_01"
    return(box1)
  })
  
  output$box_02 <- renderValueBox({
    entry_02<-20
    # box1<-valueBox(value=entry_01
    #                ,icon = icon("users",lib="font-awesome")
    #                ,width=NULL
    #                ,color = "blue"
    #                ,href="#"
    #                ,subtitle=HTML("<b>Test click on valueBox</b>")
    # )
    box2<- valueBox(subtitle = "Channel With Highest Sales"
                    , value= uiOutput("Scenario1_Sales_Channel")
                    , href = "#"
                    , icon = icon("hand-holding-usd")
                    , color = "yellow")
    
    box2$children[[1]]$attribs$class<-"action-button"
    box2$children[[1]]$attribs$id<-"button_box_02"
    return(box2)
  })
  
  output$box_03 <- renderValueBox({
    entry_03<-20
    # box1<-valueBox(value=entry_01
    #                ,icon = icon("users",lib="font-awesome")
    #                ,width=NULL
    #                ,color = "blue"
    #                ,href="#"
    #                ,subtitle=HTML("<b>Test click on valueBox</b>")
    # )
    box3<- valueBox(subtitle = "Channel With Highest ROI"
                    , value= uiOutput("Scenario1_ROI_Channel")
                    , href = "#"
                    , icon = icon("coins")
                    , color = "red")
    
    box3$children[[1]]$attribs$class<-"action-button"
    box3$children[[1]]$attribs$id<-"button_box_03"
    return(box3)
  })
  
  output$box_04 <- renderValueBox({
    entry_04<-20
    # box1<-valueBox(value=entry_01
    #                ,icon = icon("users",lib="font-awesome")
    #                ,width=NULL
    #                ,color = "blue"
    #                ,href="#"
    #                ,subtitle=HTML("<b>Test click on valueBox</b>")
    # )
    box4<- valueBox(subtitle = "Channel With Highest Contribution"
                    , value= uiOutput("Scenario1_Contribution_Channel")
                    , href = "#"
                    , icon = icon("donate")
                    ,color = "yellow")
    
    box4$children[[1]]$attribs$class<-"action-button"
    box4$children[[1]]$attribs$id<-"button_box_04"
    return(box4)
  })
  
  shiny::observeEvent(input$button_box_01, {
    #browser()
    

    
    if(is.null(reactData$viewSelected)){
      scene<-unique(combinedData$df_final1$Scenario)[1]
    }
    else{
      scene<-reactData$viewSelected
    }
    
    drill_data <- combinedData$df_final1[which(combinedData$df_final1$Scenario==scene),]
    
    df_spend_drill <- drill_data %>% select(c(Spend_Channel,Month,Spends)) %>% group_by(Spend_Channel,Month) %>% summarise_all(funs(sum))
    
    
    
    
    filtered_df<-combinedData$final_data[which(combinedData$final_data$Scenario==scene),]
    x_df<-filtered_df[which(filtered_df$data_time_period=="Simulated"),]
    
    channel_highest_spends<- x_df[which.max(x_df$Total_Spend),]
    
    
    output$drillTable <- renderReactable({
      #browser()
      #x<-compareScenario$runScenario1
      #z<- unique(compareScenarioTable$dataset$Scenario_Name)
      #df<-aggregate(list(x$Spends), by = list(x$Spend_Channel,x$Month), sum)
      
      
      
      colnames(df_spend_drill)<-c("Channel","Month","Spends")
      df_spend_drill$Swimmers<- NA
      
      df_filtered <- df_spend_drill[df_spend_drill$Channel == as.character(channel_highest_spends$Spend_Channel),]
      
      df_filtered$Spends  <- round(df_filtered$Spends,1)
      
      my_pal <- function(x) rgb(colorRamp(c("white", "red"))(x), maxColorValue = 255)
      
      
      reactable(
        #Select only a few variables for display in the drill down
        df_filtered %>% select(Channel, Month, Spends, Swimmers),
        
        
        
        #Basic table features - self explanatory
        defaultPageSize = 4,
        striped = TRUE,
        highlight = TRUE,
        searchable = TRUE,
        
        #Adjusting column widths
        columns = list(
          Channel     = colDef(width=100,align = "center"),
          Month    = colDef(width=80,align = "center"),
          Spends   = colDef(width=105,align = "center",
                            format = colFormat(percent = FALSE, digits = 0),
                            style = function(value) {
                              #normalized based on present values (though this is a percent already)
                              normalized <- (value - min(df_filtered$Spends)) * 0.75 / (max(df_filtered$Spends) - min(df_filtered$Spends))
                              #adapted from documentation, my_pal() is in assets.R
                              color <- my_pal(normalized)
                              list(background = color)
                            })
          ,
          
          
          #Swimmer Plot - custom embedded HTML widget using highcharter
          #The swimmer plot uses drill_data() to compute the inline chart
          
          Swimmers = colDef(name = "Swimmer Plot",
                            align = "center",
                            cell = function(value,index) {
                              
                              roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
                                if(length(x) != 1) stop("'x' must be of length 1")
                                10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
                              }
                              
                              #Swimmer plot logic starts here
                              df_filtered[index,] %>%
                                hchart("bar",
                                       hcaes(x = Month , y = Spends),
                                       #name is the argument which determines what appears in the tool tip for this series
                                       name = "Total Spends") %>%
                                
                                
                                
                                
                                
                                #Axis
                                #Keep yAxis labels in for now - you could remove them since the plot is interactable but it helps in comparisons across patients
                                hc_yAxis(title = list(text = " "), min=0, max= 1.5 * round(max(df_filtered$Spends),1), tickInterval= roundUpNice((round(max(df_filtered$Spends),1) / 5)), labels = list(enabled=TRUE)) %>%
                                hc_xAxis(title = list(text = " "), labels = list(enabled=FALSE)) %>%
                                
                                #Misc plot options
                                hc_legend(enabled = FALSE) %>%
                                hc_size(width = 700, height = 80) %>%
                                
                                #Custom tool tip formatting - basic JS string
                                hc_tooltip(formatter = JS("function(){return (this.series.name + `:  ` + this.y)}"))
                              
                              
                            } #end cell function
          ) #end Swimmer
        ) #end columns list
      )
      
      
    })
    
  })
  
  shiny::observeEvent(input$button_box_02, {
    #browser()
    
    
    
    if(is.null(reactData$viewSelected)){
      scene<-unique(combinedData$df_final1$Scenario)[1]
    }
    else{
      scene<-reactData$viewSelected
    }
    
    
    drill_data <- combinedData$df_final1[which(combinedData$df_final1$Scenario==scene),]
    
    df_sales_drill <- drill_data %>% select(c(Spend_Channel,Month,Sales)) %>% group_by(Spend_Channel,Month) %>% summarise_all(funs(sum))
    
    
    filtered_df<-combinedData$final_data[which(combinedData$final_data$Scenario==scene),]
    x_df<-filtered_df[which(filtered_df$data_time_period=="Simulated"),]
    channel_highest_sales<- x_df[which.max(x_df$Total_Sales),]
    
    
    output$drillTable <- renderReactable({
      #browser()
      #x<-compareScenario$runScenario1
      #z<- unique(compareScenarioTable$dataset$Scenario_Name)
      #df<-aggregate(list(x$Spends), by = list(x$Spend_Channel,x$Month), sum)
      

      
      colnames(df_sales_drill)<-c("Channel","Month","Sales")
      df_sales_drill$Swimmers<- NA
      
      df_filtered <- df_sales_drill[df_sales_drill$Channel == as.character(channel_highest_sales$Spend_Channel),]
      
      df_filtered$Sales  <- round(df_filtered$Sales,1)
      
      my_pal <- function(x) rgb(colorRamp(c("white", "red"))(x), maxColorValue = 255)
      
      reactable(
        #Select only a few variables for display in the drill down
        df_filtered %>% select(Channel, Month, Sales, Swimmers),
        
        
        
        #Basic table features - self explanatory
        defaultPageSize = 4,
        striped = TRUE,
        highlight = TRUE,
        searchable = TRUE,
        
        #Adjusting column widths
        columns = list(
          Channel     = colDef(width=100,align = "center"),
          Month    = colDef(width=80,align = "center"),
          Sales   = colDef(width=105,align = "center",
                           format = colFormat(percent = FALSE, digits = 0),
                           style = function(value) {
                             #normalized based on present values (though this is a percent already)
                             normalized <- (value - min(df_filtered$Sales)) * 0.75 / (max(df_filtered$Sales) - min(df_filtered$Sales))
                             #adapted from documentation, my_pal() is in assets.R
                             color <- my_pal(normalized)
                             list(background = color)
                           }),
          
          
          #Swimmer Plot - custom embedded HTML widget using highcharter
          #The swimmer plot uses drill_data() to compute the inline chart
          
          Swimmers = colDef(name = "Swimmer Plot",
                            align = "center",
                            cell = function(value,index) {
                              
                              roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
                                if(length(x) != 1) stop("'x' must be of length 1")
                                10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
                              }
                              
                              #Swimmer plot logic starts here
                              df_filtered[index,] %>%
                                hchart("bar",
                                       hcaes(x = Month , y = Sales),
                                       #name is the argument which determines what appears in the tool tip for this series
                                       name = "Total Sales") %>%
                                
                                
                                
                                
                                
                                #Axis
                                #Keep yAxis labels in for now - you could remove them since the plot is interactable but it helps in comparisons across patients
                                hc_yAxis(title = list(text = " "), min=0, max= 1.5 * round(max(df_filtered$Sales),1), tickInterval= roundUpNice((round(max(df_filtered$Sales),1) / 5)), labels = list(enabled=TRUE)) %>%
                                hc_xAxis(title = list(text = " "), labels = list(enabled=FALSE)) %>%
                                
                                #Misc plot options
                                hc_legend(enabled = FALSE) %>%
                                hc_size(width = 700, height = 80) %>%
                                
                                #Custom tool tip formatting - basic JS string
                                hc_tooltip(formatter = JS("function(){return (this.series.name + `:  ` + this.y)}"))
                              
                              
                            } #end cell function
          ) #end Swimmer
        ) #end columns list
      )
      
      
    })
    
  })
  
  shiny::observeEvent(input$button_box_03, {
    #browser()
    
    
    if(is.null(reactData$viewSelected)){
      scene<-unique(combinedData$df_final1$Scenario)[1]
    }
    else{
      scene<-reactData$viewSelected
    }
    
    drill_data <- combinedData$df_final1[which(combinedData$df_final1$Scenario==scene),]
    
    df_sales_drill <- drill_data %>% select(c(Spend_Channel,Month,Sales)) %>% group_by(Spend_Channel,Month) %>% summarise_all(funs(sum))
    
    
    filtered_df<-combinedData$final_data[which(combinedData$final_data$Scenario==scene),]
    x_df<-filtered_df[which(filtered_df$data_time_period=="Simulated"),]
    channel_highest_sales<- x_df[which.max(x_df$Total_Sales),]
    
    
    output$drillTable <- renderReactable({
      #browser()
      #x<-compareScenario$runScenario1
      #z<- unique(compareScenarioTable$dataset$Scenario_Name)
      #df<-aggregate(list(x$Spends), by = list(x$Spend_Channel,x$Month), sum)
      
      x_df_vars <- names(x_df) %in% c("data_time_period", "Scenario")
      x_df_new <- x_df[!x_df_vars]
      
      #colnames(x_df)<-c("Channel","Month","Sales")
      x_df_new$Swimmers<- NA
      
      #df_filtered <- df_sales_drill[df_sales_drill$Channel == as.character(channel_highest_sales$Spend_Channel),]
      
      x_df_new$Total_Contribution  <- round(x_df_new$Total_Contribution,1)
      x_df_new$Total_ROI  <- round(x_df_new$Total_ROI ,1)
      
      my_pal <- function(x) rgb(colorRamp(c("white", "red"))(x), maxColorValue = 255)
      
      reactable(
        #Select only a few variables for display in the drill down
        x_df_new %>% select(Spend_Channel, Total_Contribution, Total_ROI, Swimmers),
        
        
        
        #Basic table features - self explanatory
        defaultPageSize = 4,
        striped = TRUE,
        highlight = TRUE,
        searchable = TRUE,
        
        #Adjusting column widths
        columns = list(
          Spend_Channel     = colDef(width=130,align = "center"),
          Total_Contribution   = colDef(width=140,align = "center"),
          Total_ROI   = colDef(width=105,align = "center",
                           format = colFormat(percent = FALSE, digits = 0),
                           style = function(value) {
                             #normalized based on present values (though this is a percent already)
                             normalized <- (value - min(x_df_new$Total_ROI)) * 0.75 / (max(x_df_new$Total_ROI) - min(x_df_new$Total_ROI))
                             #adapted from documentation, my_pal() is in assets.R
                             color <- my_pal(normalized)
                             list(background = color)
                           }),
          
          
          #Swimmer Plot - custom embedded HTML widget using highcharter
          #The swimmer plot uses drill_data() to compute the inline chart
          
          Swimmers = colDef(name = "Swimmer Plot",
                            align = "center",
                            cell = function(value,index) {
                              
                              roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
                                if(length(x) != 1) stop("'x' must be of length 1")
                                10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
                              }
                              
                              #Swimmer plot logic starts here
                              x_df_new[index,] %>%
                                hchart("bar",
                                       hcaes(x = Spend_Channel , y = Total_ROI),
                                       #name is the argument which determines what appears in the tool tip for this series
                                       name = "ROI") %>%
                                
                                
                                
                                
                                
                                #Axis
                                #Keep yAxis labels in for now - you could remove them since the plot is interactable but it helps in comparisons across patients
                                hc_yAxis(title = list(text = " "), min=0, max= 1.5 * round(max(x_df_new$Total_ROI),1), tickInterval= roundUpNice((round(max(x_df_new$Total_ROI),1) / 5)), labels = list(enabled=TRUE)) %>%
                                hc_xAxis(title = list(text = " "), labels = list(enabled=FALSE)) %>%
                                
                                #Misc plot options
                                hc_legend(enabled = FALSE) %>%
                                hc_size(width = 700, height = 80) %>%
                                
                                #Custom tool tip formatting - basic JS string
                                hc_tooltip(formatter = JS("function(){return (this.series.name + `:  ` + this.y)}"))
                              
                              
                            } #end cell function
          ) #end Swimmer
        ) #end columns list
      )
      
      
    })
    
  })
  
  
  shiny::observeEvent(input$button_box_04, {
    #browser()
    
    
    if(is.null(reactData$viewSelected)){
      scene<-unique(combinedData$df_final1$Scenario)[1]
    }
    else{
      scene<-reactData$viewSelected
    }
    
    drill_data <- combinedData$df_final1[which(combinedData$df_final1$Scenario==scene),]
    
    df_sales_drill <- drill_data %>% select(c(Spend_Channel,Month,Sales)) %>% group_by(Spend_Channel,Month) %>% summarise_all(funs(sum))
    
    
    filtered_df<-combinedData$final_data[which(combinedData$final_data$Scenario==scene),]
    x_df<-filtered_df[which(filtered_df$data_time_period=="Simulated"),]
    channel_highest_sales<- x_df[which.max(x_df$Total_Sales),]
    
    
    output$drillTable <- renderReactable({
      #browser()
      #x<-compareScenario$runScenario1
      #z<- unique(compareScenarioTable$dataset$Scenario_Name)
      #df<-aggregate(list(x$Spends), by = list(x$Spend_Channel,x$Month), sum)
      
      x_df_vars <- names(x_df) %in% c("data_time_period", "Scenario")
      x_df_new <- x_df[!x_df_vars]
      
      #colnames(x_df)<-c("Channel","Month","Sales")
      x_df_new$Swimmers<- NA
      
      #df_filtered <- df_sales_drill[df_sales_drill$Channel == as.character(channel_highest_sales$Spend_Channel),]
      
      x_df_new$Total_Contribution  <- round(x_df_new$Total_Contribution,1)
      x_df_new$Total_ROI  <- round(x_df_new$Total_ROI ,1)
      
      my_pal <- function(x) rgb(colorRamp(c("white", "red"))(x), maxColorValue = 255)
      
      reactable(
        #Select only a few variables for display in the drill down
        x_df_new %>% select(Spend_Channel, Total_Contribution, Total_ROI, Swimmers),
        
        
        
        #Basic table features - self explanatory
        defaultPageSize = 4,
        striped = TRUE,
        highlight = TRUE,
        searchable = TRUE,
        
        #Adjusting column widths
        columns = list(
          Spend_Channel     = colDef(width=130,align = "center"),
          Total_ROI   = colDef(width=140,align = "center"),
          Total_Contribution   = colDef(width=105,align = "center",
                               format = colFormat(percent = FALSE, digits = 0),
                               style = function(value) {
                                 #normalized based on present values (though this is a percent already)
                                 normalized <- (value - min(x_df_new$Total_Contribution)) * 0.75 / (max(x_df_new$Total_Contribution) - min(x_df_new$Total_Contribution))
                                 #adapted from documentation, my_pal() is in assets.R
                                 color <- my_pal(normalized)
                                 list(background = color)
                               }),
          
          
          #Swimmer Plot - custom embedded HTML widget using highcharter
          #The swimmer plot uses drill_data() to compute the inline chart
          
          Swimmers = colDef(name = "Swimmer Plot",
                            align = "center",
                            cell = function(value,index) {
                              
                              roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
                                if(length(x) != 1) stop("'x' must be of length 1")
                                10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
                              }
                              
                              #Swimmer plot logic starts here
                              x_df_new[index,] %>%
                                hchart("bar",
                                       hcaes(x = Spend_Channel , y = Total_Contribution),
                                       #name is the argument which determines what appears in the tool tip for this series
                                       name = "Contribution") %>%
                                
                                
                                
                                
                                
                                #Axis
                                #Keep yAxis labels in for now - you could remove them since the plot is interactable but it helps in comparisons across patients
                                hc_yAxis(title = list(text = " "), min=0, max= 1.5 * round(max(x_df_new$Total_Contribution),1), tickInterval= roundUpNice((round(max(x_df_new$Total_Contribution),1) / 5)), labels = list(enabled=TRUE)) %>%
                                hc_xAxis(title = list(text = " "), labels = list(enabled=FALSE)) %>%
                                
                                #Misc plot options
                                hc_legend(enabled = FALSE) %>%
                                hc_size(width = 700, height = 80) %>%
                                
                                #Custom tool tip formatting - basic JS string
                                hc_tooltip(formatter = JS("function(){return (this.series.name + `:  ` + this.y)}"))
                              
                              
                            } #end cell function
          ) #end Swimmer
        ) #end columns list
      )
      
      
    })
    
  })
  
  
  shiny::observeEvent(input$recommendationOptButton, {
    tryCatch({
      # Error handling
      if (is.null(reactData$dataset) | is.null(mmxModel$model) | is.null(mmxModel$train_data)){
        shiny::showNotification("Please complete the Compare Scenario Comparison", type = c("error")); return()
      }else{
        # enable tab when clicking the button
        js$enableMenu("tabRecommend")
        # enable tab when clicking the button
        # js$enableTab("tabCompareScenario")
        # Change status to complete by highlighting
        #js$greenMenu("tabCompareScenario")
        
        # switch to tab
        updateTabItems(session, "sideTabs", "tabRecommend")
      }
    },error= function(e) showNotification(paste0('Simulation Results Next Button :',e[1])))
  })
  
  shiny::observeEvent(input$recommendationOptButton2, {
    tryCatch({
      # Error handling
      if (is.null(reactData$dataset) | is.null(mmxModel$model) | is.null(mmxModel$train_data)){
        shiny::showNotification("Please complete the Compare Scenario Comparison", type = c("error")); return()
      }else{
        # enable tab when clicking the button
        js$enableMenu("tabRecommend")
        # enable tab when clicking the button
        # js$enableTab("tabCompareScenario")
        # Change status to complete by highlighting
        #js$greenMenu("tabCompareScenario")
        
        # switch to tab
        updateTabItems(session, "sideTabs", "tabRecommend")
      }
    },error= function(e) showNotification(paste0('Simulation Results Next Button :',e[1])))
  })
  
  shiny::observeEvent(input$recommendationOptButton1, {
    tryCatch({
      # Error handling
      if (is.null(reactData$dataset) | is.null(mmxModel$model) | is.null(mmxModel$train_data)){
        shiny::showNotification("Please complete the Compare Scenario Comparison", type = c("error")); return()
      }else{
        # enable tab when clicking the button
        js$enableMenu("tabRecommend")
        # enable tab when clicking the button
        # js$enableTab("tabCompareScenario")
        # Change status to complete by highlighting
        #js$greenMenu("tabCompareScenario")
        
        # switch to tab
        updateTabItems(session, "sideTabs", "tabRecommend")
      }
    },error= function(e) showNotification(paste0('Simulation Results Next Button :',e[1])))
  })
  
 
  output$Recommender_Text<- shiny :: renderText({
    x<- getCardData(dataset = scenarioSummaryData,
                    kpi_col = "KPI",
                    kpi = "Total_ROI",
                    data_flag = adstock$firstScenario,
                    currency_symbol = "",
                    magnitude = "")
    y<- getCardData(dataset = scenarioSummaryData,
                    kpi_col = "KPI",
                    kpi = "Total_ROI",
                    data_flag = adstock$secondScenario,
                    currency_symbol = "",
                    magnitude = "")
    x<-as.numeric(x)
    y<-as.numeric(y)
    z<- unique(compareScenarioTable$dataset$Scenario_Name)
    a<-as.character(z[1])
    b<-as.character(z[2])
    if(x > y){
      return(paste(a," gives a ROI of ","(", x, ")", "which is better than the ROI of ", b, "(", y, ")"))
    }
    else if(x == y){
      return(paste(b, " & ", a, " both give the same ROI", "(", x, ")"))
    }
    else{
      return(paste(b," gives a ROI of ","(", y, ")", "which is better than the ROI of ", a, "(", x, ")"))
    }
  })
  
  output$Recommender_Text1<- shiny :: renderText({
    ss_names<-scenarioTable$dataset$Scenario_Name
    os_names<-reactData$opti_scenarioNameTable$Scenario
    df<-as.data.frame(NULL)
    df1<-as.data.frame(NULL)
    con1<-as.data.frame(NULL)
    con2<-as.data.frame(NULL)
    pan1<-as.data.frame(NULL)
    pan2<-as.data.frame(NULL)
    if(!is.null(scenarioTable$dataset$Scenario_Name)){
      ss_names<- as.character(scenarioTable$dataset$Scenario_Name)
      for(i in 1:length(ss_names)){
        ## Preparing data from scratch to make a master dataframe with all the values
        histScenario2 <- scenarioList$files[[ss_names[i]]]$historical_dataset$long_dataset
        histScenario2Wide <- scenarioList$files[[ss_names[i]]]$historical_dataset$dataset
        allHistScenario2Wide <- scenarioList$files[[ss_names[i]]]$all_data$dataset
        runScenario2Wide <- scenarioList$files[[ss_names[i]]]$scheduling_dataset$runSimulationDataset
        runScenario2 <- melt(runScenario2Wide, measure.vars=reactData$spendVar)
        setnames(runScenario2, old=c('variable', 'value'), new=c('Spend_Channel', 'Spends'))
        drops <- c('Simulated_Date.1')
        runScenario2Wide <- runScenario2Wide[ , !(names(runScenario2Wide) %in% drops)]
        ## Because we are considering for overall values i.e Panel=TOTAL
        allHistPanelScenario2Wide <- allHistScenario2Wide
        runPanelScenario2Wide <- runScenario2Wide
        histPanelScenario2Wide <- histScenario2Wide
        
        simContribList <- getCompareScenarioContribData(scenarioAllHistData = allHistPanelScenario2Wide,
                                                        scenarioSimData = runPanelScenario2Wide,
                                                        scenarioName = ss_names[i],
                                                        trainData = mmxModel$train_data,
                                                        spendVar = reactData$spendVar,
                                                        adstockSpendVar = as.character(adstock$dataset$Spend_Channel),
                                                        adstockDecayRate = adstock$dataset$Adstock_Decay,
                                                        dateVar = reactData$DateVar,
                                                        panelVar = reactData$panelVar,
                                                        modelRun = mmxModel$model,
                                                        simStartDate = reactData$simStartDateVar,
                                                        simEndDate = reactData$simEndDateVar,
                                                        histStartDate = reactData$histStartDateVar,
                                                        histEndDate = reactData$histEndDateVar)
        
        simPanelData <- getCompareScenarioSalesSummaryData(scenarioHistData = histPanelScenario2Wide,
                                                           scenarioSimData = runPanelScenario2Wide,
                                                           scenarioName = ss_names[i],
                                                           dateVar = reactData$DateVar,
                                                           spendVar = reactData$spendVar,
                                                           targetVar = reactData$targetVar)
        
        
        
        y<-scenarioList$files[[ss_names[i]]]$scheduling_dataset$runSimulationDataset
        y$Scenario<-ss_names[i]
        df<-rbind(df,y)
        pan1<-rbind(pan1,simPanelData)
        con1<-rbind(con1,simContribList[[2]])
        
      }
    }
    if(!is.null(reactData$opti_scenarioNameTable$Scenario)){
      os_names<-as.character(reactData$opti_scenarioNameTable$Scenario)
      for(i in 1:length(os_names)){
        ## Preparing data from scratch to make a master dataframe with all the values
        
        histScenario2 <- optimisationScenarioList$files[[os_names[i]]]$optimising_dataset$scenario_histData_long
        histScenario2Wide <- optimisationScenarioList$files[[os_names[i]]]$optimising_dataset$scenario_histData_wide
        allHistScenario2Wide <- optimisationScenarioList$files[[os_names[i]]]$all_dataset$dataset
        runScenario2Wide <- optimisationScenarioList$files[[os_names[i]]]$optimising_dataset$optimised_dataset
        runScenario2 <- melt(runScenario2Wide, measure.vars=reactData$spendVar)
        setnames(runScenario2, old=c('variable', 'value'), new=c('Spend_Channel', 'Spends'))
        
        ## Because we are considering for overall values i.e Panel=TOTAL
        allHistPanelScenario2Wide <- allHistScenario2Wide
        runPanelScenario2Wide <- runScenario2Wide
        histPanelScenario2Wide <- histScenario2Wide
        optContribList <- getCompareScenarioContribData(scenarioAllHistData = allHistPanelScenario2Wide,
                                                        scenarioSimData = runPanelScenario2Wide,
                                                        scenarioName = os_names[i],
                                                        trainData = mmxModel$train_data,
                                                        spendVar = reactData$spendVar,
                                                        adstockSpendVar = as.character(adstock$dataset$Spend_Channel),
                                                        adstockDecayRate = adstock$dataset$Adstock_Decay,
                                                        dateVar = reactData$DateVar,
                                                        panelVar = reactData$panelVar,
                                                        modelRun = mmxModel$model,
                                                        simStartDate = reactData$simStartDateVar,
                                                        simEndDate = reactData$simEndDateVar,
                                                        histStartDate = reactData$histStartDateVar,
                                                        histEndDate = reactData$histEndDateVar)
        
        optPanelData <- getCompareScenarioSalesSummaryData(scenarioHistData = histPanelScenario2Wide,
                                                           scenarioSimData = runPanelScenario2Wide,
                                                           scenarioName = os_names[i],
                                                           dateVar = reactData$DateVar,
                                                           spendVar = reactData$spendVar,
                                                           targetVar = reactData$targetVar)
        
        y<-optimisationScenarioList$files[[os_names[i]]]$optimising_dataset$optimised_dataset
        y$Scenario<-os_names[i]
        df1<-rbind(df1,y)
        pan2<-rbind(pan2,optPanelData)
        con2<-rbind(con2,optContribList[[2]])
      }
    }
    
    
    Contrib_Combined <- rbind(con1,con2)
    Panel_Combined <- rbind(pan1,pan2)
    
    #Panel Data Preparation
    df_final<-rbind(df,df1)
    
    combinedData$df_final1<-melt(df_final, measure.vars=reactData$spendVar)
    
    df_final1<-combinedData$df_final1
    setnames(df_final1, old=c('variable', 'value'), new=c('Spend_Channel', 'Spends'))
    
    ### Merging summarized contributions data with sumarrized sales/spends data
    Panel_Contib<- merge(Contrib_Combined, Panel_Combined, by=c('Spend_Channel', 'data_time_period', 'Scenario'))
    
    ### Create ROI column
    combinedData$final_data <- Panel_Contib %>% mutate(Total_ROI = Total_Contribution/Total_Spend)
    
    final_data<-combinedData$final_data
    #Just for simulated time-period
    final_data<-final_data[which(final_data$data_time_period=="Simulated"),]
    keeps <- c('Scenario', 'data_time_period','Spend_Channel',  'Total_Sales', 'Total_Spend', 'Total_Contribution', 'Total_ROI')
    final_data <- final_data[keeps] # This is the final dataframe
    abc<-dplyr::group_by_at(final_data, c('data_time_period', 'Scenario'))
    abc<- dplyr::summarise(abc, Total_Sales = mean(Total_Sales), Total_Spend = sum(Total_Spend), Total_Contribution = sum(Total_Contribution), Total_ROI = (Total_Contribution/Total_Spend))
    combinedData$end_data<-abc
    #browser()
    # This section contains the string showing best scenarios wrt ROI
    ROI_Scenarios<-arrange(abc,desc(Total_ROI))
    #Printing the ROI Bullet point
    Best_Scenario_ROI<-paste("<li>","The most optimal ROI is generated by ", ROI_Scenarios$Scenario[1], "with a value of ", "(", round(ROI_Scenarios$Total_ROI[1],2),")","</li>","<br>")
    #Second_Scenario_ROI<-paste("<li>"," ", ROI_Scenarios$Scenario[2], "with a value of ", "(", round(ROI_Scenarios$Total_ROI[2],2),")","</li>","<br>")
    #ROI<-paste(Best_Scenario_ROI,Second_Scenario_ROI)
    
    ROI<-paste(Best_Scenario_ROI)
    
    # This section contains the string showing best scenarios wrt Sales
    Sales_Scenarios<-arrange(abc,desc(Total_Sales))
    #Printing the Sales Bullet Point
    Best_Scenario_Sales<-paste("<li>","The highest sales is generated by ", Sales_Scenarios$Scenario[1], "with a value of ", "($", round(Sales_Scenarios$Total_Sales[1]/1e+06,2),"M)","</li>","<br>")
    Second_Scenario_Sales<-paste("<li>","The second highest sales is generated by ", Sales_Scenarios$Scenario[2], "with a value of ", "($", round(Sales_Scenarios$Total_Sales[2]/1e+06,2),"M)","</li>","<br>")
    Sales<-paste(Best_Scenario_Sales,Second_Scenario_Sales)
    
    # This section contains the string showing best scenarios wrt Spends
    Spends_Scenarios<-arrange(abc,desc(Total_Spend))
    #Printing the Spends Bullet Point
    Best_Scenario_Spends<-paste("<li>","Spends are highest for ", Spends_Scenarios$Scenario[1], "with a value of ", "($", round(Spends_Scenarios$Total_Spend[1]/1e+06,2),"M)","</li>","<br>")
    #Second_Scenario_Spends<-paste("<li>","The Scenario with the second highest Spend is ", Spends_Scenarios$Scenario[2], "with a value of ", "($", round(Spends_Scenarios$Total_Spend[2]/1e+06,2),"M)","</li>","<br>")
    
    #Spends<-paste(Best_Scenario_Spends,Second_Scenario_Spends)
    
    Spends<-paste(Best_Scenario_Spends)
    
    #Grouping the results at Scenario Level
    Scenario<-paste(ROI,Sales,Spends)
    
    
    
    #Panel level data metrixs
    
    
    # x<-df_final1%>%filter(df_final1$Scenario=='Scenario_1') First filter the Scenario
    # x1<-dplyr::group_by_at(x, c('Month_Date','State'))  Then group by
    # x2<-dplyr::summarise(x1, Total_Sales = mean(Sales))
    
    agg_df<-aggregate(list(df_final1$Sales), by = list(df_final1$Scenario,df_final1$State), sum)
    colnames(agg_df)<-c("Scenario","State","Sales")
    scene<-unique(agg_df$Scenario)
    text<-NULL
    for(i in 1:length(scene)){
      # x<-df_final1%>%filter(df_final1$Scenario==scene[i])
      # x1<-dplyr::group_by_at(x, c('State','Spend_Channel'))
      # filtered_df<-dplyr::summarise(x1, Sales = sum(Sales))
      filtered_df<-agg_df[which(agg_df$Scenario==scene[i]),]
      best_panel<-filtered_df[which.max(filtered_df$Sales),]
      state<-as.character(best_panel$State)
      sale<-round(best_panel$Sales/1e+06,2)
      text<-paste(text,"<li>", "Panel ", state, "generates the most Sales ","($",sale, "M)" ,"for ",best_panel$Scenario, "</li>","<br>")
    }
    
    
    # Final Result Printing
    HTML(paste0("<ul>",Scenario,text,"</ul>"))
    
  })
  
  
  
  output$RecommendationStackedChart<-renderHighchart({
    if(is.null(reactData$viewSelected)){
      scene<-unique(combinedData$df_final1$Scenario)[1]
    }
    else{
      scene<-reactData$viewSelected
    }
    filtered_df<-combinedData$final_data[which(combinedData$final_data$Scenario==scene),]
    # x<-scenarioPanelHistData
    # y<-scenarioPanelSimData
    # z<- unique(compareScenarioTable$dataset$Scenario_Name)
    # s1_sim_Data<- y[which(y$Scenario==as.character(z[1])),] 
    # s2_sim_Data<- y[which(y$Scenario==as.character(z[2])),] 
    # s1_hist_Data<-x[which(x$Scenario==as.character(z[1])),]
    s1_hist_Data<-filtered_df[which(filtered_df$data_time_period=='Historical'),]
    s1_sim_Data<-filtered_df[which(filtered_df$data_time_period=='Simulated'),]
    s1_hist_Data$Scenario<-"Historical"
    s1_sim_Data$mix<-round((s1_sim_Data$Total_Spend/sum(s1_sim_Data$Total_Spend)*100),1)
    s1_hist_Data$mix<-round((s1_hist_Data$Total_Spend/sum(s1_hist_Data$Total_Spend)*100),1)
    # s2_sim_Data$mix<-round((s2_sim_Data$Total_Spend/sum(s2_sim_Data$Total_Spend)*100),1)
    df<-rbind(s1_hist_Data,s1_sim_Data)
    
    
    
    d1<-as.character(simStartDateVar)
    d2<-as.character(reactData$simEndDateVar)
    d3<- paste("Period Of Simulated Scenarios :","From", d1,"to",d2, sep= " ")
    hc<- df %>% 
      hchart(
        'column', hcaes(x = 'data_time_period', y = 'mix', group = 'Spend_Channel'),
        stacking = "normal"
      ) %>%
      hc_colors(c("#e36387", "#f2aaaa","#a6dcef","#ddf3f5")) %>%
      
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text="Spend Mix (%) At Channel Level",align="center") %>%
      hc_subtitle(text=d3,align="center")
    hc
  })  
  
  
  output$viewSelectedMonthUI <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(4,shiny::selectInput("viewMonthSelected", "Select Month",
                                                       choices = unique(combinedData$df_final1$Month),
                                                       multiple = FALSE, selected = NULL)),
                    shiny::column(style='padding-top:25px', class='button_pad',2,
                                  shiny::actionButton('viewMonthSelectedButton','Submit'))
    )
  })
  
  
  shiny::observeEvent(input$viewMonthSelectedButton, {
    tryCatch({
      reactData$viewMonthSelected <- input$viewMonthSelected
      
      ######## Progress bar #########
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Processing...")
      
      ####### Error handling ###########
      if(reactData$viewMonthSelected==""){shiny::showNotification('No scenarios found', type='error'); return()}
      
      # reactData$viewScenarioSelectedData <- scenarioList$files[[reactData$viewScenarioSelected]]$scheduling_dataset$runSimulationDataset
      
    },error=function(e) showNotification(paste0('View Selected Month :',e[1])))
  })
  
  
  
  output$radarPlot <- renderPlot({
    # Create data: note in High school for several students
    # set.seed(99)
    # data = as.data.frame(matrix(sample(0:20 , 12 , replace = F) , ncol = 4))
    # colnames(data) = c("Direct" , "Web" , "Course" , "Program")
    # rownames(data) = paste("Scenario " , letters[1:3] , sep = "-")
    
    x<-combinedData$df_final1
    # df<-x[-c(3,5)]
    if(is.null(reactData$viewMonthSelected)){
      mon<-1
    }
    
    else{
      mon<-as.numeric(reactData$viewMonthSelected)
    }
    df<-x[which(x$Month==mon),]
    scene<-unique(df$Scenario)
    finaldf<-data.frame()
    for(i in 1:length(scene)){
      df1<-df[which(df$Scenario==scene[i]),]
      df_final<-aggregate(list(df1$Spends), by = list(df1$Spend_Channel), sum)
      df_final_1<- as.data.frame(t(df_final),stringsAsFactors = FALSE )
      setnames(df_final_1, as.character(df_final_1[1,]))
      df_final_1<-df_final_1[-c(1),]
      rownames(df_final_1)<-NULL
      df_final_1<-df_final_1<-as.data.frame(lapply(df_final_1, as.numeric))
      df_final_1$Scenario<-as.character(scene[i])
      finaldf<-rbind(finaldf,df_final_1)
    }
    
    
    
    rownames(finaldf)<-finaldf$Scenario
    finaldf<-select(finaldf,-c(Scenario))
    a<-max(finaldf)
    b<-(20/100*a)+a
    b<-round(b,0)
    c<-round(b/5,0)
    finaldf1<-rbind(rep(b,length(finaldf)),rep(0,length(finaldf)),finaldf)
    t<- paste("Spread (in $) of Spends for Month_", mon," across Generated Scenarios")
    
    # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
    # data = rbind(rep(20, 5) , rep(0, 5) , data)
    
    colors_border = c("#a8e6cf",
                      "#dcedc1","#ffd3b6","#ffaaa5","#ffa45b","#ffda77","#aee6e6","#fd3a69")
    
    colors_in = c("#a8e6cf",
                  "#dcedc1","#ffd3b6","#ffaaa5","#ffa45b","#ffda77","#aee6e6","#fd3a69")
    
    radarchart(
      finaldf1  ,
      title = t, 
      axistype = 1 ,
      #custom polygon
      pcol = colors_border ,
      pfcol = colors_in ,
      plwd = 4 ,
      plty = 1,
      
      #custom the grid
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey",
      caxislabels = seq(0, b, c),
      cglwd = 0.8,
      #custom labels
      vlcex = 0.8
    )
    legend(
      x = 0.9,
      y = 1,
      legend = rownames(finaldf1[-c(1, 2), ]),
      bty = "n",
      pch = 20 ,
      col = colors_in ,
      text.col = "grey",
      cex = 1.2,
      pt.cex = 3
    )
  })
  
  output$viewSelectedScenarioUi <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(4,shiny::selectInput("viewSelected", "",
                                                       choices = unique(combinedData$df_final1$Scenario),
                                                       multiple = FALSE, selected = unique(combinedData$df_final1$Scenario)[1])),
                    shiny::column(style='padding-top:25px', class='button_pad',2,
                                  shiny::actionButton('viewSelectedButton','Submit'))
    )
  })
  
  
  shiny::observeEvent(input$viewSelectedButton, {
    tryCatch({
      reactData$viewSelected <- input$viewSelected
      
      ######## Progress bar #########
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Processing...")
      
      ####### Error handling ###########
      if(reactData$viewSelected==""){shiny::showNotification('No scenarios found', type='error'); return()}
      
      # reactData$viewScenarioSelectedData <- scenarioList$files[[reactData$viewScenarioSelected]]$scheduling_dataset$runSimulationDataset
      
    },error=function(e) showNotification(paste0('View Selected Month :',e[1])))
  })  
  
  
  
  output$Scenario1_Spend_Channel<- shiny :: renderUI({
    if(is.null(reactData$viewSelected)){
      scene<-unique(combinedData$df_final1$Scenario)[1]
    }
    else{
      scene<-reactData$viewSelected
    }
    filtered_df<-combinedData$final_data[which(combinedData$final_data$Scenario==scene),]
    x_df<-filtered_df[which(filtered_df$data_time_period=="Simulated"),]
    channel_highest_spends<- x_df[which.max(x_df$Total_Spend),]
    return(as.character(channel_highest_spends$Spend_Channel))
  })
  
  
  output$Scenario1_Sales_Channel<- shiny :: renderUI({
    if(is.null(reactData$viewSelected)){
      scene<-unique(combinedData$df_final1$Scenario)[1]
    }
    else{
      scene<-reactData$viewSelected
    }
    filtered_df<-combinedData$final_data[which(combinedData$final_data$Scenario==scene),]
    x_df<-filtered_df[which(filtered_df$data_time_period=="Simulated"),]
    channel_highest_sales<- x_df[which.max(x_df$Total_Sales),]
    return(as.character(channel_highest_sales$Spend_Channel))
  })
  
  
  output$Scenario1_ROI_Channel<- shiny :: renderUI({
    #browser()
    if(is.null(reactData$viewSelected)){
      scene<-unique(combinedData$df_final1$Scenario)[1]
    }
    else{
      scene<-reactData$viewSelected
    }
    filtered_df<-combinedData$final_data[which(combinedData$final_data$Scenario==scene),]
    y_df<-filtered_df[which(filtered_df$data_time_period=="Simulated"),]
    channel_highest_rois<- y_df[which.max(y_df$Total_ROI),]
    return(as.character(channel_highest_rois$Spend_Channel))
  })
  
  
  
  output$Scenario1_Contribution_Channel<- shiny :: renderUI({
    if(is.null(reactData$viewSelected)){
      scene<-unique(combinedData$df_final1$Scenario)[1]
    }
    else{
      scene<-reactData$viewSelected
    }
    filtered_df<-combinedData$final_data[which(combinedData$final_data$Scenario==scene),]
    y_df<-filtered_df[which(filtered_df$data_time_period=="Simulated"),]
    channel_highest_contribution<- y_df[which.max(y_df$Total_Contribution),]
    return(as.character(channel_highest_contribution$Spend_Channel))
  })
  
  
  output$Scenario1_name<- shiny:: renderText({
    z<- unique(compareScenarioTable$dataset$Scenario_Name)
    return(as.character(z[1]))
  })
  
  output$Scenario2_name<- shiny:: renderText({
    z<- unique(compareScenarioTable$dataset$Scenario_Name)
    return(as.character(z[2]))
  })
  
  
  
  shiny::observeEvent(input$submitCompareScenarioChannel, {
    tryCatch({
    channelSelected <- input$selectCompareScenarioChannel
    pctChannelSelected <- paste0('pct_contrib_', input$selectCompareScenarioChannel)
    nonPctChannelSelected <- paste0('contrib_', input$selectCompareScenarioChannel)
    
    ######## Progress bar #########
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Processing...")
    
    ###### Error handling #############
    if(is.null(reactData$DateVar) | is.null(reactData$targetVar)){shiny::showNotification('Please configure spend, target, date and panel variables as described in the Configuration Section', type='error'); return()}
    
    if(nrow(reactData$compareScenarioAllData)<=0 | is.null(reactData$compareScenarioAllData)){shiny::showNotification('Please view panel level results before attempting to view the corresponding channel level results', type='error'); return()}
    
    if(channelSelected==""){
      shiny::showNotification('Please select a valid channel from the list', type='error'); return()
    }
    
    
    if("Total" %in% channelSelected){
      allData <- reactData$compareScenarioAllData
      
    }
    else{

      allData <- reactData$compareScenarioAllData %>% filter(reactData$compareScenarioAllData$Spend_Channel == channelSelected)
      
    }
    ##### Aggregating Dataset to time period and scenario level #####
    allChannelData <- allData %>% group_by_at(c('Time_Period', 'Scenario')) %>% summarize(Total_Sales=mean(Total_Sales), Total_Spend=sum(Total_Spend), Total_Contribution=sum(Total_Contribution))
    
    ##### Calculate ROI on aggregated dataframe
    allChannelData <- allChannelData %>% mutate(Total_ROI = Total_Contribution/Total_Spend)
    
    ###### Plotting Spends Chart ###########
    
    spendsCSTSggplot <- timeSeriesCompareScenariosPlot(dataset = allChannelData, chartTitle = "Spend", xvar = "Time_Period", xlab = "Time", ylab = "Total Spends", yvar = "Total_Spend", lvar="Scenario", opti_seq = reactData$optiDateSequence, hist_seq = reactData$histDateSequence, seq_xvar = reactData$DateVar,  panelLabel = reactData$panelSelected, channelLabel = channelSelected) + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
    
    output$scenarioSpendSimTSPlot <- shiny::renderPlot({spendsCSTSggplot})
    
    
    ###### Plotting Sales Chart ###########
  
    salesCSTSggplot <- timeSeriesCompareScenariosPlot(dataset = allChannelData, chartTitle = "Sales", xvar = "Time_Period", xlab = "Time", ylab = "Total Sales", yvar = "Total_Sales", lvar="Scenario", opti_seq = reactData$optiDateSequence, hist_seq = reactData$histDateSequence, seq_xvar = reactData$DateVar,  panelLabel = reactData$panelSelected, channelLabel = channelSelected) + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
    
    output$scenarioSalesSimTSPlot <- shiny::renderPlot({salesCSTSggplot})
    
    ###### Plotting Contribution Chart ########### 
    
    contribCSTSggplot <- timeSeriesCompareScenariosPlot(dataset = allChannelData, chartTitle = "Contribution", xvar = "Time_Period", xlab = "Time", ylab = "Total Contribution", yvar = "Total_Contribution", lvar="Scenario", opti_seq = reactData$optiDateSequence, hist_seq = reactData$histDateSequence, seq_xvar = reactData$DateVar,  panelLabel = reactData$panelSelected, channelLabel = channelSelected) + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
    
    output$scenarioContributionSimTSPlot <- shiny::renderPlot({contribCSTSggplot})
    
    ####### Plotting ROI Chart ################
    
    roiCSTSggPlot <- timeSeriesCompareScenariosPlot(dataset = allChannelData, chartTitle = "ROI", xvar = "Time_Period", xlab = "Time", ylab = "Total ROI", yvar = "Total_ROI", lvar="Scenario", opti_seq = reactData$optiDateSequence, hist_seq = reactData$histDateSequence, seq_xvar = reactData$DateVar,  panelLabel = reactData$panelSelected, channelLabel = channelSelected)
    
    output$scenarioROISimTSPlot <- shiny::renderPlot({roiCSTSggPlot})
    
    ##################### Arrgange ggplot objects for downloading plots as image #################
    grobCompareScenarioChannelResultsPlots<- arrangeGrob(salesCSTSggplot, spendsCSTSggplot, contribCSTSggplot, roiCSTSggPlot, ncol=2)
    
    ########################## Download handler for plots ################################
    output$downloadCompareScenarioChannelResultsPlots <- shiny::downloadHandler(
      filename = function(){'Compare_Scenario_Time_Series_Plots.png'},
      content = function(file) {
        ggsave(file, plot=grobCompareScenarioChannelResultsPlots, device='png', width = 45, height = 25, units = 'cm')
      },
      shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
    )
  
    ########################## Download handler for data ################################
    output$downloadCompareScenarioChannelResultsData <- shiny::downloadHandler(
      filename = function(){'Compare_Scenario_Time_Series_Data.csv'},
      content = function(file) {
        write.csv(allChannelData, file, row.names = FALSE)
      }
    )
    
    #### Plot for report
    reactData$salesCSTSplot <- salesCSTSggplot
    reactData$spendsCSTSplot <- spendsCSTSggplot
    reactData$contribCSTSplot <- contribCSTSggplot
    reactData$roiCSTSPlot <- roiCSTSggPlot
    
  },error=function(e) showNotification(paste0('View Compare Scenario Time Series :',e[1])))
  })
  
  ##########################################################################################
  ################### Optimization Time Period Setup UI  ######################################
  ##########################################################################################
  
  output$viewOptimisationDates_atConstraints <- shiny::renderUI({
    if(is.null(reactData$simStartDateVar)) return(shiny::HTML(''))
    list(shiny::tags$h5(shiny::HTML(paste0('<b>Historical Comparison Period: </b>', reactData$histStartDateVar,' to ', reactData$histEndDateVar))),
         shiny::tags$h5(shiny::HTML(paste0('<b>Optimization Period: </b>', reactData$simStartDateVar,' to ', reactData$simEndDateVar))))
  })
  
  output$viewOptimisationDates_atTechnique <- shiny::renderUI({
    if(is.null(reactData$simStartDateVar)) return(shiny::HTML(''))
    list(shiny::tags$h5(shiny::HTML(paste0('<b>Historical Comparison Period: </b>', reactData$histStartDateVar,' to ', reactData$histEndDateVar))),
         shiny::tags$h5(shiny::HTML(paste0('<b>Optimization Period: </b>', reactData$simStartDateVar,' to ', reactData$simEndDateVar))))
  })
  
  output$viewOptimisationDates_atResults <- shiny::renderUI({
    if(is.null(reactData$simStartDateVar)) return(shiny::HTML(''))
    list(shiny::tags$h5(shiny::HTML(paste0('<b>Historical Comparison Period: </b>', reactData$histStartDateVar,' to ', reactData$histEndDateVar))),
         shiny::tags$h5(shiny::HTML(paste0('<b>Optimization Period: </b>', reactData$simStartDateVar,' to ', reactData$simEndDateVar))))
  })
  
  ##########################################################################################
  ################### Optimization brick begins here  ######################################
  ##########################################################################################
  
  ### Checks for click of help icon
  shiny::observeEvent(input$viewBusinessObjectiveHelpText,{
    
      showModal(modalDialog(
        title = "Business Objective - Help",
        shiny::p(shiny::HTML("There are two Business Objectives which the brick currently supports while optimizing : <ul><li><strong>Maximize Sales :</strong> For this choice of business objective the algorithm iterates through the range of spend variable combinations and calculates sales, the highest sales number generated by the spending structure is finalized. This choice of optimization goal is selected when aim is increasing sale at the expense ROI.</li>
<li><strong>Maximize ROI :</strong> This business objective is chosen when the goal for the comapany is to maximize the the ratio of sales over spends, this is done at expense of Sales.</li>
<li><strong>Revenue Chase :</strong> This business objective is chosen when the sales target is set and the marketing team is looking for optimal spends to achieve it.</li></ul>")),
        footer = tagList(
          modalButton("OK")
        )
      ))
    })
  
  # Taking Business Priorities for Further Analysis
  output$businessPriority <- shiny::renderUI({
    list(shiny::fluidRow(class='box1',
                         shiny::column(12,shiny::radioButtons('selectedObjective', 'Select Objective', c("Maximize Sales", "Maximize ROI", "Revenue Chase"), inline = TRUE,  width = NULL))),
         shiny::conditionalPanel('input.selectedObjective=="Revenue Chase"'
                                 , shiny::column(5,shiny::textInput('targetRevenue','Enter Sales Target',value = 6000000))
                                 # , shiny::column(2, style='padding-top:25px', class='button_pad', shiny::actionButton('submittedTargetRevenue','Submit'))
         )
         
         # shiny::fluidRow(class='box1',
         #                 shiny::column(class='button_pad',2,
         #                               shiny::actionButton('submittedObjective','Submit')))
    )
    # output$businessPriorityMessage <- renderText((paste0("Business Priority: ", input$selectedObjective)))
  })
  
  output$businessPriorityMessage <- renderText((paste0("Business Objective: ", input$selectedObjective)))
  
  
  ##################################
  ### Calculating Budget from Past data
  ##################################
  shiny::observeEvent(input$dateVarSelection,{
    tryCatch({
      ################ matching the column format of historical to uploaded model train data
      # Converting historical data column formats to as that of model format to avoid future errors
      req_col <- data.frame(reactData$dataset, stringsAsFactors = FALSE)
      # Converting the Formats of Extra Dataset into the formats from train dataset
      req_col_colnames <- colnames(req_col)
      trainData_req_col <- mmxModel$train_data[req_col_colnames]
      # extracting the format info of the train datset columns
      trainData_class <- trainData_req_col %>% map(~{
        paste0("as.",class(.))
      }) %>% data.frame(stringsAsFactors = F) 
      # converting into the required formats
      req_col <- req_col_colnames %>% map(~{
        do.call(trainData_class[[.]],list(req_col[[.]]))
      })
      
      names(req_col) <- req_col_colnames
      req_col <- data.frame(req_col,stringsAsFactors = FALSE)
      reactDataOpt$dataset <- req_col
      ##################Calculating Budget
      budget_dataset <- data.frame(reactDataOpt$dataset, stringsAsFactors = FALSE)
      simdates <- reactData$histDateSequence
      
      budget_dataset <- merge(simdates, budget_dataset, by = c(reactData$DateVar), all.x = TRUE )
      #budget_dataset <- budget_dataset %>% filter(!is.na(Month_Date.y))
      
      budget_dataset <- melt(budget_dataset, measure.vars = c(reactData$spendVar))
      budget_dataset <- budget_dataset %>% summarise(value= sum(value))
      
      reactData$budget <- round(budget_dataset,2)
      output$lastYearBudget <- shiny::renderUI({
        shiny::fluidRow(class='box1',
                        shiny::column(5,style='padding-top:5px',h4(shiny::HTML(paste0("Historical Budget:</br> $",strong(format(round(reactData$budget), big.mark = ",")) )))),
                        shiny::column(5,shiny::textInput('budgetPercent','Enter Budget Increment (%)',value = 10)),
                        shiny::column(2,style='padding-top:25px',shiny::actionButton('selectedPercent','Submit')))
      })
    },error=function(e) showNotification(paste0('Error while Calculating Budget: ',e[1])))
  })
  
  ##################################
  ### Calculating Future Budget
  ##################################
  shiny::observeEvent(input$selectedPercent,{
    tryCatch({
      if(reactData$budget<0){
        showNotification('Wrong Budget Value: Enter Budget before you Submit this',type='message')
        return()
      }
      # browser()
      
      budget <- as.numeric(reactData$budget)
      budgetPercent <- 1 + (as.numeric(input$budgetPercent)/100)
      optimisedBudget <- budget*budgetPercent
      reactData$optimisedBudget <- optimisedBudget
      
      ######################################################################################
      ######################################################################################
      ### This section is for Business Objective chosen as Revenue Chase 
      output$revenueChaseMsg <- NULL
      newSpendDataset <- merge(reactData$histDateSequence, reactDataOpt$dataset, by = reactData$DateVar, all.x = T)
      targetRevenue <- as.numeric(input$targetRevenue)
      if(input$selectedObjective == "Revenue Chase") {
        newRevenue <- sum(predict(mmxModel$model, newSpendDataset))
        # while(newRevenue > 1.1*targetRevenue | newRevenue < 0.9*targetRevenue) {
        #   if(newRevenue>targetRevenue){
        #     newSpendDataset[reactData$spendVar] <- 0.98*newSpendDataset[reactData$spendVar]
        #     newRevenue <- sum(predict(mmxModel$model,newSpendDataset))
        #     reactData$optimisedBudget <- sum(newSpendDataset[reactData$spendVar])
        #     # print(newRevenue)
        #     # print(sum(newSpendDataset[reactData$spendVar]))
        #   }else{
        #     newSpendDataset[reactData$spendVar] <- 1.02*newSpendDataset[reactData$spendVar]
        #     newRevenue <- sum(predict(mmxModel$model,newSpendDataset))
        #     reactData$optimisedBudget <- sum(newSpendDataset[reactData$spendVar])
        #     # print(newRevenue)
        #     # print(sum(newSpendDataset[reactData$spendVar]))
        #   }
        # }
        
        output$revenueChaseMsg <- renderText((paste0("As per user selection Business Objective is selected as ", input$selectedObjective, " for the target revenue the optimal budget required will be around $", reactData$optimisedBudget)))
      }
      ################################################################################################
      ################################################################################################
      
      output$newBudget <- shiny::renderUI({
        shiny::fluidRow(class='box1',
                        shiny::column(12,style='padding-top:5px',shiny::h4(HTML(paste0("Maximum Budget Allocated: </br>$",strong(format(round(reactData$optimisedBudget), big.mark = ",")) ))))
        )
      })
    },error=function(e) showNotification(paste0('Error while Calculating Future Budget: ',e[1])))
  })
  
  ################### Creating Constraint for Budget Entered #################
  ####################################################################
  
  shiny::observeEvent(input$selectedPercent,{
    tryCatch({
      col_names <- c(reactData$DateVar, reactData$panelVar,"Operation","Equation")
      col_names_len <- length(col_names)
      
      ## Creating Budget Constraint Equation
      Equation <- paste(paste0(reactData$spendVar, collapse = "+"), reactData$optimisedBudget, sep = "<")
      
      local_mat <- matrix(rep('All', col_names_len -1), ncol = col_names_len)
      local_mat[1,col_names_len] <- Equation
      local_tab <- data.frame(local_mat)
      colnames(local_tab) <- col_names
      #Adding the Budget Constraint in the final constraint table
      #reactData$constraint <- NULL # this is done to make sure if we re-enter the budget again its taken care of
      userInput <- as.vector(local_tab$Equation)
      
      finalData <- c(1:length(userInput)) %>% map(~
      {
        i <- .
        if(grepl("<",userInput[i])){
          split_val <- strsplit(userInput[i],"<")[[1]]
          split_val <- c(split_val,'<')
          
        }
        if(grepl(">",userInput[i])){
          split_val <- strsplit(userInput[i],">")[[1]]
          split_val <- c(split_val,'>')
        }
        return(split_val)
      }) %>% do.call(rbind,.)
      
      colnames(finalData)<-c("LHS","RHS","Compare")
      finalData <- data.frame(finalData)[,c("LHS","Compare","RHS")]
      local_tab <- data.frame(cbind(local_tab, finalData), stringsAsFactors = FALSE)
      local_tab <- subset(local_tab, select = -Equation)
      
      if(!is.null(reactData$table_constraint)){
        reactData$budget_constraint <- data.frame(local_tab, stringsAsFactors = FALSE)
        reactData$constraint <- rbind(reactData$budget_constraint, reactData$table_constraint)
        reactData$constraint <- unique(reactData$constraint)
      }else{
        reactData$budget_constraint <- data.frame(local_tab, stringsAsFactors = FALSE)
        reactData$constraint <- unique(reactData$budget_constraint)
      }
    },error=function(e) showNotification(paste0('Error while adding budget as Constraint to Constraint Table: ',e[1])))
  })
  ################### Help Text for Business Constraints #################
  ########################################################################
  shiny::observeEvent(input$loadBusinessConstraintsHelpText,{
    showModal(modalDialog(
      title = "Business Constraints - Help",
      p(HTML('This option gives user the flexibility to choose the current business constraints that may be applicabel for the time period in concern:
             <ul><li>Choose level of constraints for the below options i.e Date and Geographic Location</li>
             <li>To apply the costraint on individual levels user shall choose "Apply Constraint On" as "Each Panel Individually", in case the constraints are to be applied on the block of the level selected by the user the user shall choose "All Panels Combined"</li>
             <li>Enter constraints in the provided space and press Submit Constraint</li>
             <li>For user help we have also given a drop down to choose dataset columns in the dropdown, to use select a variable and press submit text </li></ul>
             Eg. 1: Spends through Channel "X" should not excees 10000
             <ul><li>Select Level of Data : "All", for all level of column options</li>
             <li>Select Apply Constraint On as "All Panels Combined"</li>
             <li>Enter Constraints as "X<10000"</li></ul>
             Eg. 2: Spends through a panel "P" should not exceed 500
             <ul><li>Select Date: "All", & Select Panel: "P"</li>
             <li>Select Apply Constraint On as "All Panels Combined"</li>
             <li>Enter Constraints as "X+Y+Z<500", where X,Y and Z are all available spend channels</li></ul>')),
      footer = tagList(
        modalButton("OK")
      )
    ))
  }
  )
  
  shiny::observeEvent(input$optBoundHelp,{
    showModal(modalDialog(
      title = "Optimization Bounds - Help",
      p(HTML('1. The Lower and Upper bounds have been calculated as per budget and bound percentage
            <ul><li>The bounds specify the range around the new spends for which the optimization function tries to find the optimal solution</li></ul>
              2. The bounds are calculated around the increased budget
            <ul><li>Lower Bound = Historical_Spends*Increment_in_Budget(%)*Bound_Percent(%), similarly the Upper Bound is calculated</li></ul>')),
      footer = tagList(
        modalButton("OK")
      )
    ))
  })
  ################### Creating Hierarchy Table #################
  ##############################################################
  
  shiny::observeEvent(input$selectedObjective, {
    #
    # Error Handling to understand 
    if(is.null(reactData$spendVar)){shiny::showNotification('Please upload model and select necessary variables', type='error'); return()}
    
    simdates <- reactData$optiDateSequence
    # browser()
    hierarchy_tab <- unique(data.frame(reactDataOpt$dataset, stringsAsFactors = FALSE) %>% select_at(c(reactData$panelVar)))
    cross_join_data <- merge(simdates, hierarchy_tab, all=TRUE)
    hierarchy_tab <- cross_join_data %>% arrange_at(c(reactData$DateVar, reactData$panelVar))
    hierarchy_tab$number_r <- 1:nrow(hierarchy_tab)
    reactData$hierarchy_tab <- data.frame(hierarchy_tab, stringsAsFactors = FALSE)
  })
  
  ############## Creating Constraints and Bounds Table ##############
  
  op_type <- list("Each Panel Individually"="Each", "All Panels Combined"="All")
  comp_type <- c(">", "<")
  reactData$constraint <- data.frame(NULL)
  reactData$table_constraint <- data.frame(NULL)
  reactData$budget_constraint <- data.frame(NULL)
  reactData$hirearchy_tab <- as.data.frame(NULL)
  reactData$bound <- as.data.frame(NULL)
  
  output$enteredConstraint <- shiny::renderUI({
    #
    input_var <- c(reactData$DateVar, reactData$panelVar)
    
    out_1 <-lapply(1:length(input_var), function(i){
      list(shiny::column(3,shiny::selectizeInput(paste0(input_var[i]), paste0('Select ', input_var[i]),
                                                 choices =c("All",unique(reactData$hierarchy_tab[input_var[i]])), selected ="All",multiple=TRUE)))
    })
    
    out_2 <- list(shiny::fluidRow(class='box1',
                                  shiny::column(4,shiny::selectInput('Operation','Apply Constraint On',
                                                                     choices =op_type, multiple=FALSE, selected = NULL))),
                  shiny::fluidRow(class='box1',
                                  
                                  shiny::column(6,shiny::textInput("text", "Enter Constraint", value = "Direct<Web",width='100%')),
                                  shiny::column(2,shiny::selectInput("Option","Search Keyword", 
                                                                     choices =reactData$spendVar,multiple=TRUE, selected = NULL)),                    
                                  shiny::column(class='button_pad',2,style='padding-top:25px',
                                                shiny::actionButton('selectedtext','Append Keyword to Constraint'))),
                  shiny::fluidRow(class='box1',
                                  shiny::column(class='button_pad',4,
                                                shiny::actionButton('selectedConstraint','Submit Constraint',style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))))
    
    out <- list(out_1, out_2)
    return(out)
  })
  
  shiny::observeEvent(input$selectedtext,{
    tryCatch({
      reactData$text <- input$text
      reactData$Option <- input$Option
      shiny::updateSelectInput(session,'text',selected=paste(reactData$text, reactData$Option, sep =''))
      #print(cbind(reactData$text, reactData$Option))
    })
  })
  
  ## Taking constraint from shiny UI and showing output
  shiny::observeEvent(input$selectedConstraint, {
    tryCatch({
      if(is.null(reactData$budget)){shiny::showNotification('Invalid Budget Value: Submit budget for the Optimization Time', type='error'); return()}
      
      col_names <- c(reactData$DateVar, reactData$panelVar, "Operation", "Equation")
      
      panelData <- data.frame(NULL)
      panelData <- sapply(1:length(reactData$panelVar), function(i){
        if(nrow(panelData)){
          panelData <- cbind(panelData, paste(as.vector(input[[reactData$panelVar[i]]]), collapse = ","))
        }else{
          panelData <- paste(as.vector(input[[reactData$panelVar[i]]]), collapse = ",")
        }
      })
      
      Operation <- input$Operation
      Equation <- input$text
      
      constraint_data <- cbind(paste(as.vector(input[[reactData$DateVar]]), collapse = ","), panelData, Operation, Equation)
      colnames(constraint_data) <- col_names
      constraint_data <- data.frame(constraint_data, stringsAsFactors = F)
      # browser()
      userInput <- as.vector(constraint_data$Equation)
      
      finalData <- c(1:length(userInput)) %>% map(~
      {
        i <- .
        split_val <- NULL
        if(grepl("<",userInput[i])){
          split_val <- strsplit(userInput[i],"<")[[1]]
          split_val <- c(split_val,'<')
          
        }
        if(grepl(">",userInput[i])){
          split_val <- strsplit(userInput[i],">")[[1]]
          split_val <- c(split_val,'>')
        }
        return(split_val)
      }) %>% do.call(rbind,.)
      
      if(is.null(finalData[[1]])){
        showNotification('Invalid constraint',type='error')
        return()
      }
      colnames(finalData)<-c("LHS","RHS","Compare")
      finalData <- data.frame(finalData)[,c("LHS","Compare","RHS")]
      constraint_data <- data.frame(cbind(constraint_data, finalData), stringsAsFactors = FALSE)
      constraint_data <- subset(constraint_data, select = -Equation)
      
      if(nrow(reactData$table_constraint)>0){
        if(!is.null(reactData$budget_constraint)){
          reactData$table_constraint <- rbind(reactData$table_constraint, data.frame(constraint_data, stringsAsFactors = FALSE))
          # reactData$table_constraint <- data.frame(constraint_data, stringsAsFactors = FALSE)
          reactData$constraint <- rbind(reactData$budget_constraint, reactData$table_constraint)
          reactData$constraint <- unique(reactData$constraint)
        }else{
          reactData$table_constraint <- rbind(reactData$table_constraint, data.frame(constraint_data, stringsAsFactors = FALSE))
          reactData$constraint <- reactData$table_constraint
          reactData$constraint <- unique(reactData$table_constraint)
        }
      }else{
        if(!is.null(reactData$budget_constraint)){
          reactData$table_constraint <- data.frame(constraint_data, stringsAsFactors = FALSE)
          # reactData$table_constraint <- data.frame(constraint_data, stringsAsFactors = FALSE)
          reactData$constraint <- rbind(reactData$budget_constraint, reactData$table_constraint)
          reactData$constraint <- unique(reactData$constraint)
        }else{
          reactData$table_constraint <- data.frame(constraint_data, stringsAsFactors = FALSE)
          reactData$constraint <- reactData$table_constraint
          reactData$constraint <- unique(reactData$table_constraint)
        }
      }
    },error= function(e) showNotification(paste0('Error while creating constraint table: ',e[1]), type='error'))
  })
  
  #### Delete rows for the for Constraint Table
  shiny::observeEvent(input$deleteConstraintRow,{
    tryCatch({
      
      if(nrow(reactData$constraint)<=0){shiny::showNotification('No scenarios found', type='error'); return()}
      rows <- input$constraint_rows_selected
      if(length(rows)==0) return()
      if(nrow(reactData$budget_constraint)>0){
        if(rows==1){
          dat <- reactData$constraint
          dat1 <- reactData$budget_constraint
          rownames(dat) <- NULL
          rownames(dat1) <- NULL
          updated_dat <- dat[-c(rows),]
          updated_dat1 <- dat1[-c(rows),]
          reactData$constraint <- updated_dat
          reactData$budget_constraint <- updated_dat1
        }else{
          dat <- reactData$constraint
          dat1 <- reactData$table_constraint
          rownames(dat) <- NULL
          rownames(dat1) <- NULL
          updated_dat <- dat[-c(rows),]
          updated_dat1 <- dat1[-c(rows-1),]
          reactData$constraint <- updated_dat
          reactData$table_constraint <- updated_dat1
        }
      }else{
        dat <- reactData$constraint
        dat1 <- reactData$table_constraint
        rownames(dat) <- NULL
        rownames(dat1) <- NULL
        #deleted_dat <- dat[c(rows),]
        #scenarioList$files[[deleted_dat$Scenario_Name]] <- NULL
        updated_dat <- dat[-c(rows),]
        updated_dat1 <- dat1[-c(rows),]
        reactData$constraint <- updated_dat
        reactData$table_constraint <- updated_dat1
      }
    
    },error= function(e) showNotification(paste0('Error while deleting constraint row: ',e[1]), type='error'))
  })
  
  ### Printing out Constraints
  output$constraint <-  DT::renderDataTable({
    DT::datatable(reactData$constraint,options=list(scrollX=T, pagelength=10)) #%>% formatRound(numCols)
  })
  
  ## Calculating Bounds for Stuff
  shiny::observeEvent({
    input$selectedPercent
    input$boundPercent
  },{
    tryCatch({
      #browser()
      #spends <- data.frame(mmxModel$train_data, stringsAsFactors = FALSE)
      budget <- as.numeric(reactData$budget)
      budgetPercent <- 1 + (as.numeric(input$budgetPercent)/100)
      lower_boundPercent <- 1 - (as.numeric(input$boundPercent)/100)
      upper_boundPercent <- 1 + (as.numeric(input$boundPercent)/100)
      resultant_lower_boundPercent <- budgetPercent*lower_boundPercent
      resultant_upper_boundPercent <- budgetPercent*upper_boundPercent
      
      spends <- data.frame(reactDataOpt$dataset, stringsAsFactors = FALSE)
      spends <- melt(spends, measure.vars = c(reactData$spendVar))
      #Filter for required time period
      simdates <- reactData$histDateSequence
      spends <- merge(simdates, spends, by = c(reactData$DateVar), all.x = TRUE)
      
      spends <- merge(simdates, spends, by = c(reactData$DateVar), all.x = TRUE)
      spends <- spends %>% group_by_at(c(reactData$DateVar, reactData$panelVar, "variable")) %>% summarise(value = sum(value))
      spends <- data.frame(spends, stringsAsFactors = FALSE)
      #Calculating Lower and Upper Bounds
      spends <- spends %>% mutate(Lower = resultant_lower_boundPercent*value, Upper = resultant_upper_boundPercent*value )
      # spends <- cbind(spends, Operation = c("Each"))
      # spends <- spends %>% select(-value)
      colnames(spends)[which(colnames(spends)=="value")] <- "Historical_Spends"
      setnames(spends, old = c('variable'), new = c('Spend_Channel'))
      
      spends[reactData$DateVar] <- data.frame(apply(spends[reactData$DateVar], 1,
                                                    function(x) { return(as.character(seq(as.Date(x), length=2, by="+1 years")[2])) }),stringsAsFactors = FALSE)
      # Reactive Variable for the Channel Bounds Table  
      reactData$bound <- spends
      # Reactve Variable for Calculating Last Year Spends
      # browser()
      ly_spends <- spends %>% group_by(Spend_Channel) %>% summarise(Historical_Spends = sum(Historical_Spends))
      ly_spends <- data.frame(ly_spends, stringsAsFactors = FALSE)
      reactData$ly_spends <-ly_spends
      
      ######################## Last Year Channel Bounds ##############################################
      
      nums <-  unlist(lapply(reactData$ly_spends, is.numeric)) 
      # output$ly_channelSpends <- DT::renderDataTable(DT::datatable(reactData$ly_spends),options=list(scrollX=T, pagelength=6))
      
      output$ly_channelSpends <- DT::renderDataTable(DT::datatable(roundOffDataFrame(head(reactData$ly_spends),TRUE,2),options=list(scrollX=T, searching=FALSE, dom = 't'))%>%
                                                       formatCurrency(colnames(reactData$ly_spends[nums]),currency = "$", interval = 3, mark = ","))
      
    },error=function(e) showNotification(paste0('Error in Bounds:',e[1])))
  },ignoreInit = T)
  output$channelBudgetHeader <- shiny::renderUI({
    shiny::fluidRow(column(12, h4(paste0('Budget for Historical Comparison Period (', reactData$histStartDateVar,' to ', reactData$histEndDateVar,")"))))
  })
  
  
  ######################## Output UI for Custome Editable RhandsOn Table #########################
  output$channelBounds <-  renderRHandsontable({
    data <- reactData$bound
    rhandsontable(data, height = 300, width=800) %>% # actual rhandsontable object
      hot_table(highlightCol = TRUE, highlightRow = TRUE, readOnly = TRUE) %>%
      hot_col("Lower", readOnly = FALSE, renderer = color_renderer) %>%
      hot_col("Upper", readOnly = FALSE, renderer = color_renderer)
  })
  
  ############# Observe Event in Case Rhandson Table is edited at UI end by User #################
  observeEvent(input$channelBounds,{
    reactData$bound <- hot_to_r(input$channelBounds) 
    # print(head(reactData$bound))
  })
  
  ################################################################################################
  #########           Download and Upload the user-edited constraints file            ############
  ################################################################################################
  
  output$checkboxEditConstraint <- shiny::renderUI({
    tagList(shiny::fluidRow(style='padding-left:10px',
                            column(12, h3('Upload Custom Constraints')),
                            column(12,shiny::checkboxInput('modify_constrintNbound_checkbox','Do you want to edit and upload the sample constraints file or load a previously saved constraint configuration?', width = '100%')),
                            column(12,
                                   conditionalPanel('input.modify_constrintNbound_checkbox',
                                                    shiny::wellPanel(fluidRow(class='box1',
                                                                              #                    column(11, h4('Follow the below steps to edit:')),
                                                                              #                    column(11, shiny::HTML('<ul><li>To edit/review input constraints and default bounds Click "Download Constraints" </li>
                                                                              # <li>Open the Excel file in your local to edit or add more constraints/ bounds to the file</li>
                                                                              # </ul>')),
                                                                              column(11, shiny::downloadButton("downloadConstraint", "Download Excel File")),
                                                                              shiny::br(),
                                                                              column(11, shiny::fileInput('uploadedConstraint', 'Upload Custom Constraint File'))
                                                    ))))))
  })
  
  ### Downlaoding the constrainst and bounds file
  output$downloadConstraint <- downloadHandler(
    filename = function() {
      paste("customConstraints_Bounds", ".xlsx", sep = "")
    },
    content = function(file) {
      xlsx::write.xlsx(reactData$constraint, file, sheetName = "constraint", row.names = FALSE)
      xlsx::write.xlsx(reactData$bound, file, sheetName = "bound", append = TRUE, row.names = FALSE)
    },
    shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
  )
  
  ### Conductor to load edited constraints/ bounds file into reactive shiny tables
  shiny::observeEvent(input$uploadedConstraint,{
    tryCatch({
      infile <- input$uploadedConstraint
      reactData$constraint <- data.frame(read.xlsx(infile$datapath, sheetName = "constraint"), stringsAsFactors = FALSE)
      reactData$bound <- data.frame(read.xlsx(infile$datapath, sheetName = "bound"), stringsAsFactors = FALSE)
    },error= function(e) showNotification(paste0('Error while uploading file: ',e[1]), type='error'))
  })
  
  ################################################################################################
  # # # # # # # # # # # ###########--Confirm Constraints in Play--############## # # # # # # # # # 
  ################################################################################################
  
  # output$goahead <-  shiny::renderUI({
  #   
  #   shiny::fluidRow(class='box1',
  #                   shiny::column(class='button_pad',2,
  #                                 shiny::actionButton('submittedFinalConstraint','Confirm Constraints')))
  #   
  # })
  
  # output$constr <- renderText((paste0("Constraints submitted Successfully!")))
  
  ################################################################################################
  # # # # # # # # # # ##############----Optimization Begins----################# # # # # # # # # # 
  ################################################################################################
  
  # observeEvent(input$submittedFinalConstraint,{
  #   
  #   if(is.null(reactData$budget)){shiny::showNotification('Invalid Budget Value: Submit budget for the Optimisation Time', type='error'); return()}
  #   
  #   userInput <- as.vector(reactData$constraint$Equation)
  #   
  #   finalData <- c(1:length(userInput)) %>% map(~
  #   {
  #     i <- .
  #     if(grepl("<",userInput[i])){
  #       split_val <- strsplit(userInput[i],"<")[[1]]
  #       split_val <- c(split_val,'<')
  #       
  #     }
  #     if(grepl(">",userInput[i])){
  #       split_val <- strsplit(userInput[i],">")[[1]]
  #       split_val <- c(split_val,'>')
  #     }
  #     return(split_val)
  #   }) %>% do.call(rbind,.)
  #   
  #   colnames(finalData)<-c("LHS","RHS","Compare")
  #   reactData$constraint <- data.frame(cbind(reactData$constraint, finalData), stringsAsFactors = FALSE)
  #   reactData$constraint <- subset(reactData$constraint, select = -Equation)
  #   
  # })
  
  ################################################################################################
  # # # # # # # # # # ###########---Choose Optimization Techinique---########### # # # # # # # # # 
  ################################################################################################
  
  shiny::observeEvent(input$optiTechniqueHelpText,{
    tryCatch({
      showModal(modalDialog(
        title = "Optimization Technique - Help",
        shiny::HTML('<ul><li>This option gives user the flexibility to choose the Optimization Technique from the below given option to further analysis</li>
                    <li>Optimization Techniques: COBYLA, ISRES supported</li>
                    <li>Inequality Constraints are allowed with Optimization Techniques: COBYLA, ISRES</li>
                    <li>Equality Constraints are allowed with Optimization Techniques: ISRES</li></ul>'),
        footer = tagList(
          modalButton("OK")
        )
      ))
    },error= function(e) showNotification(paste0('Error in Help Text: ',e[1]), type='error'))
  })
  
  
  #### Choose the Optimization Technique for Analysis
  #### the below list is used to give user the choice in the apt format while keeping in mind the fact that it stays that choices are kept in short ids to keep the code clean
  
  tech_choice <- list("Constrained optimization by Linear Approximation (COBYLA)"="COBYLA", "Improved Stochastic Ranking Evolution Strategy (ISRES)"="ISRES")
  output$optimisationTechnique <- shiny::renderUI({
    list(shiny::fluidRow(class='box1',
                         shiny::column(12,h4("Select Optimization Technique")),
                         shiny::column(12,shiny::selectInput('selectedTechnique', 'Select Technique', choices=c("", tech_choice), selected = "", multiple = FALSE,  width = "70%")),
                         shiny::column(12,h4("Choose Parameter Setting")),
                         shiny::column(4,shiny::textInput('maxIteration','Set Max Evaluations',value = 10)),
                         shiny::column(2, style='padding-top:25px', class='button_pad',
                                       shiny::actionButton('selectedOptimisation','Submit'))
    )
    
    )
  })
  
  shiny::observeEvent(input$selectedOptimisation,{
    tryCatch({
      
      ######## Progress bar #########
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Running Optimization: This may take some time")
      
      if(input$selectedTechnique==""){
        shiny::showNotification('Please select a valid optimization technique from the list', type='error'); return()
      }
      # browser()
      ## Hierarchy Table loading
      hierarchy_tab <- reactData$hierarchy_tab
      
      constraint_table <- reactData$constraint
      
      ## Bring Order/ Constraint number to the constraint table
      constraint_table$Order <- 1:nrow(constraint_table)
      constraint_table_cols <- colnames(constraint_table)
      
      # Expanding "All" terms into corresponding dates or states
      # handle_all is a function to take care of that
      handle_all <- function(row,all_option_cols,row_colnames){
        names(row) <- row_colnames
        row_all_option <- row[all_option_cols]
        row_non_all <- row_all_option[which(row_all_option!='All')]
        row_all <- row_all_option[which(row_all_option=='All')]
        
        # handle values with out all
        # row_non_all_names <- names(row_non_all)
        row_non_all_df <- NULL
        row_all_df <- NULL
        
        if(length(row_non_all)>0){
          row_non_all <- row_non_all %>% map(~{
            values <- unlist(strsplit(.,','))
            return(values)
          })
          
          row_non_all_df <- data.frame(row_non_all[1],stringsAsFactors = F)
          
          if(length(row_non_all)>=2){
            for(i in 2:length(row_non_all)){
              check = data.frame(row_non_all[i],stringsAsFactors = F)
              row_non_all_df = merge(row_non_all_df,check)
            }
          }
        }
        
        # handle values with all
        if(length(row_all)>0){
          row_all <- names(row_all) %>% map(~{
            colname <- .
            values <- unique(hierarchy_tab[colname])
            return(values)
          })
          
          row_all_df <- data.frame(row_all[1],stringsAsFactors = F)
          
          if(length(row_all)>=2){
            for(i in 2:length(row_all)){
              check <- data.frame(row_all[i],stringsAsFactors = F)
              row_all_df <- merge(row_all_df,check)
            }
          }
        }
        
        # join 'non_all' and 'All' columns
        if(!is.null(row_all_df) & !is.null(row_non_all_df))
          final_df <- merge(row_all_df,row_non_all_df)
        else if(!is.null(row_all_df))
          final_df <- row_all_df
        else
          final_df <- row_non_all_df
        
        # hierarchy mapping
        panel_hierarchy <- unique(hierarchy_tab[,reactData$panelVar,drop=FALSE])
        
        # Merge with hierarchy mapping
        final_df <- merge(final_df,panel_hierarchy,on=row_all_option)
        
        # Write other columns as well
        other_cols <- setdiff(row_colnames,all_option_cols)
        
        final_df[,other_cols] <- other_cols %>% map(~{row[.]})
        
        return(final_df)
      }
      
      ## Handling Date column in constraint_table to replace "All"
      constraint_table[reactData$DateVar] <- paste(sapply(reactData$optiDateSequence[reactData$DateVar], as.character), collapse = ",")
      
      # Other Colnames which need to replace 'All' using handle_all function
      all_col_names <- c(reactData$DateVar, reactData$panelVar)
      constraint_table2 <- apply(constraint_table, 1, FUN = handle_all, all_col_names, constraint_table_cols) %>% do.call(rbind,.) %>% data.frame(stringsAsFactors = FALSE)
      
      ## Creating formula from LHS and RHS
      createFormula <- function(row, cols){
        #
        LHS <- which(cols == "LHS")
        RHS <- which(cols == "RHS")
        Compare <- which(cols == "Compare")
        
        formula <- switch(row[Compare],
                          "<"=paste0(row[RHS], "-", row[LHS]),
                          ">"=paste0(row[LHS], "-", row[RHS]))
        
      }
      ## Formula Column added to the constraint Table
      constraint_table2$formula <- apply(constraint_table2, 1, FUN = createFormula, constraint_table_cols)
      
      ## Importing number_r column from hierarchy tab
      x_join_col <- c(reactData$DateVar, reactData$panelVar)
      y_join_col <- c(reactData$DateVar, reactData$panelVar)
      
      constraint_table2 <- merge(x = hierarchy_tab, y = constraint_table2, by.x = x_join_col, by.y = y_join_col , all.x = TRUE)
      reactData$constraint_table2 <- constraint_table2
      # Merge with hierarchy mapping
      #final_df <- merge(final_df,panel_hierarchy,on=row_all_option)
      
      #####################44444444444444444444444######################$$$$$$$$$$$$$$$$$$$$$$$$$$
      
      bound <- reactData$bound
      bound$Order <- 1:nrow(bound)
      # col_names <- colnames(bound)
      bound_table2 <- merge(x = hierarchy_tab, y = bound, by.x = x_join_col, by.y = y_join_col , all.x = TRUE)
      reactData$bound_table2 <- bound_table2
      ###### Creating extradataset for the model
      ## master dataset
      dates_dataset <- cbind(reactData$histDateSequence[reactData$DateVar], reactData$optiDateSequence[reactData$DateVar])
      colnames(dates_dataset) <- c("Hist_Date", reactData$DateVar)
      ## 
      extra_data <- merge(dates_dataset, reactDataOpt$dataset, by.x = "Hist_Date", by.y = reactData$DateVar, all.x = TRUE)
      drops <- c("Hist_Date", mmxModel$full_object$y_variable, reactData$spendVar)
      extra_data <- extra_data[, !(names(extra_data) %in% drops)]
      switch (!is.null(reactData$timeOrderVar),
              extra_data[reactData$timeOrderVar] <- extra_data[reactData$timeOrderVar] + reactData$numDateFrequency
      )
      ### List of panel variables on which we need to join
      switch (is.null(reactData$mergeVar),
              reactData$mergeVar <- reactData$DateVar,
              reactData$mergeVar <- c(reactData$mergeVar, reactData$DateVar)
      )
      ### Conditional merge in case external dataset is uploaded
      switch (nrow(extReactData$dataset)<1,
              extra_data,
              extra_data <- merge(extra_data, extReactData$dataset, by=c(reactData$mergeVar), all.x=TRUE)
      )
      
      reactData$extraData <- extra_data
      
      ###### Creating a seperate environment to save variables, in order to access them in nessted functions
      # OFN <- new.env()
      cob_out <- optimization_fn(businessObjective = input$selectedObjective
                                 , targetRevenue = as.numeric(input$targetRevenue)
                                 , optiTechnique = input$selectedTechnique
                                 , model = mmxModel$model
                                 , maxIteration = input$maxIteration
                                 , spendVar = reactData$spendVar
                                 , DateVar = reactData$DateVar
                                 , panelVar = reactData$panelVar
                                 , bound_table_dataset = bound_table2
                                 , constraint_table_dataset = constraint_table2
                                 , extraData = reactData$extraData
                                 , train_data = mmxModel$train_data
                                 , hierarchy_tab = reactData$hierarchy_tab
                                 , historical_dataset = reactDataOpt$dataset
                                 , histDateSequence = reactData$histDateSequence
                                 , optiDateSequence = reactData$optiDateSequence
                                 , spend_variables_seq = as.character(adstock$dataset$Spend_Channel)
                                 , decay_rate_dataset = adstock$dataset$Adstock_Decay)
      
      reactData$cob_out <- cob_out
      reactData$optimal_value <- round(cob_out$value, 2)
      #reactData$optimal_spends <-  as.data.frame(cob_out$par)
      reactData$final_message <- cob_out$message
      output$value <- renderText(reactData$final_message)
      #output$message <- renderText(HTML(paste0(reactData$final_message)))
      
      ##################### Dataset preparation for Visualization/ Results
      # browser()
      optimised_dataset <- build_dataset(cob_out$par)
      pred <- predict(mmxModel$model,optimised_dataset)
      #optimised_dataset$Sales <- as.vector(pred)
      
      optimised_dataset[reactData$targetVar] <- as.vector(pred)
      
      
      # browser()
      ######## Apply Adstock
      ### Apply Add Stock to Optimised Dataset
      req_col_lvl_of_data <- unique(c(reactData$DateVar,reactData$panelVar, colnames(reactData$extraData)))
      req_col_to_melt_data <- unique(c(mmxModel$full_object$y_variable, reactData$spendVar))

      ##### Creating Master Dataset
      whole_data_long <- fn_rbind_data(hist_dataset = data.frame(reactDataOpt$dataset, stringsAsFactors = FALSE)
                                        , opti_dataset = optimised_dataset
                                        , lvl_of_data = req_col_lvl_of_data
                                        , melt_data = req_col_to_melt_data)
      whole_data_wide <- spread(whole_data_long
                                 , key = "variable"
                                 , value = "value")
      adstock_optimised_dataset <- apply_adstock(dataset=whole_data_wide,
                                                 spend_variables=as.character(adstock$dataset$Spend_Channel),
                                                 decay_rate=adstock$dataset$Adstock_Decay,
                                                 time_variable=reactData$DateVar,
                                                 adstock_panel_variables=reactData$panelVar)
      adstock_optimised_dataset <- merge(reactData$optiDateSequence[reactData$DateVar], adstock_optimised_dataset, by = reactData$DateVar, all.x = T)
      adstock_optimised_dataset <- adstock_optimised_dataset %>% arrange_at(c(reactData$DateVar, reactData$panelVar))
      pred <- predict(mmxModel$model,adstock_optimised_dataset)
      
      optimised_dataset <- optimised_dataset %>% arrange_at(c(reactData$DateVar, reactData$panelVar))
      #optimised_dataset$Sales <- as.vector(pred)
      
      optimised_dataset[reactData$targetVar] <- as.vector(pred)
      
      reactData$optimised_dataset <- optimised_dataset
      
      ########################### Creating Datasets for Scenario Comparison ###############################
      ############################################################################
      ### Scenario Historical Datsaet
      # browser()
      #Historical Data Wide Format
      reactData$scenario_histData_wide <- merge(reactData$histDateSequence[reactData$DateVar], reactDataOpt$dataset, by = reactData$DateVar, all.x = TRUE)
      #Historical Data long Format
      reactData$scenario_histData_long <- melt(reactData$scenario_histData_wide, measure.vars = (c(reactData$spendVar)))
      setnames(reactData$scenario_histData_long, old = c("variable", "value"), new = c("Spend_Channel", "Spends"))
      reactData$scenario_optimised_dataset <- reactData$optimised_dataset
      
    },error= function(e) showNotification(paste0('Error in Running Optimization: ',e[1]), type='error'))
  })
  
  
  ########################### UI to add optimisation scenario to Scenario- list for Comparison ###############################
  ######################################################################################################################
  
  output$add_opti_scenario_ui <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(12, h2('Save Generated Scenario')),
                    shiny::column(4,shiny::textInput('opti_scenarioName','Enter Scenario Name',value = "Optimal_Scenario_1")),
                    shiny::column(4,shiny::textInput('opti_scenarioDescription','Enter Scenario Description',value = "10%_Increment")),
                    shiny::column(3, style='padding-top:25px', class='button_pad', shiny::actionButton('addOptiScenario','Submit'))
    )
  })
  
  #browser()
  output$opti_scenarioTable_UI <- DT::renderDataTable(DT::datatable(reactData$opti_scenarioNameTable),options=list(scrollX=T, pagelength=6))
  
  
  observeEvent(input$addOptiScenario,{
    tryCatch({
      # browser()
      reactData$scenarioName <- input$opti_scenarioName
      ### In case if the user tries to add multiple scenarios with same name
      if(reactData$scenarioName %in% reactData$opti_scenarioNameTable$Scenario | reactData$scenarioName %in% scenarioTable$dataset$Scenario_Name){shiny::showNotification('Scenario already exists with the same name! Please save scenario with a different name', type='error'); return()}
      ######################################################################################
      ########### To Create Opti Table After Savign Opti Scenarios #########################
      ######################################################################################
      reactData$scenarioDescription <- input$opti_scenarioDescription
      combinedColumn <- data.frame(cbind(reactData$scenarioName, reactData$scenarioDescription), stringsAsFactors = FALSE)
      colnames(combinedColumn) <- c("Scenario", "Description")
      if(!is.null(reactData$opti_scenarioNameTable)){
        reactData$opti_scenarioNameTable <- rbind(reactData$opti_scenarioNameTable, combinedColumn)
      }else{
        reactData$opti_scenarioNameTable <- combinedColumn
      }
      
      reactData$opti_scenarioNameTable <- data.frame(reactData$opti_scenarioNameTable, stringsAsFactors = FALSE)
      
      ## waypoint
      
      scenario_history_df$data[nrow(scenario_history_df$data) + 1,] = c(reactData$scenarioName,reactData$scenarioDescription ,  "Optimization Scenario", shinyInput_custom(actionButton, nrow(scenario_history_df$data)+1 , 'button_', label = "View", class = "third_button",onclick = 'Shiny.onInputChange(\"chill_button\",  this.id)' ))
      
      #browser()
      
      
      ########################################################################################################
      ##############################END--------END----------END###############################################
      ########################################################################################################
      
      ######## Save scenario details to list #######
      listToWrite = list(optimising_dataset = shiny::reactiveValuesToList(reactData, all.names = T), all_dataset = shiny::reactiveValuesToList(reactDataOpt, all.names = T), adstock_dataset=shiny::reactiveValuesToList(adstock, all.names=T))
      # compareScenarioList$files[[opti_scenarioName]]$scheduling_dataset$optimised_dataset <- reactData$optimised_dataset
      optimisationScenarioList$files[[reactData$scenarioName]] <- listToWrite
      
      #### Saving optimised scenarios in the system
      # saveRDS(optimisationScenarioList$files[[reactData$scenarioName]], file = paste0("Scenarios/", c(reactData$scenarioName), ".RDS"))
      
      #### Compare Scenario Part 
      optimisationScenarioTable <- as.data.frame(optimisationScenarioList$files %>% map(~{
        c(.$optimising_dataset$scenarioName)
      }) %>% do.call(rbind,.))
      # browser()
      simulationScenarioTable <- as.data.frame(scenarioList$files %>% map(~{
        c(.$scheduling_dataset$scenarioName)
      }) %>% do.call(rbind,.))
      
      if(nrow(optimisationScenarioTable)<=0 | is.null(optimisationScenarioTable))
      {
        compareScenarioTable$dataset <- simulationScenarioTable
      }
      if(nrow(simulationScenarioTable)<=0 | is.null(simulationScenarioTable))
      {
        compareScenarioTable$dataset <- optimisationScenarioTable
      }
      
      compareScenarioTable$dataset <- rbind(optimisationScenarioTable, simulationScenarioTable)
      
      colnames(compareScenarioTable$dataset) <- c("Scenario_Name")
      # unique(compareScenarioTable$dataset$Scenario_Name)
    },error= function(e) showNotification(paste0('Error while adding optimization scenarios to the list: ',e[1]), type='error'))
  })
  
  #################################################################
  ################ Delete Optimisation Scenario ###################
  #################################################################
  
  shiny::observeEvent(input$opti_scenario_delete,{
    tryCatch({
      ######## Progress bar #########
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Processing...")
      # browser()
      ####### Error handling ###########
      if(is.null(reactData$opti_scenarioNameTable)){shiny::showNotification('No scenarios found', type='error'); return()}
      # browser()
      rows <- input$opti_scenarioTable_UI_rows_selected
      if(length(rows)==0) return()
      ## Deleting Data from backend
      dat <- reactData$opti_scenarioNameTable
      rownames(dat) <- NULL
      deleted_dat <- dat[c(rows),]
      optimisationScenarioList$files[[deleted_dat$Scenario]] <- NULL
      updated_dat <- dat[-c(rows),]
      reactData$opti_scenarioNameTable <- updated_dat
      
      #### Compare Scenario Part 
      optimisationScenarioTable <- as.data.frame(optimisationScenarioList$files %>% map(~{
        c(.$optimising_dataset$scenarioName)
      }) %>% do.call(rbind,.))
      # browser()
      simulationScenarioTable <- as.data.frame(scenarioList$files %>% map(~{
        c(.$scheduling_dataset$scenarioName)
      }) %>% do.call(rbind,.))
      
      if(nrow(optimisationScenarioTable)<=0 | is.null(optimisationScenarioTable))
      {
        compareScenarioTable$dataset <- simulationScenarioTable
      }
      if(nrow(simulationScenarioTable)<=0 | is.null(simulationScenarioTable))
      {
        compareScenarioTable$dataset <- optimisationScenarioTable
      }
      
      compareScenarioTable$dataset <- rbind(optimisationScenarioTable, simulationScenarioTable)
      
      colnames(compareScenarioTable$dataset) <- c("Scenario_Name")
      
      
    },error=function(e) showNotification(paste0('Delete Optimal Scenario :',e[1])))
  })
  
  ########################### Plots for Optimisation Summary Output ###############################
  ################################################################################################
  
  output$chooseOptiScenarioSelectionMode <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(12,shiny::radioButtons('selectOptiMode', 'Select Scenario Mode', c("View Generated Scenarios", "Upload Scenario RDS"), inline = TRUE,  width = NULL))
    )
  })
  
  ### Conductor for loading External Optimization Scenario RDS files
  optiScenarioObject <- shiny::reactive({
    infile <- input$optiScenarioFile
    if(is.null(infile))
    {
      # User has not uploaded a file yet
      return()
    }
    else
    {
      if(file_ext(infile$datapath)!='RDS')
      {
        shiny::showNotification('Please load only RDS files', type='error');
        return()
      }
      objectList <- readRDS(infile$datapath)
      return(objectList)
    }
  })

  ### This nugget is for when you upload an Optimization Scenario RDS file
  shiny::observeEvent(input$submitUploadedOptiScenario,{
    tryCatch({
      # browser()
      scenarioObjectList <- optiScenarioObject()
      ## Updating in the list available
      optimisationScenarioList$files[[scenarioObjectList$optimising_dataset$scenarioName]] <- scenarioObjectList
      # optimisationScenarioTable$dataset <- rbind(optimisationScenarioTable$dataset, data.frame(Scenario_Name = c(scenarioObjectList$optimising_dataset$scenarioName), Scheduling_Method= c("X")) )
      #############################################
      ### Updating Comapre Scenario Table #########
      #############################################
      optimisationScenarioTable <- as.data.frame(optimisationScenarioList$files %>% map(~{
        c(.$optimising_dataset$scenarioName)
      }) %>% do.call(rbind,.))

      simulationScenarioTable <- as.data.frame(scenarioList$files %>% map(~{
        c(.$scheduling_dataset$scenarioName)
      }) %>% do.call(rbind,.))

      if(nrow(optimisationScenarioTable)<=0 | is.null(optimisationScenarioTable))
      {
        compareScenarioTable$dataset <- simulationScenarioTable
      }
      if(nrow(simulationScenarioTable)<=0 | is.null(simulationScenarioTable))
      {
        compareScenarioTable$dataset <- optimisationScenarioTable
      }

      compareScenarioTable$dataset <- rbind(optimisationScenarioTable, simulationScenarioTable)
      compareScenarioTable$dataset <- unique(compareScenarioTable$dataset)
      colnames(compareScenarioTable$dataset) <- c("Scenario_Name")
      #######################################################################
      #######################################################################

      ## Updating Dataset variable for result processing
      reactData <- scenarioObjectList$optimising_dataset
      reactDataOpt <- scenarioObjectList$all_dataset
      adstock <- scenarioObjectList$adstock_dataset
      # browser()
      output$uploadedOptiScenarioText <- shiny::renderText(paste0(
        "Scenario :  ", input$optiScenarioFile$name, " has been laoded successfuly.\n"))
    },error=function(e) showNotification(paste0('View input$submitUploadedOptiScenario :',e[1])))
  })
  
  output$scenarioSelect <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(4,shiny::selectInput("chooseOptiScenario", "Select Scenario",
                                                       choices = reactData$opti_scenarioNameTable$Scenario,
                                                       multiple = FALSE, selected = viewValue$selected))
                    ,shiny::column(2, style='padding-top:25px', class='button_pad', shiny::actionButton('submittedOptiScenario','Submit'))
    )
  }) 
  
  observeEvent(input$submittedOptiScenario,{
    scenarioName <- input$chooseOptiScenario
    
    reactData$optimised_dataset <- optimisationScenarioList$files[[scenarioName]]$optimising_dataset$optimised_dataset
  })
  
  output$optimisedResults <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(4,shiny::selectInput("choosePanel", "Select Panel",
                                                       choices = c("", "Total", unique(reactDataOpt$dataset[reactData$panelVar])),
                                                       multiple = FALSE, selected = 'Total'))
                    ,shiny::column(2, style='padding-top:25px', class='button_pad', shiny::actionButton('submittedPanel','Submit'))
                    
    )
  })
  
  shiny::observeEvent(input$submittedPanel,{
    #browser()
    tryCatch({
    #progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    #on.exit(progress$close())
    #progress$set(message = "Loading Results...")
      
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Loading Results...")
      # browser()
      #### Input Variables
      panelSelected <- input$choosePanel

      #############Getting Historical/Optimized Datasets
      hist_dataset_contrib <- data.frame(reactDataOpt$dataset, stringsAsFactors = FALSE)
      optimised_dataset <- reactData$optimised_dataset
      optimal_dataset_contrib <- optimised_dataset

      ######### Necessary Columns
      contrib_req_col <- unique(c(reactData$DateVar,reactData$panelVar, colnames(reactData$extraData)))

      ##### Processsing historical and optimized datasets :: Merege two datasets
      ##### Creating list to variables to be passed
      req_col_lvl_of_data <- unique(c(reactData$DateVar,reactData$panelVar, colnames(reactData$extraData)))
      req_col_to_melt_data <- unique(c(mmxModel$full_object$y_variable, reactData$spendVar))

      ##### Creating Master Dataset
      master_data_long <- fn_rbind_data(hist_dataset = hist_dataset_contrib
                                        , opti_dataset = optimal_dataset_contrib
                                        , lvl_of_data = req_col_lvl_of_data
                                        , melt_data = req_col_to_melt_data)
      master_data_wide <- spread(master_data_long
                                 , key = "variable"
                                 , value = "value")
      ##### Applying adstock to total data
      allAdstockData <- apply_adstock(dataset = as.data.frame(master_data_wide), 
                                      spend_variables = as.character(adstock$dataset$Spend_Channel), 
                                      decay_rate = adstock$dataset$Adstock_Decay, 
                                      time_variable = reactData$DateVar, 
                                      adstock_panel_variables = reactData$panelVar)
      #### Calculating Contributions
      contributionData_timeSeries <- fn_contrib(mmx_model = mmxModel$model
                                                , master_dataset = allAdstockData
                                                , spendVar = reactData$spendVar
                                                , req_contrib_cols = c(reactData$DateVar, reactData$panelVar)
                                                , DateVar = reactData$DateVar
                                                , panelVar = reactData$panelVar)
      

      #### Saving Dataset to Reactive element
      reactData$master_data_long <- master_data_long
      reactData$master_data_wide <- master_data_wide
      reactData$contributionData_timeSeries <- contributionData_timeSeries

      ############### Overall Spends and Contibution Datasets - Filtered for Selected Panel #############################################################################
      summary_spends_dataset <- fn_filter_dataset(dataset = master_data_long, filter_col = reactData$panelVar, filter_val = panelSelected, flag = 1)
      reactData$summary_spends_dataset <- summary_spends_dataset
      summary_contrib_dataset <- fn_filter_dataset(dataset = contributionData_timeSeries, filter_col = c("variable"), filter_val = c(reactData$spendVar, "baseline"), flag = 1)
      reactData$summary_contrib_dataset <- summary_contrib_dataset
      ##########################################################################################################################################
      
     
      
      #########################################################################################
      ################### Optimization Summary Tab
      #########################################################################################
      
      ################ Tabular View 
      tabularViewTable <- fn_filter_dataset(dataset = summary_spends_dataset, filter_col = reactData$DateVar, filter_val = reactData$optiDateSequence[reactData$DateVar][,1], flag = 1)
      tabularViewTable <- spread(tabularViewTable, key = "variable", value = "value")
      nums <-  unlist(lapply(tabularViewTable, is.numeric))
      output$opti_data_table <- DT::renderDataTable(DT::datatable(roundOffDataFrame((tabularViewTable),TRUE,2),options=list(scrollX=T, searching=TRUE, dom = 't'))%>%
                                                      formatCurrency(colnames(tabularViewTable[nums]),currency = "", interval = 3, mark = ","))
      #### Sales Dataset and Plots 
      ######### Sales Panel Plot (All Panel Plotted Individually)
      summarySalesPanelData <- fn_filter_dataset(dataset = summary_spends_dataset, filter_col = "variable", filter_val = reactData$spendVar, flag = 0)
      summarySalesPanelData <- fn_filter_dataset(dataset = summarySalesPanelData, filter_col = reactData$DateVar, filter_val = reactData$optiDateSequence[reactData$DateVar][,1], flag = 1)
      ## Plot
      
      allggSalesPlot <- summarySalesPlot(dataset = summarySalesPanelData, chartTitle = "Sales",xvar = reactData$DateVar, xlab = "Time", yvar = "value", ylab = "Sales", lvar = reactData$panelVar, panelLabel = "All", channelLabel = "All")
      allggSalesPlot <- allggSalesPlot + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
      output$allSalesPlot <- renderPlot({allggSalesPlot})
      ######### Sales Plot (Aggregated Value of plot)
      summarySalesData <- summarySalesPanelData %>% group_by_at(c(reactData$DateVar, "variable")) %>% summarise(value = sum(value)) %>% data.frame(stringsAsFactors = F)
      ## Plot
      ggSalesPlot <- summarySalesPlot(dataset = summarySalesData, chartTitle = "Sales",xvar = reactData$DateVar, xlab = "Time", yvar = "value", ylab = "Sales", lvar = "variable", panelLabel = "Total")
      ggSalesPlot <- ggSalesPlot + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
      output$SalesPlot <- renderPlot({ggSalesPlot})
      
      #### Spends Dataset and Plot
      summarySpendsData <- fn_filter_dataset(dataset = summary_spends_dataset, filter_col = reactData$DateVar, filter_val = reactData$optiDateSequence[reactData$DateVar][,1], flag = 1)
      summarySpendsData <- fn_filter_dataset(dataset = summarySpendsData, filter_col = "variable", filter_val = reactData$spendVar, flag = 1)
      summarySpendsData <- fn_filter_dataset(dataset = summarySpendsData, filter_col = reactData$panelVar, filter_val = panelSelected, flag = 1)
      summarySpendsData <- summarySpendsData %>% group_by_at(c(reactData$DateVar,"variable")) %>% summarise(value = sum(value)) %>% data.frame(stringsAsFactors = FALSE) %>% arrange(variable)
      #### Percent Contribution Dataset
      summarySpendsData <- summarySpendsData %>% group_by_at(c(reactData$DateVar)) %>% mutate(agg_value = sum(value)) %>% mutate(pct_value = round((value/agg_value)*100, 2)) %>% data.frame(stringsAsFactors = FALSE) 
      ## Plot
      
      ggSpendAreaPlot <- summaryAreaPlot(dataset = summarySpendsData, chartTitle = "Spends",xvar = reactData$DateVar, xlab = "Time", yvar = "value", ylab = "Spends", lvar = "variable", panelLabel = panelSelected)
      ggSpendAreaPlot <- ggSpendAreaPlot + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
      output$SpendAreaPlot <- renderPlot({ggSpendAreaPlot})
      # ggSalesPlot, ggSpendAreaPlot, ggContribution, ggpctContribution, ggROI, ncol=2
      ggpctSpendAreaPlot <- summaryAreaPlot(dataset = summarySpendsData, chartTitle = "Spends",xvar = reactData$DateVar, xlab = "Time", yvar = "pct_value", ylab = "Spends", lvar = "variable", panelLabel = panelSelected)
      output$pctSpendAreaPlot <- renderPlot({ggpctSpendAreaPlot})
      #### Contribution Dataset and Plot
      summaryContribData <- fn_filter_dataset(dataset = summary_contrib_dataset, filter_col = reactData$DateVar, filter_val = reactData$optiDateSequence[reactData$DateVar][,1], flag = 1)
      summaryContribData <- fn_filter_dataset(dataset = summaryContribData, filter_col = "variable", filter_val = c(reactData$spendVar, "baseline"), flag = 1)
      summaryContribData <- fn_filter_dataset(dataset = summaryContribData, filter_col = reactData$panelVar, filter_val = panelSelected, flag = 1)
      summaryContribData <- summaryContribData %>% group_by_at(c(reactData$DateVar,"variable")) %>% summarise(value = sum(value)) %>% data.frame(stringsAsFactors = FALSE)
      #### Percent Contribution Dataset
      summaryContribData <- summaryContribData %>% group_by_at(c(reactData$DateVar)) %>% mutate(agg_value = sum(value)) %>% mutate(pct_value = round((value/agg_value)*100, 2)) %>% data.frame(stringsAsFactors = FALSE)
      ## Plot
      ggContribution <- summaryAreaPlot(dataset = summaryContribData, chartTitle = "Contribution",xvar = reactData$DateVar, xlab = "Time", yvar = "value", ylab = "Contribution", lvar = "variable", panelLabel = panelSelected)
      ggContribution <- ggContribution + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
      output$Contribution <- renderPlot({ggContribution})
      # ggSalesPlot, ggSpendAreaPlot, ggContribution, ggpctContribution, ggROI, ncol=2
      ggpctContribution <- summaryAreaPlot(dataset = summaryContribData, chartTitle = "Contribution",xvar = reactData$DateVar, xlab = "Time", yvar = "pct_value", ylab = "Contribution (in %)", lvar = "variable", panelLabel = panelSelected)
      output$pctContribution <- renderPlot({ggpctContribution})
      
      print("1....................................................")
      # browser()
      ###### ROI 
      roi_spend_data <- fn_filter_dataset(dataset = summary_spends_dataset, filter_col = reactData$DateVar, filter_val = reactData$optiDateSequence[reactData$DateVar][,1], flag = 1)
      roi_spend_data <- fn_filter_dataset(dataset = roi_spend_data, filter_col = "variable", filter_val = reactData$spendVar, flag = 1)
      roi_spend_data <- roi_spend_data  %>% group_by_at(c(reactData$DateVar,"variable")) %>% summarise(value = sum(value)) %>% data.frame(stringsAsFactors = FALSE)
      roi_contrib_data <- fn_filter_dataset(dataset = summary_contrib_dataset, filter_col = reactData$DateVar, filter_val = reactData$optiDateSequence[reactData$DateVar][,1], flag = 1)
      roi_contrib_data <- fn_filter_dataset(dataset = roi_contrib_data, filter_col = "variable", filter_val = reactData$spendVar, flag = 1)
      roi_contrib_data <- roi_contrib_data  %>% group_by_at(c(reactData$DateVar,"variable")) %>% summarise(value = sum(value)) %>% data.frame(stringsAsFactors = FALSE)
      roi_data <- merge(roi_contrib_data, roi_spend_data, by = c(reactData$DateVar, "variable"), all.x = T) %>% mutate(ROI = round((value.x/value.y),2))
      ## Plot
      ggROI <- summarySalesPlot(dataset = roi_data, chartTitle = "ROI",xvar = reactData$DateVar, xlab = "Time", yvar = "ROI", ylab = "ROI", lvar = "variable", panelLabel = panelSelected)
      output$ROI <- renderPlot({ggROI})
      
      print("2....................................................")
      
      #########################################################################################
      ################### Summary Comparison Tab
      #########################################################################################
      
      salesData <- fn_filter_dataset(dataset = summary_spends_dataset, filter_col = "variable", filter_val = reactData$spendVar, flag = 0)
      histSalesData <- fn_filter_dataset(dataset = salesData, filter_col = reactData$DateVar, filter_val = c(reactData$histDateSequence[reactData$DateVar][,1]), flag = 1) %>% mutate(duration = "Historical")
      optiSalesData <- fn_filter_dataset(dataset = salesData, filter_col = reactData$DateVar, filter_val = c(reactData$optiDateSequence[reactData$DateVar][,1]), flag = 1) %>% mutate(duration = "Optimized")
      salesComparisonData <- rbind(histSalesData, optiSalesData)
      salesComparisonData <- salesComparisonData %>% group_by_at(c("duration", "variable")) %>% summarise(value = sum(value)) %>% data.frame(stringsAsFactors = F)
      ggsalesComparison <- fn_bar_graph(dataset =  salesComparisonData, chartTitle = "Sales Comparison" , xvar = "variable", xlab = "", yvar = "value", ylab = "Sales", lvar = "duration", panelLabel = panelSelected) + 
        scale_y_continuous(labels=scales::dollar_format(prefix="$")) +
        geom_text(aes(label = dollar(value)), position=position_dodge(width=0.9), vjust=-0.25)
      output$salesComparison <- renderPlot({ggsalesComparison})
      
      spendsData <- fn_filter_dataset(dataset = summary_spends_dataset, filter_col = "variable", filter_val = reactData$spendVar, flag = 1)
      histSpendsData <- fn_filter_dataset(dataset = spendsData, filter_col = reactData$DateVar, filter_val = c(reactData$histDateSequence[reactData$DateVar][,1]), flag = 1) %>% mutate(duration = "Historical")
      optiSpendsData <- fn_filter_dataset(dataset = spendsData, filter_col = reactData$DateVar, filter_val = c(reactData$optiDateSequence[reactData$DateVar][,1]), flag = 1) %>% mutate(duration = "Optimized")
      spendsComparisonData <- rbind(histSpendsData, optiSpendsData)
      spendsComparisonData <- spendsComparisonData %>% group_by_at(c("duration", "variable")) %>% summarise(value = sum(value)) %>% data.frame(stringsAsFactors = F)
      ggspendsComparison <- fn_bar_graph(dataset =  spendsComparisonData, chartTitle = "Spends Comparison" , xvar = "variable", xlab = "Channel", yvar = "value", ylab = "Spends", lvar = "duration", panelLabel = panelSelected) + 
        scale_y_continuous(labels=scales::dollar_format(prefix="$")) +
        geom_text(aes(label = dollar(value)), position=position_dodge(width=0.9), vjust=-0.25)
      output$spendsComparison <- renderPlot({ggspendsComparison})
      
      conribData <- fn_filter_dataset(dataset = summary_contrib_dataset, filter_col = "variable", filter_val = c(reactData$spendVar, "baseline"), flag = 1)
      histcontribData <- fn_filter_dataset(dataset = conribData, filter_col = reactData$DateVar, filter_val = c(reactData$histDateSequence[reactData$DateVar][,1]), flag = 1) %>% mutate(duration = "Historical")
      opticontribData <- fn_filter_dataset(dataset = conribData, filter_col = reactData$DateVar, filter_val = c(reactData$optiDateSequence[reactData$DateVar][,1]), flag = 1) %>% mutate(duration = "Optimized")
      contribComparisonData <- rbind(histcontribData, opticontribData)
      contribComparisonData <- contribComparisonData %>% group_by_at(c("duration", "variable")) %>% summarise(value = sum(value)) %>% data.frame(stringsAsFactors = F)
      ggcontribComparison <- fn_bar_graph(dataset =  contribComparisonData, chartTitle = "Contribution Comparison" , xvar = "variable", xlab = "Channel", yvar = "value", ylab = "Contribution", lvar = "duration", panelLabel = panelSelected) + 
        scale_y_continuous(labels=scales::dollar_format(prefix="$")) +
        geom_text(aes(label = dollar(value)), position=position_dodge(width=0.9), vjust=-0.25)
      output$contribComparison <- renderPlot({ggcontribComparison})
      # browser()
      roiComparisonData <- merge(spendsComparisonData, contribComparisonData, by = c("duration", "variable"), all.x = T)
      roiComparisonData <- roiComparisonData %>% mutate(value = round((value.y/value.x), 2))
      ggroiComparison <- fn_bar_graph(dataset =  roiComparisonData, chartTitle = "ROI Comparison" , xvar = "variable", xlab = "Channel", yvar = "value", ylab = "ROI", lvar = "duration", panelLabel = panelSelected) + 
        geom_text(aes(label = format(round(value, 2), big.mark = ",")), position=position_dodge(width=0.9), vjust=-0.25)
      output$roiComparison <- renderPlot({ggroiComparison})
      
      ## Combining Optimisation Summary Tab Graphs for download
      grobOptiSummaryTab_Plots <- arrangeGrob(ggSalesPlot, allggSalesPlot, ggSpendAreaPlot, ggpctSpendAreaPlot, ggContribution, ggpctContribution, ggROI, ncol=2)
      
      ####### Graphs for Report
      reactData$reportSalesSummary <- ggSalesPlot
      reactData$reportallSalesSummary <- allggSalesPlot
      reactData$reportSpendsSummary <- ggSpendAreaPlot
      reactData$reportpctSpendsSummary <- ggpctSpendAreaPlot
      reactData$reportContributionSummary <- ggContribution
      reactData$reportpctContributionSummary <- ggpctContribution
      reactData$reportROISummary <- ggROI
      
      output$grobOptiSummaryTab_Plots_UI <- shiny::downloadHandler(
        filename = function(){'Optimised_Summary_Tab_Plots.png'},
        content = function(file) {
          ggsave(file, plot=grobOptiSummaryTab_Plots, device='png', width = 45, height = 25, units = 'cm')
        },
        shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
      )
      
      
      ### Downlaoding the Opti Summary  Data
      output$downloadOptiSummaryData <- downloadHandler(
        filename = function() {
          paste("OptiSummaryData", ".xlsx", sep = "")
        },
       
        content = function(file) {
          print(file)
          xlsx::write.xlsx(summarySalesPanelData, file, sheetName = "Total_Sales", row.names = FALSE)
          xlsx::write.xlsx(summarySalesData, file, sheetName = "All_Sales", append = TRUE, row.names = FALSE)
          xlsx::write.xlsx(summarySpendsData, file, sheetName = "Spends", append = TRUE, row.names = FALSE)
          xlsx::write.xlsx(summaryContribData, file, sheetName = "Contrib", append = TRUE, row.names = FALSE)
          # xlsx::write.xlsx(pctContributionData, file, sheetName = "pctContrib", append = TRUE, row.names = FALSE)
          xlsx::write.xlsx(roi_data, file, sheetName = "ROI", append = TRUE, row.names = FALSE)
        }
      )
     
      ## Combining Summary Comparison Tab Graphs for download
      grobSummaryComparisonTab_Plots <- arrangeGrob(ggsalesComparison, ggspendsComparison, ggcontribComparison, ggroiComparison, ncol=2)
      ####### Graphs for Report
      # reactData$reportSalesSpendsSC <- arrangeGrob(ggsalesComparison, ggspendsComparison, ncol=2)
      # reactData$reportContributionROISC <- arrangeGrob(ggcontribComparison, ggroiComparison, ncol=2)
      reactData$reportSalesSC <- ggsalesComparison
      reactData$reportSpendsSC <- ggspendsComparison
      reactData$reportContributionSC <- ggcontribComparison
      reactData$reportROISC <- ggroiComparison
      


      output$grobSummaryComparisonTab_Plots_UI <- shiny::downloadHandler(
        filename = function(){'Summary_Comparison_Tab_Plots.png'},
        content = function(file) {
          ggsave(file, plot=grobSummaryComparisonTab_Plots, device='png', width = 45, height = 25, units = 'cm')
        },
        shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv')
      )
      
      ### Downlaoding the Opti Comparison Data
      output$downloadOptiSummaryComparisonData <- downloadHandler(
        filename = function() {
          paste("OptiSummaryComparisonData", ".xlsx", sep = "")
        },
        content = function(file) {
          xlsx::write.xlsx(salesComparisonData, file, sheetName = "Sales", row.names = FALSE)
          xlsx::write.xlsx(spendsComparisonData, file, sheetName = "Spends", append = TRUE, row.names = FALSE)
          xlsx::write.xlsx(contribComparisonData, file, sheetName = "Contrib", append = TRUE, row.names = FALSE)
          xlsx::write.xlsx(roiComparisonData, file, sheetName = "ROI", append = TRUE, row.names = FALSE)
        }
      )
      
      ########################################################################################
      ############################# Creating Card for Output #################################
      ########################################################################################
      # browser()
      row1 <- salesComparisonData %>% group_by(duration) %>% summarise(value = sum(value)) %>% mutate(variable = "Sales") %>% data.frame(stringsAsFactors = F)
      row2 <- spendsComparisonData %>% group_by(duration) %>% summarise(value = sum(value)) %>% mutate(variable = "Spends") %>% data.frame(stringsAsFactors = F)
      row3 <- contribComparisonData %>% filter_at("variable",any_vars(.%in%reactData$spendVar)) %>%  group_by(duration) %>% summarise(value = sum(value)) %>% mutate(variable = "Contribution") %>% data.frame(stringsAsFactors = F)
      row4 <- roiComparisonData %>% group_by(duration) %>% summarise(value.x = sum(value.x), value.y = sum(value.y)) %>% mutate(variable = "ROI", value = round((value.y/value.x), 2)) %>% select(duration, variable, value) %>% data.frame(stringsAsFactors = F)
      
      Summary_Table <- rbind(row1, row2, row3, row4)
      Summary_Table <- spread(Summary_Table, key = "duration", value = "value")
      
      # browser()
      print("3..............................................")
      output$histCard_Spend <- shiny::renderUI({
        hist_spend <- Summary_Table[(which(Summary_Table$variable=="Spends")),"Historical"]
        hist_spend_char <- shiny::tags$p(shiny::HTML(paste0("$"," ",round(hist_spend/1e+06,1),"M")))
        return(hist_spend_char)
      })
      
      output$optiCard_Spend <- shiny::renderUI({
        sim_spend <- Summary_Table[(which(Summary_Table$variable=="Spends")),"Optimized"]
        sim_spend_char <- paste0("$"," ",as.character(round(sim_spend/1e+06,1)),"M")

        return(sim_spend_char)
      })

      output$histCard_Sales <- shiny::renderUI({
        hist_sales <- Summary_Table[(which(Summary_Table$variable=="Sales")),"Historical"]
        hist_sales_char <- paste0("$"," ",round(hist_sales/1e+06,1),"M")
        return(hist_sales_char)
      })

      output$optiCard_Sales <- shiny::renderUI({
        sim_sales <- Summary_Table[(which(Summary_Table$variable=="Sales")),"Optimized"]
        sim_sales_char <- paste0("$"," ",round(sim_sales/1e+06,1),"M")
        return(sim_sales_char)
      })

      output$histCard_Contrib <- shiny::renderUI({
        hist_contrib <- Summary_Table[(which(Summary_Table$variable=="Contribution")),"Historical"]
        hist_contrib_char <- paste0("$"," ",round(hist_contrib/1e+06,1),"M")
        return(hist_contrib_char)
      })

      output$optiCard_Contrib <- shiny::renderUI({
        sim_contrib <- Summary_Table[(which(Summary_Table$variable=="Contribution")),"Optimized"]
        sim_contrib_char <- paste0("$"," ",round(sim_contrib/1e+06,1),"M")
        return(sim_contrib_char)
      })

      output$histCard_ROI <- shiny::renderUI({
        hist_roi <- Summary_Table[(which(Summary_Table$variable=="ROI")),"Historical"]
        hist_roi_char <- paste0(round(hist_roi,1))
        return(hist_roi_char)
      })

      output$optiCard_ROI <- shiny::renderUI({
        sim_roi <- Summary_Table[(which(Summary_Table$variable=="ROI")),"Optimized"]
        sim_roi_char <- paste0(round(sim_roi,1))
        return(sim_roi_char)
      })
      
      print("Last.............................................")
      
    },error=function(e) showNotification(paste0('Error in Optimization results: ',e[1]),type='error'))
  })
  
  
  ########################### Plots for Optimisation Summary (Time-Series)Output ###############################
  ##############################################################################################################
  
  output$channelComparison <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(6,shiny::selectInput("chooseChannel", "Select Channel",
                                                       choices = c("", "Total", unique(reactData$spendVar)), multiple = FALSE, selected = 'Total'))
                    ,shiny::column(2, style='padding-top:25px', class='button_pad', shiny::actionButton('submittedChannel','Submit'))
    )
  })
  
  observeEvent(input$submittedChannel,{
    
    tryCatch({
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Loading Results...")
      ### Input Variables
      selectedChannel <- input$chooseChannel
      panelSelected <- input$choosePanel
      
      # browser()
      #### Retreiving Datasets from saved Reactive element
      master_data_long <- reactData$master_data_long
      master_data_wide <- reactData$master_data_wide 
      contributionData_timeSeries <- reactData$contributionData_timeSeries
      
      # browser()
      
      ############### Overall Spends and Contibution Datasets - Filtered for Selected Panel #############################################################################
      summary_spends_dataset <- reactData$summary_spends_dataset
      summary_contrib_dataset <- reactData$summary_contrib_dataset
      
      #########################################################################################
      ################### Time Series Tab's Overlay Plots 
      #########################################################################################
      
      salesData <- fn_filter_dataset(dataset = summary_spends_dataset, filter_col = "variable", filter_val = reactData$spendVar, flag = 0)
      histSalesData <- fn_filter_dataset(dataset = salesData, filter_col = reactData$DateVar, filter_val = c(reactData$histDateSequence[reactData$DateVar][,1]), flag = 1) %>% mutate(duration = "Historical")
      optiSalesData <- fn_filter_dataset(dataset = salesData, filter_col = reactData$DateVar, filter_val = c(reactData$optiDateSequence[reactData$DateVar][,1]), flag = 1) %>% mutate(duration = "Optimized")
      salesComparisonData <- rbind(histSalesData, optiSalesData)
      salesComparisonData <- salesComparisonData %>% group_by_at(c(reactData$DateVar, "duration")) %>% summarise(value = sum(value))
      salesComparisonData <- build_timeSeriesDataset(salesComparisonData, dateFrequency = reactData$dateFrequency, DateVar = reactData$DateVar)
      ## Plots
      ggsalesComparison_trend <-  timeSeriesOverlayPlot(dataset = salesComparisonData, chartTitle = "Sales", xvar = "Time_Num", xlab = "Time", ylab = "Total Sales", yvar = "value", lvar = "duration", panelLabel = input$choosePanel, channelLabel = "Total") +
        scale_y_continuous(labels=scales::dollar_format(prefix="$"))
      output$salesComparison_trend <- renderPlot({ggsalesComparison_trend})
      
      spendsData <- fn_filter_dataset(dataset = summary_spends_dataset, filter_col = "variable", filter_val = reactData$spendVar, flag = 1)
      spendsData <- fn_filter_dataset(dataset = spendsData, filter_col = "variable", filter_val = selectedChannel, flag = 1)
      histSpendsData <- fn_filter_dataset(dataset = spendsData, filter_col = reactData$DateVar, filter_val = c(reactData$histDateSequence[reactData$DateVar][,1]), flag = 1) %>% mutate(duration = "Historical")
      optiSpendsData <- fn_filter_dataset(dataset = spendsData, filter_col = reactData$DateVar, filter_val = c(reactData$optiDateSequence[reactData$DateVar][,1]), flag = 1) %>% mutate(duration = "Optimized")
      spendsComparisonData <- rbind(histSpendsData, optiSpendsData)
      spendsComparisonData <- spendsComparisonData %>% group_by_at(c(reactData$DateVar, "duration")) %>% summarise(value = sum(value)) %>% data.frame(stringsAsFactors = F)
      spendsComparisonData <- build_timeSeriesDataset(spendsComparisonData, dateFrequency = reactData$dateFrequency, DateVar = reactData$DateVar)
      
      ggspendsComparison_trend <-  timeSeriesOverlayPlot(dataset = spendsComparisonData, chartTitle = "Spends", xvar = "Time_Num", xlab = "Time", ylab = "Spends", yvar = "value", lvar = "duration", panelLabel = input$choosePanel, channelLabel = selectedChannel) +
        scale_y_continuous(labels=scales::dollar_format(prefix="$"))
      output$spendsComparison_trend <- renderPlot({ggspendsComparison_trend})
      
      conribData <- fn_filter_dataset(dataset = summary_contrib_dataset, filter_col = "variable", filter_val = reactData$spendVar, flag = 1)
      conribData <- fn_filter_dataset(dataset = conribData, filter_col = "variable", filter_val = selectedChannel, flag = 1)
      histcontribData <- fn_filter_dataset(dataset = conribData, filter_col = reactData$DateVar, filter_val = c(reactData$histDateSequence[reactData$DateVar][,1]), flag = 1) %>% mutate(duration = "Historical")
      opticontribData <- fn_filter_dataset(dataset = conribData, filter_col = reactData$DateVar, filter_val = c(reactData$optiDateSequence[reactData$DateVar][,1]), flag = 1) %>% mutate(duration = "Optimized")
      contribComparisonData <- rbind(histcontribData, opticontribData)
      contribComparisonData <- contribComparisonData %>% group_by_at(c(reactData$DateVar, "duration")) %>% summarise(value = sum(value)) %>% data.frame(stringsAsFactors = F)
      contribComparisonData <- build_timeSeriesDataset(contribComparisonData, dateFrequency = reactData$dateFrequency, DateVar = reactData$DateVar)
      
      ggcontribComparison_trend <-  timeSeriesOverlayPlot(dataset = contribComparisonData, chartTitle = "Contribution", xvar = "Time_Num", xlab = "Time", ylab = "Contribution", yvar = "value", lvar = "duration", panelLabel = input$choosePanel, channelLabel = selectedChannel) +
        scale_y_continuous(labels=scales::dollar_format(prefix="$"))
      output$contribComparison_trend <- renderPlot({ggcontribComparison_trend})
      
      #### except value(.x/.y) all .x and .y suffix columns are redundant and are not used anywhere in calculation
      roiComparisonData <- merge(contribComparisonData, spendsComparisonData, by = c(reactData$DateVar, "duration"), all.x = T)
      roiComparisonData <- roiComparisonData %>% mutate(value = round((value.x/value.y), 2))
      roiComparisonData <- build_timeSeriesDataset(roiComparisonData, dateFrequency = reactData$dateFrequency, DateVar = reactData$DateVar)
      
      ggroi_trend <-  timeSeriesOverlayPlot(dataset = roiComparisonData, chartTitle = "ROI", xvar = "Time_Num", xlab = "Time", ylab = "ROI", yvar = "value", lvar = "duration", panelLabel = input$choosePanel, channelLabel = selectedChannel) 
      output$roi_trend <- renderPlot({ggroi_trend})
      
      #########################################################################################
      ################### Time Series Tab's Continuous Plots 
      #########################################################################################
      
      salesContinuous_dataset <- fn_filter_dataset(dataset = summary_spends_dataset, filter_col = "variable", filter_val = reactData$spendVar, flag = 0)
      salesContinuous_dataset <- salesContinuous_dataset %>% group_by_at(c(reactData$DateVar)) %>% summarise(value = sum(value)) %>% data.frame(stringsAsFactors = F) %>% mutate(variable = "Sales")
      ggsalesContinuous_trend <- timeSeriesPlot(salesContinuous_dataset, chartTitle = "Sales", xvar = reactData$DateVar, xlab = "Time", ylab = "Total Sales", yvar = "value", opti_seq = reactData$optiDateSequence, hist_seq = reactData$histDateSequence, seq_xvar = reactData$DateVar, panelLabel = input$choosePanel, channelLabel = "All", legendVar="Optimization") + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
      output$salesContinuous_trend <- renderPlot({ggsalesContinuous_trend})
      
      spendsContinuous_dataset <- fn_filter_dataset(dataset = summary_spends_dataset, filter_col = "variable", filter_val = reactData$spendVar, flag = 1)
      spendsContinuous_dataset <- fn_filter_dataset(dataset = spendsContinuous_dataset, filter_col = "variable", filter_val = selectedChannel, flag = 1)
      spendsContinuous_dataset <- spendsContinuous_dataset %>% group_by_at(c(reactData$DateVar)) %>% summarise(value = sum(value)) %>% data.frame(stringsAsFactors = F) %>% mutate(variable = "Spends")
      ggspendsContinuous_trend <- timeSeriesPlot(spendsContinuous_dataset, chartTitle = "Spends", xvar = reactData$DateVar, xlab = "Time", ylab = "Total Spend", yvar = "value", opti_seq = reactData$optiDateSequence, hist_seq = reactData$histDateSequence, seq_xvar = reactData$DateVar, panelLabel = input$choosePanel, channelLabel = selectedChannel, legendVar="Optimization") + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
      output$spendsContinuous_trend <- renderPlot({ggspendsContinuous_trend})
      
      continuous_contrib_compare_dataset <- fn_filter_dataset(dataset = summary_contrib_dataset, filter_col = "variable", filter_val = reactData$spendVar, flag = 1)
      continuous_contrib_compare_dataset <- fn_filter_dataset(dataset = continuous_contrib_compare_dataset, filter_col = "variable", filter_val = selectedChannel, flag = 1)
      continuous_contrib_compare_dataset <- continuous_contrib_compare_dataset %>% group_by_at(c(reactData$DateVar)) %>% summarise(value = sum(value)) %>% data.frame(stringsAsFactors = F) %>% mutate(variable = "Contribution")
      ggcontribContinuous_trend <- timeSeriesPlot(continuous_contrib_compare_dataset, chartTitle = "Contribution", xvar = reactData$DateVar, xlab = "Time", ylab = "Total Contribution", yvar = "value", opti_seq = reactData$optiDateSequence, hist_seq = reactData$histDateSequence, seq_xvar = reactData$DateVar, panelLabel = input$choosePanel, channelLabel = selectedChannel, legendVar="Optimization") + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
      output$contribContinuous_trend <- renderPlot({ggcontribContinuous_trend})
      
      roi_continuous_dataset <- merge(continuous_contrib_compare_dataset, spendsContinuous_dataset, by = c(reactData$DateVar), all.x = TRUE) %>% mutate(value = round(value.x/value.y,2), variable = "ROI")
      ggroi_Continuous_trend <- timeSeriesPlot(roi_continuous_dataset, chartTitle = "ROI", xvar = reactData$DateVar, xlab = "Time", yvar = "value", ylab = "Total ROI", opti_seq = reactData$optiDateSequence, hist_seq = reactData$histDateSequence, seq_xvar = reactData$DateVar, panelLabel = input$choosePanel, channelLabel = selectedChannel, legendVar="Optimization")
      output$roi_Continuous_trend <- renderPlot({ggroi_Continuous_trend})
      
      ## Combining Time Series Comparison Tab Graphs for download
      grobTimeSeriesComparisonTab_Plots <- arrangeGrob(ggsalesComparison_trend, ggsalesContinuous_trend, ggspendsComparison_trend, ggspendsContinuous_trend, ggcontribComparison_trend, ggcontribContinuous_trend, ggroi_trend, ggroi_Continuous_trend, ncol=2)
      
      reactData$reportSalesTS <- ggsalesComparison_trend
      reactData$reportSalesContinuousTS <- ggsalesContinuous_trend
      reactData$reportSpendsTS <- ggspendsComparison_trend
      reactData$reportSpendsContinuousTS <- ggspendsContinuous_trend
      reactData$reportContributionTS <- ggcontribComparison_trend
      reactData$reportContributionContinuousTS <- ggcontribContinuous_trend
      reactData$reportROITS <- ggroi_trend
      reactData$reportROIContinuousTS <- ggroi_Continuous_trend
      
      output$grobTimeSeriesComparisonTab_Plots_UI <- shiny::downloadHandler(
        filename = function(){'Time_Series_Comparison_Tab_Plots.png'},
        content = function(file){
          ggsave(file, plot=grobTimeSeriesComparisonTab_Plots, device='png', width = 45, height = 25, units = 'cm')
        },
        shiny::showNotification('Download functionality is fully supported on the web browser. For RShiny internal browser, rename the file object and add relevant file-extension. eg: .png or .csv'))
      ### Downlaoding the Opti Comparison Time Series Data
      output$downloadOptiTimeSeriesData <- downloadHandler(
        filename = function() {
          paste("OptiTimeSeriesData", ".xlsx", sep = "")
        },
        content = function(file) {
          xlsx::write.xlsx(data.frame(salesComparisonData), file, sheetName = "Sales", row.names = FALSE)
          xlsx::write.xlsx(salesContinuous_dataset, file, sheetName = "Sales_Continuous", append = TRUE, row.names = FALSE)
          xlsx::write.xlsx(spendsComparisonData, file, sheetName = "Spends", append = TRUE, row.names = FALSE)
          xlsx::write.xlsx(spendsContinuous_dataset, file, sheetName = "Spends_Continuous", append = TRUE, row.names = FALSE)
          xlsx::write.xlsx(contribComparisonData, file, sheetName = "Contrib", append = TRUE, row.names = FALSE)
          xlsx::write.xlsx(continuous_contrib_compare_dataset, file, sheetName = "Contrib_Continuous", append = TRUE, row.names = FALSE)
          xlsx::write.xlsx(roiComparisonData, file, sheetName = "ROI", append = TRUE, row.names = FALSE)
          xlsx::write.xlsx(roi_continuous_dataset, file, sheetName = "ROI_Continuous", append = TRUE, row.names = FALSE)
        }
      )
      
    },error=function(e) showNotification(paste0('Error in Optimization results: Select Channel Nugget ',e[1]),type='error'))
  })
  
  shiny::observeEvent(input$reportDownload,{
    if(length(scenarioList$files)>0 |length(optimisationScenarioList$files)>0){
      tryCatch({
        output$downloadErrorMsg <- shiny::renderText("")
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        
        progress$set(message = "Downloading Report")
        fileName <- paste0('MMx_Simulation_and_Optimization_Report', format(Sys.time(),"%Y_%m_%d_%H_%M_%S"))
        format <- switch("HTML", PDF = ".pdf'", HTML = ".html'",PrettyHTML = "_pretty.html'", MSWord = ".doc'")
        
        start <- proc.time()
        ########## Delete this after you make the report file
        ########## Add additional reactive elements here
        mmxRDS <- list(
          format = format,
          reactData = shiny::reactiveValuesToList(reactData),
          histReactData = shiny::reactiveValuesToList(histReactData),
          reactDataOpt = shiny::reactiveValuesToList(reactDataOpt),
          input = shiny::reactiveValuesToList(input),
          mmxModel = shiny::reactiveValuesToList(mmxModel),
          adstock = shiny::reactiveValuesToList(adstock),
          scenarioList = shiny::reactiveValuesToList(scenarioList),
          scenarioTable = shiny::reactiveValuesToList(scenarioTable),
          viewScenario = shiny::reactiveValuesToList(scenarioTable),
          runPanelNonPctContribution = shiny::reactiveValuesToList(runPanelNonPctContribution),
          runPanel = shiny::reactiveValuesToList(runPanel),
          runPanelHist = shiny::reactiveValuesToList(runPanelHist),
          histPanelNonPctContribution = shiny::reactiveValuesToList(histPanelNonPctContribution),
          runPanelAll = shiny::reactiveValuesToList(runPanelAll),
          compareScenario = shiny::reactiveValuesToList(compareScenario),
          allPctContribDataScenario1 = shiny::reactiveValuesToList(allPctContribDataScenario1),
          allPctContribDataScenario2 = shiny::reactiveValuesToList(allPctContribDataScenario2),
          allNonPctContribDataScenario1 = shiny::reactiveValuesToList(allNonPctContribDataScenario1),
          allNonPctContribDataScenario2 = shiny::reactiveValuesToList(allNonPctContribDataScenario2),
          allSalesDataScenario1 = shiny::reactiveValuesToList(allSalesDataScenario1),
          allSalesDataScenario2 = shiny::reactiveValuesToList(allSalesDataScenario2)
        )
        # saveRDS(mmxRDS, file = paste0("mmxRDS",".RDS"))
        ########## Delete this after you make the report file
        
        downloadReport(mmxRDS , fileName)
        combinedData$filename<- fileName
        
        print(proc.time()-start)
        print(2)
        output$reportDownloadFinish <- shiny::renderText(paste0("Your report will be  downloaded as '",
                                                                paste(fileName, format,sep='')))
        
      },error=function(e)
      {
        reactData$downloadErrorMsg <- as.character(e)
        output$downloadErrorMsg <- shiny::renderText(as.character(e))
      })
    } 
    else{
      shiny::showNotification('Please first create a scenario', type='error'); 
      return()
    }
  })
  
  
  session$onSessionEnded(stopApp) 
  
})


