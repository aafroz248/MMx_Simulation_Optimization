optimisationResults <- div(div(class='card',div(class='container-fluid',
                                                column(11, h2('View Optimization Results')),
                                                column(12, uiOutput("viewOptimisationDates_atResults")),
                                                column(12, uiOutput("chooseOptiScenarioSelectionMode")),
                                                
                                                shiny::conditionalPanel('input.selectOptiMode=="View Generated Scenarios"',
                                                                      #######  column(12,uiOutput('scenarioSelect'))
                                                ),
                                                shiny::conditionalPanel('input.selectOptiMode=="Upload Scenario RDS"',
                                                                        column(style='margin-left:10px', 4, fileInput('optiScenarioFile', 'Upload Scenario RDS file'))
                                                                        , column(style='padding-top:25px', class='button_pad', 2, actionButton("submitUploadedOptiScenario", "Submit" ))
                                                                        , column(12, shiny::div(style="color:green", shiny::textOutput("uploadedOptiScenarioText")))
                                                ),
                                                ###### column(12, uiOutput("optimisedResults")) 
                                                
)),


tabSimulationResultsBody <- div(div(class='card',div(class='container-fluid',
                                                     
                                                     column(11,shiny::tags$h2('View Simulation Results')),
                                                     
                                                     column(1, style='padding-top:25px', actionButton("viewSimulationResultsHelpText", label = NULL,
                                                                                                      icon = icon("info-sign", lib = "glyphicon"))),
                                                     # column(12, shiny::tags$hr()),
                                                     column(12, uiOutput('viewSimDatesText')),
                                                     # column(12, uiOutput('histSimDatesText')),
                                                     # column(12,shiny::tags$p(shiny::HTML(viewSimulationResultsText))),
                                                     column(12,uiOutput('scenarioFilePathUI')),
                                                     column(12,uiOutput('chooseScenarioSelectionMode')),
                                                     
                                                     shiny::conditionalPanel('input.selectMode=="View Generated Scenarios"',
                                                                        #######     column(12,uiOutput('viewSelectedScenarioUI'))
                                                                             ),
                                                     shiny::conditionalPanel('input.selectMode=="Upload Scenario RDS"',
                                                                             column(style='margin-left:10px', 4, fileInput('scenarioFile', 'Upload Scenario RDS file')),
                                                                             column(style='padding-top:25px', class='button_pad', 2, actionButton("submitUploadedScenario", "Submit" )),
                                                                             column(12, shiny::div(style="color:green", shiny::textOutput("uploadedScenarioText")))
                                                     ),
                                                ###### column(12,uiOutput('simulationTimeSeriesUI'))
)),



  output$scenarioSelect <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(4,shiny::selectInput("chooseOptiScenario", "Select Scenario",
                                                       choices = reactData$opti_scenarioNameTable$Scenario,
                                                       multiple = FALSE, selected = NULL))
                    ,shiny::column(2, style='padding-top:25px', class='button_pad', shiny::actionButton('submittedOptiScenario','Submit'))
    )
  })


    observeEvent(input$submittedOptiScenario,{
    scenarioName <- input$chooseOptiScenario
    
    reactData$optimised_dataset <- optimisationScenarioList$files[[scenarioName]]$optimising_dataset$optimised_dataset
  })


    output$viewSelectedScenarioUI <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(4,shiny::selectInput("viewScenarioSelected", "Select Scenario",
                                                       choices = unique(scenarioTable$dataset$Scenario_Name),
                                                       multiple = FALSE, selected = NULL)),
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



    output$optimisedResults <- shiny::renderUI({
    shiny::fluidRow(class='box1',
                    shiny::column(4,shiny::selectInput("choosePanel", "Select Panel",
                                                       choices = c("", "Total", unique(reactDataOpt$dataset[reactData$panelVar])),
                                                       multiple = FALSE, selected = 'Total'))
                    ,shiny::column(2, style='padding-top:25px', class='button_pad', shiny::actionButton('submittedPanel','Submit'))
                    
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