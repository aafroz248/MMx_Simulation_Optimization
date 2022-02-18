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
# Description : Upload a marketing mix model and simulate/optimize different spend scenarions
####################################################################################



#################### JS Code for selective enabling and disabling of tabs ############################
jscode <- "
shinyjs.disableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');

tab.bind('click.tab', function(e) {
e.preventDefault();
return false;
});
tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.unbind('click.tab');
tab.removeClass('disabled');
}

shinyjs.disableMenu = function(name) {
var menu = $('.sidebar-menu li a[data-value=' + name + ']');

menu.bind('click.menu', function(e) {
e.preventDefault();
return false;
});
menu.addClass('menu_disabled');
}

shinyjs.enableMenu = function(name) {
var menu = $('.sidebar-menu li a[data-value=' + name + ']');
menu.unbind('click.menu');
menu.removeClass('menu_disabled');
}

shinyjs.greenTab = function(name){
var tab  =  $('.navbar-nav li a[data-value=' + name + ']');
tab.addClass('tab_green');
}



"
# shinyjs.greenMenu = function(name){
#   var menu  =  $('.sidebar-menu li a[data-value=' + name + ']');
#   menu.addClass('menu_green');
# }


css <- "
.disabled {
background-color: #ffffff !important;
color: #aaa !important;
cursor: not-allowed !important;
}
.menu_disabled {
background-color: #222d32 !important;
color: #aaa !important;
cursor: not-allowed !important;
}

.tab_green {
  border-top: 4px solid limegreen !important;
  position:relative;
 
}

.tab_green:after{
    content: '';
    display: block;
    height: 20px; /*height of icon */
    width: 15px;  /*width of icon */
    position: absolute;
      /*where to replace the icon */
    top: 18px;
    left: 2px;
      /*background */
    background: url('Logos/Flat_check_icon.png') no-repeat;
}

#configureNav >li:nth-child(4) {
  position:absolute;
  left:91.5%;
  background-color: black;
  border:2px;
  border-top: 3px ;
}

#configureNav li:nth-child(4) >a {
    color: white;
    border-top: 4px solid transparent;
}

#configureNav >li:nth-child(4) >a  {
    background-color: #c23b22 !important ;
}
#configureNav > li:nth-child(4) >a:focus{
    background-color: #c23b22 !important;
}

#configureNav >li:nth-child(4) >a:hover{
    background-color: #c23b22 !important;
}

.dt-center {
font-size:130%;
}



.menu_green {
  border-left: 6px solid limegreen !important;
  position:relative;
}


"

require("reactable")

############################## UI Tab elements ##################################

simulation_number <- 0
optimization_number <- 0 


tabHomeBody <- div(class='card',
                   fluidPage(
                     fluidRow(
                       column(12,
                              includeHTML("./www/Logos/html_test.html"),
                              includeCSS("./www/Logos/welcome.css")
                       ),
                       shiny::column(7,style="padding-right: -200px;height: 60px;", 
                                     shiny::actionButton('beginButton', 
                                                         'Begin', width = '170px',
                                                         #height = '60px',
                                                         class='begin_button')
                       )
                     )
                   )
)





tabConfigureBody <- div(class='card',

                       
                        navbarPage("",id='configureNav',
                                   
                                   # tabsetPanel(id='configureNav', 
                                   
                                   tabPanel(div(style="padding-left:5px","1. Upload Historical Dataset & Model")
                                            , value='uploadDataTab',
                                            #column(11,shiny::tags$p(shiny::HTML('<strong>The historical dataset which was used to create the model needs to be uploaded here. The spend patterns and simulation period will be decided on the basis of this dataset. The marketing mix model also needs to be uploaded here and will be used to simulate/optimize results for the specified range of spends. You will soon see the model equation and summary stats after you upload the same.</strong>'))),
                                            box(width=12,
                                                column(11, h2("Load Historical Data")),
                                                column(1, style='padding-top:25px', actionButton("historicalHelpText", label = NULL,
                                                                                                 icon = icon("info-sign", lib = "glyphicon"))),
                                                fluidRow(
                                                  column(9, fileInput('histDataFile', 'Choose CSV file')),
                                                  column(3, actionButton('preData', 'Load Sample Dataset'), style = "margin-top: 2.45vh;")
                                                ),
                                                # column(12, shiny::tags$hr()),
                                                # column(12, fileInput('histDataFile', 'Choose CSV file')),
                                                column(12,
                                                       uiOutput("histTableText"),
                                                       textOutput("First 6 rows have been shown below in order to ensure that relevant data has been loaded for the analysis."),
                                                       DT::dataTableOutput("histHeadTable")
                                                      )
                                                ),
                                            shiny::br(),
                                            column(12, verbatimTextOutput("loadSummaryStats")),
                                            column(12, verbatimTextOutput("loadDataStr")),
                                            box(width=12,
                                                column(11, h2('Load MMx Model')),
                                                column(1, style='padding-top:25px',actionButton("loadModelHelpText", label = NULL,
                                                                                                icon = icon("info-sign", lib = "glyphicon"))),
                                                # column(12, shiny::tags$hr()),
                                                fluidRow(
                                                  column(9, fileInput('modelFile', 'Choose RDS file')),
                                                  column(3, actionButton('preModel', 'Load Sample Model'), style = "margin-top: 2.45vh;")
                                                ),
                                                ### Load File Widget - Source for getting user uploaded MMX model object as an RDS file
                                                # column(12, fileInput('modelFile', 'Choose RDS file')),
                                                column(12, uiOutput("loadModelText")),
                                                # column(12, uiOutput("trainDataTakenFromModelText")),
                                                column(12, uiOutput("loadTrainData")),
                                                column(12, uiOutput("loadTrainDataOutput"))),
                                            column(12, uiOutput("uploadDataTabUI"))),
                                   tabPanel(div(style="padding-left:5px",' 2. Configure Variables'), value='configureVarTab',
                                            box(width=12,
                                            column(11, shiny::tags$h2('Select Date Columns')),
                                            column(1, style='padding-top:25px',actionButton("loadDateHelpText", label = NULL,
                                                                                            icon = icon("info-sign", lib = "glyphicon"))),
                                            # column(12, shiny::tags$hr()),
                                            column(12, uiOutput("loadDateTime")),
                                            column(12, verbatimTextOutput("preprocessing")),
                                            column(12, div(style = "color:red",
                                                           textOutput("datetime_warning_text"))),
                                            column(12, div(style = "color:red",
                                                           textOutput("datetime_error_text")))),
                                            box(width=12,
                                            column(11, shiny::tags$h2('Select Spend, Panel and Target Columns')),
                                            column(1, style='padding-top:25px',actionButton("loadVariableHelpText", label = NULL,
                                                                                            icon = icon("info-sign", lib = "glyphicon"))),
                                            # column(12, shiny::tags$hr()),
                                            column(12, uiOutput("loadVariables")),
                                            column(12, verbatimTextOutput("spend_preprocessing")),
                                            column(12, h4('View Historical Sales Plot')),
                                            column(12, uiOutput("histPlotSelectInput")),
                                            column(12, uiOutput("histPlotsUI"))),
                                            shiny::br(),
                                            shiny::br(),
                                            box(width=12,
                                            column(12, shiny::tags$h2('Configure Adstock Decay Rates')),
                                            # column(12, shiny::tags$hr()),
                                            column(12,uiOutput('adstockDecayTextUI')),
                                            column(12, shiny::tags$p(shiny::HTML(adstockConfigureText))),
                                            column(12, shiny::tags$span(style="color:red",shiny::HTML(adstockConfigurePercentText))),
                                            column(12, rHandsontableOutput("adstockTable")),
                                            column(12, uiOutput('adstockSubmitUI')),
                                            
                                            column(12, uiOutput('adstockPanelUI')),
                                            column(12, shiny::plotOutput('adstockPlot'))),
                                            column(12, shiny::uiOutput('configureVarTabUI'))),
                                   
                                   tabPanel(div(style="padding-left:5px",'3. Define Simulation/Optimization Period'), value='setTimePeriodTab',
                                            box(width=12,
                                            column(11,shiny::tags$h2('Define Simulation/Optimization Period')),
                                            tags$br(),
                                            column(1, style='padding-top:25px',actionButton("simulationPeriodHelpText", label = NULL,
                                                                                            icon = icon("info-sign", lib = "glyphicon"))),
                                            column(12, uiOutput('simulationPeriodDatesText')),
                                            column(12, uiOutput('simulationPeriodSelectionUI'))),
                                            # tabPanel('4. Load Non-Spend Variables',
                                            box(width=12,
                                            column(12, style='padding-left:15px',shiny::checkboxInput('add_external_data_checkbox','Do you want to load non-spend variable(s)?'),
                                                   conditionalPanel('input.add_external_data_checkbox',column(11, shiny::tags$h2('Load Non-Spend Variables')),
                                                                    column(1, style='padding-top:25px',actionButton("extPostHelpText", label = NULL,
                                                                                                                    icon = icon("info-sign", lib = "glyphicon"))),
                                                                    # column(12, shiny::tags$hr()),
                                                                    column(12, uiOutput('externalDataDatesUI')), column(12, shiny::fileInput('extDataFile', 'Choose CSV file')),
                                                                    column(12, uiOutput('externalDataInfoUI')),
                                                                    column(12, uiOutput('externalDataInputUI'))))),
                                            #column(12, shiny::actionButton('historicalPerformanceNextButton', 'View Summary', class='begin_button'))
                                            
                                            shiny::column(8,style="padding-right: -200px;height: 60px;", 
                                                          shiny::actionButton('beginScenarioPlanningButton', 
                                                                              'Begin Scenario Planning', width = '220px',
                                                                              #height = '60px',
                                                                              class='begin_button'),
                                                          shiny::actionButton('historicalPerformanceNextButton', 
                                                                              'View Summary', width = '220px',
                                                                              #height = '60px',
                                                                              class='begin_button')
                                            )
                                            
                                            # column(12, shiny::tags$p(shiny::HTML(exPostText))),
                                            ### Load File Widget - Source for getting user uploaded external data as a CSV file
                                            
                                            # column(6, uiOutput('gotoSimulation')),
                                            # column(3, uiOutput('gotoOptimization'))
                                            ), 
                                   tabPanel(div("View Summary"),value='viewHistPerformance', 
                                            box(width=12,
                                                column(11, shiny::tags$h2('View Historical Performance')),
                                                column(1, style='padding-top:25px', actionButton("histPerformanceHelpText", label = NULL,
                                                                                                 icon = icon("info-sign", lib = "glyphicon"))),
                                                # column(12, shiny::tags$hr()),
                                                column(12, uiOutput('histDatesText')),
                                                # column(12, uiOutput('histSimDatesText')),
                                                # column(12, shiny::tags$p(shiny::HTML(histPerformanceText))),
                                                column(12, shiny::tags$h3('Summary')),
                                                column(12, uiOutput('historicalPerformanceUI')),
                                                # column(12, DT::dataTableOutput("agg_data_long")),
                                                column(12, shiny::tags$h3('Time Series view')),
                                                column(12, shiny::uiOutput('historicalPerformancePlotsUI')),
                                                column(12, shiny::tags$h3('Marginal ROI plots')),
                                                column(12, shiny::tags$p(shiny::HTML(mROIPlotsText))),
                                                column(12, shiny::uiOutput('mRoiPlotsUI')),
                                                column(12, shiny::uiOutput('mRoiPlots1'))),
                                            
                                            shiny::column(7,style="padding-right: -200px;height: 60px;", 
                                                          shiny::actionButton('beginScenarioPlanningButtonSummary', 
                                                                              'Begin Scenario Planning', width = '220px',
                                                                              #height = '60px',
                                                                              class='begin_button')
                                            )
                                            
                                            #column(7, uiOutput('gotoSimulation')),
                                            #column(3, uiOutput('gotoOptimization'))
                                            )
                                   )
)

tabGenerateScenariosBody <- div(class='card',
                                navbarPage("", id='scenarioNav',
                                           tabPanel(div(style="padding-left:5px","1. Define Scenarios"),value='createScenarioTab',
                                                    box(width=12,
                                                    column(11, shiny::tags$h2('Create Simulation Scenarios')),
                                                    column(1, style='padding-top:25px', actionButton("simScenarioHelpText", label = NULL,
                                                                                                     icon = icon("info-sign", lib = "glyphicon"))),
                                                    # column(12, shiny::tags$hr()),
                                                    column(12, uiOutput('simScenarioDatesText')),
                                                    # column(12, uiOutput('histSimDatesText')),
                                                    # column(12, shiny::tags$p(shiny::HTML(simScenarioText))),
                                                    column(11, shiny::tags$h3('Select Spend Scheduling Strategy')),
                                                    column(1, style='padding-top:25px', actionButton("schedulingMethodHelpText", label = NULL,
                                                                                                     icon = icon("info-sign", lib = "glyphicon"))),
                                                    column(12, uiOutput('selectSchedulingStrategyUI')),
                                                    column(12, uiOutput('schedulingStrategy1InfoUI')),
                                                    column(12, uiOutput('schedulingStrategy2InfoUI')),
                                                    column(12, uiOutput('schedulingStrategy3InfoUI')),
                                                    column(12,  shiny::div(style="color:green", shiny::textOutput("method1SaveMssg"))),
                                                    column(12, shiny::div(style="color:green", shiny::textOutput("method2SaveMssg"))),
                                                    column(12, shiny::div(style="color:green", shiny::textOutput("method3SaveMssg"))),
                                                    shiny::br(),
                                                    column(12, uiOutput('scenarioListTable'))),
                                                    column(12, uiOutput('createScenarioTabUI'))),
                                           
                                           tabPanel(div(style="padding-left:5px","2. Generate and Run Simulation Data"),value='runSimulationTab',
                                                    box(width=12,
                                                    column(11, shiny::tags$h2('Generate Simulation Spend Data')),
                                                    column(1, style='padding-top:25px', actionButton("genSimDataHelpText", label = NULL,
                                                                                                     icon = icon("info-sign", lib = "glyphicon"))),
                                                    
                                                    column(12, uiOutput('genSimDatesText')),
                                                    column(12,  uiOutput('generateScenarioUI')),
                                                    column(12, shiny::div(style="color:green", shiny::textOutput("generateScenarioSelectedStatusText"))),
                                                    column(12, uiOutput('editScenarioUI')),
                                                    column(12, uiOutput('viewGeneratedScenarioUI')),
                                                    column(12, uiOutput('viewGeneratedScenarioChartsUI'))),
                                                    box(width=12,
                                                    column(11, shiny::tags$h2('Run Simulation')),
                                                    column(1, style='padding-top:25px', actionButton("runSimulationHelpText", label = NULL,
                                                                                                     icon = icon("info-sign", lib = "glyphicon"))),
                                                    
                                                    column(12, uiOutput('runSimDatesText')),
                                                    column(12, shiny::br()),
                                                    column(12, uiOutput('runSimulationUI')),
                                                    column(12, shiny::div(style="color:green", shiny::textOutput("runScenarioSelectedStatusText")))),
                                                    column(12, uiOutput('runSimulationTabUI'))))

)


tabSimulationResultsBody<-div(class='card',
                              navbarPage("", id='resultsNav',
                                         tabPanel(div(style="padding-left:5px","Simulation Results"),value='viewSimulationResults',
                                                  div(div(class='card',div(class='container-fluid',

                                                  #column(11,shiny::tags$h2('Simulation Results')),

                                                  #column(1, style='padding-top:25px', actionButton("viewSimulationResultsHelpText", label = NULL,
                                                                                                   #icon = icon("info-sign", lib = "glyphicon"))),
                                                  # column(12, shiny::tags$hr()),
                                                  column(12, uiOutput('viewSimDatesText')),
                                                  # column(12, uiOutput('histSimDatesText')),
                                                  # column(12,shiny::tags$p(shiny::HTML(viewSimulationResultsText))),
                                                  column(12,uiOutput('scenarioFilePathUI')),
                                                  column(12,uiOutput('chooseScenarioSelectionMode')),
                                                  shiny::conditionalPanel('input.selectMode=="View Generated Scenarios"',
                                                                          column(12,uiOutput('viewSelectedScenarioUI'))
                                                  ),
                                                  shiny::conditionalPanel('input.selectMode=="Upload Scenario RDS"',
                                                                          column(style='margin-left:10px', 4, fileInput('scenarioFile', 'Upload Scenario RDS file')),
                                                                          column(style='padding-top:25px', class='button_pad', 2, actionButton("submitUploadedScenario", "Submit" )),
                                                                          column(12, shiny::div(style="color:green", shiny::textOutput("uploadedScenarioText")))
                                                  ),
                                                  column(12,uiOutput('simulationTimeSeriesUI'))
                         )),
                         div(class='card',div(class='container-fluid',
                                              #Scenario 1 and 2 Sales box
                                              column(3,
                                                     br(),
                                                     fluidRow(
                                                       # column(3, img(src="/Images/imp_sales.png",width = "100%",height = "100%"),align="right"),
                                                       column(12,
                                                              fluidRow(
                                                                column(3, "",style = "font-size: 120%", align="left"),
                                                                column(6,tags$b("Sales",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
                                                                column(3,"",style = "font-size: 120%", align="left")
                                                              ),
                                                              br(),

                                                              fluidRow(
                                                                column(5, uiOutput("hist_sales"), style="font-size: 120%;font-weight:bold;color:#1F77B4;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
                                                                column(1, "|", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
                                                                column(5, uiOutput("sim_sales"), style="font-size: 120%;font-weight:bold;color:#9cb3d0;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
                                                              ),
                                                              br(),                                                                                                    fluidRow(
                                                                column(5, tags$p("Historical"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
                                                                column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
                                                                column(5, tags$p("Simulated"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
                                                              )
                                                       )),
                                                     br(),
                                                     class="opti_out_kpi_box2"),
                                              column(3,
                                                     br(),
                                                     #Scenario 1 and 2 Spends box
                                                     fluidRow(
                                                       # column(3, img(src="/Images/spend.png",width = "120%",height = "120%"),align="right"),
                                                       column(12,
                                                              fluidRow(
                                                                column(3, "",style = "font-size: 120%", align="left"),
                                                                column(6,tags$b("Spends",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
                                                                column(3,"",style = "font-size: 120%", align="left")),
                                                              br(),
                                                              fluidRow(
                                                                column(5, uiOutput("hist_spend"), style="font-size: 120%;color:#1F77B4;font-weight:bold;margin-left: 6.7%;padding-top: 0.5%;padding-left: 0%; padding-right: 0%;", align="left"),
                                                                column(1, "|",style = "font-size: 120%;margin-left: 0%;padding-right: 0%;padding-left: 0%;", align="left"),
                                                                column(5, uiOutput("sim_spend"), style = "font-size: 120%;color:#9cb3d0;font-weight:bold;padding-left: 0%;padding-top: 0.5%; padding-right: 0%;", align="left")
                                                              ),
                                                              br(),                                                                                                    fluidRow(
                                                                column(5, tags$p("Historical"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
                                                                column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
                                                                column(5, tags$p("Simulated"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
                                                              )
                                                       )),
                                                     br(),
                                                     class="opti_out_kpi_box1"),
                                              column(3,
                                                     br(),
                                                     #Scenario 1 and 2 Contributions box
                                                     fluidRow(
                                                       # column(3, img(src="/Images/spend.png",width = "100%",height = "100%"),align="right"),
                                                       column(12,
                                                              fluidRow(
                                                                column(3, "",style = "font-size: 120%", align="left"),
                                                                column(6,tags$b("Contribution",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
                                                                column(3,"",style = "font-size: 120%", align="left")),
                                                              br(),
                                                              fluidRow(
                                                                column(5, uiOutput("hist_contrib"), style="font-size: 120%;font-weight:bold;color:#1F77B4;margin-left: 6.7%;padding-top: 0.5%;padding-left: 0%; padding-right: 0%;", align="left"),
                                                                column(1, "|",style = "font-size: 120%;margin-left: 0%;padding-right: 0%;padding-left: 0%;", align="left"),
                                                                column(5, uiOutput("sim_contrib"), style = "font-size: 120%;font-weight:bold;color:#9cb3d0;padding-left: 0%;padding-top: 0.5%; padding-right: 0%;", align="left")
                                                              ),
                                                              br(),                                                                                                  fluidRow(
                                                                column(5, tags$p("Historical"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
                                                                column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
                                                                column(5, tags$p("Simulated"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
                                                              )
                                                       )),
                                                     br(),
                                                     class="opti_out_kpi_box1"),
                                              #Scenario 1 and 2 ROI box
                                              column(3,
                                                     br(),
                                                     fluidRow(
                                                       # column(3, img(src="/Images/spend.png",width = "100%",height = "100%"),align="right"),
                                                       column(12,
                                                              fluidRow(
                                                                column(3, "",style = "font-size: 120%", align="left"),
                                                                column(6,tags$b("ROI",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
                                                                column(3,"",style = "font-size: 120%", align="right")),
                                                              br(),
                                                              fluidRow(
                                                                column(5, uiOutput("hist_roi"), style="font-size: 120%;font-weight:bold;color:#1F77B4;margin-left: 6.7%;padding-top: 0.5%;padding-left: 0%; padding-right: 0%;", align="left"),
                                                                column(1, "|",style = "font-size: 120%;margin-left: 0%;padding-right: 0%;padding-left: 0%;", align="left"),
                                                                column(5, uiOutput("sim_roi"), style = "font-size: 120%;font-weight:bold;color:#9cb3d0;padding-left: 0%;padding-top: 0.5%; padding-right: 0%;", align="left")
                                                              ),
                                                              br(),                                                                                                    fluidRow(
                                                                column(5, tags$p("Historical"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
                                                                column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
                                                                column(5, tags$p("Simulated"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
                                                              )
                                                       )),
                                                     br(),
                                                     class="opti_out_kpi_box3")

                         ), shiny::br()),
                         div(class='card',div(class='container-fluid',navbarPage("",
                                                                                 tabPanel("1. Tabular View",
                                                                                          column(12,DT::dataTableOutput("runSimDataOutput"))),
                                                                                 tabPanel("2. Simulation Summary",
                                                                                          column(11,uiOutput('scenarioTimeSeriesInfoUI')),
                                                                                          column(1, style='padding-top:25px', actionButton("viewSimulationTSHelpText", label = NULL,
                                                                                                                                           icon = icon("info-sign", lib = "glyphicon"))),
                                                                                          # column(12,uiOutput('simulationTimeSeriesUI')),
                                                                                          column(12,uiOutput('simulationTimeSeriesPlotsUI'))),
                                                                                 tabPanel("3. Summary Comparison with Historical Period",
                                                                                          column(11,uiOutput('scenarioHistoricalComparisonInfoUI')),
                                                                                          column(1, style='padding-top:25px', actionButton("viewComparisonSummaryHelpText", label = NULL,
                                                                                                                                           icon = icon("info-sign", lib = "glyphicon"))),
                                                                                          column(12,uiOutput('scenarioHistoricalComparisonUI')),
                                                                                          column(12,uiOutput('scenarioHistoricalComparisonsPlotsUI'))),
                                                                                 tabPanel("4. Time Series Comparison with Historical Period",
                                                                                          column(12,uiOutput('scenarioTimeSeriesComparisonInfoUI')),
                                                                                          column(12,uiOutput('scenarioTimeSeriesComparisonUI')),
                                                                                          column(12,uiOutput('scenarioTimeSeriesComparisonPlotsUI')))
                         ))),
                         div(class='card',div(class='container-fluid',
                                              shiny::column(12,
                                                            shiny::actionButton('simulationResultsSimButton', 'Add Simulation Scenario', class='begin_button'),
                                                            shiny::actionButton('simulationResultsOptButton', 'Add Optimization Scenario', class='begin_button'),
                                                            shiny::actionButton('simulationResultsCompareButton', 'Compare Scenarios', class='second_button'),
                                                            shiny::actionButton('recommendationOptButton', 'Recommendations', class='second_button')

                                                            ))))
                                         ),
                         tabPanel(div(style="padding-left:5px","Optimization Results"),value='viewOptimizationResults',
                                  div(div(class='card',div(class='container-fluid',
                                 #column(11, h2('Optimization Results')),
                                 column(12, uiOutput("viewOptimisationDates_atResults")),
                                 column(12, uiOutput("chooseOptiScenarioSelectionMode")),

                                 shiny::conditionalPanel('input.selectOptiMode=="View Generated Scenarios"',
                                                         column(12,uiOutput('scenarioSelect'))
                                 ),
                                 shiny::conditionalPanel('input.selectOptiMode=="Upload Scenario RDS"',
                                                         column(style='margin-left:10px', 4, fileInput('optiScenarioFile', 'Upload Scenario RDS file'))
                                                         , column(style='padding-top:25px', class='button_pad', 2, actionButton("submitUploadedOptiScenario", "Submit" ))
                                                         , column(12, shiny::div(style="color:green", shiny::textOutput("uploadedOptiScenarioText")))
                                 ),
                                 # column(12, uiOutput("scenarioSelect")) ,
                                 column(12, uiOutput("optimisedResults"))

        )),
        div(class='card',div(class='container-fluid',
                             #Scenario 1 and 2 Sales box
                             column(3,
                                    br(),
                                    fluidRow(
                                      # column(3, img(src="/Images/imp_sales.png",width = "100%",height = "100%"),align="right"),
                                      column(12,
                                             fluidRow(
                                               column(3, "",style = "font-size: 120%", align="left"),
                                               column(6,tags$b("Sales",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
                                               column(3,"",style = "font-size: 120%", align="left")),
                                             br(),
                                             fluidRow(
                                               column(5, uiOutput("histCard_Sales"), style="font-size: 120%;font-weight:bold;color:#1F77B4;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
                                               column(1, "|", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
                                               column(5, uiOutput("optiCard_Sales"), style="font-size: 120%;font-weight:bold;color:#9cb3d0;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
                                             ),
                                             br(),                                                                                                    fluidRow(
                                               column(5, tags$p("Historical"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
                                               column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
                                               column(5, tags$p("Optimized"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
                                             )
                                      )),
                                    br(),
                                    class="opti_out_kpi_box2"),
                             column(3,
                                    br(),
                                    #Scenario 1 and 2 Spends box
                                    fluidRow(
                                      # column(3, img(src="/Images/spend.png",width = "120%",height = "120%"),align="right"),
                                      column(12,
                                             fluidRow(
                                               column(3, "",style = "font-size: 120%", align="left"),
                                               column(6,tags$b("Spend",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
                                               column(3,"",style = "font-size: 120%", align="left")),
                                             br(),
                                             fluidRow(
                                               column(5, uiOutput("histCard_Spend"), style="font-size: 120%;color:#1F77B4;font-weight:bold;margin-left: 6.7%;padding-top: 0.5%;padding-left: 0%; padding-right: 0%;", align="left"),
                                               column(1, "|",style = "font-size: 120%;margin-left: 0%;padding-right: 0%;padding-left: 0%;", align="left"),
                                               column(5, uiOutput("optiCard_Spend"), style = "font-size: 120%;color:#9cb3d0;font-weight:bold;padding-left: 0%;padding-top: 0.5%; padding-right: 0%;", align="left")
                                             ),
                                             br(),                                                                                                    fluidRow(
                                               column(5, tags$p("Historical"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
                                               column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
                                               column(5, tags$p("Optimized"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
                                             )
                                      )),
                                    br(),
                                    class="opti_out_kpi_box1"),
                             column(3,
                                    br(),
                                    #Scenario 1 and 2 Contributions box
                                    fluidRow(
                                      # column(3, img(src="/Images/spend.png",width = "100%",height = "100%"),align="right"),
                                      column(12,
                                             fluidRow(
                                               column(3, "",style = "font-size: 120%", align="left"),
                                               column(6,tags$b("Contribution",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
                                               column(3,"",style = "font-size: 120%", align="left")),
                                             br(),
                                             fluidRow(
                                               column(5, uiOutput("histCard_Contrib"), style="font-size: 120%;font-weight:bold;color:#1F77B4;margin-left: 6.7%;padding-top: 0.5%;padding-left: 0%; padding-right: 0%;", align="left"),
                                               column(1, "|",style = "font-size: 120%;margin-left: 0%;padding-right: 0%;padding-left: 0%;", align="left"),
                                               column(5, uiOutput("optiCard_Contrib"), style = "font-size: 120%;font-weight:bold;color:#9cb3d0;padding-left: 0%;padding-top: 0.5%; padding-right: 0%;", align="left")
                                             ),
                                             br(),                                                                                                  fluidRow(
                                               column(5, tags$p("Historical"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
                                               column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
                                               column(5, tags$p("Optimized"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
                                             )
                                      )),
                                    br(),
                                    class="opti_out_kpi_box1"),
                             #Scenario 1 and 2 ROI box
                             column(3,
                                    br(),
                                    fluidRow(
                                      # column(3, img(src="/Images/spend.png",width = "100%",height = "100%"),align="right"),
                                      column(12,
                                             fluidRow(
                                               column(3, "",style = "font-size: 120%", align="left"),
                                               column(6,tags$b("ROI",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
                                               column(3,"",style = "font-size: 120%", align="left")),

                                             br(),
                                             fluidRow(
                                               column(5, uiOutput("histCard_ROI"), style="font-size: 120%;font-weight:bold;color:#1F77B4;margin-left: 6.7%;padding-top: 0.5%;padding-left: 0%; padding-right: 0%;", align="left"),
                                               column(1, "|",style = "font-size: 120%;margin-left: 0%;padding-right: 0%;padding-left: 0%;", align="left"),
                                               column(5, uiOutput("optiCard_ROI"), style = "font-size: 120%;font-weight:bold;color:#9cb3d0;padding-left: 0%;padding-top: 0.5%; padding-right: 0%;", align="left")
                                             ),
                                             br(),                                                                                                    fluidRow(
                                               column(5, tags$p("Historical"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
                                               column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
                                               column(5, tags$p("Optimized"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
                                             )
                                      )),
                                    br(),
                                    class="opti_out_kpi_box3")

        ), shiny::br()),

        div(div(class='card',div(class='container-fluid',
                                 navbarPage("",
                                            # tabPanel("1. Overall Summary",
                                            #          column(11, h3('Summary Table for Selected Panel')),
                                            #          column(12, DT::dataTableOutput("summaryTable"))
                                            # ),
                                            tabPanel("1. Tabular View",
                                                     column(11, h4('Optimized Spends Dataset')),
                                                     column(12, DT::dataTableOutput("opti_data_table"))
                                            ),
                                            tabPanel("2. Optimization Summary",
                                                     shiny::fluidRow(class='box1',
                                                                     shiny::column(8, h4('Optimization Summary')),
                                                                     shiny::conditionalPanel("input.submittedPanel>0", column(2, style='padding:25px', downloadButton("grobOptiSummaryTab_Plots_UI", "Download Plot(s)"))),
                                                                     shiny::conditionalPanel("input.submittedPanel>0", column(2, style='padding:25px', downloadButton("downloadOptiSummaryData", "Download Data")))
                                                     ),
                                                     column(12,shiny::tabsetPanel(type = "tabs",
                                                                                  shiny::tabPanel("Sales",
                                                                                                  shiny::fluidRow(shiny::column(6, shiny::radioButtons("Sales_ChartType", "Chart Type", choices=c("Aggregate", "All"), inline = TRUE))),
                                                                                                  conditionalPanel('input.Sales_ChartType=="Aggregate"',
                                                                                                                   shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('SalesPlot'))
                                                                                                                   ))
                                                                                                  ,conditionalPanel('input.Sales_ChartType=="All"',
                                                                                                                    shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('allSalesPlot'))
                                                                                                                    ))),
                                                                                  shiny::tabPanel("Spends",
                                                                                                  shiny::fluidRow(shiny::column(6, shiny::radioButtons("Spend_ChartType", "Chart Type", choices=c("Absolute", "Percentage"), inline = TRUE))),
                                                                                                  conditionalPanel('input.Spend_ChartType=="Absolute"',
                                                                                                                   shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('SpendAreaPlot'))
                                                                                                                   ))
                                                                                                  ,conditionalPanel('input.Spend_ChartType=="Percentage"',
                                                                                                                    shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('pctSpendAreaPlot'))
                                                                                                                    ))
                                                                                  ),
                                                                                  shiny::tabPanel("Contribution",
                                                                                                  shiny::fluidRow(shiny::column(6, shiny::radioButtons("Conrib_ChartType", "Chart Type", choices=c("Absolute", "Percentage"), inline = TRUE))),
                                                                                                  conditionalPanel('input.Conrib_ChartType=="Absolute"',
                                                                                                                   shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('Contribution'))
                                                                                                                   ))
                                                                                                  ,conditionalPanel('input.Conrib_ChartType=="Percentage"',
                                                                                                                    shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('pctContribution'))
                                                                                                                    ))
                                                                                  ),
                                                                                  shiny::tabPanel("ROI",
                                                                                                  shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('ROI')))
                                                                                  )))
                                            ),
                                            tabPanel("3. Summary Comparison with Historical Period",
                                                     shiny::fluidRow(class='box1',
                                                                     shiny::column(8, h4('Summary Comparison with Historical Period')),
                                                                     shiny::conditionalPanel("input.submittedPanel>0", column(2, style='padding:10px', downloadButton("grobSummaryComparisonTab_Plots_UI", "Download Plot(s)"))),
                                                                     shiny::conditionalPanel("input.submittedPanel>0", column(2, style='padding:10px', downloadButton("downloadOptiSummaryComparisonData", "Download Data")))
                                                     ),
                                                     column(12,shiny::tabsetPanel(type = "tabs",
                                                                                  shiny::tabPanel("Sales",
                                                                                                  shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('salesComparison'))
                                                                                                                  #shiny::column(6, shiny::plotOutput('scenario2SalesPlot'))
                                                                                                  )),
                                                                                  shiny::tabPanel("Spend",
                                                                                                  shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('spendsComparison'))
                                                                                                                  #shiny::column(6, shiny::plotOutput('scenario2SpendPlot'))
                                                                                                  )),
                                                                                  shiny::tabPanel("Contribution",
                                                                                                  shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('contribComparison'))
                                                                                                                  #shiny::column(6, shiny::plotOutput('scenario2SpendPlot'))
                                                                                                  )),
                                                                                  shiny::tabPanel("ROI",
                                                                                                  shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('roiComparison'))
                                                                                                                  #shiny::column(6, shiny::plotOutput('scenario2SpendPlot'))
                                                                                                  ))))

                                            ),
                                            tabPanel("4. Time Series Comparison with Historical Period",
                                                     shiny::fluidRow(class='box1',
                                                                     column(8, uiOutput("channelComparison")),
                                                                     shiny::conditionalPanel("input.submittedChannel>0", column(2, style='padding:10px', downloadButton("grobTimeSeriesComparisonTab_Plots_UI", "Download Plot(s)"))),
                                                                     shiny::conditionalPanel("input.submittedChannel>0", column(2, style='padding:10px', downloadButton("downloadOptiTimeSeriesData", "Download Data")))
                                                     ),
                                                     # column(11, h3('Click on below tabs to Navigate')),
                                                     column(12,shiny::tabsetPanel(type = "tabs",
                                                                                  shiny::tabPanel("Sales",
                                                                                                  shiny::fluidRow(shiny::column(6, shiny::radioButtons("salesChartType", "Chart Type", choices=c("Sales", "Sales Overlay"), inline = TRUE))),
                                                                                                  conditionalPanel('input.salesChartType=="Sales Overlay"',
                                                                                                                   shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('salesComparison_trend'))
                                                                                                                   )                                                                                                                                                               )
                                                                                                  ,conditionalPanel('input.salesChartType=="Sales"',
                                                                                                                    shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('salesContinuous_trend'))
                                                                                                                    ) )),
                                                                                  shiny::tabPanel("Spends",
                                                                                                  shiny::fluidRow(shiny::column(6, shiny::radioButtons("spendsChartType", "Chart Type", choices=c("Spends", "Spends Overlay"), inline = TRUE))),
                                                                                                  conditionalPanel('input.spendsChartType=="Spends Overlay"',
                                                                                                                   shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('spendsComparison_trend'))
                                                                                                                   )                                                                                                                                                               )
                                                                                                  ,conditionalPanel('input.spendsChartType=="Spends"',
                                                                                                                    shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('spendsContinuous_trend'))
                                                                                                                    ) )),
                                                                                  shiny::tabPanel("Contribution",
                                                                                                  shiny::fluidRow(shiny::column(6, shiny::radioButtons("comparisonChartType", "Chart Type", choices=c("Contribution", "Contribution Overlay"), inline = TRUE))),
                                                                                                  conditionalPanel('input.comparisonChartType=="Contribution"',
                                                                                                                   shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('contribContinuous_trend'))
                                                                                                                   ))
                                                                                                  ,conditionalPanel('input.comparisonChartType=="Contribution Overlay"',
                                                                                                                    shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('contribComparison_trend'))
                                                                                                                    ))
                                                                                  ),
                                                                                  shiny::tabPanel("ROI",
                                                                                                  shiny::fluidRow(shiny::column(6, shiny::radioButtons("ROIChartType", "Chart Type", choices=c("ROI", "ROI Overlay"), inline = TRUE))),
                                                                                                  conditionalPanel('input.ROIChartType=="ROI"',
                                                                                                                   shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('roi_Continuous_trend'))
                                                                                                                   ))
                                                                                                  ,conditionalPanel('input.ROIChartType=="ROI Overlay"',
                                                                                                                    shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('roi_trend'))
                                                                                                                    ))
                                                                                  )
                                                     ))
                                            )

                                 )))),
        div(class='card',div(class='container-fluid',
                             shiny::column(12,
                                           shiny::actionButton('simulationResultsSimButtonSecond', 'Add Simulation Scenario', class='begin_button'),
                                           shiny::actionButton('simulationResultsOptButtonSecond', 'Add Optimization Scenario', class='begin_button'),
                                           shiny::actionButton('optimizationResultsCompareButton', 'Compare Scenarios', class='second_button'),
                                           shiny::actionButton('recommendationOptButton1', 'Recommendations', class='second_button')
                                           
                                           #shiny::actionButton('simulationResultsSimButton', 'Generate Simulation Scenarios', class='next_button', icon = icon("check-circle")),
                                           #shiny::actionButton('simulationResultsOptButton', 'Generate Optimization Scenarios', class='next_button', icon = icon("check-circle"))
                                           )
                             )))
)


)
)







# tabSimulationResultsBody <- div(div(class='card',column(11,shiny::tags$h2('Scenario Results'))), 
#                                 column(12,uiOutput('chooseResultsSelectionMode')),
#                                 shiny::conditionalPanel('input.selectResult=="View Simulation Results"',
#                                                         div(div(class='card',div(class='container-fluid',
#                                                                                  
#                                                                                  #column(11,shiny::tags$h2('Simulation Results')),
#                                                                                  
#                                                                                  #column(1, style='padding-top:25px', actionButton("viewSimulationResultsHelpText", label = NULL,
#                                                                                                                                   #icon = icon("info-sign", lib = "glyphicon"))),
#                                                                                  # column(12, shiny::tags$hr()),
#                                                                                  column(12, uiOutput('viewSimDatesText')),
#                                                                                  # column(12, uiOutput('histSimDatesText')),
#                                                                                  # column(12,shiny::tags$p(shiny::HTML(viewSimulationResultsText))),
#                                                                                  column(12,uiOutput('scenarioFilePathUI')),
#                                                                                  column(12,uiOutput('chooseScenarioSelectionMode')),
#                                                                                  shiny::conditionalPanel('input.selectMode=="View Generated Scenarios"',
#                                                                                                          column(12,uiOutput('viewSelectedScenarioUI'))
#                                                                                  ),
#                                                                                  shiny::conditionalPanel('input.selectMode=="Upload Scenario RDS"',
#                                                                                                          column(style='margin-left:10px', 4, fileInput('scenarioFile', 'Upload Scenario RDS file')),
#                                                                                                          column(style='padding-top:25px', class='button_pad', 2, actionButton("submitUploadedScenario", "Submit" )),
#                                                                                                          column(12, shiny::div(style="color:green", shiny::textOutput("uploadedScenarioText")))
#                                                                                  ),
#                                                                                  column(12,uiOutput('simulationTimeSeriesUI'))
#                                                         )),
#                                                         div(class='card',div(class='container-fluid',
#                                                                              #Scenario 1 and 2 Sales box      
#                                                                              column(3,
#                                                                                     br(),
#                                                                                     fluidRow(
#                                                                                       # column(3, img(src="/Images/imp_sales.png",width = "100%",height = "100%"),align="right"),
#                                                                                       column(12, 
#                                                                                              fluidRow(
#                                                                                                column(3, "",style = "font-size: 120%", align="left"),
#                                                                                                column(6,tags$b("Sales",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
#                                                                                                column(3,"",style = "font-size: 120%", align="left")
#                                                                                              ),
#                                                                                              br(),
#                                                                                              
#                                                                                              fluidRow(
#                                                                                                column(5, uiOutput("hist_sales"), style="font-size: 120%;font-weight:bold;color:#1F77B4;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
#                                                                                                column(1, "|", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
#                                                                                                column(5, uiOutput("sim_sales"), style="font-size: 120%;font-weight:bold;color:#9cb3d0;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
#                                                                                              ),
#                                                                                              br(),                                                                                                    fluidRow(
#                                                                                                column(5, tags$p("Historical"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
#                                                                                                column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
#                                                                                                column(5, tags$p("Simulated"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
#                                                                                              )                                                                                                    
#                                                                                       )),
#                                                                                     br(),
#                                                                                     class="opti_out_kpi_box2"),
#                                                                              column(3,
#                                                                                     br(),
#                                                                                     #Scenario 1 and 2 Spends box
#                                                                                     fluidRow(
#                                                                                       # column(3, img(src="/Images/spend.png",width = "120%",height = "120%"),align="right"),
#                                                                                       column(12, 
#                                                                                              fluidRow(
#                                                                                                column(3, "",style = "font-size: 120%", align="left"),
#                                                                                                column(6,tags$b("Spends",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
#                                                                                                column(3,"",style = "font-size: 120%", align="left")),
#                                                                                              br(),
#                                                                                              fluidRow(
#                                                                                                column(5, uiOutput("hist_spend"), style="font-size: 120%;color:#1F77B4;font-weight:bold;margin-left: 6.7%;padding-top: 0.5%;padding-left: 0%; padding-right: 0%;", align="left"),
#                                                                                                column(1, "|",style = "font-size: 120%;margin-left: 0%;padding-right: 0%;padding-left: 0%;", align="left"),
#                                                                                                column(5, uiOutput("sim_spend"), style = "font-size: 120%;color:#9cb3d0;font-weight:bold;padding-left: 0%;padding-top: 0.5%; padding-right: 0%;", align="left")
#                                                                                              ),
#                                                                                              br(),                                                                                                    fluidRow(
#                                                                                                column(5, tags$p("Historical"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
#                                                                                                column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
#                                                                                                column(5, tags$p("Simulated"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
#                                                                                              )
#                                                                                       )),
#                                                                                     br(),
#                                                                                     class="opti_out_kpi_box1"),
#                                                                              column(3,
#                                                                                     br(),
#                                                                                     #Scenario 1 and 2 Contributions box
#                                                                                     fluidRow(
#                                                                                       # column(3, img(src="/Images/spend.png",width = "100%",height = "100%"),align="right"),
#                                                                                       column(12, 
#                                                                                              fluidRow(
#                                                                                                column(3, "",style = "font-size: 120%", align="left"),
#                                                                                                column(6,tags$b("Contribution",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
#                                                                                                column(3,"",style = "font-size: 120%", align="left")),
#                                                                                              br(),
#                                                                                              fluidRow(
#                                                                                                column(5, uiOutput("hist_contrib"), style="font-size: 120%;font-weight:bold;color:#1F77B4;margin-left: 6.7%;padding-top: 0.5%;padding-left: 0%; padding-right: 0%;", align="left"),
#                                                                                                column(1, "|",style = "font-size: 120%;margin-left: 0%;padding-right: 0%;padding-left: 0%;", align="left"),
#                                                                                                column(5, uiOutput("sim_contrib"), style = "font-size: 120%;font-weight:bold;color:#9cb3d0;padding-left: 0%;padding-top: 0.5%; padding-right: 0%;", align="left")
#                                                                                              ),
#                                                                                              br(),                                                                                                  fluidRow(
#                                                                                                column(5, tags$p("Historical"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
#                                                                                                column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
#                                                                                                column(5, tags$p("Simulated"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
#                                                                                              )
#                                                                                       )),
#                                                                                     br(),
#                                                                                     class="opti_out_kpi_box1"),
#                                                                              #Scenario 1 and 2 ROI box
#                                                                              column(3,
#                                                                                     br(),
#                                                                                     fluidRow(
#                                                                                       # column(3, img(src="/Images/spend.png",width = "100%",height = "100%"),align="right"),
#                                                                                       column(12, 
#                                                                                              fluidRow(
#                                                                                                column(3, "",style = "font-size: 120%", align="left"),
#                                                                                                column(6,tags$b("ROI",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
#                                                                                                column(3,"",style = "font-size: 120%", align="right")),
#                                                                                              br(),
#                                                                                              fluidRow(
#                                                                                                column(5, uiOutput("hist_roi"), style="font-size: 120%;font-weight:bold;color:#1F77B4;margin-left: 6.7%;padding-top: 0.5%;padding-left: 0%; padding-right: 0%;", align="left"),
#                                                                                                column(1, "|",style = "font-size: 120%;margin-left: 0%;padding-right: 0%;padding-left: 0%;", align="left"),
#                                                                                                column(5, uiOutput("sim_roi"), style = "font-size: 120%;font-weight:bold;color:#9cb3d0;padding-left: 0%;padding-top: 0.5%; padding-right: 0%;", align="left")
#                                                                                              ),
#                                                                                              br(),                                                                                                    fluidRow(
#                                                                                                column(5, tags$p("Historical"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
#                                                                                                column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
#                                                                                                column(5, tags$p("Simulated"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
#                                                                                              )
#                                                                                       )),
#                                                                                     br(),
#                                                                                     class="opti_out_kpi_box3")
#                                                                              
#                                                         ), shiny::br()),
#                                                         div(class='card',div(class='container-fluid',navbarPage("",
#                                                                                                                 tabPanel("1. Tabular View",
#                                                                                                                          column(12,DT::dataTableOutput("runSimDataOutput"))),
#                                                                                                                 tabPanel("2. Simulation Summary",
#                                                                                                                          column(11,uiOutput('scenarioTimeSeriesInfoUI')),
#                                                                                                                          column(1, style='padding-top:25px', actionButton("viewSimulationTSHelpText", label = NULL,
#                                                                                                                                                                           icon = icon("info-sign", lib = "glyphicon"))),
#                                                                                                                          # column(12,uiOutput('simulationTimeSeriesUI')),
#                                                                                                                          column(12,uiOutput('simulationTimeSeriesPlotsUI'))),
#                                                                                                                 tabPanel("3. Summary Comparison with Historical Period",
#                                                                                                                          column(11,uiOutput('scenarioHistoricalComparisonInfoUI')),
#                                                                                                                          column(1, style='padding-top:25px', actionButton("viewComparisonSummaryHelpText", label = NULL,
#                                                                                                                                                                           icon = icon("info-sign", lib = "glyphicon"))),
#                                                                                                                          column(12,uiOutput('scenarioHistoricalComparisonUI')),
#                                                                                                                          column(12,uiOutput('scenarioHistoricalComparisonsPlotsUI'))),
#                                                                                                                 tabPanel("4. Time Series Comparison with Historical Period",
#                                                                                                                          column(12,uiOutput('scenarioTimeSeriesComparisonInfoUI')),
#                                                                                                                          column(12,uiOutput('scenarioTimeSeriesComparisonUI')),
#                                                                                                                          column(12,uiOutput('scenarioTimeSeriesComparisonPlotsUI')))
#                                                         ))),
#                                                         div(class='card',div(class='container-fluid',
#                                                                              shiny::column(12,
#                                                                                            shiny::actionButton('simulationResultsSimButton', 'Generate Simulation Scenarios', class='next_button', icon = icon("check-circle")),
#                                                                                            shiny::actionButton('simulationResultsOptButton', 'Generate Optimization Scenarios', class='next_button', icon = icon("check-circle")), 
#                                                                                            shiny::actionButton('simulationResultsCompareButton', 'Compare Scenarios', class='next_button', icon = icon("check-circle")),
#                                                                                            shiny::actionButton('recommendationOptButton', 'Recommendations', class='next_button', icon = icon("check-circle"))
#                                                                              
#                                                                                            )))) 
#                                                                                             
#                                 ),
#                                 shiny::conditionalPanel('input.selectResult=="View Optimization Results"',
#                                                         div(div(class='card',div(class='container-fluid',
#                                                                                  #column(11, h2('Optimization Results')),
#                                                                                  column(12, uiOutput("viewOptimisationDates_atResults")),
#                                                                                  column(12, uiOutput("chooseOptiScenarioSelectionMode")),
#                                                                                  
#                                                                                  shiny::conditionalPanel('input.selectOptiMode=="View Generated Scenarios"',
#                                                                                                          column(12,uiOutput('scenarioSelect'))
#                                                                                  ),
#                                                                                  shiny::conditionalPanel('input.selectOptiMode=="Upload Scenario RDS"',
#                                                                                                          column(style='margin-left:10px', 4, fileInput('optiScenarioFile', 'Upload Scenario RDS file'))
#                                                                                                          , column(style='padding-top:25px', class='button_pad', 2, actionButton("submitUploadedOptiScenario", "Submit" ))
#                                                                                                          , column(12, shiny::div(style="color:green", shiny::textOutput("uploadedOptiScenarioText")))
#                                                                                  ),
#                                                                                  # column(12, uiOutput("scenarioSelect")) ,
#                                                                                  column(12, uiOutput("optimisedResults")) 
#                                                                                  
#                                                         )),
#                                                         div(class='card',div(class='container-fluid',
#                                                                              #Scenario 1 and 2 Sales box      
#                                                                              column(3,
#                                                                                     br(),
#                                                                                     fluidRow(
#                                                                                       # column(3, img(src="/Images/imp_sales.png",width = "100%",height = "100%"),align="right"),
#                                                                                       column(12, 
#                                                                                              fluidRow(
#                                                                                                column(3, "",style = "font-size: 120%", align="left"),
#                                                                                                column(6,tags$b("Sales",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
#                                                                                                column(3,"",style = "font-size: 120%", align="left")),
#                                                                                              br(),
#                                                                                              fluidRow(
#                                                                                                column(5, uiOutput("histCard_Sales"), style="font-size: 120%;font-weight:bold;color:#1F77B4;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
#                                                                                                column(1, "|", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
#                                                                                                column(5, uiOutput("optiCard_Sales"), style="font-size: 120%;font-weight:bold;color:#9cb3d0;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
#                                                                                              ),
#                                                                                              br(),                                                                                                    fluidRow(
#                                                                                                column(5, tags$p("Historical"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
#                                                                                                column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
#                                                                                                column(5, tags$p("Optimized"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
#                                                                                              )                                                                                                    
#                                                                                       )),
#                                                                                     br(),
#                                                                                     class="opti_out_kpi_box2"),
#                                                                              column(3,
#                                                                                     br(),
#                                                                                     #Scenario 1 and 2 Spends box
#                                                                                     fluidRow(
#                                                                                       # column(3, img(src="/Images/spend.png",width = "120%",height = "120%"),align="right"),
#                                                                                       column(12, 
#                                                                                              fluidRow(
#                                                                                                column(3, "",style = "font-size: 120%", align="left"),
#                                                                                                column(6,tags$b("Spend",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
#                                                                                                column(3,"",style = "font-size: 120%", align="left")),
#                                                                                              br(),
#                                                                                              fluidRow(
#                                                                                                column(5, uiOutput("histCard_Spend"), style="font-size: 120%;color:#1F77B4;font-weight:bold;margin-left: 6.7%;padding-top: 0.5%;padding-left: 0%; padding-right: 0%;", align="left"),
#                                                                                                column(1, "|",style = "font-size: 120%;margin-left: 0%;padding-right: 0%;padding-left: 0%;", align="left"),
#                                                                                                column(5, uiOutput("optiCard_Spend"), style = "font-size: 120%;color:#9cb3d0;font-weight:bold;padding-left: 0%;padding-top: 0.5%; padding-right: 0%;", align="left")
#                                                                                              ),
#                                                                                              br(),                                                                                                    fluidRow(
#                                                                                                column(5, tags$p("Historical"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
#                                                                                                column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
#                                                                                                column(5, tags$p("Optimized"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
#                                                                                              )
#                                                                                       )),
#                                                                                     br(),
#                                                                                     class="opti_out_kpi_box1"),
#                                                                              column(3,
#                                                                                     br(),
#                                                                                     #Scenario 1 and 2 Contributions box
#                                                                                     fluidRow(
#                                                                                       # column(3, img(src="/Images/spend.png",width = "100%",height = "100%"),align="right"),
#                                                                                       column(12, 
#                                                                                              fluidRow(
#                                                                                                column(3, "",style = "font-size: 120%", align="left"),
#                                                                                                column(6,tags$b("Contribution",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
#                                                                                                column(3,"",style = "font-size: 120%", align="left")),
#                                                                                              br(),
#                                                                                              fluidRow(
#                                                                                                column(5, uiOutput("histCard_Contrib"), style="font-size: 120%;font-weight:bold;color:#1F77B4;margin-left: 6.7%;padding-top: 0.5%;padding-left: 0%; padding-right: 0%;", align="left"),
#                                                                                                column(1, "|",style = "font-size: 120%;margin-left: 0%;padding-right: 0%;padding-left: 0%;", align="left"),
#                                                                                                column(5, uiOutput("optiCard_Contrib"), style = "font-size: 120%;font-weight:bold;color:#9cb3d0;padding-left: 0%;padding-top: 0.5%; padding-right: 0%;", align="left")
#                                                                                              ),
#                                                                                              br(),                                                                                                  fluidRow(
#                                                                                                column(5, tags$p("Historical"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
#                                                                                                column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
#                                                                                                column(5, tags$p("Optimized"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
#                                                                                              )
#                                                                                       )),
#                                                                                     br(),
#                                                                                     class="opti_out_kpi_box1"),
#                                                                              #Scenario 1 and 2 ROI box
#                                                                              column(3,
#                                                                                     br(),
#                                                                                     fluidRow(
#                                                                                       # column(3, img(src="/Images/spend.png",width = "100%",height = "100%"),align="right"),
#                                                                                       column(12, 
#                                                                                              fluidRow(
#                                                                                                column(3, "",style = "font-size: 120%", align="left"),
#                                                                                                column(6,tags$b("ROI",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
#                                                                                                column(3,"",style = "font-size: 120%", align="left")),
#                                                                                              
#                                                                                              br(),
#                                                                                              fluidRow(
#                                                                                                column(5, uiOutput("histCard_ROI"), style="font-size: 120%;font-weight:bold;color:#1F77B4;margin-left: 6.7%;padding-top: 0.5%;padding-left: 0%; padding-right: 0%;", align="left"),
#                                                                                                column(1, "|",style = "font-size: 120%;margin-left: 0%;padding-right: 0%;padding-left: 0%;", align="left"),
#                                                                                                column(5, uiOutput("optiCard_ROI"), style = "font-size: 120%;font-weight:bold;color:#9cb3d0;padding-left: 0%;padding-top: 0.5%; padding-right: 0%;", align="left")
#                                                                                              ),
#                                                                                              br(),                                                                                                    fluidRow(
#                                                                                                column(5, tags$p("Historical"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
#                                                                                                column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
#                                                                                                column(5, tags$p("Optimized"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
#                                                                                              )
#                                                                                       )),
#                                                                                     br(),
#                                                                                     class="opti_out_kpi_box3")
#                                                                              
#                                                         ), shiny::br()),
#                                                         
#                                                         div(div(class='card',div(class='container-fluid',
#                                                                                  navbarPage("",
#                                                                                             # tabPanel("1. Overall Summary",
#                                                                                             #          column(11, h3('Summary Table for Selected Panel')),
#                                                                                             #          column(12, DT::dataTableOutput("summaryTable"))
#                                                                                             # ),
#                                                                                             tabPanel("1. Tabular View",
#                                                                                                      column(11, h4('Optimized Spends Dataset')),
#                                                                                                      column(12, DT::dataTableOutput("opti_data_table"))
#                                                                                             ), 
#                                                                                             tabPanel("2. Optimization Summary",
#                                                                                                      shiny::fluidRow(class='box1',
#                                                                                                                      shiny::column(8, h4('Optimization Summary')),
#                                                                                                                      shiny::conditionalPanel("input.submittedPanel>0", column(2, style='padding:25px', downloadButton("grobOptiSummaryTab_Plots_UI", "Download Plot(s)"))),
#                                                                                                                      shiny::conditionalPanel("input.submittedPanel>0", column(2, style='padding:25px', downloadButton("downloadOptiSummaryData", "Download Data")))
#                                                                                                      ),
#                                                                                                      column(12,shiny::tabsetPanel(type = "tabs",
#                                                                                                                                   shiny::tabPanel("Sales", 
#                                                                                                                                                   shiny::fluidRow(shiny::column(6, shiny::radioButtons("Sales_ChartType", "Chart Type", choices=c("Aggregate", "All"), inline = TRUE))),
#                                                                                                                                                   conditionalPanel('input.Sales_ChartType=="Aggregate"',
#                                                                                                                                                                    shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('SalesPlot'))
#                                                                                                                                                                    ))
#                                                                                                                                                   ,conditionalPanel('input.Sales_ChartType=="All"',
#                                                                                                                                                                     shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('allSalesPlot'))
#                                                                                                                                                                     ))),
#                                                                                                                                   shiny::tabPanel("Spends", 
#                                                                                                                                                   shiny::fluidRow(shiny::column(6, shiny::radioButtons("Spend_ChartType", "Chart Type", choices=c("Absolute", "Percentage"), inline = TRUE))),
#                                                                                                                                                   conditionalPanel('input.Spend_ChartType=="Absolute"',
#                                                                                                                                                                    shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('SpendAreaPlot'))
#                                                                                                                                                                    ))
#                                                                                                                                                   ,conditionalPanel('input.Spend_ChartType=="Percentage"',
#                                                                                                                                                                     shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('pctSpendAreaPlot'))
#                                                                                                                                                                     ))
#                                                                                                                                   ),
#                                                                                                                                   shiny::tabPanel("Contribution", 
#                                                                                                                                                   shiny::fluidRow(shiny::column(6, shiny::radioButtons("Conrib_ChartType", "Chart Type", choices=c("Absolute", "Percentage"), inline = TRUE))),
#                                                                                                                                                   conditionalPanel('input.Conrib_ChartType=="Absolute"',
#                                                                                                                                                                    shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('Contribution'))
#                                                                                                                                                                    ))
#                                                                                                                                                   ,conditionalPanel('input.Conrib_ChartType=="Percentage"',
#                                                                                                                                                                     shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('pctContribution'))
#                                                                                                                                                                     ))
#                                                                                                                                   ),
#                                                                                                                                   shiny::tabPanel("ROI",
#                                                                                                                                                   shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('ROI')))
#                                                                                                                                   )))
#                                                                                             ),
#                                                                                             tabPanel("3. Summary Comparison with Historical Period",
#                                                                                                      shiny::fluidRow(class='box1',
#                                                                                                                      shiny::column(8, h4('Summary Comparison with Historical Period')),
#                                                                                                                      shiny::conditionalPanel("input.submittedPanel>0", column(2, style='padding:10px', downloadButton("grobSummaryComparisonTab_Plots_UI", "Download Plot(s)"))),
#                                                                                                                      shiny::conditionalPanel("input.submittedPanel>0", column(2, style='padding:10px', downloadButton("downloadOptiSummaryComparisonData", "Download Data")))
#                                                                                                      ),
#                                                                                                      column(12,shiny::tabsetPanel(type = "tabs",
#                                                                                                                                   shiny::tabPanel("Sales",
#                                                                                                                                                   shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('salesComparison'))
#                                                                                                                                                                   #shiny::column(6, shiny::plotOutput('scenario2SalesPlot'))
#                                                                                                                                                   )),
#                                                                                                                                   shiny::tabPanel("Spend", 
#                                                                                                                                                   shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('spendsComparison'))
#                                                                                                                                                                   #shiny::column(6, shiny::plotOutput('scenario2SpendPlot'))
#                                                                                                                                                   )),
#                                                                                                                                   shiny::tabPanel("Contribution", 
#                                                                                                                                                   shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('contribComparison'))
#                                                                                                                                                                   #shiny::column(6, shiny::plotOutput('scenario2SpendPlot'))
#                                                                                                                                                   )),
#                                                                                                                                   shiny::tabPanel("ROI", 
#                                                                                                                                                   shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('roiComparison'))
#                                                                                                                                                                   #shiny::column(6, shiny::plotOutput('scenario2SpendPlot'))
#                                                                                                                                                   ))))
#                                                                                                      
#                                                                                             ), 
#                                                                                             tabPanel("4. Time Series Comparison with Historical Period",
#                                                                                                      shiny::fluidRow(class='box1',
#                                                                                                                      column(8, uiOutput("channelComparison")),
#                                                                                                                      shiny::conditionalPanel("input.submittedChannel>0", column(2, style='padding:10px', downloadButton("grobTimeSeriesComparisonTab_Plots_UI", "Download Plot(s)"))),
#                                                                                                                      shiny::conditionalPanel("input.submittedChannel>0", column(2, style='padding:10px', downloadButton("downloadOptiTimeSeriesData", "Download Data")))
#                                                                                                      ),
#                                                                                                      # column(11, h3('Click on below tabs to Navigate')),
#                                                                                                      column(12,shiny::tabsetPanel(type = "tabs",
#                                                                                                                                   shiny::tabPanel("Sales", 
#                                                                                                                                                   shiny::fluidRow(shiny::column(6, shiny::radioButtons("salesChartType", "Chart Type", choices=c("Sales", "Sales Overlay"), inline = TRUE))),
#                                                                                                                                                   conditionalPanel('input.salesChartType=="Sales Overlay"',
#                                                                                                                                                                    shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('salesComparison_trend'))
#                                                                                                                                                                    )                                                                                                                                                               )
#                                                                                                                                                   ,conditionalPanel('input.salesChartType=="Sales"',
#                                                                                                                                                                     shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('salesContinuous_trend'))
#                                                                                                                                                                     ) )),
#                                                                                                                                   shiny::tabPanel("Spends",
#                                                                                                                                                   shiny::fluidRow(shiny::column(6, shiny::radioButtons("spendsChartType", "Chart Type", choices=c("Spends", "Spends Overlay"), inline = TRUE))),
#                                                                                                                                                   conditionalPanel('input.spendsChartType=="Spends Overlay"',
#                                                                                                                                                                    shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('spendsComparison_trend'))
#                                                                                                                                                                    )                                                                                                                                                               )
#                                                                                                                                                   ,conditionalPanel('input.spendsChartType=="Spends"',
#                                                                                                                                                                     shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('spendsContinuous_trend'))
#                                                                                                                                                                     ) )),
#                                                                                                                                   shiny::tabPanel("Contribution", 
#                                                                                                                                                   shiny::fluidRow(shiny::column(6, shiny::radioButtons("comparisonChartType", "Chart Type", choices=c("Contribution", "Contribution Overlay"), inline = TRUE))),
#                                                                                                                                                   conditionalPanel('input.comparisonChartType=="Contribution"',
#                                                                                                                                                                    shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('contribContinuous_trend'))
#                                                                                                                                                                    ))
#                                                                                                                                                   ,conditionalPanel('input.comparisonChartType=="Contribution Overlay"',
#                                                                                                                                                                     shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('contribComparison_trend'))
#                                                                                                                                                                     )) 
#                                                                                                                                   ),
#                                                                                                                                   shiny::tabPanel("ROI", 
#                                                                                                                                                   shiny::fluidRow(shiny::column(6, shiny::radioButtons("ROIChartType", "Chart Type", choices=c("ROI", "ROI Overlay"), inline = TRUE))),
#                                                                                                                                                   conditionalPanel('input.ROIChartType=="ROI"',
#                                                                                                                                                                    shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('roi_Continuous_trend'))
#                                                                                                                                                                    ))
#                                                                                                                                                   ,conditionalPanel('input.ROIChartType=="ROI Overlay"',
#                                                                                                                                                                     shiny::fluidRow(class='box1', shiny::column(12, shiny::plotOutput('roi_trend'))
#                                                                                                                                                                     )) 
#                                                                                                                                   )
#                                                                                                      ))
#                                                                                             )
#                                                                                             
#                                                                                  )))),
#                                                         div(class='card',div(class='container-fluid',
#                                                                              shiny::column(12,
#                                                                                            shiny::actionButton('optimizationResultsCompareButton', 'Compare Scenarios', class='next_button', icon = icon("check-circle"))
#                                                                                            #shiny::actionButton('simulationResultsSimButton', 'Generate Simulation Scenarios', class='next_button', icon = icon("check-circle")),
#                                                                                            #shiny::actionButton('simulationResultsOptButton', 'Generate Optimization Scenarios', class='next_button', icon = icon("check-circle")) 
#                                                                                            )
#                                                                              )))
#                                 )
#                                 #### Simulation Div
# 
# 
# )


selObj <- div(class='card',navbarPage("",id="optScenarioNav",
                                      tabPanel(div(style="padding-left:5px","1. Add Business Constraints"),value="selectObjectiveTab",
                                               box(
                                                 width = 12,
                                                 column(11, h2('Select Business Objective')),
                                                 column(1, style='padding-top:25px', actionButton("viewBusinessObjectiveHelpText", label = NULL,
                                                                                                  icon = icon("info-sign", lib = "glyphicon"))),
                                                 column(7, uiOutput("viewOptimisationDates_atConstraints")),
                                                 # column(11, shiny::HTML('This option gives user the flexibility to choose the current business priority from the below given option to further analysis
                                                 #                        <ul><li>Maximize Sales option gives user an option to maximize the sales under current constraint</li>
                                                 #                        <li>Maximize ROI option gives user an option to maximize the return under given spends constraint</li>
                                                 #                        <li>Revenue Chase option can be used for optimizing spends to achieve a target revenue</li>
                                                 #                        </ul>')),
                                                 column(12, uiOutput("businessPriority")),
                                                 column(12, verbatimTextOutput("businessPriorityMessage"))
                                                 
                                               ),
                                               
                                               box(
                                                 width = 12,
                                                 column(11, h2('Set Budget')),
                                                 # column(7, uiOutput("viewOptimisationDates_atConstraints")),
                                                 column(12, uiOutput("channelBudgetHeader")),
                                                 column(12, DT::dataTableOutput("ly_channelSpends")),
                                                 column(7, uiOutput("lastYearBudget")),
                                                 column(4, style='margin-left:50px', uiOutput("newBudget")),
                                                 column(12, shiny::div(style="color:red", shiny::textOutput("revenueChaseMsg")))
                                                 # column(11, tags$h5('*Budget is added in Constraint Table, You can scroll down to view.')),
                                               ),
                                               # column(12, tags$hr(style="border-color: grey;")),
                                               box(
                                                 width = 12,
                                                 column(11, h2('Configure Custom Constraints')),
                                                 column(12, shiny::tags$p(shiny::HTML(constraintsText))),
                                                 column(11, h3('Add Custom Constraints')),
                                                 column(12,shiny::tabsetPanel(type = "tabs",
                                                                              shiny::tabPanel("Business Constraints",
                                                                                              
                                                                                              column(11, h4('Enter Business Constraints')),
                                                                                              column(1, style='padding-top:10px',actionButton("loadBusinessConstraintsHelpText", label = NULL,
                                                                                                                                              icon = icon("info-sign", lib = "glyphicon"))),
                                                                                              column(12, uiOutput("enteredConstraint",style='border:1px solid lightgrey; padding:20px')),
                                                                                              shiny::br(),
                                                                                              column(12,shiny::h4('Constraints List')),
                                                                                              column(12, DT::dataTableOutput("constraint")),
                                                                                              column(12,align='right',shiny::actionButton('deleteConstraintRow','Delete Selected Constraint',icon=icon('trash')))
                                                                              ),
                                                                              shiny::tabPanel("Channel Bounds",
                                                                                              column(11, h4('Upper and Lower Bounds')),
                                                                                              column(1, style='padding-top:15px', actionButton("optBoundHelp", label = NULL,
                                                                                                                                               icon = icon("info-sign", lib = "glyphicon"), 
                                                                                                                                               width = "40px")),
                                                                                              # column(7, uiOutput("boundPercentage")),
                                                                                              shiny::column(3,shiny::textInput('boundPercent','Enter Bound Window (%)',value = 10)),
                                                                                              # shiny::column(2,style='padding-top:25px',shiny::actionButton('selectedboundPercent','Submit')),
                                                                                              
                                                                                              column(12,p('The Lower and Upper bounds have been calculated as per budget and bound percentage. The lower and upper bound values in the table are editable. Click on the cell to edit its value.')),
                                                                                              # column(11, shiny::helpText("This file allows user to input custom constraints as required")), 
                                                                                              column(12, rHandsontableOutput("channelBounds"))
                                                                              ))),
                                                 column(12, tags$hr()),
                                                 # column(12,shiny::h4('Constraints List')),
                                                 # column(12, DT::dataTableOutput("constraint")),
                                                 # column(12,align='right',shiny::actionButton('deleteConstraintRow','Delete Selected Constraint',icon=icon('trash'))),
                                                 column(12, uiOutput("checkboxEditConstraint")),
                                                 #column(12, uiOutput("downloadConstraintFile")),
                                                 #column(12, uiOutput("constraint_file")),
                                                 column(12, uiOutput("goahead"))
                                                 #column(12, DT::dataTableOutput("optimised_table")),
                                               ),
                                              column(12, uiOutput("addConstraintsTabUI"))
                                               
                                               
                                      ), 
                                      tabPanel(div(style="padding-left:5px","2. Configure Optimization"), value = "selectOptimizationTab",
                                               box(
                                                 width = 12,
                                                 column(12, h2('Configure Optimization Parameters')),
                                                 column(10, uiOutput("viewOptimisationDates_atTechnique")),
                                                 column(1, style='padding-top:15px', actionButton("optiTechniqueHelpText", label = NULL,
                                                                                                  icon = icon("info-sign", lib = "glyphicon"), 
                                                                                                  width = "40px")),
                                                 column(12, uiOutput("optimisationTechnique")),
                                                 column(12, verbatimTextOutput("message")), 
                                                 column(12, verbatimTextOutput("value"))
                                                 ),
                                               box(
                                                 width = 12,
                                                 column(12, uiOutput("add_opti_scenario_ui")),
                                                 column(12, h4("List of submitted scenarios")),
                                                 column(12, DT::dataTableOutput("opti_scenarioTable_UI")),
                                                 column(12, style='padding:25px', align='right',shiny::actionButton('opti_scenario_delete','Delete Selected Scenarios',icon=icon('trash')))
                                               ),
                                               column(12, uiOutput("selectOptimizationTabUI"))
                                               
                                               
                                      )
                                               )
)

#optimisationResults <- 


tabCompareScenarioBody <- div(div(class='card',div(class='container-fluid',
                                                   column(11,shiny::tags$h2('Compare Scenarios')),
                                                   column(1, style='padding-top:25px', actionButton("viewCompareScenarioHelpText", label = NULL,
                                                                                                    icon = icon("info-sign", lib = "glyphicon"))),
                                                   column(12, uiOutput('viewCompareScenarioDatesText')),
                                                   column(12,shiny::tags$h3('Select Scenarios')),
                                                   column(12, uiOutput('test_a')),
                                                   column(12,uiOutput('compareScenariosUI')),
                                                   column(12,uiOutput('compareScenariosSummaryPanelUI')))),
                              #Boxes creation for the overall comparison between Scenario 1 and Scenario 2 numbers(Spend, Sales, ROI)
                              
                              
                              div(class='card',div(class='container-fluid',
                                                   #Scenario 1 and 2 Sales box      
                                                   column(3,
                                                          br(),
                                                          fluidRow(
                                                            # column(3, img(src="/Images/imp_sales.png",width = "100%",height = "100%"),align="right"),
                                                            column(12, 
                                                                   fluidRow(
                                                                     column(3, "",style = "font-size: 120%", align="left"),
                                                                     column(6,tags$b("Sales",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
                                                                     column(3,"",style = "font-size: 120%", align="left")),
                                                                   br(),                                                                                               
                                                                   fluidRow(
                                                                     column(5, uiOutput("scenario1_sales"), style="font-size: 120%;font-weight:bold;color:#1F77B4;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
                                                                     column(1, "|", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
                                                                     column(5, uiOutput("scenario2_sales"), style="font-size: 120%;font-weight:bold;color:#9cb3d0;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
                                                                   ),
                                                                   br(),                                                                                                    fluidRow(
                                                                     column(5, tags$p("First Scenario"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
                                                                     column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
                                                                     column(5, tags$p("Second Scenario"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
                                                                   )                                                                                                    
                                                            )),
                                                          br(),
                                                          class="opti_out_kpi_box4"),
                                                   column(3,
                                                          br(),
                                                          #Scenario 1 and 2 Spends box
                                                          fluidRow(
                                                            # column(3, img(src="/Images/spend.png",width = "120%",height = "120%"),align="right"),
                                                            column(12, 
                                                                   fluidRow(
                                                                     column(3, "",style = "font-size: 120%", align="left"),
                                                                     column(6,tags$b("Spend",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
                                                                     column(3,"",style = "font-size: 120%", align="left")), br(),
                                                                   
                                                                   fluidRow(
                                                                     column(5, uiOutput("scenario1_spend"), style="font-size: 120%;font-weight:bold;color:#1F77B4;margin-left: 6.7%;padding-top: 0.5%;padding-left: 0%; padding-right: 0%;", align="left"),
                                                                     column(1, "|",style = "font-size: 120%;margin-left: 0%;padding-right: 0%;padding-left: 0%;", align="left"),
                                                                     column(5, uiOutput("scenario2_spend"), style = "font-size: 120%;font-weight:bold;color:#9cb3d0;padding-left: 0%;padding-top: 0.5%; padding-right: 0%;", align="left")
                                                                   ),
                                                                   br(),                                                                                                    fluidRow(
                                                                     column(5, tags$p("First Scenario"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
                                                                     column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
                                                                     column(5, tags$p("Second Scenario"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
                                                                   )
                                                            )),
                                                          br(),
                                                          class="opti_out_kpi_box4"),
                                                   column(3,
                                                          br(),
                                                          #Scenario 1 and 2 Contributions box
                                                          fluidRow(
                                                            # column(3, img(src="/Images/spend.png",width = "100%",height = "100%"),align="right"),
                                                            column(12, 
                                                                   fluidRow(
                                                                     column(3, "",style = "font-size: 120%", align="left"),
                                                                     column(6,tags$b("Contribution",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
                                                                     column(3,"",style = "font-size: 120%", align="left")), br(),
                                                                   
                                                                   fluidRow(
                                                                     column(5, uiOutput("scenario1_contrib"), style="font-size: 120%;font-weight:bold;color:#1F77B4;margin-left: 6.7%;padding-top: 0.5%;padding-left: 0%; padding-right: 0%;", align="left"),
                                                                     column(1, "|",style = "font-size: 120%;margin-left: 0%;padding-right: 0%;padding-left: 0%;", align="left"),
                                                                     column(5, uiOutput("scenario2_contrib"), style = "font-size: 120%;font-weight:bold;color:#9cb3d0;padding-left: 0%;padding-top: 0.5%; padding-right: 0%;", align="left")
                                                                   ),
                                                                   br(),                                                                                                  fluidRow(
                                                                     column(5, tags$p("First Scenario"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
                                                                     column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
                                                                     column(5, tags$p("Second Scenario"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
                                                                   )
                                                            )),
                                                          br(),
                                                          class="opti_out_kpi_box4"),
                                                   #Scenario 1 and 2 ROI box
                                                   column(3,
                                                          br(),
                                                          fluidRow(
                                                            # column(3, img(src="/Images/spend.png",width = "100%",height = "100%"),align="right"),
                                                            column(12, 
                                                                   fluidRow(
                                                                     column(3, "",style = "font-size: 120%", align="left"),
                                                                     column(6,tags$b("ROI",style="font-size : 120%;color:rgb(110,110,110);",align="center")),
                                                                     column(3,"",style = "font-size: 120%", align="left")),
                                                                   br(),                                                                                             
                                                                   fluidRow(
                                                                     column(5, uiOutput("scenario1_roi"), style="font-size: 120%;font-weight:bold;color:#1F77B4;margin-left: 6.7%;padding-top: 0.5%;padding-left: 0%; padding-right: 0%;", align="left"),
                                                                     column(1, "|",style = "font-size: 120%;margin-left: 0%;padding-right: 0%;padding-left: 0%;", align="left"),
                                                                     column(5, uiOutput("scenario2_roi"), style = "font-size: 120%;font-weight:bold;color:#9cb3d0;padding-left: 0%;padding-top: 0.5%; padding-right: 0%;", align="left")
                                                                   ),
                                                                   br(),                                                                                                    fluidRow(
                                                                     column(5, tags$p("First Scenario"), style="font-size: 100%;color:#1F77B4;font-weight:bold;margin-left: 6.5%;padding-right: 0%;margin-top: 0.5%;padding-left: 0%;", align="left"),
                                                                     column(1, " ", style="font-size: 120%;padding-left: 0%;padding-right: 0%;", align="left"),
                                                                     column(5, tags$p("Second Scenario"), style="font-size: 100%;color:#9cb3d0;font-weight:bold;margin-top: 0.5%;margin-left: 1%;padding-left: 0%;padding-right: 0%;" , align="left")
                                                                   )
                                                            )),
                                                          br(),
                                                          class="opti_out_kpi_box4")
                                                   
                              ), shiny::br()),
                              div(class='card',div(class='container-fluid',navbarPage("",
                                                                                      tabPanel("1. Tabular View",
                                                                                               column(12,uiOutput('compareScenariosSummaryTablesUI'))),
                                                                                      tabPanel("2. Summary View",
                                                                                               column(12,uiOutput('compareScenariosSummaryInfoUI')),
                                                                                               column(12,uiOutput('compareScenariosSummaryPlotsUI'))),
                                                                                      tabPanel("3. Time Series View",
                                                                                               column(12,uiOutput('compareScenariosTimeSeriesUI')),
                                                                                               column(12,uiOutput('compareScenariosTimeSeriesPlotsUI')))
                              ))),
                              
                              div(class='card',div(class='container-fluid',
                                                   shiny::column(12,
                                                                 shiny::actionButton('simulationResultsSimButtonCompare', 'Add Simulation Scenario', class='begin_button'),
                                                                 shiny::actionButton('simulationResultsOptButtonCompare', 'Add Optimization Scenario', class='begin_button'),
                                                                 shiny::actionButton('beginScenarioPlanningButtonCompare', 'View Created Scenarios', class='begin_button'),
                                                                 shiny::actionButton('recommendationOptButton2', 'Recommendations', class='second_button')
                                                                 
                                                                 
                                                                 )))
                              
                              )

tabRecommendBody<- div(
                        div(class='card',div(class='container-fluid',
                                             column(12,shiny::tags$h1('Recommendations')),
                                             column(11,shiny::tags$h2('Insights'))
                        )),
                        
                        div(class='card',div(class='container-fluid',
                                             box( width = 12,fluidRow(
                                               htmlOutput("Recommender_Text1")%>%
                                                         htmltools::h3(style="color:#235e71")
                                                      )
                                             )
                        )
                        ),
                        div(class='card',div(class='container-fluid',
                                             
                                             column(12,shiny::tags$h2('Select Scenario for Detailed Insights'))
                                             
                        )),
                        
                        div(class='card',div(class='container-fluid',
                                             box(width = 12,
                                             column(12,uiOutput("viewSelectedScenarioUi"))
                                             )
                        )),
                        
                        div(class='card',div(class='container-fluid',
                                             column(12,shiny::tags$h3('Channel Mix'))
                        ),
                        
                        div(class='card',
                            div(class='card',div(class='container-fluid',
                                                 box( width = 11,
                                                      fluidRow(
                                                        column(12,highchartOutput('RecommendationStackedChart')),
                                                      )
                                                      
                                                 )
                            )
                            ))),
                        
                        div(class='card',div(class='container-fluid',
                                             column(12,shiny::tags$h3('Metric Information'))
                        )),

                        div(class='card',div(class='container-fluid',
                                             column(12,tags$b(tags$i('Click on any of the boxes below for Drill Down View')))
                        )),
                        
                        div(class='card',div(class='container-fluid',
                                             box( width = 11,
                                                  fluidRow(
                                                    
                                                    #valueBox(subtitle = "Channel With Highest Spends", value= uiOutput("Scenario1_Spend_Channel"), icon = icon("credit-card")),
                                                    valueBoxOutput("box_01"),
                                                    valueBoxOutput("box_02"),
                                                    valueBoxOutput("box_03"),
                                                    valueBoxOutput("box_04")
                                                    #valueBox(subtitle = "Channel With Highest Sales", value= uiOutput("Scenario1_Sales_Channel"), icon = icon("hand-holding-usd"),color = "red"),
                                                    #valueBox(subtitle = "Channel With Highest ROI", value= uiOutput("Scenario1_ROI_Channel"), icon = icon("coins"),color = "yellow"),
                                                    #valueBox(subtitle = "Channel With Highest Contribution", value= uiOutput("Scenario1_Contribution_Channel"), icon = icon("donate"),color = "yellow"),
                                                    #textOutput("print")
                                                  )
                                             )  
                        )
                        ),
                        
                        
                        
                        div(class='card',div(class='container-fluid',
                                             column(12,shiny::tags$h3('Drill Down Table')),
                                             
                        ),
                        
                        div(class='card',
                            div(class='card',div(class='container-fluid',
                                                 box( width = 11,
                                                      fluidRow(
                                                        column(11,reactableOutput('drillTable')),
                                                      )
                                                      
                                                 )
                            )
                            ))),
                        
                        div(class='card',div(class='container-fluid',
                                             column(12,shiny::tags$h3('Spends Distribution'))
                        ),
                        
                        div(class='card',div(class='container-fluid',
                                             column(12,uiOutput("viewSelectedMonthUI"))
                        ),
                        
                        div(class='card',
                            div(class='card',div(class='container-fluid',
                                                 box( width = 12,
                                                      fluidRow(
                                                        #column(12,plotOutput('radarPlot')),
                                                        column(12,plotOutput('radarPlot', width = 800, height = 600)),
                                                      )
                                                      
                                                 )
                            )
                            ))))
                        
                        
  
)

#browser()
#stripped_sim_number_test <- textOutput("selected_var",inline = TRUE)

tabScenarioHomeBody<- div(
  div(class='card',div(class='container-fluid',
                       column(12,shiny::tags$h1('Scenario History'))
  ),
  br(),
  br()),

  div(class='card',div(class='container-fluid',
                       uiOutput('scenario_history'))
                       #box( width = 12,fluidRow(
                         #DT::dataTableOutput("scenario_history"),
                         #textOutput('myText')
                       #)
                       )
  )




####################################### App Layout #####################################################
# Application title
version <- "20.11.04"
title <- dashboardHeader(title = tags$div(tags$div(img(src = 'Logos/logo.png', title = 'Mu Sigma'
                                                       ,height = "40px"),style='float:left')
                                          # , span(style='font-size:15px',paste0('v',version))
) 
, titleWidth = 100
# tags$div(),
# tags$li(class = "dropdown", paste0('MMx Simulation and Optimization Brick ', 'v', version), style = 'font-size:20px;color:white;margin:10px;width:100%;display:inline-block;text-align:center'),
, tags$li(class = "dropdown", actionButton("scenarioDownload", "Download Scenario",style='margin-top:6px;margin-right:6px;',icon=icon('download')))
, tags$li(class = "dropdown", actionButton("reportDownload", "Download Report",style='margin-top:6px;margin-right:6px;',icon=icon('download')))
# dropdownMenuOutput("messageMenu")

)

test_char <- paste0( " Created So Far")
test_char_history <- paste0( "Total: ")
stripped_sim_number <- textOutput("selected_var",inline = TRUE)
stripped_opti_number <- textOutput("selected_optivar",inline = TRUE)
stripped_history_number <- textOutput("selected_historyvar",inline = TRUE)

#print(stripped_sim_number)

sidebar <- dashboardSidebar(
  sidebarMenu(id="sideTabs",
              # selected="Configure", id='mynavlist',
              menuItem("Home", tabName = "tabHome",icon = icon("home", class="fa-pull-left"), selected = T),
              menuItem("Configuration", tabName = "tabConfigure", icon = icon("cogs", class="fa-pull-left")),
              menuItem("Scenario Planning",id= 'chartsID', tabName = 'charts',startExpanded = FALSE ,expandedName = "CHARTS" ,
                       menuSubItem(HTML(paste0("Scenario History", "<br/> <i>( ", test_char_history , stripped_history_number, " Scenarios Created )</i>")), tabName = "hiddenCharts", icon = icon("history")),
                       menuSubItem(HTML(paste0("Simulation Scenarios", "<br/> <i>( ", stripped_sim_number,test_char , " )</i>")), tabName = "tabGenerateScenarios", icon = icon("tasks")),
                       #menuSubItem("Simulation Results", tabName = "tabSimulationResults", icon = icon("bar-chart-o")),
                       menuSubItem(HTML(paste("Optimization Scenarios", "<br/> <i>( ", stripped_opti_number,test_char , " )</i>")), tabName = "tabOptimization", icon = icon("wrench")),
                       menuSubItem("All Scenario Results", tabName = "tabSimulationResults", icon = icon("bar-chart-o"))
                       ,icon = icon("play-circle", class="fa-pull-left")),
              #menuSubItem("Optimization Results", tabName = "tabOptimizationResults", icon = icon("bar-chart-o")))
              #menuItem("Simulation Scenarios", tabName = "tabGenerateScenarios", icon = icon("tasks", class="fa-pull-right")),
              #menuItem("Simulation Results", tabName = "tabSimulationResults", icon = icon("bar-chart-o", class="fa-pull-right")),
              #menuItem("Optimization Scenarios", tabName = "tabOptimization", icon = icon("wrench", class="fa-pull-right")),
              #menuItem("Optimization Results", tabName = "tabOptimizationResults", icon = icon("bar-chart-o", class="fa-pull-right")),
              menuItem("Scenario Comparison", tabName = "tabCompareScenario", icon = icon("th", class="fa-pull-left")),
              menuItem("Recommendations", tabName = "tabRecommend", icon = icon("thumbs-up ",class="fa-pull-left"))
              #hidden(menuItem("hiddenCharts", tabName = "hiddenCharts"))
  )
  , width = 200
  #, collapsed = TRUE
  
)

body <- dashboardBody(
  useShinyjs(),
  ### Comment the line below to enable all tabs at once
  extendShinyjs(text = jscode, functions = c("disableTab", "enableTab", "disableMenu", "enableMenu", "greenTab", "greenMenu")),
  inlineCSS(css),
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    
                    "))
    ),
  includeCSS('./www/Styles/styles.css'),
  shinyBS::bsModal("reportDownloadPopup", "Click on the button below to download Report", "reportDownload", size = "large",
                   column(12,uiOutput('downloadReportUI')),
                   #shiny::div(style = "color:green",shiny::textOutput("reportDownloadFinish")),
                   shiny::div(style = "color:red",shiny::textOutput("downloadErrorMsg"))
                   ),
  shinyBS::bsModal("scenarioDownloadPopup", "Choose from the generated scenarios", "scenarioDownload", size = "large",
                   column(12,uiOutput('downloadScenarioUI')),
                   column(12, div(style = "color:green",
                                  textOutput("ScenarioDownloadText")))
                   ),
  
  # This section is used to add header or text or title to the right side of sidebar 
  tags$head(tags$style(HTML(
    '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 0px;
        overflow: hidden;
        color: white;
    }
      .myClassVersion { 
        font-size: 12px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 0px;
        overflow: hidden;
        color: white;
      }
      
      
    '))),
  tags$script(HTML(paste0('$(document).ready(function() {
	        $("header").find("nav").append(\'<span class="myClass"> MMx Simulation and Optimization Brick ','</span>\');
      })'))),
  tags$script(HTML(paste0('$(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClassVersion">', 'v', version ,'</span>\');
      })'))),
  tabItems(
    tabItem(tabName = "tabHome", tabHomeBody),
    tabItem(tabName = "tabConfigure", tabConfigureBody),
    tabItem(tabName = "hiddenCharts", tabScenarioHomeBody),
    tabItem(tabName = "tabGenerateScenarios", tabGenerateScenariosBody),
    tabItem(tabName = "tabSimulationResults", tabSimulationResultsBody),
    tabItem(tabName = "tabOptimization", selObj),
    #tabItem(tabName = "tabOptimizationResults", optimisationResults),
    tabItem(tabName = "tabCompareScenario", tabCompareScenarioBody),
    tabItem(tabName = "tabRecommend",tabRecommendBody)
  )
    )


ui <- function(request){ dashboardPage(
  # skin = 'black',
  title="MMx Simulation and Optimization Brick",
  title,
  sidebar,
  body)

}
