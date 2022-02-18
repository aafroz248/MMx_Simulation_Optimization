####################################################################################
# Title : Marketing Mix Simulation and Optimization Tool
# Created : 8th November 2018
# Author : Abhinav Karnwal, Sunuganty Achyut Raj, Vyshali B
# Version : 19.06.01
# Description : Upload a marketing mix model and simulate/optimize different spend scenarions
####################################################################################





loadPackage <- function(pkgName){
    
    if (!do.call(require, list(pkgName))) {
        install.packages(pkgName, dependencies = T, repos = "http://cran.us.r-project.org")
        do.call(library, list(pkgName))
    }
}

packages <- c('shiny',
              'shinyjs',
              'shinydashboard',
              'shinyBS',
              'jsonlite',
              'scales',
              'DT',
              'tidyr',
              'ggplot2',
              'reshape',
              'purrr',
              'data.table',
              'rhandsontable',
              'formattable',
              'gdata',
              'tools',
              'nloptr',
              'xlsx',
              "prediction",
              "mopsocd",
              "prettydoc",
              "highcharter",
              "fmsb",
              "reactable"
)

#Importing all packages
lapply(packages, FUN = function(p){ loadPackage(p) })

### Initializing environment for nested functions used in optimization
OFN <- new.env()

vcolors <- c('#1f77b4',
                    "#aec7e8",
                    '#ff7f0e',
                    '#ffbb78',
                    '#2ca02c',
                    '#98df8a',
                    '#d62728',
                    '#ff9896',
                    '#9467bd',
                    '#c5b0d5',
                    '#8c564b',
                    '#c49c94',
                    '#e377c2',
                    '#f7b6d2',
                    '#7f7f7f',
                    '#c7c7c7',
                    '#bcbd22',
                    '#dbdb8d',
                    '#17becf',
                    '#9edae5')


vcolors <- c('#1f77b4',"#aec7e8",'#ffbb78','#17becf','#9edae5','#7f7f7f','#ff9896','#ff7f0e','#2ca02c'
             ,'#d62728','#9467bd','#98df8a','#c5b0d5','#8c564b','#c49c94','#e377c2','#f7b6d2','#c7c7c7'
             ,'#bcbd22','#dbdb8d')

summarise_at_fun <- function(variables, func, data){
  data2 <- data %>%
    summarise_at(vars(variables), funs(get(func)(.)))
  return(data2)
}


loadTrainDataText <- "This will load a sample of the training dataset used in the modeling phase. This will be used to transform the schema of the simulation dataset to that of the training dataset in order to run the model on the former without any hiccups. A sample of the training dataset used is sufficient for schema emulation."

loadModelTextData <- "This will load the MMx model object created in the modeling phase and display the inherent formula used to forecast the spends. This model object will be used to simulate results for the specified range of spends. Please create the RDS object with the following variables:
               <ul>
               <li>model* - Containing the MMx model object created in the modeling phase</li>
               <li>pred_funs* - The class of the model with the predict function defined</li>
               <li>spend_variables - Containing a list of channels/spend variables used in the model</li>
               <li>y_variable - Containing the target variable for which predictions are made</li>
               <li>panel_column - Containing the panel variable, for eg. State, region, country, etc. </li>
               <li>seasonal_column - Containing the seasonal variable, for example Month, period, week, etc. </li>
               <li>trend_column - Containing the time trend variable</li>
               <li>date_column - Containing the date of the data </li>
               <li>date_format - Containing the format of the date </li>
               <li>train_data - Containing the data used to train the model</li>
               </ul>
               *These variables must be present in the model object"

loadHistoricalDataText <- "Upload the historical dataset (superset of train dataset) based on which the MMx model was built. <ul><li>Spend patterns will be decided based on this dataset. </li><li>Spend variables must be present in the wide format, i.e. each spend channel must be specified as individual columns with spend values populated as rows corresponding to each column.</li></ul>"

variableConfigureText <- 'The following columns need to be specified for generating the simulation data:
  <ul><li>Spend columns specify all the marketing channels whose spends are to be adjusted based on budget constraints</li>
  <li>Panel columns specify the sampling group over which the marketing mix analysis needs to be carried out</li>
  <li>Seasonal columns specify the column for seasonality component (based on which the seasonality has to be checked). Example: Month, period, week, etc. </li>
  <li>Trend column specifies the trend component of the mix</li>
  </ul>'

dateConfigureText <- '<h2>Date Column Selection</h2><p>Select the Datetime variable and format. Specify the order in which the date elements (year, month, day) are and the function figures out the rest.</p><strong>Note:</strong> The default for the timezone format methods is “ymdHMS”. Some of the examples are given below<p></p>
  <table class="table table-condensed">
  <thead>
  <tr class="header">
  <th>DateTime</th>
  <th>format</th>
  </tr>
  </thead>
  <tbody>
  <tr class="odd">
  <td>09/24/2014</td>
  <td>mdy</td>
  </tr>
  <tr class="even">
  <td>09/24/2014 15-23-10</td>
  <td>mdy HMS</td>
  </tr>
  <tr class="odd">
  <td>090102</td>
  <td>ymd</td>
  </tr>
  </tbody>
  </table>'

adstockConfigureText <- '<strong>If you wish to modify the adstock decay values, double-click on cells in Adstock_Decay column, enter new values and click on Apply Adstock button to adstock tranform the spends. Else, you can apply default adstock value of 0 by directly clicking on Apply Adstock button</strong>'

adstockConfigurePercentText <- 'If you wish to enter 20 percent adstock value for a channel, enter 0.2 in the Adstock_Decay column for the concerned channel'

mROIPlotsText <- 'mROI plots from the model object will be displayed here. These plots can help you place constraints and bounds on spends for designing optimization and simulation scenarios.'
  
timePeriodConfigText <- 'This will define the simulation/optimization period for which sales will be predicted based on the simulated/optimized spends. <ul><li>Trends from the historical period corresponding to the selected period will be used to generate the simulation/optimization dataset.</li> <li>The period selected must have a comparable historical period in the uploaded historical dataset.</li> <li>The time series frequency refers to the level of the date column specified, i.e. is it monthly, daily or weekly data.</li></ul>'

exPostText <- "Non-spend variables are such variables which can't be extrapolated from historical data but will be needed for simulating future scenarios.<ul><li>For such cases, the user has to load the dataset for the simulation period with the same column names as specified in the historical dataset. For example: year round holidays flag/ sale days are examples of such non-spend variables which can't be extrapolated </li>
<li> All external variables used in the model creation must have corresponding historical variables in the uploaded historical dataset</li></ul>"

histPerformanceText <- 'Observe and interpret historical behaviour of marketing channel spends, sales, contributions and RoI. <br><ul><li>The following graphs show both, a consolidated view of spend across channels and a time series view in order to understand spend variation in months of peak marketing activity and understand the campaign effectiveness.</li> <li>By selecting the panel of interest, the same results can be viewed for the selected panel as well.</li></ul>'

simScenarioText <- 'This functionality allows one to create and save multiple feasible scenarios (one at a time) and view them later for comparison or review by the client'

schedulingMethodText <- 'For each scenario, the user has to pick one of the three spend scheduling strategies described below for creating the simulation dataset and supply the relevant inputs.'

genSimDataText <- 'This section creates a simulation dataset based on the variation parameter and spend strategy specified in the previous section. A user can additionally, download this simulated data, edit it if required, and upload for running the simulation'

editSimDataText <- 'To tweak the generated scenario data, a user can additionally, download this simulated data, edit it as required and upload for running the simulation. Skip this section if you do not want to make any customizations to the default scenarios generated.'

runSimulationText <- 'Running the model on simulation data generated above creates the simulation results dataframe. The simulation results show the Sales corresponding to different combinations of spend values from the data generated in the section above.'

viewSimulationResultsText <- 'Observe and interpret simulated behaviour of marketing channel spends, sales, contributions and RoI. <br><ul><li>The following graphs show both, a consolidated view of spend across channels and a time series view in order to understand spend variation in months of peak marketing activity and understand the campaign effectiveness.</li> <li>By selecting the panel of interest, the same results can be viewed for the selected panel as well. By default, the results are aggregated at an estate level.</li></ul>'

scenarioTimeSeriesText <- 'By looking at a time series graph of spends and sales, one can answer questions such as:
    <ul>
      <li>Which channels contribute the most to my sales and in which months?</li>
      <li>Am I spending enough in high return channels? Have these channels changed with last year?</li>
      <li>In months of peak activity like Christmas and Black Friday, are my returns aligned with market expectations?</li>
    </ul>'

scenarioSummaryText <- 'By looking at a consolidated view of spends and sales, across the simulation time period, one can answer questions such as:
  <ul>
    <li>How have the simulated sales increased from that of the historical value and what type of spending has led to this behavior?</li>
    <li>Has my strategy to increase spends in one channel over others, yielded better results than last year?</li>
    <li>Are there channels that worked better for me in the simulated year compared to the last?</li>
  </ul>'

compareScenarioText <- 'The visualizations below allow a user to compare two scenarios graphically by selecting the scenario of interest and helps answer the question - Of the two selected scenarios, which gives better results?'

constraintsText <- 'Add constraints in one of two ways as illustrated below:
<ul><li>By editing the constraints directly as specified in the Business Constraints and Channel Bounds tabs</li>
<li>By downloading sample constraint files, editing and uploading it to the Upload Constraint File section</li></ul>'
