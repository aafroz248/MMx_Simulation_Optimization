## Dependency packages
packages <- c('lmerTest',"magrittr", 'dplyr','lme4','ggplot2',"magrittr",
              'dplyr',
              'lme4',
              'mgcv',
              'purrr',
              'gridExtra', 
              'ggplot2',
              'reshape2',
              'tidyr',
              'parallel',
              'Metrics',
              'reactable')

# function to test if the package is already installed
# x <- package to be tested/installed
pkgInstall <- function(x){
  if(x %in% rownames(installed.packages()) == FALSE){
    install.packages(x, dependencies = TRUE)
    if(!require(x,character.only = TRUE)) stop(paste(x, "package not found"))
  }
  else {
    # print(paste(x, "is installed"))
  }
  do.call("library", list(x))
}

# installing packages
lapply(packages, FUN=pkgInstall)

## Range calculation
## Method 1

## This method involves spend range calculation by determining mean value of each spend variable 
## from the historical data. The upper and lower bounds of the spends range can then be defined 
## as increment and decrement by some percentage of mean value, which can be given as an input by user.

# Function to calculate lower bound value
lb_fun <- function(x,lb){
  avg_val <- mean(x)
  lb <- avg_val-(avg_val*lb)
  return(lb)
}

# Function to calculate upper bound value
ub_fun <- function(x,ub){
  avg_val <- mean(x)
  ub <- avg_val+(avg_val*ub)
  return(ub)
}

# Calculating min and max range for each spend variable

get_ranges_by_percent <- function(spend_variables, hist_df, lb=0.1, ub=0.1){
  range_df <- as.data.frame(matrix(1:(length(spend_variables)*2), nrow = length(spend_variables), ncol=2))
  colnames(range_df) <- c("min_val", "max_val")
  rownames(range_df) <- spend_variables
  range_df[,1] <- apply(hist_df[,spend_variables], 2 , lb_fun, lb=lb)
  range_df[,2] <- apply(hist_df[,spend_variables], 2 , ub_fun, ub=ub) 
  return(range_df)
}

##Method 2
# This method involves spend range calculation by determining maximum and minimum values 
# of each spend variable from the historical data.
#Calculating min and max range for each spend variable
get_ranges <- function(spend_variables, hist_df){
  range_df <- as.data.frame(matrix(1:(length(spend_variables)*2), nrow = length(spend_variables), ncol=2))
  colnames(range_df) <- c( "min_val", "max_val")
  rownames(range_df) <- spend_variables
  range_df[,1] <- apply(hist_df[,spend_variables], 2 , min)
  range_df[,2] <- apply(hist_df[,spend_variables], 2 , max)
  return(range_df)
}

## Simulation Data
# The spend variable values for simulation are populated at equal intervals and within the range defined in the 
# previous section. The user can provide a step input to set this interval. The interval between each value of a 
# spend variable is defined as the product of step input and it's range. 

# Function to obtain values of spend variables in defined range and increment
spend_values_fun <- function(x, range_df){
  y <- seq(from = range_df[x,]$min_val,  
           to = range_df[x,]$max_val, 
           by = (range_df[x,]$max_val -range_df[x,]$min_val)*stp
  )
  return(y)
}

create_simulation_data <- function(range_df, stp, input_panel_cols, input_seasonal_col, 
                                   input_trend_col, budget=0, object_list){
  
  # Creating data for different combinations of spend variable values
  sim_data <- lapply(1:nrow(range_df), spend_values_fun, range_df=range_df)
  sim_data <- do.call(expand.grid, sim_data)
  colnames(sim_data) <- rownames(range_df) 
  
  # Adding trend and seasonal columns to the data
  sim_data[,trend_column] <- input_trend_col
  sim_data[,seasonal_column] <- NA
  sim_data[,seasonal_column] <- sim_data[,seasonal_column] %>% as.factor
  levels(sim_data[,seasonal_column]) <- levels(object_list$train.data[,seasonal_column])
  sim_data[,seasonal_column] <- as.factor(input_seasonal_col)
  
  # Expanding data for each panel column variable
  # panel_df <- as.data.frame(matrix(rep(x = 0, length(input_panel_cols)), nrow = length(input_panel_cols), ncol = 1))
  panel_df <- as.data.frame(matrix(c(1:length(input_panel_cols)), 
                                   nrow = length(input_panel_cols), ncol = 1))
  colnames(panel_df) <- panel_column
  panel_df[, panel_column] <- panel_df[, panel_column] %>% as.character %>% factor
  levels(panel_df[, panel_column]) <- levels(data.frame(object_list$train_data)[,panel_column])
  # panel_df[, panel_column] <- factor(panel_df[, panel_column], 
  #                                    levels = levels(data.frame(object_list$train_data)[,panel_column]))
  # panel_df[,panel_column] <- panel_df[,panel_column] %>% as.factor
  # levels(panel_df[,panel_column]) <- levels(object_list$train_data[,panel_column])
  # panel_df[,panel_column] <- as.factor(input_panel_cols)
  sim_data <- merge(sim_data, panel_df , all=TRUE)
  
  # Budget Constraint
  if(budget!=0)
    sim_data <- subset(sim_data, rowSums(sim_data[, spend_variables]) <= budget)
  
  return(sim_data)  
}

## Simulation Results
# The simulation results show the Sales corresponding to different combinations of spend values 
# from the data generated in the section above. 
# Predicting y_variable using the market mix model
get_mmx_sales <- function(market_mix_model, sim_data, y_variable){
  # class(market_mix_model) <- 'lmerMod'
  y <- predict(market_mix_model, newdata = sim_data)
  sim_data[,y_variable] <- NA
  sim_data[,y_variable] <- y
  return(sim_data)
}

generatePanelPlot <- function(sim_data_map, input_spend_var1, input_spend_var2, panel_variable) {
  h_map <- ggplot2::ggplot(data = sim_data_map, aes_string(x = sim_data_map[,input_spend_var1], 
                                                           y = sim_data_map[,input_spend_var2])) + 
    ggplot2::geom_tile(stat = "identity", aes_string(fill = sim_data_map[,y_variable])) + 
    labs(x = input_spend_var1, y = input_spend_var2, fill = y_variable, title = unique(sim_data_map[, panel_variable])[1]) +
    scale_fill_gradient(low = "#619BFF", high = "#ffc151")
  h_map  
}

plot_data_map <- function(range_df, sim_data_map, input_spend_var1, input_spend_var2, panel_variable, no_of_panels = NULL){
  # browser()
  other_vars <- subset(spend_variables, !(spend_variables %in% c(input_spend_var1, input_spend_var2)))
  for(i in 1:length(other_vars)){  
    sim_data_map <- subset(sim_data_map, sim_data_map[,other_vars][i] == range_df[other_vars[i], "min_val"]) 
  } 
  
  noOfIterations <- length(unique(sim_data_map[, panel_variable]))
  if(!is.null(no_of_panels) && is.numeric(no_of_panels)){
    noOfIterations <- min(no_of_panels, length(unique(sim_data_map[, panel_variable])))
  }
  
  plotOutput <- list()
  invisible(
    lapply(1:noOfIterations, function(x){
       tempData <- sim_data_map %>% 
         filter_at(panel_variable, all_vars(. %in% levels(sim_data_map[, panel_variable])[x]))
       plotOutput <<- append(plotOutput, 
                             list(generatePanelPlot(tempData, input_spend_var1, input_spend_var2, panel_variable)))
    })
  )
  
  # h_map <- ggplot2::ggplot(data = sim_data_map, aes_string(x = sim_data_map[,input_spend_var1], 
  #                                                          y = sim_data_map[,input_spend_var2])) + 
  #   ggplot2::geom_tile(stat = "identity", aes_string(fill = sim_data_map[,y_variable])) + 
  #   labs(x = input_spend_var1, y = input_spend_var2, fill = y_variable ) 
  # h_map  
  return(plotOutput)
}

## Main function
run_simulation <- function(date_column, date_format, seasonal_column, panel_column,
                           trend_column, y_variable, spend_variables, budget=0, input_seasonal_col,
                           input_trend_col, input_panel_cols, from_date, to_date, model_file,
                           input_file, lb, ub, stp, input_spend_var1, input_spend_var2, method1){
  object_list <- readRDS(model_file)
  
  #Historical input data
  input_df <- read.csv(input_file,stringsAsFactors = FALSE)
  # Historical data from which the spends range is to be considered
  hist_df <- input_df[input_df[,panel_column] %in% input_panel_cols,] %>% filter_at(date_column,any_vars(. >= from_date & . <= to_date))
  
  # Select the market mix model from the object_list
  market_mix_model <- object_list$model
  # print(market_mix_model)
  
  range_df <- NULL
  # getting ranges using method 1
  if(method1){
    range_df <- get_ranges_by_percent(spend_variables, hist_df, lb, ub)
    head(range_df)
  }
  else {
    # getting ranges using method 2
    range_df <- get_ranges(spend_variables, hist_df)
    head(range_df)
  }
  
  # creating simulation data
  sim_data <- create_simulation_data(range_df, stp, input_panel_cols, input_seasonal_col, input_trend_col, budget, object_list)
  head(sim_data)
  
  # Run the model
  sim_data <- get_mmx_sales(market_mix_model, sim_data)
  head(sim_data)
  
  # plot data map
  plot_data_map(range_df, sim_data, input_spend_var1, input_spend_var2, panel_column)
}

# ###### Download Report

downloadReport <- function(mmxRDS, fileName){
  require(rmarkdown)
  
  # env = new.env(hash = TRUE, parent = parent.frame())
  # names(mmxRDS) %>% map(~{
  #   assign(.,mmxRDS[[.]],envir=env)
  # })
  
  rmarkdown::render('Source/report.Rmd'
                    ,params = mmxRDS,
                    switch(mmxRDS$format,
                           PDF = pdf_document(),
                           PrettyHTML = prettydoc::html_pretty(css = "../www/Styles/pretty_styles.css"),
                           HTML = html_document(css = "../www/Styles/html_styles.css" ,toc= T,toc_float= T),
                           
                           MSWord = word_document(toc = T)),
                    output_dir = "Downloads/" , 
                    output_file = paste(fileName,'.html',sep='')
  )
}


###############################################################################################
##################### Function to generate simulation data based on method selected ###########
###############################################################################################

gen_sim_data <- function(schedulingMethod, 
                         dataframe, 
                         adstock_df, 
                         parameter_df, 
                         all_date_df,
                         date_column, 
                         date_format,
                         panel_column,
                         trend_column, 
                         y_variable, 
                         frequency_variable
                         ){
  
  colnames(dataframe)[which(colnames(dataframe)=='variable')] <- 'Spend_Channel'
  colnames(dataframe)[which(colnames(dataframe)=='value')] <- 'Spends'
  
  if(date_column %in% colnames(parameter_df)){
    all_date_df[[paste0('Opti_', date_column)]] <- lubridate::parse_date_time(all_date_df[[paste0('Opti_', date_column)]],orders = date_format)
    all_date_df[[paste0('Hist_', date_column)]] <- lubridate::parse_date_time(all_date_df[[paste0('Hist_', date_column)]],orders = date_format)
    parameter_df[[date_column]] <- lubridate::parse_date_time(parameter_df[[date_column]],orders = date_format)
    parameter_df <- merge(parameter_df, all_date_df, by.x=c(date_column), by.y=c(paste0('Opti_', date_column)), all.x=TRUE)
    drops <- c(date_column)
    parameter_df <- parameter_df[ , !(names(parameter_df) %in% drops)]
    setnames(parameter_df, old=c(paste0('Hist_', date_column)), new=date_column)
  }
  
  
  ##### Calculate simulated spend based on method selected ####
  
  if(schedulingMethod == 'Custom Spend Pattern'){
    print("#################### Here ####################")
    
    
    
    final_df = merge(dataframe, parameter_df, by=c("Spend_Channel", panel_column, date_column), all.x=TRUE)
    final_df$Total_Historical_Spend[is.na(final_df$Total_Historical_Spend)] <- (final_df$Spends[is.na(final_df$Total_Historical_Spend)])
    setnames(final_df, old=c("Total_Historical_Spend"), new=c("Simulated_Spend"))
    drops <- c("Spends", y_variable, "time")
  }
  if(schedulingMethod == 'Percentage change from historical pattern'){
    parameter_df$Percentage_Change <- as.numeric(as.character(parameter_df$Percentage_Change))
    grouped_data = dplyr::group_by_at(dataframe, c(date_column, "Spend_Channel", panel_column))
    agg_data = dplyr::summarise(grouped_data, Total_Spend = sum(Spends, na.rm = TRUE))
    agg_data = merge(agg_data, parameter_df, by="Spend_Channel", all.x=TRUE)
    agg_data$Simulated_Spend = agg_data$Total_Spend + agg_data$Total_Spend*agg_data$Percentage_Change
    final_df = merge(dataframe, agg_data, by=c("Spend_Channel", date_column, panel_column), all.x=TRUE)
    final_df = distinct(final_df)
    drops <- c(y_variable, "Total_Spend", "Spends", "Percentage_Change", "Average_Historical_Spend", "Total_Historical_Spend", "Total_Historical_Sales", "time")
  }
  if(schedulingMethod == 'Spend concentrated in a few months'){
    
    parameter_df$Percentage_Contribution <- as.numeric(as.character(parameter_df$Percentage_Contribution))
    # grouped_data = dplyr::group_by_at(dataframe, c("Spend_Channel", panel_column))
    # agg_data = dplyr::summarise(grouped_data, Total_Spend = sum(Spends, na.rm = TRUE))
    # agg_data = merge(agg_data, parameter_df, by=c("Spend_Channel", panel_column), all.x=TRUE)
    final_df = merge(dataframe, parameter_df, by=c("Spend_Channel", date_column, panel_column), all.x=TRUE)
    final_df$Simulated_Spend = final_df$Spend_Per_Panel_Channel*final_df$Percentage_Contribution
    final_df$Simulated_Spend[is.na(final_df$Simulated_Spend)] <- (final_df$Spends[is.na(final_df$Simulated_Spend)])
    final_df = distinct(final_df)
    
    drops <- c(y_variable, "Total_Spend", "Spends", "Percentage_Contribution", "Average_Historical_Spend", "time", 'Spend_Per_Panel_Channel')
  }
  print("#################### Here ####################")
  
  #### Remove unnecessary columns ####
  final_df <- final_df[ , !(names(final_df) %in% drops)]
  
  ### Convert dataframe from long format to wide for adstock transforming simulated spends done later
  wide_df <- tidyr::spread(final_df, key = Spend_Channel, value = Simulated_Spend)
  
  ### Increment historical trend variable to get simulation period trend
  if(!is.null(trend_column))
  {
    wide_df[trend_column] <- wide_df[trend_column] + frequency_variable
  }
  
  
  
  return(wide_df)
}

generateDatesDataframe <- function(startDate,
                         endDate,
                         dateFrequency,
                         dateVar){
  seq_by <- switch(dateFrequency,
                   "Monthly" = "months",
                   "Weekly"  = "weeks",
                   "Daily"   = "days")
  
  dateDataframe <- data.frame(seq(startDate, endDate, by = seq_by), stringsAsFactors = FALSE)
  colnames(dateDataframe) <- c(dateVar)
  dateDataframe <- dateDataframe %>% arrange_at(dateVar)
  dateDataframe$time <- 1:nrow(dateDataframe)
  return(dateDataframe)
}

getFutureDataset <- function(dataset,
                             external_df,
                             date_column,
                             merge_variables,
                             external_date_variable){
  #### Simulation period dates are picked corresponding to each historical period date
  dataset <- merge(dataset, external_df, by=c(merge_variables), all.y=TRUE)
  
  #### Drop and rename columns as required ####
  drops <- c(date_column, 'Simulated_Date.1', 'Simulated_Date.2', 'Simulated_Date.3')
  dataset <- dataset[ , !(names(dataset) %in% drops)]
  setnames(dataset, old=c(external_date_variable), new=date_column)
  return(dataset)
}

getAllPanelsDataset <- function(dataset, 
                                lvl_of_data, 
                                melt_data,
                                y_variable
                                ){
  group_variables <- c(lvl_of_data, "variable")
  dataset <- melt(dataset, measure.vars= c(melt_data)) %>% data.frame(stringsAsFactors = F)
  dataset <- dataset %>% group_by_at(group_variables) 
  # Create a look-up list with function names and variable to apply
  look_list <- list(sum = y_variable,
                    sum = "value")
  # Apply the summarise_at_fun
  dataset <- map2(look_list, names(look_list), summarise_at_fun, data = dataset) %>%
    reduce(left_join, by = group_variables)
  dataset <- data.frame(dataset, stringsAsFactors = F)
  return(dataset)
}

getSpendsDataset <- function(dataset,
                             lvl_of_data,
                             spend_variable
                             ){
  
  all_spend_variable <- "All_Spend"
  pctSpendDataset <- dplyr::group_by_at(dataset, lvl_of_data)
  # Create a look-up list with function names and variable to apply
  look_list <- list(sum = spend_variable)
  # Apply the summarise_at_fun
  pctSpendDataset <- map2(look_list, names(look_list), summarise_at_fun, data = pctSpendDataset) %>%
    reduce(left_join, by = lvl_of_data) %>% data.frame()
  setnames(pctSpendDataset, old=c(spend_variable), new=c(all_spend_variable))
  dataset <- merge(dataset, pctSpendDataset, by=lvl_of_data, all.x=TRUE)
  dataset <- dplyr::mutate(dataset, Total_Pct_Spend = (dataset[[spend_variable]]/dataset[[all_spend_variable]])*100)
  return(dataset)
}

getCardData <- function(dataset,
                        kpi_col,
                        kpi,
                        data_flag,
                        currency_symbol="",
                        magnitude=""
                        ){
  dataset<-as.data.frame(dataset)
  card_kpi <- dataset[(which(dataset[kpi_col]==kpi)),data_flag]

  if(magnitude!="" & currency_symbol!=""){
    card_kpi_char <- paste0(currency_symbol," ",as.character(round(card_kpi/1e+06,1)), magnitude)
  }else{
    card_kpi_char <- paste0(round(card_kpi,1))
  }
  return(card_kpi_char)
}

getCompareScenarioContribData <- function(scenarioAllHistData,
                                          scenarioSimData,
                                          scenarioName,
                                          trainData,
                                          spendVar,
                                          adstockSpendVar,
                                          adstockDecayRate,
                                          dateVar,
                                          panelVar,
                                          modelRun,
                                          simStartDate,
                                          simEndDate,
                                          histStartDate,
                                          histEndDate
                                          ){
  
  
  allScenarioData <- rbind(scenarioAllHistData, scenarioSimData)
  allScenarioData <- convertBackFactors_2(trainData, allScenarioData)
  ### Adstock transforming spends of total data
  allAdstockScenarioData <- apply_adstock(dataset = as.data.frame(allScenarioData), 
                                           spend_variables = adstockSpendVar,
                                           decay_rate = adstockDecayRate, 
                                           time_variable = dateVar, 
                                           adstock_panel_variables = panelVar)
  #### Calculating Contributions
  scenarioContributionAbsData <- fn_contrib(mmx_model = modelRun
                                             , master_dataset = as.data.frame(allAdstockScenarioData)
                                             , spendVar = spendVar
                                             , req_contrib_cols = c(dateVar)
                                             , DateVar = dateVar
                                             , panelVar = panelVar)
  
  setnames(scenarioContributionAbsData, old = c(dateVar,'variable', 'value'), new = c('Time_Period','Spend_Channel', 'Total_Contribution'))
  
  #### Get percentage contributions
  scenarioContributionAllData <- fn_pct_contrib(dataset = scenarioContributionAbsData,
                                                 lvl_of_data = c('Time_Period'),
                                                 contrib_col = 'Total_Contribution')
  
  #### Add flag to differentiate between scenarios
  scenarioContributionAllData$Scenario <- rep(scenarioName, each=nrow(scenarioContributionAllData))
  
  #### Filter for historical comparison and simulation time periods ####
  scenarioContributionHistData <- filter_at(scenarioContributionAllData, 'Time_Period', any_vars(. >= histStartDate & . <= histEndDate))
  scenarioContributionSimData <- filter_at(scenarioContributionAllData, 'Time_Period', any_vars(. >= simStartDate & . <= simEndDate))
  
  #### Add flag to differentiate between data time periods ####
  scenarioContributionHistData$data_time_period <- rep('Historical', each=nrow(scenarioContributionHistData))
  scenarioContributionSimData$data_time_period <- rep('Simulated', each=nrow(scenarioContributionSimData))
  
  ###### Combine historical and simulated data
  scenarioPanelContribData <- rbind(scenarioContributionHistData, scenarioContributionSimData)
  
  ###### Aggregate contribution to scenario, channel and data_time_period level
  scenarioPanelContribData <- scenarioPanelContribData %>% group_by_at(c('Spend_Channel', 'data_time_period', 'Scenario')) %>% summarize(Total_Contribution=sum(Total_Contribution)) %>% data.frame()
  scenarioContribList <- list(scenarioContributionAllData , scenarioPanelContribData)
  return(scenarioContribList)
}

getCompareScenarioSalesSummaryData <- function(scenarioHistData,
                                               scenarioSimData,
                                               scenarioName,
                                               dateVar,
                                               spendVar,
                                               targetVar){
  ######### Creating dataframe containing both simulated and historical comparison period spends #########
  scenarioHistSimPanelData <- fn_rbind_data(hist_dataset = scenarioHistData, 
                                            opti_dataset = scenarioSimData, 
                                            lvl_of_data = dateVar, 
                                            melt_data = spendVar,
                                            type_data = 'Simulated',
                                            agg_variable = targetVar)
  
  ######## Rename columns ##########
  setnames(scenarioHistSimPanelData, old=c('variable', 'value', dateVar, targetVar), new=c('Spend_Channel', 'Total_Spend', 'Time_Period', 'Total_Sales'))
  
  #### Add flag to differentiate between scenarios
  scenarioHistSimPanelData$Scenario <- rep(scenarioName, each=nrow(scenarioHistSimPanelData))
  
  ###### Aggregate contribution to scenario, channel and data_time_period level
  scenarioPanelData <- scenarioHistSimPanelData %>% group_by_at(c('Spend_Channel', 'data_time_period', 'Scenario')) %>% summarize(Total_Sales=sum(Total_Sales), Total_Spend=sum(Total_Spend)) %>% data.frame()
  
  return(scenarioPanelData)
}

getCompareScenarioSalesTimeSeriesData <- function(scenarioAllHistData,
                                               scenarioSimData,
                                               scenarioName,
                                               dateVar,
                                               spendVar,
                                               targetVar){
  
  ########### Creating dataframe containing all spends
  scenarioAllPanelData <- fn_rbind_data(hist_dataset = scenarioAllHistData, 
                                         opti_dataset = scenarioSimData, 
                                         lvl_of_data = dateVar, 
                                         melt_data = spendVar,
                                         type_data = 'Simulated',
                                         agg_variable = targetVar)
  
  ######## Rename columns ##########
  setnames(scenarioAllPanelData, old=c('variable', 'value', dateVar, targetVar), new=c('Spend_Channel', 'Total_Spend', 'Time_Period', 'Total_Sales'))
  
  ####### Add scenario column #######
  scenarioAllPanelData$Scenario <- rep(scenarioName, each=nrow(scenarioAllPanelData))
  
  return(scenarioAllPanelData)
}

# # run main function
# run_simulation()

## Parameter descriptions guide
# # ~~~~~~~~~ set run params ~~~~~~~~~
# 
# #Variable Definition
# date_column <- 'Month_Date'
# date_format <- '%Y-%m-%d'
# seasonal_column <- 'Month'
# panel_column <- 'State'
# trend_column <- 'time_order'
# y_variable <- 'Sales'
# spend_variables <- c('Direct','Web','Programs','Course')
# 
# # Input budget constraints
# budget <- 300000000
# 
# # User input for seasonal and trend columns
# input_seasonal_col <- 4
# input_trend_col <- 15
# 
# # panel_var <- as.vector(unique(input_df[,panel_column]))
# 
# # User input for panel variables
# input_panel_cols <- c("Chu-Shikoku", "Osaka", "Kyushu", "Tokai", "Kitakanto-koshinetsu", "Tokyo", "Tohoku", "Minamikantou", "Keiji-Hokuriku", "Hokkaido")
# 
# # Select date range for historical data
# from_date <- '2016-12-01'
# to_date <- '2017-05-31'
# 
# # Model file
# model_file <- 'Data/mix_model.rds'
# # model_file <- 'Data/Model_linear.RDS'
# # model_file <- 'Data/Model.RDS'
# # model_file <- 'Data/log_rds1.RDS'
# 
# # Input data file
# input_file <- 'Data/input_ds.csv'
# 
# # User input for lower and upper bound percent for range selection (Method 1)
# lb <- 0.1 #percent decrement
# ub <- 0.1 #percent increment
# 
# # User to input percent step increment between two consecutive spend values in simulation data
# stp <- 0.05
# 
# # Data map axes
# input_spend_var1 <- "Web"
# input_spend_var2 <- "Direct"
# 
# # ~~~~~~~~~ end of run params ~~~~~~~~~