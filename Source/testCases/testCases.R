############################################################################
################# Optimization Function Test Case :---------- ##############
################# Started working on test cases and wrote a test case for :
################## To check if optimized spends lie in the range of user specified Upper and Lower bounds.
################## To check if optimized budget is less than the user specified budget
############################################################################

###### Load Necessary components and Functions 

OFN <- new.env()

folder_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(folder_path)

sanatized_data <- read.csv(paste0(folder_path,"/data/Sanitized_Data.csv")) 
sanatized_data$Month_Date <- as.Date(sanatized_data$Month_Date)

mmxRDS <- readRDS(paste0(folder_path,"/data/mmxRDS.RDS"))
reactData <- mmxRDS$reactData
reactDataOpt <- mmxRDS$reactDataOpt
input <- mmxRDS$input
mmxModel <- mmxRDS$mmxModel
adstock1 <- mmxRDS$adstock
scenarioList <- mmxRDS$scenarioList

########### Load required functions
source('../../www/MMX_Utils/Utils/adstock_decay_utility_functions.R')
source('../../www/MMX_Utils/Utils/simulation_utils.R')
source('../../www/MMX_Utils/Utils/optimization_utils.R')
# source('./www/MMX_Utils/Utils/contribution_utility_function.R')
source('../../www/MMX_Utils/Utils/contrib_marg_utility_functions.R')
source('../../www/MMX_Utils/Utils/customPrecision.R')
library(testthat)

allResults <- list()
allResultsSummary <- list()

#######################################################################################################################
################# Optimization Function test case
#######################################################################################################################


test_optimization_fn <- function(sanatized_data, folder_path, reactData, mmxModel, adstock1, reactDataOpt, input){
  # browser()
  failedTestResults <- list()
  test_data <- merge(reactData$optiDateSequence[reactData$DateVar], sanatized_data, by = reactData$DateVar, all.x = T)
  test_budget <- test_data %>% melt(measure.vars = (c(reactData$spendVar))) %>% summarise(sum(value)) %>% as.numeric()
  # constraint_table2 <- reactData$constraint_table2
  # bound_table2 <- reactData$bound_table2
  # write.csv(constraint_table2, file = paste0(folder_path,'/data/bc.xlsx'), sheetName = "constraint", row.names = FALSE)
  # write.csv(bound_table2, file = paste0(folder_path,'/data/bc.xlsx'), sheetName = "bound", append = TRUE, row.names = FALSE)
  # write.csv(constraint_table2, paste0(folder_path,"/data/constraint.csv"), row.names = FALSE)
  # write.csv(bound_table2, paste0(folder_path,"/data/bound.csv"), row.names = FALSE)
  
  constraint_table2 <- data.frame(read.csv(paste0(folder_path,'/data/constraint.csv')), stringsAsFactors = FALSE)
  # constraint_table2 <- constraint_table2 %>% filter(Order == 1)
  # constraint_table2$RHS <- test_budget
  # constraint_table2$formula <- paste0(constraint_table2$RHS,"-",constraint_table2$LHS)
  # constraint_table2$formula <- as.factor(constraint_table2$formula)
  
  bound_table2 <- data.frame(read.csv(paste0(folder_path,'/data/bound.csv')), stringsAsFactors = FALSE)
  # bound_table2 <- bound_table2 %>% select(-c(Historical_Spends, Lower, Upper))
  # test_spends <- test_data %>% melt(measure.vars = (c(reactData$spendVar))) %>% group_by_at(c(reactData$DateVar, reactData$panelVar, "variable")) %>% summarise(Historical_Spends = sum(value)) %>% data.frame(stringsAsFactors = FALSE)
  # bound_table2 <- merge(bound_table2, test_spends, by.x = c(reactData$DateVar, reactData$panelVar, "Spend_Channel"), by.y = c(reactData$DateVar, reactData$panelVar, "variable"), all.x = T) %>% mutate(Lower = 0.99*Historical_Spends, Upper = 1.01*Historical_Spends)
  
  req_col <- data.frame(constraint_table2, stringsAsFactors = FALSE)
  # Converting the Formats of Extra Dataset into the formats from train dataset
  req_col_colnames <- colnames(req_col)
  trainData_req_col <- reactData$constraint_table2[req_col_colnames]
  # extracting the format info of the train datset columns
  trainData_class <- trainData_req_col %>% map(~{
    paste0("as.",class(.))
  }) %>% data.frame(stringsAsFactors = F)
  # converting into the required formats
  req_col <- req_col_colnames %>% map(~{
    do.call(trainData_class[[.]],list(req_col[[.]]))
  })
  names(req_col) <- req_col_colnames
  constraint_table2 <- data.frame(req_col,stringsAsFactors = FALSE)
  
  req_col <- data.frame(bound_table2, stringsAsFactors = FALSE)
  # Converting the Formats of Extra Dataset into the formats from train dataset
  req_col_colnames <- colnames(req_col)
  trainData_req_col <- reactData$bound_table2[req_col_colnames]
  # extracting the format info of the train datset columns
  trainData_class <- trainData_req_col %>% map(~{
    paste0("as.",class(.))
  }) %>% data.frame(stringsAsFactors = F)
  # converting into the required formats
  req_col <- req_col_colnames %>% map(~{
    do.call(trainData_class[[.]],list(req_col[[.]]))
  })
  names(req_col) <- req_col_colnames
  bound_table2 <- data.frame(req_col,stringsAsFactors = FALSE)
  
  #####################44444444444444444444444######################$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  cob_out <- optimization_fn(businessObjective = input$selectedObjective
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
                             , optiDateSequence = reactData$optiDateSequence
                             , spend_variables_seq = as.character(adstock1$dataset[["Spend_Channel"]])
                             , decay_rate_dataset = adstock1$dataset[["Adstock_Decay"]])
  
  optimised_dataset <- build_dataset(cob_out$par)
  pred <- predict(mmxModel$model,optimised_dataset)
  optimised_dataset$Sales <- as.vector(pred)
  ### Apply Add Stock to Optimised Dataset
  req_col_lvl_of_data <- unique(c(reactData$DateVar,reactData$panelVar, colnames(reactData$extraData)))
  req_col_to_melt_data <- unique(c(mmxModel$full_object$y_variable, reactData$spendVar))
  
  ##### Creating Master Dataset
  master_data_long <- fn_rbind_data(hist_dataset = data.frame(reactDataOpt$dataset, stringsAsFactors = FALSE)
                                    , opti_dataset = optimised_dataset
                                    , lvl_of_data = req_col_lvl_of_data
                                    , melt_data = req_col_to_melt_data)
  master_data_wide <- spread(master_data_long
                             , key = "variable"
                             , value = "value")
  
  adstock_optimised_dataset <- apply_adstock(dataset=master_data_wide,
                                             spend_variables=as.character(adstock1$dataset$Spend_Channel),
                                             decay_rate=c(0.2,0.2,0.2,0.2),
                                             time_variable=reactData$DateVar,
                                             adstock_panel_variables=reactData$panelVar)
  adstock_optimised_dataset <- merge(reactData$optiDateSequence[reactData$DateVar], adstock_optimised_dataset, by = reactData$DateVar, all.x = T)
  adstock_optimised_dataset <- adstock_optimised_dataset %>% arrange_at(c(reactData$DateVar, reactData$panelVar))
  pred <- predict(mmxModel$model,adstock_optimised_dataset)
  
  optimised_dataset <- optimised_dataset %>% arrange_at(c(reactData$DateVar, reactData$panelVar))
  optimised_dataset$Sales <- as.vector(pred)
  
  
  #### Test Cases for spends 
  hist_data <- test_data %>% melt(measure.vars = (c(reactData$spendVar))) %>% group_by_at(c(reactData$DateVar, reactData$panelVar, "variable")) %>% summarise(value = sum(value)) %>% data.frame(stringsAsFactors = F)
  opti_data <- melt(optimised_dataset, measure.vars = (c(reactData$spendVar)))
  
  bound_check <- merge(bound_table2, opti_data, by.x = c(reactData$DateVar, reactData$panelVar, "Spend_Channel"), by.y = c(reactData$DateVar, reactData$panelVar, "variable"), all.x = T) 
  ############## Should be 0 if all constraints applied are within the limits
  each_spend_check <- bound_check %>% mutate(Lower_check = value<Lower, Upper_check = value>Upper) %>% summarise(sum(Lower_check), sum(Upper_check)) %>% sum()
  ############## Should Be 1 if constraints are working 
  budget_check <- sum(sum(bound_check$value) < test_budget)
  
  expected_output <- c(0,1)
  
  result <- tryCatch({
    output <- c(as.numeric(each_spend_check),as.numeric(budget_check))
        test_that("Optimization Correctly Applied",{
      expect_equivalent(output, expected_output)
    })
    
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  if(length(failedTestResults) == 0){
    return("Optimization Test passed")
  } else {
    return(failedTestResults)
  }
}

test_optimization_fnResults <- test_optimization_fn(sanatized_data, folder_path, reactData, mmxModel, adstock1, reactDataOpt, input)
if(test_optimization_fnResults != "Optimization Test passed") {
  allResults <- c(allResults,'test_optimization_fn', test_optimization_fnResults)
}else{
  allResults <- c(allResults,'test_optimization_fn', "Passed")
}

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################


#######################################################################################################################
################# Adstock Function test case
#######################################################################################################################
# adstock_data <- read.csv(paste0(folder_path,"/data/apply_adstock.csv")) 
# adstock_data$Month_Date <- as.Date(adstock_data$Month_Date)

test_apply_adstock <-  function(folder_path, adstock1, reactData){
  # browser()
  failedTestResults <- list()
  adstock_data <- read.csv(paste0(folder_path,"/data/apply_adstock_data.csv")) 
  adstock_data$Month_Date <- as.Date(adstock_data$Month_Date)
  expected_output <- read.csv(paste0(folder_path,"/data/apply_adstock_result.csv")) 
  expected_output$Month_Date <- as.Date(expected_output$Month_Date)
  result <- tryCatch({
    output <- apply_adstock(dataset=adstock_data,
                          spend_variables=as.character(adstock1$dataset$Spend_Channel),
                          decay_rate=c(0.2,0.2,0.2,0.2),
                          time_variable=reactData$DateVar,
                          adstock_panel_variables=reactData$panelVar)
    # write.csv(output, paste0(folder_path,"/data/apply_adstock_result.csv"), row.names = FALSE)
    test_that("Adstock Correctly Applied",{
      expect_equivalent(output, expected_output)
    })
    
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  if(length(failedTestResults) == 0){
    return("Adstock Test passed")
  } else {
    return(failedTestResults)
  }
}

test_apply_adstockResults <- test_apply_adstock(folder_path, adstock1, reactData)

if(test_apply_adstockResults != "Adstock Test passed") {
  allResults <- c(allResults,'test_apply_adstock', test_apply_adstockResults)
}else{
  allResults <- c(allResults,'test_apply_adstock', "Passed")
}

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

test_calculate_contributions5 <- function(sanatized_data, folder_path, reactData, mmxModel, adstock1, reactDataOpt, input){
  failedTestResults <- list()
  
  contrib_dataset <- sanatized_data %>% select(-c(District, year_flag)) %>% melt(measure.vars = c(reactData$spendVar,"Sales"))
  contrib_dataset <- contrib_dataset %>% group_by_at(c(reactData$DateVar, reactData$panelVar, "Month", "time_order", "variable")) %>% summarise(value = sum(value)) %>% data.frame(stringsAsFactors = F)
  contrib_dataset <- contrib_dataset %>% filter_at(reactData$DateVar,any_vars(.=="2015-01-01"))
  contrib_dataset <- spread(contrib_dataset, key = "variable", value = "value")
  
  req_col <- data.frame(contrib_dataset, stringsAsFactors = FALSE)
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
  contrib_dataset <- data.frame(req_col,stringsAsFactors = FALSE)
  
  contrib_lmer_raw_1 <- calculate_contributions5(model = mmxModel$model
                                                 , dataset = contrib_dataset
                                                 , spend_variables = reactData$spendVar
                                                 , by_cols = reactData$DateVar
  )
  contrib_lmer_raw_1 <- data.frame(contrib_lmer_raw_1, stringsAsFactors = F)
  contribVar <- paste("contrib_", reactData$spendVar, sep="")
  pctContribVar <- paste("pct_contrib_", reactData$spendVar, sep="")
  contribVar <- c(contribVar, "contrib_baseline")
  pctContribVar <- c(pctContribVar, "pct_contrib_baseline")
  keeps <- c(contribVar, pctContribVar, reactData$DateVar)
  contrib_lmer_raw_1 <- contrib_lmer_raw_1[keeps]
  contrib_lmer_raw_1 <- as.data.frame(contrib_lmer_raw_1)
  contributionData <- melt(contrib_lmer_raw_1, measure.vars=contribVar)
  pctContributionData <- melt(contrib_lmer_raw_1, measure.vars=pctContribVar)
  ######### 100% Check
  tot_ch_perc <- sum(pctContributionData$value)
  ######### Sales contribution in range with Actual Sales
  tot_ch_spends <- sum(contributionData$value)
  tot_actual_sales <- sum(contrib_dataset$Sales)
  spends_range <- tot_ch_spends < tot_actual_sales*1.05 & tot_ch_spends > tot_actual_sales*0.95
  
  expected_output <- c(100, TRUE)
  result <- tryCatch({
    
    output <- c(tot_ch_perc,spends_range)
    # write.csv(output, paste0(folder_path,"/data/apply_adstock_result.csv"), row.names = FALSE)
    test_that("Contribution Correctly Calculated",{
      expect_equivalent(output, expected_output)
    })
    
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  if(length(failedTestResults) == 0){
    return("Contribution Correctly Calculated")
  } else {
    return(failedTestResults)
  }
}

test_calculate_contributions5Results <- test_calculate_contributions5(sanatized_data, folder_path, reactData, mmxModel, adstock1, reactDataOpt, input)

if(test_calculate_contributions5Results != "Contribution Correctly Calculated") {
  allResults <- c(allResults,'test_calculate_contributions5', test_calculate_contributions5Results)
}else{
  allResults <- c(allResults,'test_calculate_contributions5', "Passed")
}

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################


test_gen_sim_data_SC1 <- function(mmxRDS, reactData, Scenario_Name){
  # browser()
  failedTestResults <- list()
  
  genData <- gen_sim_data(schedulingMethod = mmxRDS$scenarioList$files[[Scenario_Name]]$scheduling_dataset$schedulingStrategy, 
                          dataframe = mmxRDS$scenarioList$files[[Scenario_Name]]$all_data$long_dataset,
                          parameter_df = mmxRDS$scenarioList$files[[Scenario_Name]]$scheduling_dataset$uploadedDataset, 
                          all_date_df = reactData$allDateSequence,
                          date_column = reactData$DateVar,
                          date_format = reactData$DTformat,
                          panel_column = reactData$panelVar,
                          trend_column = reactData$timeOrderVar,
                          y_variable = reactData$targetVar,
                          frequency_variable = reactData$numDateFrequency
  )
  genSpends <- melt(genData, measure.vars = c(reactData$spendVar)) %>% summarise(value = sum(value))
  Spends <- mmxRDS$scenarioList$files[[Scenario_Name]]$all_data$long_dataset %>% summarise(value = sum(value))
  
  
  expected_output <- 1.2
  result <- tryCatch({
    output <- genSpends/Spends
    test_that("Sim1 Data Correctly Generated",{
      expect_equivalent(output, expected_output)
    })
    
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  if(length(failedTestResults) == 0){
    return("Sim1 Data Correctly Generated")
  } else {
    return(failedTestResults)
  }
}

test_gen_sim_data_SC1Results <- test_gen_sim_data_SC1(mmxRDS, reactData, Scenario_Name = "Scenario_1")
if(test_gen_sim_data_SC1Results != "Sim1 Data Correctly Generated") {
  allResults <- c(allResults,'test_gen_sim_data_SC1', test_gen_sim_data_SC1Results)
}else{
  allResults <- c(allResults,'test_gen_sim_data_SC1', "Passed")
}

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################


test_gen_sim_data_SC2 <- function(mmxRDS, reactData, Scenario_Name){
  # browser()
  failedTestResults <- list()
  
  genData <- gen_sim_data(schedulingMethod = mmxRDS$scenarioList$files[[Scenario_Name]]$scheduling_dataset$schedulingStrategy, 
                          dataframe = mmxRDS$scenarioList$files[[Scenario_Name]]$all_data$long_dataset,
                          parameter_df = mmxRDS$scenarioList$files[[Scenario_Name]]$scheduling_dataset$uploadedDataset, 
                          all_date_df = reactData$allDateSequence,
                          date_column = reactData$DateVar,
                          date_format = reactData$DTformat,
                          panel_column = reactData$panelVar,
                          trend_column = reactData$timeOrderVar,
                          y_variable = reactData$targetVar,
                          frequency_variable = reactData$numDateFrequency
  )
  genSpends <- melt(genData, measure.vars = c(reactData$spendVar)) %>% summarise(value = sum(value))
  Spends <- mmxRDS$scenarioList$files[[Scenario_Name]]$all_data$long_dataset %>% summarise(value = sum(value))
  
  
  expected_output <- 1
  result <- tryCatch({
    output <- genSpends/Spends
    test_that("Sim2 Data Correctly Generated",{
      expect_equivalent(output, expected_output)
    })
    
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  if(length(failedTestResults) == 0){
    return("Sim2 Data Correctly Generated")
  } else {
    return(failedTestResults)
  }
}

test_gen_sim_data_SC2Results <- test_gen_sim_data_SC2(mmxRDS, reactData, Scenario_Name = "Scenario_2")
if(test_gen_sim_data_SC2Results != "Sim2 Data Correctly Generated") {
  allResults <- c(allResults,'test_gen_sim_data_SC2', test_gen_sim_data_SC2Results)
}else{
  allResults <- c(allResults,'test_gen_sim_data_SC2', "Passed")
}

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################


# # plot_data <- sanatized_data_adstocked %>% select(Month_Date, State,Sales, test_sales) %>% mutate(Diff = Sales - test_sales) %>% mutate(Perc = Diff/Sales) 
# 
# test_mmxModel <- function(sanatized_data, mmxRDS, mmxModel, reactData){
#   
#   failedTestResults <- list()
#   sanatized_data <- read.csv(paste0(folder_path,"/data/Sanitized_Data.csv")) 
#   sanatized_data$Month_Date <- as.Date(sanatized_data$Month_Date)
#   sanatized_data <- melt(sanatized_data, measure.vars = (c(reactData$spendVar, "Sales")))
#   req_col_lvl_of_data <- unique(c(reactData$DateVar,reactData$panelVar, colnames(reactData$extraData)))
#   sanatized_data <- sanatized_data %>% group_by_at(c(req_col_lvl_of_data, "variable")) %>% summarise(value = sum(value)) %>% data.frame(stringsAsFactors = F)
#   sanatized_data <- sanatized_data %>% spread(key = "variable", value = "value")
#   
#   
#   sanatized_data_adstocked <- apply_adstock(dataset=sanatized_data,
#                                             spend_variables=as.character(adstock1$dataset$Spend_Channel),
#                                             decay_rate=c(0.2,0.2,0.2,0.2),
#                                             time_variable=reactData$DateVar,
#                                             adstock_panel_variables=reactData$panelVar)
#   sanatized_data_adstocked <- merge(reactData$optiDateSequence[reactData$DateVar], sanatized_data_adstocked, by = reactData$DateVar, all.x = T)
#   sanatized_data_adstocked$Month <- as.factor(sanatized_data_adstocked$Month)
#   
#   pred <- predict(mmxModel$model,sanatized_data_adstocked)
#   sanatized_data_adstocked$test_sales <- as.vector(pred)
#   
#   expected_output <- 1.2
#   
#   result <- tryCatch({
#     output <- genSpends/Spends
#     test_that("Validate Model",{
#       expect_equivalent(output, expected_output)
#     })
#     
#   }, error = function(e){return(e)})
#   if(!isTRUE(result)){
#     failedTestResults <- c(failedTestResults, as.character(result))
#   }
#   if(length(failedTestResults) == 0){
#     return("Model projecting good results")
#   } else {
#     return(failedTestResults)
#   }
# }
# 
# test_mmxModelResults <- test_gen_sim_data_SC1(mmxRDS, reactData, Scenario_Name = "Scenario_1")
# if(test_mmxModelResults != "Model projecting good results") {
#   allResults <- c(allResults,'test_mmxModel', test_mmxModelResults)
# }else{
#   allResults <- c(allResults,'test_mmxModel', "Passed")
# }

#########################################################################################
#########################################################################################
#########################################################################################

#########################################################################################
#########################################################################################
#########################################################################################

if(length(allResults)>0){
  Results <- data.frame(matrix(unlist(allResults), nrow=sum(lengths(allResults))/2, ncol = 2, byrow=T))
  colnames(Results) <- c("Function", "Message")
  write.csv(Results, paste0(folder_path,"/testResults.csv"), row.names = FALSE)
}

