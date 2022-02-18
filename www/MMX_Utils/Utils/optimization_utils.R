## Dependency packages
packages <- c('DT', 'dplyr', 'lme4', 'pso', 'magicfor', 'nloptr',"magrittr",
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

# Conversions (Data Pre-processing)
preprocess_data <- function(ads_df,date_column,date_format,panel_column,seasonal_column){
  ads_df[[date_column]] <- as.Date(ads_df[[date_column]],format=date_format)
  if(!is.null(panel_column)) ads_df[[panel_column]]  <- as.factor(ads_df[[panel_column]])
  if(!is.null(seasonal_column)) ads_df[[seasonal_column]]  <- as.factor(ads_df[[seasonal_column]])
  return(ads_df)
}

# Quick peek into Data
view_data_details <- function(ads_df, spend_variables, date_column, panel_column){
  cat(paste0('The training dataset contains ',ncol(ads_df),' columns and ',nrow(ads_df),' rows.\n'))
  cat('\n')
  cat(paste0('Spend variables : ',paste(spend_variables, collapse=', ')))
  cat('\n')
  
  ### counts
  cat(paste('No. of Months :',length(unique(ads_df[[date_column]])) ))
  cat('\n\n')
  cat(paste0('Hierarchy in the dataset : State, District with District being the granular level \n'))
  cat(paste('No. of States :',length(unique(ads_df[[panel_column]]))))
  cat(paste('\nTime period : ',paste(range(ads_df[[date_column]]),collapse=' to ')))
  
  head(ads_df)
  #num_cols <- colnames(ads_df)[sapply(ads_df,is.numeric)]
  #datatable(head(ads_df),options=list(dom='t',scrollX=TRUE)) %>% formatRound(num_cols)  
}

##Variable Transformations

variable_transformation <- function(effect.order, ads_df, spend_variables, scaling, scaling_type, y_variable, scaling_factor){
  # Transform variables according to selected transformations in 'effect.order'
  if(!is.null(effect.order)){

    for(effect in effect.order){
      arg.variable.name <- paste0(effect,'.params')
      args <- tryCatch(get(arg.variable.name),error=function(e){ stop(paste0('Parameters not found for ',effect)) })
      # ads_df[,spend_variables] <- lapply(ads_df[,spend_variables,drop=FALSE],var.transform,method=effect,args)
      ads_df[,spend_variables] <- mapply(var.transform,ads_df[,spend_variables,drop=FALSE],args,MoreArgs = list(method=effect),SIMPLIFY = FALSE)
    }

  }

  # Variables scaling
  if(!is.null(scaling)){
    switch(scaling_type,
           'down-scale'={
             ads_df <- ads_df %>% mutate_at(c(y_variable,spend_variables),~./scaling_factor)
           },
           'log-scale'={
             ads_df <- ads_df %>% mutate_at(c(y_variable,spend_variables),~log(1+.))
           })
  }

  return(ads_df)
}

## Train-Test split
split_train_test <- function(ads_df, date_column, train_date_max, test_date_max){
  train.data <- ads_df %>% filter_at(date_column,any_vars(. <= train_date_max))
  test.data <- ads_df %>% filter_at(date_column,any_vars(. > train_date_max & . <= test_date_max))

  # cat(paste0(c('Train time period : ',as.character(range(train.data[[date_column]])))))
  # cat(paste0(c('\nTest time period : ',as.character(range(test.data[[date_column]])))))

  all_data <- list(trainData=train.data, testData=test.data)
  return(all_data)
}

## Model creation

# This section creates a mixed model based on the type of model selected and the formula specified.
create_model <- function(formula, train.data){
  mix_model <-  lmer(formula=as.formula(formula), data = train.data, REML=TRUE)
  summary(mix_model) 
  return(mix_model)
}

# Optimization 
##Particle Swarm Optimization Technique
# Optimization function that returns test dataframe with optimised spends
# z: test data
pso_fun <- function(z, mix_model, spend_variables, y_variable, Budget) {
  print(rownames(z))
  z$Total_Spends <- sum(z[, spend_variables])
  
  # Objective function that returns predicted sales for different values of spends
  sales_fun <- function(x){
    if(x[1]+x[2]+x[3]+x[4] > Budget) return(0)
    else if (x[1] < 0) return(0)
    else if (x[2] < 0) return(0)
    else if (x[3] < 0) return(0)
    else if (x[4] < 0) return(0)
    else
      z[, spend_variables][1] <- x[1]
      z[, spend_variables][2] <- x[2]
      z[, spend_variables][3] <- x[3]
      z[, spend_variables][4] <- x[4]
    
    y <- predict(mix_model, newdata=z)
    
    # To get iteration values
    # put(x,y)
    # itr_val <<- c(itr_val,list("x" = x, "y" = y))
    
    return(y)
  }
  
  pso_result <- psoptim(
    par= rep(x=NA ,4), 
    fn = sales_fun, 
    lower = c(0,0,0,0), 
    control = list(fnscale = -1, 
                   maxit= 10))
  
  z[, optimised_spends] <- NA
  z[, optimised_spends][1] <- pso_result$par[1]
  z[, optimised_spends][2] <- pso_result$par[2]
  z[, optimised_spends][3] <- pso_result$par[3]
  z[, optimised_spends][4] <- pso_result$par[4]
  
  z$Total_Optimised_Spends <- sum(pso_result$par)
  z$Percent_Change_Spends <- (z$Total_Optimised_Spends - z$Total_Spends)*100 / z$Total_Spends
  
  z$Predicted_Sales <- abs(pso_result$value)
  z$Percent_Change_Sales <- (z$Predicted_Sales - z[, y_variable])*100/z[, y_variable]
  
  z$ROI <- (z[, y_variable] - z$Total_Spends) / z$Total_Spends
  z$ROI_Optimised <- (z$Predicted_Sales - z$Total_Optimised_Spends)*100 / z$Total_Optimised_Spends
  
  return(z)
}

###Results

#Runnning optimization function
# z: test data
run_particle_swarm_optimization <- function(z, mix_model, spend_variables, y_variable, Budget){
  # print("Running Particle Swarm Optimization for all rows")
  optim_df <- lapply(1:nrow(z), function(x) pso_fun(z[x,], mix_model, spend_variables, y_variable, Budget))
  finaldf <-do.call(rbind,optim_df)
  # print("Done running Particle Swarm Optimization")
  #Output data with optimized results
  return(finaldf)
  # datatable(finaldf,options=list(dom='t',scrollX=TRUE))
}

# ##Non Linear Optimization Technique using nloptr
# run_non_linear_optimization <- function(z, spend_variables, x0, lb, ub){
#   # Standard parameters
#   local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
#                       "xtol_rel" = 1.0e-7 )
#   opts <- list( "algorithm" = "NLOPT_LD_AUGLAG",
#                 "xtol_rel" = 1.0e-7,
#                 "maxeval" = 1000,
#                 "local_opts" = local_opts )
#   
#   # constraint functions
#   # inequalities
#   eval_g_ineq <- function( x ) {
#     constr <- c(sum(z[,spend_variables]) - x[1] - x[2] - x[3] - x[4] )
#     grad <- c( - x[1] - x[2] - x[3] ,
#                - x[2] - x[3] - x[4],
#                - x[1] - x[3] - x[4],
#                - x[1] - x[2] - x[4] )
#     return( list( "constraints"=constr, "jacobian"=grad ) )
#   }
#   # equalities
#   eval_g_eq <- function( x ) {
#     constr <- c( x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 - 40 )
#     grad <- c( 2.0*x[1],
#                2.0*x[2],
#                2.0*x[3],
#                2.0*x[4] )
#     return( list( "constraints"=constr, "jacobian"=grad ) )
#   }
#   
#   res <- nloptr( x0=x0,
#                  eval_f=eval_f,
#                  lb=lb,
#                  ub=ub,
#                  eval_g_ineq=eval_g_ineq,
#                  eval_g_eq=eval_g_eq,
#                  opts=opts)
#   return(res)
# }
# 
# finaldf2 <- run_non_linear_optimization(test.data, spend_variables, x0, lb, ub)
# head(finaldf2)

## Main optimization function
# Returns data frame with optimal ROI
run_optimization <- function(ads_df, date_column, date_format, panel_column, spend_variables, optimised_spends,
                             seasonal_column, trend_column, y_variable, scaling, scaling_type, scaling_factor,
                             train_date_max, test_date_max, model_type, effect.order, formula, Budget){
  # Conversions (Data Pre-processing)
  ads_df <- preprocess_data(ads_df,date_column,date_format,panel_column,seasonal_column)
  
  # Quick peek into Data
  view_data_details(ads_df, spend_variables, date_column, panel_column)
  
  # Variable Transformations
  ads_df <- variable_transformation(effect.order, ads_df, spend_variables, scaling, scaling_type, y_variable, scaling_factor)
  head(ads_df)
  
  # Train-Test split
  all_data <- split_train_test(ads_df, date_column, train_date_max, test_date_max)
  train.data <- all_data[["trainData"]]
  test.data <- all_data[["testData"]]
  str(train.data)
  str(test.data)
  
  ## Model creation
  # This section creates a mixed model based on the type of model selected and the formula specified.
  mix_model <- create_model(formula, train.data)
  
  # Particle Swarm Optimization
  finaldf <- run_particle_swarm_optimization(test.data, mix_model, spend_variables, y_variable, Budget)
  head(finaldf)
  return(finaldf)
}

# Parameter descriptions
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~ User Inputs - Run Parameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Input dataset
# ads_df <- read.csv('Data/input_ds.csv',stringsAsFactors = FALSE)
# 
# ## User inputs
# date_column <- 'Month_Date'
# date_format <- '%Y-%m-%d'
# panel_column <- 'State'
# spend_variables <- c('Direct','Web','Programs','Course')  
# optimised_spends <- c('Optimised_Direct', 'Optimised_Web','Optimised_Programs','Optimised_Course')
# seasonal_column <- 'Month'
# trend_column <- 'time_order'
# y_variable <- 'Sales'
# train_date_max <- '2016-05-01'
# test_date_max <- '2017-06-01'
# model_type <- 'lmer' # or gam
# effect.order <- NULL
# 
# ## Model formula
# 
# formula <- "Sales ~ Month + time_order + log(Direct+1) + log(Web+1) + log(Course+1) + log(Programs+1) + ( log(Direct+1) + log(Web+1) + log(Course+1) + log(Programs+1) | State)"
# 
# ## Set parameter for Particle Swarm Optimization
# Budget <- 999999999
# 
# # ## Set parameters for non-linear optimizer
# # #initial values x0
# # x0 = c(0,0,0,0)
# # # lower and upper bounds of control
# # lb <- c( 0, 0, 0, 0 )
# # ub <- c( 5, 5, 5, 5 )
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~ End of User Inputs - Run Parameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~




get_opt_spend_by_panel <- function(sim_data,y_variable,panel_column){
  # # Predicting sales using the market mix model
  y <- predict(market_mix_model, newdata = sim_data)
  sim_data[,y_variable] <- NA
  sim_data[,y_variable] <- y
  sim_data <- subset(sim_data, sim_data[, y_variable] > 0)
  
  panel_ymax <- as.data.frame(aggregate(sim_data[,y_variable], 
                                        by = list(sim_data[,panel_column]), 
                                        max)
  )
  
  opti_fun <- function(x){
    opti_df <- sim_data[sim_data[, y_variable] %in% as.vector(x) , ]
    return(opti_df)
  }
  
  opti_df <- lapply(1:nrow(panel_ymax), function(x) opti_fun(panel_ymax[x,2])) %>% do.call(rbind, .)
  
}



#------------------------------------------------------------------------------------------------------------
# Function to load a saved model and all other relevant variables
#------------------------------------------------------------------------------------------------------------

#' Function to load a saved model and all other relevant variables
#' @param path The full path to the object RDS object containing the model and other relevant variables
#' @return None
#' @export

load_saved_model <- function(path){
  # This function loads the saved model object from the path and loads all the relevant variables to global environment.
  object_list <<- readRDS(path)
  
  # Search for all the variables in the var_list below in object_list and assign the values to variable with the same name in global environment
  var_list <- c('date_column','date_format','spend_variables','y_variable','train_max_date',
                'ads_df', 'trend_column', "seasonal_column", "panel_column")
  var_list_stat <- var_list %>% map(~assign(.,object_list[[.]],envir = .GlobalEnv))
  
  return(0)
}

##########################################################################
##################### Function to Create Time Series Charts in Results Tab
##########################################################################

summarySalesPlot <- function(dataset, chartTitle, xvar, xlab, yvar, ylab, lvar, panelLabel = "", channelLabel = ""){
  # browser()
  dataset[[xvar]] <- as.Date(dataset[[xvar]])
  ggplot(dataset, aes_string(x=xvar, y=yvar, colour=lvar)) +
    # geom_line(size=1.5) + geom_point(size=4) +  theme_light() + theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=12), legend.text=element_text(size=12), legend.spacing.x = unit(0.2, 'cm')) + labs(x=xlab, y=ylab)
    geom_line(size=1.5) + geom_point(size=4) +  theme_light() + 
    theme(plot.title = element_text(hjust = 0.5, size = 14), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=14), legend.spacing.x = unit(0.2, 'cm')) + 
    scale_color_manual(values=vcolors) + scale_x_date(labels = scales::date_format("%Y-%b")) + 
    labs(title = paste0("Time Series Plot of ", chartTitle, " - Panel : ", panelLabel, " Channel: ", channelLabel), x = xlab, y = ylab) 
    # scale_y_continuous(labels=scales::dollar_format(prefix="$"))
}

salesLinePlot <- function(dataset, chartTitle, xvar, xlab, yvar, ylab, lvar, panelLabel = "", channelLabel = ""){
  
  dataset[[xvar]] <- as.Date(dataset[[xvar]])
  ggplot(dataset, aes_string(x=xvar, y=yvar)) +
    # geom_line(size=1.5) + geom_point(size=4) +  theme_light() + theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=12), legend.text=element_text(size=12), legend.spacing.x = unit(0.2, 'cm')) + labs(x=xlab, y=ylab)
    geom_line(color=lvar, size=1.5) + geom_point(color='#1F77B4', size=4) +  theme_light() + 
    theme(plot.title = element_text(hjust = 0.5, size = 14), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=14), legend.spacing.x = unit(0.2, 'cm')) + 
    scale_color_manual(values=vcolors) + scale_x_date(labels = scales::date_format("%Y-%b")) + 
    labs(title = paste0("Time Series Plot of ", chartTitle, " - Panel : ", panelLabel), x = xlab, y = ylab) 
  # scale_y_continuous(labels=scales::dollar_format(prefix="$"))
}

summaryAreaPlot <- function(dataset, chartTitle, xvar, xlab, yvar, ylab, lvar, panelLabel = ""){
  # browser()
  dataset[[xvar]] <- as.Date(dataset[[xvar]])
  ggplot(dataset, aes_string(x=xvar, y=yvar, fill=lvar)) +
    # geom_line(size=1.5) + geom_point(size=4) +  theme_light() + theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=12), legend.text=element_text(size=12), legend.spacing.x = unit(0.2, 'cm')) + labs(x=xlab, y=ylab)
    geom_area() +  theme_light() + 
    theme(plot.title = element_text(hjust = 0.5, size = 14), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=14), legend.spacing.x = unit(0.2, 'cm')) + 
    scale_fill_manual(values=vcolors) + scale_x_date(labels = scales::date_format("%Y-%b")) + 
    labs(title = paste0("Time Series Plot of ", chartTitle, " - Panel : ", panelLabel), x = xlab, y = ylab)
    # scale_y_continuous(labels=scales::dollar_format(prefix="$"))
}

fn_bar_graph <- function(dataset, chartTitle, xvar, xlab, yvar, ylab, lvar, panelLabel = ""){
  
  ggplot(dataset, aes_string(x= xvar , y = yvar, fill = lvar)) +
    geom_bar(stat = "identity", position = "dodge") + 
    ggtitle(paste0(chartTitle," - Panel :", panelLabel)) + 
    theme_light() + 
    theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=12), legend.text=element_text(size=12), legend.spacing.x = unit(0.2, 'cm')) + 
    scale_fill_manual(values=vcolors) + 
    labs(x = xlab, y = ylab) 
  # + 
    # geom_text(aes(label = dollar(yvar)), position=position_dodge(width=0.9), vjust=-0.25)
}

sales_fn_bar_graph <- function(dataset, chartTitle, xvar, xlab, yvar, ylab, lvar, panelLabel = ""){
  
  ggplot(dataset, aes_string(x= xvar , y = yvar)) +
    geom_bar(fill = lvar, stat = "identity", position = "dodge", width = 0.1) + 
    ggtitle(paste0(chartTitle," - Panel :", panelLabel)) + 
    theme_light() + 
    theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=12), legend.text=element_text(size=12), legend.spacing.x = unit(0.2, 'cm')) + 
    scale_fill_manual(values=vcolors) + 
    labs(x = xlab, y = ylab) 
  # + 
  # geom_text(aes(label = dollar(yvar)), position=position_dodge(width=0.9), vjust=-0.25)
}

timeSeriesPlot <- function(dataset, chartTitle, xvar, xlab, yvar, ylab, opti_seq, hist_seq, seq_xvar, panelLabel, channelLabel, legendVar){
  
  dataset[[xvar]] <- as.Date(dataset[[xvar]])
  opti_seq[[seq_xvar]] <- as.Date(opti_seq[[seq_xvar]])
  simDataset <- dataset %>% filter_at(xvar,any_vars(. %in% opti_seq[seq_xvar][,1]))
  
  ggplot(dataset , aes_string(x=xvar, y = yvar)) +
    geom_line(position="dodge", stat="identity", color='#1F77B4', size=1.5) + geom_point(color='#1F77B4', size=4) + 
    labs(title = paste0("Time Series Plot of ", chartTitle, " - Panel: ", panelLabel, " , Channel: ", channelLabel), x = xlab, y = ylab) + theme_light() +
    theme(plot.title = element_text(hjust = 0.5, size = 14), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=14), legend.spacing.x = unit(0.2, 'cm')) +
    scale_x_date(labels = scales::date_format("%b-%Y")) +
    geom_vline(xintercept = as.Date(min(opti_seq[seq_xvar][,1])),color='blue',linetype='dashed') +
    geom_vline(xintercept = as.Date(max(opti_seq[seq_xvar][,1])),color='blue',linetype='dashed')  +
    annotate(color='blue',"text", x = as.Date(mean(opti_seq[seq_xvar][,1])), y = 1, label = paste0(legendVar, " Time Period")) +
    geom_vline(color='red',linetype='dashed',xintercept = as.Date(min(hist_seq[seq_xvar][,1]))) +
    annotate(color='red',"text", x = as.Date(mean(hist_seq[seq_xvar][,1])), y = 1, label = paste( "Historical Comparison Time Period"))  +
    geom_vline(linetype='dashed',color='red',xintercept = as.Date(max(hist_seq[seq_xvar][,1])))+
    geom_line(aes_string(x=xvar, y = yvar), data=simDataset, size=1.5, color='#ff6600') + geom_point(data=simDataset, color="#ff6600", size=4)
    # + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
}
timeSeriesOverlayPlot <- function(dataset, chartTitle, xvar, xlab, yvar, ylab, lvar, panelLabel, channelLabel){
  ggplot(dataset, aes_string(x= xvar, y= yvar, colour = lvar)) + 
    geom_line(size=1.5) + geom_point(size=4) +  theme_light() + 
    theme(plot.title = element_text(hjust = 0.5, size = 14), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=12), legend.text=element_text(size=12), legend.spacing.x = unit(0.2, 'cm')) + 
    labs(title = paste0("Time Series plot of ", chartTitle, " Comparison - Panel: ", panelLabel, " , Channel: ", channelLabel) ,x=xlab, y=ylab)  + 
    scale_x_discrete(limits = unique(dataset$Time_X)) + 
    # scale_y_continuous(labels=scales::dollar_format(prefix="$")) + 
    scale_color_manual(values=c('#1F77B4','#AEC7E8'))
}
timeSeriesCompareScenariosPlot <- function(dataset, chartTitle, xvar, xlab, yvar, ylab, lvar, opti_seq, hist_seq, seq_xvar, panelLabel, channelLabel){
  
  dataset[[xvar]] <- as.Date(dataset[[xvar]])
  opti_seq[[seq_xvar]] <- as.Date(opti_seq[[seq_xvar]])
  
  ggplot(dataset , aes_string(x=xvar, y = yvar, color=lvar)) +
    geom_line(position="dodge", stat="identity", size=1.5) + geom_point(size=4) + 
    labs(title = paste0("Time Series Plot of ", chartTitle, " - Panel: ", panelLabel, " , Channel: ", channelLabel), x = xlab, y = ylab) + theme_light() +
    theme(plot.title = element_text(hjust = 0.5, size = 14), legend.title=element_blank(), legend.position="bottom", axis.text = element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=14), legend.spacing.x = unit(0.2, 'cm')) +
    scale_x_date(labels = scales::date_format("%b-%Y")) +
    geom_vline(xintercept = as.Date(min(opti_seq[seq_xvar][,1])),color='blue',linetype='dashed') +
    geom_vline(xintercept = as.Date(max(opti_seq[seq_xvar][,1])),color='blue',linetype='dashed')  +
    annotate(color='blue',"text", x = as.Date(mean(opti_seq[seq_xvar][,1])), y = 1, label = paste( "Simulation/Optimization Time Period")) +
    geom_vline(color='red',linetype='dashed',xintercept = as.Date(min(hist_seq[seq_xvar][,1]))) +
    annotate(color='red',"text", x = as.Date(mean(hist_seq[seq_xvar][,1])), y = 1, label = paste( "Historical Comparison Time Period"))  +
    geom_vline(linetype='dashed',color='red',xintercept = as.Date(max(hist_seq[seq_xvar][,1])))+ scale_color_manual(values=c("#1F77B4","#AEC7E8"))
  # + scale_y_continuous(labels=scales::dollar_format(prefix="$"))
}


################################################################################
###################### Functions for Running Optimization 
################################################################################

build_dataset <- function(x){
  # browser()
  businessObjective <- get("businessObjective",envir = OFN)
  targetRevenue <- get("targetRevenue",envir = OFN)
  optiTechnique <- get("optiTechnique",envir = OFN)
  model <- get("model",envir = OFN)
  maxIteration <- get("maxIteration",envir = OFN)
  spendVar <- get("spendVar",envir = OFN)
  DateVar <- get("DateVar",envir = OFN)
  panelVar <- get("panelVar",envir = OFN)
  bound_table_dataset <- get("bound_table_dataset",envir = OFN)
  constraint_table_dataset <- get("constraint_table_dataset",envir = OFN)
  extraData <- get("extraData",envir = OFN)
  train_data <- get("train_data",envir = OFN)
  hierarchy_tab <- get("hierarchy_tab",envir = OFN)
  historical_dataset <- get("historical_dataset",envir = OFN)
  optiDateSequence <- get("optiDateSequence",envir = OFN)
  spend_variables_seq <- get("spend_variables_seq",envir = OFN)
  decay_rate_dataset <- get("decay_rate_dataset",envir = OFN)
  
  # print(extraData)
  req_col <- data.frame(extraData, stringsAsFactors = FALSE)
  # Converting the Formats of Extra Dataset into the formats from train dataset
  req_col_colnames <- colnames(req_col)
  trainData_req_col <- train_data[req_col_colnames]
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
  
  #req_col <- as.data.frame(unique(within(mmxModel$test_data %>% select(Month_Date, State, Month, time_order), rm("District"))))
  cc <- merge(x = hierarchy_tab, y = req_col, by = c(DateVar, panelVar), all.x = TRUE)
  #print(hierarchy_tab)
  cc <- as.data.frame(cc %>% arrange(number_r))
  x_matrix <- as.data.frame(matrix(x,ncol=length(spendVar),byrow = T),stringsAsFactors = FALSE)
  colnames(x_matrix) <- spendVar
  merged_data <- as.data.frame(cbind(cc,x_matrix))
  merged_data_colnames <- colnames(merged_data)
  train_data_colnames <- colnames(train_data)
  model_data_column <- intersect(train_data_colnames, merged_data_colnames)
  
  opti_data <- merged_data[model_data_column]
  opti_data_colnames <- colnames(opti_data)
  level_of_data <- setdiff(opti_data_colnames, spendVar)
  
  hist_data <- historical_dataset
  hist_data <- melt(hist_data, measure.vars = c(spendVar))
  hist_data <- hist_data %>% group_by_at(c(level_of_data, "variable")) %>% summarise(value = sum(value))
  hist_data <- dcast(hist_data, formula = ...~variable)
  
  #order by column names to rbind datasets 
  opti_data <- opti_data[,order(colnames(opti_data))]
  hist_data <- hist_data[,order(colnames(hist_data))]
  
  total_data <- rbind(opti_data, hist_data)
  
  ## creating dummy as stock variables // delete this once you intergrate
  adstock_channel <- spendVar
  adstock_value <- c(0.1, 0.2, 0.3, 0.4)
  adstock_dataset <- cbind(adstock_channel, adstock_value)
  
  adstocked_data <- apply_adstock(dataset=total_data,
                                  spend_variables = spend_variables_seq,
                                  #decay_rate=list(adstock_df$Adstock_Value),
                                  decay_rate= decay_rate_dataset,
                                  time_variable= DateVar,
                                  adstock_panel_variables= panelVar)
  
  final_data <- merge(optiDateSequence[DateVar], adstocked_data, by = c(DateVar), all.x = TRUE)
  return(final_data)
  
}
fn <- function(x){
  data <- build_dataset(x)
  
  # accessing variables from environment
  businessObjective <- get("businessObjective",envir = OFN)
  targetRevenue <- get("targetRevenue",envir = OFN)
  model <- get("model",envir = OFN)
  
  pred <- predict(model,data)
  if(businessObjective == "Maximise Sales"){
    objective <- sum(pred)
  }else if(businessObjective == "Maximise ROI")
    {
    objective <- sum(pred)/sum(x)
  }else(businessObjective == "Revenue Chase")
  {
    objective <- abs(targetRevenue-sum(pred))
  }
  print(objective)
  return(objective)
}

eval_formula <- function(row,x,cols,spendVar){
  #browser()
  row_number <- as.numeric(row[which(cols=='number_r')])
  formula <- row[which(cols=='formula')]
  
  1:length(spendVar) %>% map(~{
    formula <<- gsub(spendVar[.], paste('x[(length(spendVar)*row_number)-(length(spendVar)-',.,')]'), formula)
  }) %>% invisible()
  
  # formula <- gsub(spendVar[1], 'x[(4*row_number)-3]', formula)
  # formula <- gsub(spendVar[2], 'x[(4*row_number)-2]', formula)
  # formula <- gsub(spendVar[3], 'x[(4*row_number)-1]', formula)
  # formula <- gsub(spendVar[4], 'x[(4*row_number)-0]', formula)
  value <- eval(parse(text=formula))
  return(value)
}

eval_formula_bound_overall <- function(row,x,cols,spendVar){
  row_number <- as.numeric(row[which(cols=='number_r')])
  Channel <- row[which(cols=='Spend_Channel')]
  
  1:length(spendVar) %>% map(~{
    Channel <<- gsub(spendVar[.], paste('x[(length(spendVar)*row_number)-(length(spendVar)-',.,')]'), Channel)
  }) %>% invisible()
  value <- eval(parse(text=Channel))
  return(value)
}


ineq_fn <- function(x){
  # browser()
  constraint_table_dataset <- get("constraint_table_dataset",envir = OFN)
  bound_table_dataset <- get("bound_table_dataset",envir = OFN)
  spendVar <- get("spendVar",envir = OFN)
  
  constraint_table_cols <- colnames(constraint_table_dataset)
  constraint_table2_each <- filter(constraint_table_dataset, Operation == "Each")
  constraint_table2_all <- filter(constraint_table_dataset, Operation == 'All')
  
  constraint_table2_each$values <- apply(constraint_table2_each,1,FUN=eval_formula,x,constraint_table_cols,spendVar)
  constraint_table2_all$values <- apply(constraint_table2_all,1,FUN=eval_formula,x,constraint_table_cols,spendVar)
  overall_constraint <- constraint_table2_all %>% group_by(Order) %>% summarise(values = sum(values))
  
  bound_table_cols <- colnames(bound_table_dataset)
  bound_table2_each <- bound_table_dataset
  # bound_table2_each <- filter(bound_table2, Operation == "Each")
  # bound_table2_all <- filter(bound_table2, Operation == 'All')
  # bound_table2_all$values <- apply(bound_table2_all,1,FUN=eval_formula_bound_overall,x,bound_table_cols,reactData$spendVar)
  # 
  # lb <- bound_table2_all %>% group_by(Order) %>% summarise(Lower = mean(as.numeric(as.character(Lower))), values = sum(values))
  # lb <- lb %>% mutate(lower = Lower-values)
  # ub <- bound_table2_all %>% group_by(Order) %>% summarise(Upper = mean(as.numeric(as.character(Upper))), values = sum(values))
  # ub <- ub %>% mutate(upper = values-Upper)
  # values <- c(as.vector(constraint_table2_each$values), as.vector(overall_constraint$values), as.vector(lb$lower), as.vector(ub$upper))
  values <- c(as.vector(constraint_table2_each$values), as.vector(overall_constraint$values))
  return(values)
}


eval_formula_bound <- function(row,x,cols,spendVar){
  #browser()
  row_number <- as.numeric(row[which(cols=='number_r')])
  Channel <- row[which(cols=='Spend_Channel')]
  
  1:length(spendVar) %>% map(~{
    Channel <<- gsub(spendVar[.], paste('((length(spendVar)*row_number)-(length(spendVar)-',.,'))'), Channel)
  }) %>% invisible()
  
  value <- eval(parse(text=Channel))
  #print(value)
  return(value)
}
# lb <- function(x){
#   #browser()
#   bound_table_cols <- colnames(bound_table2)
#   bound_table2_each <- bound_table2
#   
#   bound_table2_each$values <- apply(bound_table2_each,1,FUN=eval_formula_bound,x,bound_table_cols,reactData$spendVar)
#   bound_table2_each <- bound_table2_each %>% arrange(values)
#   
#   lower <- as.numeric(as.vector(bound_table2_each$Lower))
#   
#   return(lower)
# }

lb <- function(x, bound_table_dataset,spendVar){
  #browser()
  bound_table_cols <- colnames(bound_table_dataset)
  bound_table2_each <- bound_table_dataset
  
  bound_table2_each$values <- apply(bound_table2_each,1,FUN=eval_formula_bound,x,bound_table_cols, spendVar)
  bound_table2_each <- bound_table2_each %>% arrange(values)
  
  lower <- as.numeric(as.vector(bound_table2_each$Lower))
  
  return(lower)
}

# ub <- function(x){
#   
#   bound_table_cols <- colnames(bound_table2)
#   bound_table2_each <- bound_table2
#   
#   bound_table2_each$values <- apply(bound_table2_each,1,FUN=eval_formula_bound,x,bound_table_cols,reactData$spendVar)
#   bound_table2_each <- bound_table2_each %>% arrange(values)
#   
#   upper <- as.numeric(as.vector(bound_table2_each$Upper))
#   return(upper)
# }

ub <- function(x, bound_table_dataset,spendVar){
  
  bound_table_cols <- colnames(bound_table_dataset)
  bound_table2_each <- bound_table_dataset
  
  bound_table2_each$values <- apply(bound_table2_each,1,FUN=eval_formula_bound,x,bound_table_cols,spendVar)
  bound_table2_each <- bound_table2_each %>% arrange(values)
  
  upper <- as.numeric(as.vector(bound_table2_each$Upper))
  return(upper)
}

####### Required Params for Optimization Function

optimization_fn <- function(businessObjective, targetRevenue, optiTechnique, model, maxIteration, spendVar, DateVar, panelVar, bound_table_dataset, constraint_table_dataset, extraData, train_data, hierarchy_tab, historical_dataset, histDateSequence, optiDateSequence, spend_variables_seq, decay_rate_dataset){
  # browser()
  
  ########## For testing purpose only
  # newSpendDataset <- merge(histDateSequence, historical_dataset, by = DateVar, all.x = T)
  # 
  # if(businessObjective == "Revenue Chase") {
  #   newRevenue <- sum(predict(model, newSpendDataset))
  #   if(newRevenue > 1.1*targetRevenue | newRevenue < 0.9*targetRevenue) {
  #     if(newRevenue>targetRevenue){
  #       newSpendDataset[spendVar] <- 0.95*newSpendDataset[spendVar]
  #       newRevenue <- sum(predict(model,newSpendDataset))
  #       print(newRevenue)
  #       print(sum(newSpendDataset[spendVar]))
  #     }else{
  #       newSpendDataset[spendVar] <- 1.01*newSpendDataset[spendVar]
  #       newRevenue <- sum(predict(model,newSpendDataset))
  #     }
  #   }
  # }
  # # browser()
  # newBudConstraint <- sum(newSpendDataset[spendVar])
  # budget_constraint <- constraint_table_dataset %>% filter(Order == 1)
  # ## Updating Constarint Table
  # budget_constraint["formula"] <- paste(as.character(newBudConstraint), paste(spendVar, collapse = "+"), sep = "-")
  # non_budget_constraint <- constraint_table_dataset %>% filter(Order != 1)
  # constraint_table_dataset <- rbind(budget_constraint, non_budget_constraint )
  # # reactData$constraint_table2 <- constraint_table_dataset
  # x0 <- as.vector(t(as.matrix(newSpendDataset[spendVar])))
  ###################################
  
  OFN$businessObjective = businessObjective
  OFN$targetRevenue = targetRevenue
  OFN$optiTechnique = optiTechnique
  OFN$model = model
  OFN$maxIteration = maxIteration
  OFN$spendVar = spendVar
  OFN$DateVar = DateVar
  OFN$panelVar = panelVar
  OFN$bound_table_dataset = bound_table_dataset
  OFN$constraint_table_dataset = constraint_table_dataset
  OFN$extraData = extraData
  OFN$train_data = train_data
  OFN$hierarchy_tab = hierarchy_tab
  OFN$historical_dataset = historical_dataset
  OFN$optiDateSequence = optiDateSequence
  OFN$histDateSequence = histDateSequence
  OFN$spend_variables_seq = spend_variables_seq
  OFN$decay_rate_dataset = decay_rate_dataset
  
  lb <- lb(x, bound_table_dataset, spendVar)
  ub <- ub(x, bound_table_dataset, spendVar)
  x0 <- lb(x, bound_table_dataset, spendVar) +1
  
  ###############################################################################################
  ## When business objective option is selected as REVENUE CHASE, lb and x0 should be changed 
  if(businessObjective == "Revenue Chase") {
    
    #### Lower Bound set to zero for Business Objective "REVENUE CHASE"
    lb <- lb(x, bound_table_dataset, spendVar) - lb(x, bound_table_dataset, spendVar)
    
    newSpendDataset <- merge(histDateSequence, historical_dataset, by = DateVar, all.x = T)

    if(businessObjective == "Revenue Chase") {
      newRevenue <- sum(predict(model, newSpendDataset))
      while(newRevenue > 1.1*targetRevenue | newRevenue < 0.9*targetRevenue) {
        if(newRevenue>targetRevenue){
          newSpendDataset[spendVar] <- 0.97*newSpendDataset[spendVar]
          newRevenue <- sum(predict(model,newSpendDataset))
          print(newRevenue)
          print(sum(newSpendDataset[spendVar]))
        }else{
          newSpendDataset[spendVar] <- 1.03*newSpendDataset[spendVar]
          newRevenue <- sum(predict(model,newSpendDataset))
        }
      }
    }
    ### Arranging State and Spend Variable values as per how lb and ub function will churn out
    newSpendDataset <- newSpendDataset %>% arrange_at(c(DateVar, panelVar))
    # x0 <- as.vector(t(as.matrix(newSpendDataset[spendVar])))
    x0 <- (ub+lb)/2
  }
  ###############################################################################################
  
  # browser()
  cob_out <- switch(optiTechnique,
                    "ISRES" = isres1(x0 = x0, fn = fn, hin= ineq_fn ,lower = lb, upper = ub
                                     , maxeval = maxIteration, pop.size = 20*(length(x0)+1), xtol_rel = 1e-6, nl.info = FALSE),
                    "COBYLA" = nloptr::cobyla(x0, fn, hin = ineq_fn, lower = lb, upper = ub
                                              , nl.info = TRUE, control = list(xtol_rel = 1e-8, maxeval = maxIteration)),
                    "MOPSOCD" = {
                      mopsocd::mopsocd(fn, gn = ineq_fn, varcnt=1, fncnt=1, lowerbound=lb,upperbound=ub,opt=0,
                                       popsize=100,maxgen=100,archivesize=500)
                    },
                    "PSO" = pso::psoptim(par=rep(NA,2),fn = fn, lower = lb(), upper = ub()),
                    "SANN" = stats::optim(par=x0, fn, gr = NULL, method = c("SANN"), lower = lb, upper = ub, hessian = FALSE)
  )
  return(cob_out)
}



################################################################################
###################### Functions for Creating Dataset to Plot Graphs 
################################################################################

fn_rbind_data <- function(hist_dataset, opti_dataset, lvl_of_data=c(), melt_data, type_data="", agg_variable=c()){
  
  if(!is.null(lvl_of_data)){
    group_variables <- c(lvl_of_data, "variable")
  }else{
    group_variables <- c("variable")
  }
  
  hist_dataset <- melt(hist_dataset, measure.vars= c(melt_data)) %>% data.frame(stringsAsFactors = F)
  hist_dataset <- hist_dataset %>% group_by_at(group_variables)
  # Create a look-up list with function names and variable to apply
  if(!is.null(agg_variable)){
    look_list <- list(sum = c(agg_variable, 'value'))  
  }else{
    look_list <- list(sum=c('value'))
  }
  # Apply the summarise_at_fun
  hist_dataset <- map2(look_list, names(look_list), summarise_at_fun, data = hist_dataset) %>%
    reduce(left_join, by = group_variables)
  hist_dataset <- data.frame(hist_dataset, stringsAsFactors = F)
  hist_dataset$data_time_period <- rep('Historical', each=nrow(hist_dataset))
  
  opti_dataset <- melt(opti_dataset, measure.vars= c(melt_data))
  opti_dataset <- opti_dataset %>% group_by_at(group_variables) 
  # Apply the summarise_at_fun
  opti_dataset <- map2(look_list, names(look_list), summarise_at_fun, data = opti_dataset) %>%
    reduce(left_join, by = group_variables)
  opti_dataset <- data.frame(opti_dataset, stringsAsFactors = F)
  opti_dataset$data_time_period <- rep(type_data, each=nrow(opti_dataset))
  
  ###### Merging two datasets to create one super-datasets
  master_dataset <- rbind(opti_dataset, hist_dataset)
  master_dataset <- data.frame(master_dataset, stringsAsFactors = F)
  
  return(master_dataset)
}

fn_contrib <- function(mmx_model, master_dataset, spendVar, req_contrib_cols, DateVar, panelVar){
  
  contrib_lmer_raw_timeSeries <- calculate_contributions5(model = mmx_model
                                                          , dataset = master_dataset
                                                          , spend_variables = spendVar
                                                          , by_cols = req_contrib_cols)
  contrib_lmer_raw_timeSeries <- data.frame(contrib_lmer_raw_timeSeries, stringsAsFactors = FALSE)
  contribVar <- paste("contrib_", spendVar, sep="")
  pctContribVar <- paste("pct_contrib_", spendVar, sep="")
  contribVar <- c(contribVar, "contrib_baseline")
  pctContribVar <- c(pctContribVar, "pct_contrib_baseline")
  keeps <- c(contribVar, pctContribVar, req_contrib_cols)
  contrib_lmer_raw_timeSeries <- contrib_lmer_raw_timeSeries[keeps]
  contrib_lmer_raw_timeSeries <- as.data.frame(contrib_lmer_raw_timeSeries)
  
  contributionData_timeSeries <- melt(contrib_lmer_raw_timeSeries, measure.vars=contribVar)
  # pctContributionData_timeSeries <- melt(contrib_lmer_raw_timeSeries, measure.vars=pctContribVar)
  
  contributionData_timeSeries <- contributionData_timeSeries %>% select_at(c(req_contrib_cols, "variable", "value"))
  # pctContributionData_timeSeries <- pctContributionData_timeSeries %>% select_at(c(DateVar, panelVar, "variable", "value"))
  contributionData_timeSeries <-  data.frame(contributionData_timeSeries, stringsAsFactors = FALSE)
  #### Removing Contrib_ from channel names
  contributionData_timeSeries$variable <- gsub(pattern = "contrib_*", replacement = "", x = contributionData_timeSeries$variable)
  return(contributionData_timeSeries)
  
}

fn_pct_contrib <- function(dataset, 
                           lvl_of_data, 
                           contrib_col){
  
  all_contrib_variable <- "All_Contribution"
  pctContribDataset <- dplyr::group_by_at(dataset, lvl_of_data)
  # Create a look-up list with function names and variable to apply
  look_list <- list(sum = contrib_col)
  # Apply the summarise_at_fun
  pctContribDataset <- map2(look_list, names(look_list), summarise_at_fun, data = pctContribDataset) %>%
    reduce(left_join, by = lvl_of_data) %>% data.frame()
  setnames(pctContribDataset, old=c(contrib_col), new=c(all_contrib_variable))
  dataset <- merge(dataset, pctContribDataset, by=lvl_of_data, all.x=TRUE)
  dataset <- dplyr::mutate(dataset, Total_Pct_Contribution = (dataset[[contrib_col]]/dataset[[all_contrib_variable]])*100)
  return(dataset)
  
}

build_timeSeriesDataset <- function(x, dateFrequency, DateVar){
  if(dateFrequency == 'Monthly'){
    x$Time_Num <- lubridate::month((x[[DateVar]]))
    x$Time_X <- month.abb[x$Time_Num]
  }
  if(dateFrequency == 'Weekly'){
    x$Time_X <- paste0('Week_', x$Time_Num)
  }
  if(dateFrequency == 'Daily'){
    x$Time_X <- paste0('Day_', x$Time_Num)
  }
  return(x)
}

fn_filter_dataset <- function(dataset, filter_col, filter_value, flag){
  if(filter_value %in% c("Total", "All") && flag == 1){
    data <- dataset
  }else{
    if(flag == 1){
      data <- dataset %>% filter_at(filter_col,any_vars(.%in%filter_value))
    }else{
      data <- dataset %>% filter_at(filter_col,any_vars(!.%in%filter_value))
    }
  }
  data <- data.frame(data, stringsAsFactors = FALSE)
  return(data)
}

###################### This function is used when Fractional Root Model is uploaded
power_fn <- function(x,p) x^p


