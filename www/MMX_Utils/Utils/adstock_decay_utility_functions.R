
################################################################################################
# Title : Utilities for Marketing Mix Modeling and Carry-over and Saturation effects notebooks
# Created : August 23, 2018
# Author : Sunuganty Achyut Raj
# Version : 18.08.01
# Description : Provides all the functions for data processing, modelling, plotting used in Carry-over 
# and Saturation effects notebook and the Modelling notebook 
#
# Revisions
################################################################################################


#------------------------------------------------------------------------------------------------------------------------------
# Function to install and load libraries
#------------------------------------------------------------------------------------------------------------------------------

## Dependency packages
packages <- c("magrittr",
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
  do.call("library", list(x))
}

# installing packages
packageStatus <- lapply(packages, FUN=pkgInstall)



#------------------------------------------------------------------------------------------------------------------------------
# Function for splitting input data into train and test datasets
#------------------------------------------------------------------------------------------------------------------------------
#' Function for splitting input data into train and test datasets
#' @param dataset The dataframe to perform the split on 
#' @param split_date The end date of train period
#' @param date_column The name of the column containing date values in input dataset
#' @return List of train and test datasets 
#' @export


# Function to get the train and test data from the overall dataset
get_train_test_split <- function(dataset,split_date,date_column){
  # Split the data into test and train
  
  train_data <- dataset %>% filter_at(date_column,all_vars(. <= split_date))
  test_data <- dataset %>% filter_at(date_column,all_vars(. > split_date))
  return(list(train=train_data,test=test_data))
}

#------------------------------------------------------------------------------------------------------------------------------
# Functions for applying adstock transformation on each of the spend variables
#------------------------------------------------------------------------------------------------------------------------------

#' Functions for applying adstock on a vector
#' @param x The input time-ordered vector
#' @param rate The adstock decay rate
#' @return Adstock transformed vector
#' @export

# Adstock calculation functions
adstock <- function(x,rate=0){
  return(as.numeric(stats::filter(x=x,filter=rate,method="recursive")))
}


#' Functions for applying adstock by groups (panels)
#' @param x The input vector for transformation
#' @param decay decay rate 
#' @param time_var The vector with time points corresponding to each value in x
#' @param panel_var_df The dataframe with panel columns that have rows corresponding to each value in x
#' @return Vector of astock transformed values
#' @export

# Adstock calculation functions for a dataframe
adstock_df <- function(x,decay,time_var,panel_var_df){
  panel_vars<- colnames(panel_var_df)
  df <- data.frame(x=x,time_var=time_var) %>% cbind(data.frame(panel_var_df))
  df <- df %>%
    mutate(order=c(1:nrow(df))) %>%
    arrange_at(c(panel_vars,'time_var')) %>%
    group_by_at(panel_vars) %>%
    mutate(x=adstock(x,decay)) %>%
    ungroup() %>%
    arrange(order)
  return(df[['x']])
}


#' Functions for applying adstock for multiple column in a dataset by panel
#' @param dataset The input dataset 
#' @param spend_variables The variables to perform the transformation on
#' @param decay_rate A vector of decay rate corresponding to each spend variable
#' @param time_variable The name of the column in the dataset holding the time values
#' @param adstock_panel_variables The names of columns defining the panels in the dataset
#' @return An adstock transformed dataset for column in spend_variables 
#' @export

apply_adstock <- function(dataset,spend_variables,decay_rate,time_variable,adstock_panel_variables){
  # Convert the variables to consider carry-over effects
  
  dataset[,spend_variables] <- mapply(adstock_df,
                                      x=dataset[,spend_variables,drop=FALSE],
                                      decay=decay_rate,
                                      MoreArgs = list(time_var=dataset[[time_variable]],
                                                      panel_var_df=dataset[,adstock_panel_variables,drop=FALSE]))
  
  return(dataset)
}



#---------------------------------------------------------------------------------------------------------------------------
# Function for building a mix model
#---------------------------------------------------------------------------------------------------------------------------


#' Function to build a mixed effects model given formula,data and other params
#' @param train_data Training dataset
#' @param formula The model specification
#' @param call The modelling function e.g. 'lme4::lmer'
#' @return A mixed effects model of class defined by the modeling function provided in 'call' 
#' @export

# build_model <- function(train_data,formula,call,...){
  build_model <- function(model_params,train_data){
  
  # Get the modeling function
  fun <- unlist(strsplit(model_params$call,'::'))
  # fun <- unlist(strsplit(call,'::'))
  myfun <- get(fun[2], asNamespace(fun[1]))
  
  # Get parameters apart from 'call' and 'formula'
  otherParams <- model_params[unlist(setdiff(names(model_params),c('call','formula')))]
  # otherParams <- list(...)
  
  # Concatenate all parameters to a list
  args <- c(list(formula=model_params$formula,data=train_data),if(length(otherParams)!=0) otherParams)
  
  # Call modeling function
  model <- do.call(myfun, args)
  
  if(!is.null(model_params$wrapperClass)){
    model <- list(model=model)
    class(model) <- model_params$wrapperClass
  }
  
  return(model)
  
}



#------------------------------------------------------------------------------------------------------------
# Error metrics of models built 
#------------------------------------------------------------------------------------------------------------

#' Function to get error metrics of models built for each row in decay rate grid for all spend variables
#' @param model_list The list of models created as output of build_all_models function
#' @param depVar The name of dependent variable
#' @param decay_grid The grid of decay values create using the get_decay_grid function
#' @return A dataframe reporting error corresponding to each decay value for each spend variable for each model type(saturation effect) 
#' @export
#' 
get_error_metrics <- function(model_list, depVar, decay_grid){
  # browser()
  require(Metrics)
  model_names <- names(model_list[[1]]$models)
  
  # Getting the adstock with least error for each model
  ad_error <- lapply(model_list,function(x){
    mape_values <- sapply(model_names,function(name){
      out <- Metrics::mape(x$params$test.data[[depVar]],predict(x$models[[name]],x$params$test.data))
      names(out) <- name
      return(out)
    })
    return(mape_values)
  }) %>% do.call(rbind,.)
  
  colnames(ad_error) <- model_names
  error_dat <- cbind(decay_grid,ad_error)
  return(error_dat)
}

# error_dat <- get_error_metrics(model_list,'Sales',decay.values)


#------------------------------------------------------------------------------------------------------------
# ------- Visualizing adstocks 
#------------------------------------------------------------------------------------------------------------
#' Function to visualize adstocks 
#' @param model_list The list of models created as output of build_all_models function
#' @param decay The vector of decay values considered for creating the decay grid using get_decay_grid function
#' @param ads_df The actual dataset that has not been adstock transformed 
#' @param panel_columns Names of columns in ads_df that define the panels
#' @param date_column Name of the column in ads_df containing the date values
#' @param y_variable Name of the dependent variable column
#' @param view_variable Name of the spend variable for which the adstock needs to be visualized 
#' @param group_by_cols The subset column names in panel_column variable. The values will be aggregated and then plotted at this level. 
#' @param n_plots The number of unique panels to visualize
#' @return Plot of time series of adstock values for the spend variable specified and for chosen number of panels
#' @export
#' 
visualize_adstock <- function(model_list,decay,ads_df,panel_columns,date_column,train_max_date,y_variable,view_variable,group_by_cols,n_plots=2){
  # browser()
  train_data_list <- model_list %>% map('params') %>%
    map('train.data') %>% head(.,length(decay)) %>%
    map(~dplyr::select_at(.,c(panel_columns,date_column,y_variable,view_variable))) %>% 
    do.call(rbind,.) 
  
  train_data_list$iteration <- rep(c(1:length(decay)),each=nrow(model_list[[1]]$params$train.data))
  
  train_data_list <- ads_df %>% 
    filter_at(date_column,all_vars(.<=train_max_date)) %>% 
    dplyr::select_at(c(panel_columns,date_column,view_variable,y_variable)) %>% 
    mutate(iteration=0) %>% data.frame() %>% 
    rbind(data.frame(train_data_list)) %>% 
    mutate(iteration=as.factor(iteration)) %>% 
    group_by_at(c(group_by_cols,'iteration',date_column)) %>% 
    dplyr::summarise_at(view_variable,funs(sum))
  
  # Subset data
  plot_data <- train_data_list %>% ungroup() %>% 
    filter_at(group_by_cols,all_vars(. %in% unique(.)[1:n_plots])) %>% rename(decay=iteration)
  levels(plot_data$decay) <- c(0,decay.values[,1][1:length(decay)])
  
  # Plot
  adstock_plots <- ggplot(plot_data,aes_string(x=date_column)) + geom_line(aes_string(y=view_variable,color='decay')) + facet_wrap(as.formula(paste0('~',paste(group_by_cols,collapse='+')))) + theme_bw()
  return(adstock_plots)
}

# visualize_adstock(model_list
#                   ,decay
#                   ,ads_df=ads_df1
#                   ,panel_columns= panel_column
#                   ,date_column
#                   ,y_variable='Sales'
#                   ,view_variable='Direct'
#                   ,group_by_cols = c('State'),n_plots=4)



#------------------------------------------------------------------------------------------------------------
#-------- Visualizing model fit
#------------------------------------------------------------------------------------------------------------
#' Function to plot the actual vs predicted values for train and validation time period
#' @param opt_model_list The list of optimal models seleted for each of the saturation effect types
#' @param panel_vars The names of variables that define the panels
#' @param date_column The column name containing dates
#' @param y_variable The dependent variable column name
#' @param n_panels No. of panels to generate the plots for
#' @return Plot of time series of actual vs fitted values for each model for n_panels
#' @export

visualize_fit <- function(opt_model_list,panel_vars,date_column,y_variable,n_panels){
 
  require(dplyr)
  require(tidyr)
  
  all_data <- rbind(opt_model_list[[1]]$train.data,opt_model_list[[1]]$test.data) %>% data.frame()
  train_date_max <- max(data.frame(opt_model_list[[1]]$train.data)[,date_column])
  
  predictions <- opt_model_list %>% map(~{
    model <- .[['model']]
    full_data <- rbind(.[['train.data']],.[['test.data']])
    pred <- predict(model,full_data)
  }) %>% do.call(cbind,.)
  
  plot_data1 <- data.frame(time=all_data[[date_column]]
                      ,panel=all_data[,panel_vars]
                      ,actual=all_data[[y_variable]]
                      ,predictions
  )
  
  plot_data1 <- plot_data1 %>%  filter_at('panel',all_vars(. %in% unique(.)[1:n_panels]))
  
  plot_data1 <- plot_data1 %>% group_by(time, panel) %>% summarize_all(funs(sum)) %>% melt(.,id.vars = c('time','panel')) %>% mutate(variable=as.character(variable), panel=as.character(panel))
  
  plot <- ggplot(plot_data1,aes(x=time,color=variable)) + geom_line(aes(y=value)) + geom_vline(xintercept = as.Date(train_date_max),linetype='dashed',color='Salmon') + facet_wrap(~panel)  + ylab(y_variable) + expand_limits(y=0) + theme_bw()
  
  return(plot)
}

visualize_fit2 <- function(opt_model_list,panel_vars,date_column,y_variable,n_panels){
  
  require(dplyr)
  require(tidyr)
  
  all_data <- rbind(opt_model_list[[1]]$train.data,opt_model_list[[1]]$test.data) %>% data.frame()
  train_date_max <- max(data.frame(opt_model_list[[1]]$train.data)[,date_column])
  
  predictions <- opt_model_list %>% map(~{
    model <- .[['model']]
    full_data <- rbind(.[['train.data']],.[['test.data']])
    pred <- predict(model,full_data)
  }) %>% do.call(cbind,.)
  
  plot_data1 <- data.frame(time=all_data[[date_column]]
                           ,panel=all_data[,panel_vars]
                           ,actual=all_data[[y_variable]]
                           ,predictions
  )
  # browser()
  plot_data1 <- plot_data1 %>%  filter_at('panel',all_vars(. %in% unique(.)[1:n_panels]))
  
  plot_data1 <- plot_data1  %>% melt(measure.vars = c('actual','predictions')) %>% group_by(time, panel, variable) %>% summarize(value =sum(value))
  
  plot <- ggplot(plot_data1,aes(x=time,color=variable)) + geom_line(aes(y=value)) + geom_vline(xintercept = as.Date(train_date_max),linetype='dashed',color='Salmon') + facet_wrap(~panel)  + ylab(y_variable) + expand_limits(y=0) + theme_bw()
  
  return(plot)
}

# Need prediction function for log-log model
# visualize_fit(opts$opt_models,panel_vars='State',date_column,y_variable,n_panels=4)
  


#------------------------------------------------------------------------------------------------------------
#-------- Get optimal decay error among all decay rates in decay grid
#------------------------------------------------------------------------------------------------------------
#' Function to obtain the model with least error among all the models generated by build_all_models function, thereby selecting the optimal decay values
#' @param model_list The list of models generated by build_all_models function
#' @param error_data The dataframe containing erros corresponding to each row in decay grid for each saturation effect models
#' @return A list of two elements. First element is a list of optimal models. The second is a dataframe having optimal decay values corresponding to optimal models
#' @export


get_optimal_models <- function(model_list,error_data){
  
  # Get model names
  model_names <- names(model_list[[1]]$models)
  
  # Get the decay rates with least error for each model
  min_error_list <- error_data %>% map_at(model_names,~{which(.==min(.))[1]}) %>% unlist # Get first index with minimum error rate for each model
  
  min_error_dat <- error_data[min_error_list,]  %>% 
    mutate(error = apply(.[,which(colnames(error_dat) %in% model_names)][,model_names],2,min)) # Column wise minimum error
  
  rownames(min_error_dat) <- model_names
  
  # Get the optimal models
  # model_list[min_error_list] %>% map('models') 
  optimal_models <- mapply(function(sub_model_list,name){
    model <- sub_model_list$models[[name]]
    obj <- list(model=model
                ,train.data=sub_model_list$params$train.data
                ,test.data=sub_model_list$params$test.data)
    return(obj)
  },model_list[min_error_list],model_names,SIMPLIFY = FALSE)
  
  names(optimal_models) <- model_names
  
  return(list(opt_models=optimal_models,error_stats=min_error_dat))
}

# opts <- get_optimal_models(model_list,error_dat)



#------------------------------------------------------------------------------------------------------------
#-------- Get grid of decay values considering all spend variables
#------------------------------------------------------------------------------------------------------------

#' Function to get grid of decay values considering all spend variables
#' @param decay A sequence of adstock decay values to consider
#' @param spend_variables The spend variable names for which the adstock has to calculated
#' @return A dataframe with grid of decay values considering the spend variables
#' @export
#'
get_decay_grid <- function(decay,spend_variables){
  decay.values <- expand.grid(decay,decay,decay,decay) # Corresponding to 4 spend variables 
  names(decay.values) <- spend_variables
  return(decay.values)
}



#------------------------------------------------------------------------------------------------------------
#-------- Plot contribution in a grid layout for all the optimal models
#------------------------------------------------------------------------------------------------------------
#' Function to plot contribution in a grid layout for all the optimal models
#' @param opt_model_list A names list of optimal models along with traning data
#' @param spend_variables The spend variable names for which the adstock has to calculated
#' @param contribArgs 
#' @return A dataframe with grid of decay values considering the spend variables
#' @export

plot_contrib_as_grid <- function(opt_model_list,spend_variables,contribArgs = NULL){
  plot_titles <- names(opt_model_list)
  if(is.null(plot_titles)){
    plot_titles <- paste0('Model_',c(1:length(opt_model_list)))
    names(opt_model_list) <- plot_titles
  }
  
  contrib_plots <- opt_model_list %>% map(~{
    
    args <- list(model = .[['model']]
                 , dataset = data.frame(.[['train.data']])
                 , spend_variables = spend_variables)
    
    if(!is.null(contribArgs)){
      args <- c(args,contribArgs)
    }
    
    model_contrib <- do.call(calculate_contributions5,args)
    
    pct_colnames <- colnames(model_contrib)[grep('^pct_contrib_',colnames(model_contrib))]
    pct_model_contrib <- model_contrib[pct_colnames]
    
    pct_model_contrib <- pct_model_contrib %>% melt(.)
    
    plot_model_contrib <- ggplot(pct_model_contrib,aes(x=variable,y=value)) + geom_bar(stat='identity',color='blue',fill='dodgerblue3') + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    return(plot_model_contrib)
    
  })
  
  contrib_plots <- plot_titles %>% map(~contrib_plots[[.]] + labs(y='Contribution (%)',title=.))
  
  return(contrib_plots)
}


#------------------------------------------------------------------------------------------------------------
#-------- Save optimal models
#------------------------------------------------------------------------------------------------------------

#' Function to save models and other relevant information to used in other notebooks
#' @param model_list A names list of models along with traning data and testing data
#' @param spend_variables The spend variable names for which the adstock has to calculated
#' @param contribArgs 
#' @return A dataframe with grid of decay values considering the spend variables
#' @export
#'
#'
# Note : This function is specfic to the carry over and saturation effect notebook and uses some global variables defined in the notebook

save_opt_models <- function(model_list,spend_variables,predFuns=NULL,moreArgs=NULL,dirPath=NULL){
  model_names <- names(model_list)
  
  model_names %>% map(~{
    object <- list(model=model_list[[.]]$model
                   ,train_data=model_list[[.]]$train.data
                   ,test_data=model_list[[.]]$test.data
                   ,spend_variables=spend_variables
                   ,predFuns=predFuns
                   ,ads_df=ads_df
                   ,panel_column=panel_column
                   ,date_column=date_column
                   ,date_format=date_format
                   ,y_variable=y_variable
                   ,seasonal_column=seasonal_column
                   ,trend_column=trend_column
                   ,train_max_date=train_max_date
                   ,test_max_date=test_max_date
                   ,moreArgs=moreArgs)
    
    path <- paste0(dirPath,.,'.RDS')
    saveRDS(object,path)
    print(paste0(.,' model saved to ',path))
  })
  
  return(0)
}



#------------------------------------------------------------------------------------------------------------
#-------- Function for plotting marginal errors across decay values
#------------------------------------------------------------------------------------------------------------
#' Function to save models and other relevant information to used in other notebooks
#' @param model_list A names list of models along with traning data and testing data
#' @param spend_variables The spend variable names for which the adstock has to calculated
#' @param contribArgs 
#' @return A dataframe with grid of decay values considering the spend variables
#' @export
#'
plot_decay_error <- function(error_dat, decay, spend_variables){
  spend_variables %>% map(~ {
    var <- .
    other_vars <- setdiff(spend_variables, var)
    table_vars <- setdiff(colnames(error_dat), other_vars)
    dat <- error_dat %>%
      filter_at(other_vars, all_vars(. == min(decay))) %>%
      select_at(table_vars) %>%
      melt(id.vars = var)
    colnames(dat) <- c(., 'Model', 'Error')
    
    ggplot(dat, aes_string(x = var, y = 'Error', color = 'Model')) +
      geom_line() +
      geom_point() +
      labs(y ='test MAPE') + theme_bw()
    
  }) %>% do.call(grid.arrange, .)
}



#------------------------------------------------------------------------------------------------------------
# Function for building every saturation effect model for each combination of decay values in the decay grid
#------------------------------------------------------------------------------------------------------------
#' Function for building a mmx model for each type of saturation effect across a range of adstock decay values 
#' @param models_to_build A named list of model specification along with parameters for modelling
#' @param dataset The dataset from which the training and validation datasets are generated
#' @param spend_variables Vector of spend variable column names in dataset
#' @param date_column The column name containing the date values
#' @param train_max_date The date value for partitioning dataset into train and test
#' @param adstock_panel_variables Names of columns that define the panels in dataset
#' @param decay.values The grid of decay values created using get_decay_grid function
#' @param clusterExport The names of the user defined functions, variables, object that need to be exported to clusters during parallel execution. User might be using UDF functions in the formulas. Their function definition need to be exported to clusters so that they are available during execution.
#' @return A list of models and other values correspnding to each row in the decay grid
#' @export


build_all_models <- function(models_to_build
                             ,dataset
                             ,spend_variables
                             ,date_column
                             ,train_max_date
                             ,adstock_panel_variables
                             ,decay.values,exportVars=NULL){
  
  # Execute iterations parallely
  cl <- makeCluster(mc <- getOption("cl.cores", detectCores()-1))
  varlist <- c("decay.values", "spend_variables", "adstock_df", "adstock","dataset",
               "train_max_date", "get_train_test_split", "apply_adstock",
               "build_model","adstock_panel_variables","date_column","models_to_build",if(!is.null(exportVars)) exportVars)
  clusterExport(cl=cl, varlist=varlist
                ,envir=environment())
  
  # Build the models on the adstock transformed data
  model_list <- parLapply(cl=cl,c(1:nrow(decay.values)),function(i){
    require(dplyr)
    require(purrr)
    
    # Get the decay vector
    current_decay_rate <- decay.values[i,]
    
    # Convert the variables to consider carry-over effects
    dataset <- apply_adstock(dataset,spend_variables
                             ,current_decay_rate
                             ,date_column
                             ,adstock_panel_variables)
    
    # Get train and test data
    train_test <- get_train_test_split(dataset,train_max_date,date_column)
    
    # Build models
    model_names <- names(models_to_build)
    if(is.null(model_names)){
      model_names <- paste0('model_',c(1:length(models_to_build)))
      names(models_to_build) <- model_names
    }
    
    # Build models
    # mix_models <- models_to_build %>% map(~build_model(train_test$train,.$formula,.$call))
    mix_models <- models_to_build %>% map(~build_model(.,train_test$train))
    
    # Save model, data and parameters
    mix_models <- list(models=mix_models, params=list(train.data=train_test$train,test.data=train_test$test,decay.values=current_decay_rate))
    
    return(mix_models)
  })
  
  names(model_list) <- paste0('Iter_',c(1:length(model_list)))
 
  return(model_list) 
}



#------------------------------------------------------------------------------------------------------------
# Function to get error metrics for a model
#------------------------------------------------------------------------------------------------------------
#' Function to get error metrics for models
#' @param model Model object
#' @param train_data Training D\dataset
#' @param test_data Validation dataset 
#' @param y_variable Dependent variable column name
#' @return Train and validation error for the model output as a dataframe
#' @export
#'
get_model_accuracy <- function(model,train_data,test_data,y_variable){
  require(Metrics)
  error <- list(model) %>% map(~{
    train.error <- mape(train_data[[y_variable]],predict(.,train_data))
    test.error <- mape(test_data[[y_variable]],predict(.,test_data))
    return(list(train=train.error,test=test.error))
  })
  
  error.table <- data.frame(train.mape=error %>% map('train') %>% unlist()
                            ,test.mape=error %>% map('test') %>% unlist()
  )
  
  return(error.table)
}  
  

#------------------------------------------------------------------------------------------------------------
# Function to add a time trend variable (time trend : incremental integer values for each time stamp)
#------------------------------------------------------------------------------------------------------------
#' Function to add a time trend variable (time trend : incremental integer values for each time stamp)
#' @param dataset The dataframe where the time trend column needs be added
#' @param date_column Name of the column with time values in the dataset
#' @param hierarchy Names of columns that define the panels
#' @param y_variable Dependent variable column name
#' @return Dataset with a time trend variable appended
#' @export
#'
add_time_trend_var <- function(dataset, date_column, hierarchy){
  
  dataset <- dataset %>% arrange_at(date_column) %>% group_by_at(hierarchy) %>% mutate(time_order=row_number()) 
  return(dataset)
}


#------------------------------------------------------------------------------------------------------------
# Function to plot residuals by panel
#------------------------------------------------------------------------------------------------------------
#' Function to plot residuals by panel
#' @param model Model object
#' @param train_data Training dataset
#' @param panel_column Names of columns that define the panels
#' @param time_column Name of the column having date values
#' @param n_panels No. of panels to show the plots for
#' @param mfrow Grid layout rows and column 
#' @param mar Margins of plots
#' @return No return object but plots the acf of residuals by panel
#' @export
#'
plot_acf_by_panel <- function(model,train_data,panel_column,time_column,n_panels=4,mfrow=c(1,4),mar=c(0,3,4,2)){
  require(gridExtra)
  resid.df <- data.frame(panel=train_data[[panel_column]],time=train_data[[time_column]],resid=residuals(model)) %>% group_by(panel,time) %>% dplyr::summarise_all(funs(sum)) %>% data.frame() %>% filter(panel %in% unique(panel)[1:n_panels])
  
  par(mfrow=mfrow,mar=mar)
  a <- resid.df  %>% split.data.frame(f=as.factor(as.character(resid.df$panel))) 
  plot_names <- names(a)
  a <- plot_names %>% map(~acf(a[[.]]$resid,main=.))
}


#------------------------------------------------------------------------------------------------------------
# Function to print ADS info in quick peek
#------------------------------------------------------------------------------------------------------------
#' Function to print ADS info in quick peek
#' @param ads_df ADS dataset
#' @param spend_variable Spend variables
#' @param panel_column Names of columns that define the panels
#' @param date_column Name of the column having date values
#' @return No object returned. The function print few lines of summary corresponding to the dataset
#' @export
#'

print_data_info <- function(ads_df,spend_variable,panel_column,date_column){
  cat(paste0('The training dataset contains ',ncol(ads_df),' columns and ',nrow(ads_df),' rows.\n'))
  cat('\n')
  cat(paste0('Spend variables : ',paste(spend_variables, collapse=', ')))
  cat('\n\n')
  
  ### counts
  cat(paste('Time Column Name :',date_column,'\n'))
  cat(paste('No. of Time Points :',length(unique(ads_df[[date_column]])) ))
  cat('\n\n')
  cat(paste0('Hierarchy in the dataset : ',paste(panel_column,collapse=','),'\n'))
  # cat(paste('No. of Unique',panel_column[1],' : ',length(unique(ads_df[,panel_column,drop=FALSE])),'\n'))
  cat(paste('\nTime period : ',paste(range(as.character(ads_df[[date_column]])),collapse=' to ')))
  
}


#------------------------------------------------------------------------------------------------------------
# Function to top10 rows of a dataframe
#------------------------------------------------------------------------------------------------------------

print_df_head <- function(df,round=2){
  require(DT)
  num_cols <- colnames(ads_df)[sapply(df,is.numeric)]
  datatable(head(df,10),options=list(dom='t',scrollX=TRUE)) %>% formatRound(num_cols,round)
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
  var_list <- c('date_column','date_format','spend_variables','y_variable','train_max_date','ads_df')
  var_list_stat <- var_list %>% map(~assign(.,object_list[[.]],envir = .GlobalEnv))
  
  return(0)
}

#######################################################################
# This function gives the user idea how well their model is performing
#######################################################################
modelPerformance <- function(model, train_data, test_data, targetVar, panelVar, DateVar){
  error_table <- get_model_accuracy(model,train_data,test_data,targetVar)
  data_tab <- datatable(error_table,options=list(dom='t')) %>% formatRound(c(1,2),digits=3)
  mix_list <- list(model = model, train.data = train_data, test.data = test_data)
  plot <- visualize_fit2(list(mix_list),panel_vars = panelVar, date_column = DateVar, targetVar ,n_panels=4)
  
  return(list(data_tab, plot))
}


