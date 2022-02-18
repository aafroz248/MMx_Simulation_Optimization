################################################################################################
# Title : Utilities for calculating contributions and maringal effects for marketing mix models
# Created : February 14, 2018
# Author : Sunuganty Achyut Raj
# Version : 18.02.01
# Description : Calculates contributions and marginal effects using a model-agnostic approach
#
# Initial Version : 18.02.01
# Revisions
# Date : 12th Apr 2013
# Version : 18.04.01
# Author : Sunuganty Achyut Raj
# Changes :
#   1. Added calculate_contributions5 function which calculates
#       minimum values per panel if the panel variables are provided via the panel_cols argument
#   2. Added marginal_effects section to use the get_marginal_plots5 function which plots the
#       marginals of all the panels and a mean plot if panel variables are provided via group_by_cols
#
# Date : 18th Apr 2013
# Version : 18.04.02
# Author : Sunuganty Achyut Raj
# Changes :
#   1. Corrected the conditional ggplot code in get_marginal_plots5 function. Added '.' operator to 
#       summarize functions and moved mean calculation for prediction column into the condition

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


## -------------------------------------------------------------------------------------------------------
## -- Contributions calculation function -----------------------------------------------------------------
## -------------------------------------------------------------------------------------------------------


get_contributions1 <- function(model
                               , dataset
                               , fit_values = NULL
                               , predictors
                               , seasonal_dummies = NULL
                               , group_by_cols = NULL
                               , pred_colname=ifelse(class(model)=='brmsfit','Estimate','fit')
                               , pred_struct=c('vector','other')
                               , aggregation='sum'
                               ,intercept_contrib=FALSE){
  tryCatch({
    if(is.null(fit_values)){
      pred <- predict(model,dataset)
      pClass <- class(pred)
      
      if(is.numeric(pred)) dataset$fitted <- as.numeric(pred) # For numeric class
      else dataset$fitted <- pred[,pred_colname] # For matrix or data.frame
      
    } else dataset$fitted <- fit_values
    
    
    predictors <- c(predictors,if(!is.null(seasonal_dummies)) seasonal_dummies, if(intercept_contrib) 'intercept')
    
    require(prediction)
    # model_data <- prediction::find_data(model)
    model_data <- dataset
    
    contributions_df <- do.call(cbind,lapply(predictors,function(xvar){
      # Set the values of the test column to zero
      
      dataset1 <- data.frame(dataset)
      testCol <- xvar
      
      if(xvar!='intercept'){
        # Check if testCol exists and then make the value 0
        if(!testCol %in% colnames(dataset1)) stop(paste('Column not found : ',testCol))
        dtype <- class(dataset1[,testCol])
        dataset1[,testCol] <- 0
      } else {
        local_preds <- setdiff(predictors,'intercept')
        dataset1[,local_preds] <- rep(0,length(local_preds))
      }
      
      # Convert factor columns back to factors
      col_class <- sapply(model_data,class)
      factor_cols <- names(col_class[which(col_class=='factor')])
      dataset1[,factor_cols] <- lapply(factor_cols,function(x){
        factor(dataset1[,x],levels=levels(model_data[,x]))
      })
      
      # Get predicted values for the dataset with the test column values made '0'
      prediction <- predict(model,dataset1)
      # pClass <- class(prediction)
      
      if(pred_struct[1]=='vector') dataset1$prediction <- as.numeric(prediction) # For numeric class
      else dataset1$prediction <- prediction[,pred_colname] # For matrix or data.frame
      
      # Calculate the contribution of the test column by subtracting the predictions from fitted values
      newname <- paste0('contrib_',testCol)
      if(xvar!='intercept') dataset1[,newname] <- dataset1$fitted - dataset1$prediction
      else dataset1[,newname] <- dataset1$prediction
      
      return(dataset1[,newname,drop=FALSE])
    }))
    
    
    # Add the contribution columns to the dataset
    dataset <- cbind(dataset,contributions_df)
    contrib_cols <- colnames(dataset)[grep('^contrib_',colnames(dataset))]
    
    # Rollup the dataset to overall level i.e. sum all the observation level contribution to get total contribution 
    if(!is.null(group_by_cols))
      agg_dataset <- dataset %>% select_at(vars(c(group_by_cols,contrib_cols,'fitted'))) %>% group_by_at(vars(group_by_cols)) %>% summarize_all(funs(do.call(aggregation,list(.))))
    else agg_dataset <- dataset %>% select_at(vars(c(contrib_cols,'fitted'))) %>% summarize_all(funs(do.call(aggregation,list(.))))
    
    # Add up the contributions of seasonal dummies
    if(!is.null(seasonal_dummies)){
      seasonal_dummies_contrib_cols <- paste0('contrib_',seasonal_dummies)
      agg_dataset$contrib_seasonal <- apply(agg_dataset[,seasonal_dummies_contrib_cols],1,sum)
      
      # Remove the seasonal dummies individual contribution
      agg_dataset <- agg_dataset[,!colnames(agg_dataset) %in% seasonal_dummies_contrib_cols]
    }
    
    # Calculate the baseline contribution (or intercept contribution as fitted - sum(contrib columns))
    new_contrib_cols <- colnames(agg_dataset)[grep('^contrib_',colnames(agg_dataset))]
    if(!intercept_contrib) {
      agg_dataset$contrib_baseline <- agg_dataset$fitted - apply(agg_dataset[,new_contrib_cols],1,sum)
      new_contrib_cols <- c(new_contrib_cols,'contrib_baseline')
    }
    
    # Calculate the percentage contribution 
    invisible(lapply(c(new_contrib_cols),function(col){
      pct_newname <- paste0('pct_',col)
      agg_dataset[,pct_newname] <<- agg_dataset[,col]/agg_dataset$fitted * 100
    }))
    
    agg_dataset
  },error=function(e) e[1])
}



## ------------------------------------------------------------------------------------------------------
## -- Contributions using Mean Value Theorem method -----------------------------------------------------
## ------------------------------------------------------------------------------------------------------

convertBackFactors_2 <- function(model_data,conversion_data){
  common_cols <- intersect(colnames(model_data), colnames(conversion_data))
  model_data_common <- as.data.frame(model_data[common_cols])
  col_class <- sapply(model_data_common,class)
  factor_cols <- names(col_class[which(col_class=='factor')])
  conversion_data[,factor_cols] <- lapply(factor_cols,function(x){
    factor(conversion_data[,x],levels=levels(model_data_common[,x]))
  })
  conversion_data
}

# Converts common columns into Factors 
convertBackFactors_1 <- function(model_data,conversion_data){
  #browser()
  cols <- intersect(colnames(model_data),colnames(conversion_data))
  
  col_class <- sapply(model_data[,cols,drop=FALSE],class)
  factor_cols <- names(col_class[which(col_class=='factor')])
  conversion_data[,factor_cols] <- lapply(factor_cols,function(x){
    factor(conversion_data[,x],levels=levels(model_data[,x]))
  })
  conversion_data
}

calculate_contributions <- function(model, dataset, fit_values = NULL, predictors, seasonal_dummies = NULL,
                                    group_by_cols = NULL, pred_colname=ifelse(class(model)=='brmsfit','Estimate','fit')
                                    , pred_struct=c('vector','other'), aggregation='sum',intercept_contrib=FALSE,ref_values=NULL){
  
  tryCatch({
    dataset_main <- dataset
    predictors <- c(predictors,if(!is.null(seasonal_dummies)) seasonal_dummies, if(intercept_contrib) 'intercept')
    
    # if(is.null(ref_values)){
    #   ref_values <- rep(0,length(predictors))
    #   names(ref_values) <- names(predictors)
    # }
    
    
    # Get contributions based on the derivative method (mean value method)
    contributions_df <- do.call(cbind,lapply(predictors,function(xvar){
      print(xvar)
      # Set the values of the test column to zero
      dataset1 <- data.frame(dataset)
      predictor_name <- testCol <- xvar 
      
      if(xvar!='intercept'){
        if(!testCol %in% colnames(dataset1)) stop(paste('Column not found : ', testCol))
        derivative <- dydx(dataset1,model,variable=predictor_name)
        prediction <- derivative * as.numeric(as.character(dataset1[,predictor_name]))
        prediction <- prediction[,1]
      } else {
        # For intercept make all others zeros
        dataset1[,predictors] <- rep(0,length(predictors))
        dataset1 <- convertBackFactors(dataset,dataset1)
        prediction <- predict(model,dataset1)
        
      }
      
      if(pred_struct[1]=='vector') dataset1$prediction <- as.numeric(prediction) # For numeric class
      else dataset1$prediction <- prediction[,pred_colname] # For matrix or data.frame
      
      newname <- paste0('contrib_',testCol)
      dataset1[,newname] <- prediction
      
      return(dataset1[,newname,drop=FALSE])
    }))
    
    # Add the contribution columns to the dataset
    dataset <- cbind(dataset,contributions_df)
    contrib_cols <- colnames(dataset)[grep('^contrib_',colnames(dataset))]
    
    if(is.null(fit_values)){
      pred <- predict(model,dataset_main)
      pClass <- class(pred)
      
      if(is.numeric(pred)) dataset$fitted <- as.numeric(pred) # For numeric class
      else dataset$fitted <- pred[,pred_colname] # For matrix or data.frame
      
    } else dataset$fitted <- fit_values
    
    
    # Rollup the dataset to overall level i.e. sum all the observation level contribution to get total contribution 
    if(!is.null(group_by_cols))
      agg_dataset1 <- dataset %>% select_at(vars(c(group_by_cols,contrib_cols,'fitted'))) %>% group_by_at(vars(group_by_cols)) %>% summarize_all(funs(do.call(aggregation,list(.))))
    else agg_dataset1 <- dataset %>% select_at(vars(c(contrib_cols,'fitted'))) %>% summarize_all(funs(do.call(aggregation,list(.))))
    
    # Add up the contributions of seasonal dummies
    if(!is.null(seasonal_dummies)){
      seasonal_dummies_contrib_cols <- paste0('contrib_',seasonal_dummies)
      agg_dataset1$contrib_seasonal <- apply(agg_dataset1[,seasonal_dummies_contrib_cols],1,sum)
      
      # Remove the seasonal dummies individual contribution
      agg_dataset1 <- agg_dataset1[,!colnames(agg_dataset1) %in% seasonal_dummies_contrib_cols]
    }
    
    # Calculate the baseline contribution (or intercept contribution as fitted - sum(contrib columns))
    new_contrib_cols <- colnames(agg_dataset1)[grep('^contrib_',colnames(agg_dataset1))]
    if(!intercept_contrib) {
      agg_dataset1$contrib_baseline <- agg_dataset1$fitted - apply(agg_dataset1[,new_contrib_cols],1,sum)
      new_contrib_cols <- c(new_contrib_cols,'contrib_baseline')
    }
    
    # Calculate the percentage contribution 
    invisible(lapply(c(new_contrib_cols),function(col){
      pct_newname <- paste0('pct_',col)
      agg_dataset1[,pct_newname] <<- agg_dataset1[,col]/agg_dataset1$fitted * 100
    }))
    
    return(agg_dataset1)
  },error=function(e) e[1])
}


## -------------------------------------------
## MVT contributions method with reference values(or min values) for each predictor considered as minimum of those predictors in the training data.
## Optionally, the user can provide custom reference values through the ref_values argument
## -------------------------------------------

calculate_contributions2 <- function(model
                                     , dataset
                                     , fit_values = NULL
                                     , predictors
                                     , seasonal_dummies = NULL
                                     , group_by_cols = NULL
                                     , pred_colname=ifelse(class(model)=='brmsfit','Estimate','fit')
                                     , pred_struct=c('vector','other')
                                     , aggregation='sum'
                                     , intercept_contrib=FALSE
                                     , ref_values=NULL # reference values or minimum values of predictors provided in the same order as predictors
                                     ){
  
  tryCatch({
    dataset_main <- dataset
    
    # Calculating reference/minimum values of predictors if not provided through ref_values argument
    if(is.null(ref_values)){
      min_values <- sapply(dataset_main[,predictors],min,na.rm=T)
      names(min_values) <- predictors
    } else {
      min_values <- ref_values
      names(min_values) <- predictors
    }
    
    predictors <- c(predictors,if(!is.null(seasonal_dummies)) seasonal_dummies, if(intercept_contrib) 'intercept') # Maintain the order of variables
    # Adding the min values for the seasonal_dummies included in the predictors. For dummies '0' level is considered minimum
    min_values <- c(min_values,rep(0,length(predictors)-length(min_values)))
    names(min_values) <- predictors
    
    
    # Get contributions based on the derivative method (mean value method)
    contributions_df <- do.call(cbind,lapply(predictors,function(xvar){
      print(xvar)
      # Set the values of the test column to zero
      dataset1 <- data.frame(dataset)
      predictor_name <- testCol <- xvar 
      
      if(xvar!='intercept'){
        if(!testCol %in% colnames(dataset1)) stop(paste('Column not found : ', testCol))
        derivative <- dydx(dataset1,model,variable=predictor_name)
        prediction <- derivative * (as.numeric(as.character(dataset1[,predictor_name])) - rep(min_values[predictor_name][1],nrow(dataset1)))
        prediction <- prediction[,1]
      } else {
        # For intercept make all others zeros
        dataset1[,predictors] <- rep(0,length(predictors))
        dataset1 <- convertBackFactors(dataset,dataset1)
        prediction <- predict(model,dataset1)
        
      }
      
      if(pred_struct[1]=='vector') dataset1$prediction <- as.numeric(prediction) # For numeric class
      else dataset1$prediction <- prediction[,pred_colname] # For matrix or data.frame
      
      newname <- paste0('contrib_',testCol)
      dataset1[,newname] <- prediction
      
      return(dataset1[,newname,drop=FALSE])
    }))
    
    # Add the contribution columns to the dataset
    dataset <- cbind(dataset,contributions_df)
    contrib_cols <- colnames(dataset)[grep('^contrib_',colnames(dataset))]
    
    if(is.null(fit_values)){
      pred <- predict(model,dataset_main)
      pClass <- class(pred)
      
      if(is.numeric(pred)) dataset$fitted <- as.numeric(pred) # For numeric class
      else dataset$fitted <- pred[,pred_colname] # For matrix or data.frame
      
    } else dataset$fitted <- fit_values
    
    
    # Rollup the dataset to overall level i.e. sum all the observation level contribution to get total contribution 
    if(!is.null(group_by_cols))
      agg_dataset1 <- dataset %>% select_at(vars(c(group_by_cols,contrib_cols,'fitted'))) %>% group_by_at(vars(group_by_cols)) %>% summarize_all(funs(do.call(aggregation,list(.))))
    else agg_dataset1 <- dataset %>% select_at(vars(c(contrib_cols,'fitted'))) %>% summarize_all(funs(do.call(aggregation,list(.))))
    
    # Add up the contributions of seasonal dummies
    if(!is.null(seasonal_dummies)){
      seasonal_dummies_contrib_cols <- paste0('contrib_',seasonal_dummies)
      agg_dataset1$contrib_seasonal <- apply(agg_dataset1[,seasonal_dummies_contrib_cols],1,sum)
      
      # Remove the seasonal dummies individual contribution
      agg_dataset1 <- agg_dataset1[,!colnames(agg_dataset1) %in% seasonal_dummies_contrib_cols]
    }
    
    # Calculate the baseline contribution (or intercept contribution as fitted - sum(contrib columns))
    new_contrib_cols <- colnames(agg_dataset1)[grep('^contrib_',colnames(agg_dataset1))]
    if(!intercept_contrib) {
      agg_dataset1$contrib_baseline <- agg_dataset1$fitted - apply(agg_dataset1[,new_contrib_cols],1,sum)
      new_contrib_cols <- c(new_contrib_cols,'contrib_baseline')
    }
    
    # Calculate the percentage contribution 
    invisible(lapply(c(new_contrib_cols),function(col){
      pct_newname <- paste0('pct_',col)
      agg_dataset1[,pct_newname] <<- agg_dataset1[,col]/agg_dataset1$fitted * 100
    }))
    
    return(agg_dataset1)
  },error=function(e) e[1])
}

## ----------------------------------------------------------------------------------
## Contribution calculations using minimum value of predictors as baseline values
## ----------------------------------------------------------------------------------

# Contribution function - not tidyverse complete
calculate_contributions4_0 <- function(model
                                     , dataset
                                     , spend_variables
                                     , fit_values = NULL
                                     , group_by_cols = NULL
                                     , pred_colname = ifelse(class(model)=='brmsfit','Estimate','fit')
                                     , pred_struct = c('vector','other')
                                     , aggregation = 'sum'
                                     , intercept_contrib = FALSE
                                     , ref_values = NULL
){
  
  tryCatch({
    
    # Get fitted values
    if(is.null(fit_values)){
      pred <- predict(model,dataset)
      
      if(pred_struct[1]=='vector') dataset$fitted <- as.numeric(pred) # For numeric class
      else dataset$fitted <- pred[,pred_colname] # For matrix or data.frame
    } else dataset$fitted <- fit_values
    
    # Setting minimum values for predictors (i.e. only for spend variables)
    if(!is.null(ref_values)){
      min_values <- ref_values
      names(min_values) <- spend_variables
    } else {
      min_values <- sapply(dataset[,spend_variables],min,na.rm=T)
      names(min_values) <- spend_variables
    }
    
    
    # Get prediction setting the spend variables to minimum
    dataset_min <- dataset
    dataset_min[,spend_variables] <- lapply(min_values[spend_variables],function(x) rep(x,nrow(dataset_min)))
    all_min_pred <- predict(model,dataset_min)
    if(pred_struct[1]=='vector') dataset$all_min_pred <- as.numeric(all_min_pred) # For numeric class
    else dataset$all_min_pred <- all_min_pred[,pred_colname] # For matrix or data.frame
    
    # Get contributions based on the derivative method (mean value method)
    contributions_df <- do.call(cbind, lapply(spend_variables,function(xvar){
      
      # Set the values of the test column to zero
      dataset1 <- data.frame(dataset)
      predictor_name <- testCol <- xvar 
      
      # Set all other columns apart from xvar to min value
      other_spend_vars <- setdiff(spend_variables,xvar)
      dataset1[,other_spend_vars] <- lapply(min_values[other_spend_vars],function(x) rep(x,nrow(dataset1)))
      
      other_min_pred <- predict(model,dataset1)
      if(pred_struct[1]=='vector') dataset1$other_min_pred <- as.numeric(other_min_pred) # For numeric class
      else dataset1$other_min_pred <- other_min_pred[,pred_colname] # For matrix or data.frame
      
      # Calculate contributions
      contrib_colname <- paste0('contrib_',xvar)
      dataset1 <- dataset1 %>% mutate(!!contrib_colname:= (other_min_pred - all_min_pred))
      
      return(dataset1[,contrib_colname,drop=FALSE])
    }))
    
    
    # Add the contribution columns to the dataset
    dataset <- cbind(dataset,contributions_df)
    dataset$contrib_baseline <- dataset$all_min_pred
    contrib_cols <- colnames(dataset)[grep('^contrib_',colnames(dataset))]
    
    
    # Rollup the dataset to overall level i.e. sum all the observation level contribution to get total contribution 
    if(!is.null(group_by_cols))
      agg_dataset1 <- dataset %>% select_at(vars(c(group_by_cols,contrib_cols,'fitted'))) %>% group_by_at(vars(group_by_cols)) %>% summarize_all(funs(do.call(aggregation,list(.))))
    else agg_dataset1 <- dataset %>% select_at(vars(c(contrib_cols,'fitted'))) %>% summarize_all(funs(do.call(aggregation,list(.))))
    
    
    # Calculate the baseline contribution (or intercept contribution as fitted - sum(contrib columns))
    new_contrib_cols <- colnames(agg_dataset1)[grep('^contrib_',colnames(agg_dataset1))]
    
    # Calculate the percentage contribution 
    agg_dataset1$total <- rowSums(agg_dataset1[,new_contrib_cols])
    invisible(lapply(c(new_contrib_cols),function(col){
      pct_newname <- paste0('pct_',col)
      agg_dataset1[,pct_newname] <<- agg_dataset1[,col]/agg_dataset1$total * 100
    }))
    
    agg_dataset1
  },error=function(e) return(e))
}



## ----------------------------------------------------------------------------------
## Contribution calculations using minimum value of predictors as baseline values
## Hadleyverse code
## ----------------------------------------------------------------------------------

calculate_contributions4 <- function(model
                                     , dataset
                                     , spend_variables
                                     , fit_values = NULL
                                     # , seasonal_dummies = NULL
                                     , group_by_cols = NULL
                                     , pred_colname = ifelse(class(model)=='brmsfit','Estimate','fit')
                                     , pred_struct = c('vector','other')
                                     , aggregation = 'sum'
                                     , intercept_contrib = FALSE
                                     , ref_values = NULL
){
  
  tryCatch({
    # browser()
    # Get fitted values
    if(is.null(fit_values)){
      pred <- predict(model,dataset)
      
      if(pred_struct[1]=='vector') dataset$fitted <- as.numeric(pred) # For numeric class
      else dataset$fitted <- pred[,pred_colname] # For matrix or data.frame
    } else dataset$fitted <- fit_values
    
    # Setting minimum values for predictors (i.e. only for spend variables)
    if(!is.null(ref_values)){
      min_values <- ref_values
      names(min_values) <- spend_variables
    } else {
      min_values <- sapply(dataset[,spend_variables,drop=FALSE],min,na.rm=T)
      names(min_values) <- spend_variables
    }
    
    
    # Get prediction setting the spend variables to minimum
    dataset_min <- dataset
    dataset_min[,spend_variables] <- min_values[spend_variables] %>% map(~rep(.,nrow(dataset_min)))
    
    all_min_pred <- predict(model,dataset_min)
    if(pred_struct[1]=='vector') dataset$all_min_pred <- as.numeric(all_min_pred) # For numeric class
    else dataset$all_min_pred <- all_min_pred[,pred_colname] # For matrix or data.frame
    
    # Get contributions based on the derivative method (mean value method)
    # contributions_df <- do.call(cbind, lapply(spend_variables,
    
    get_column_contrib <- function(xvar){
      
      # Set the values of the test column to zero
      dataset1 <- data.frame(dataset)
      predictor_name <- testCol <- xvar 
      
      # Set all other columns apart from xvar to min value
      other_spend_vars <- setdiff(spend_variables,xvar)
      dataset1[,other_spend_vars] <-  min_values[other_spend_vars] %>% map(~rep(.,nrow(dataset1)))
        
      other_min_pred <- predict(model,dataset1)
      if(pred_struct[1]=='vector') dataset1$other_min_pred <- as.numeric(other_min_pred) # For numeric class
      else dataset1$other_min_pred <- other_min_pred[,pred_colname] # For matrix or data.frame
      
      # Calculate contributions
      contrib_colname <- paste0('contrib_',xvar)
      dataset1 <- dataset1 %>% mutate(!!contrib_colname:= (other_min_pred - all_min_pred))
      
      return(dataset1[,contrib_colname,drop=FALSE])
    }
    # ))
    
    
    contributions_df <- do.call(cbind,spend_variables %>% map(~get_column_contrib(.)))
    
    
    # Add the contribution columns to the dataset
    dataset <- cbind(dataset,contributions_df)
    dataset$contrib_baseline <- dataset$all_min_pred
    contrib_cols <- colnames(dataset)[grep('^contrib_',colnames(dataset))]
    
    
    # Rollup the dataset to overall level i.e. sum all the observation level contribution to get total contribution 
    if(!is.null(group_by_cols))
      agg_dataset1 <- dataset %>% select_at(vars(c(group_by_cols,contrib_cols,'fitted'))) %>% group_by_at(vars(group_by_cols)) %>% summarize_all(funs(do.call(aggregation,list(.))))
    else agg_dataset1 <- dataset %>% select_at(vars(c(contrib_cols,'fitted'))) %>% summarize_all(funs(do.call(aggregation,list(.))))
    
    new_contrib_cols <- colnames(agg_dataset1)[grep('^contrib_',colnames(agg_dataset1))]
    
    # Calculate the percentage contribution 
    agg_dataset1$total <- rowSums(agg_dataset1[,new_contrib_cols,drop=FALSE])
    pct_colnames <- paste0('pct_',new_contrib_cols)
    agg_dataset1[,pct_colnames] <- agg_dataset1[,new_contrib_cols,drop=FALSE] %>% map(~./agg_dataset1$total * 100)
    
    return(agg_dataset1)
  },error=function(e) return(e))
}



## ----------------------------------------------------------------------------------
## Contribution calculations using minimum value of predictors as baseline values
## By-panel min-max baseline calculation if panel columns are provided
## ----------------------------------------------------------------------------------

calculate_contributions5 <- function(model
                                     , dataset
                                     , spend_variables
                                     , fit_values = NULL
                                     , panel_cols = NULL
                                     , by_cols = NULL
                                     , pred_colname = ifelse(class(model)=='brmsfit','Estimate','fit')
                                     , pred_struct = c('vector','other')
                                     , aggregation = 'sum'
                                     , intercept_contrib = FALSE
                                     , ref_values = NULL # Dataframe with spend variables as columns and panels as rows, values being the minimum values
){
  #browser()
  tryCatch({
    
    group_by_cols <- panel_cols
    
    # Get fitted values
    if(is.null(fit_values)){
      pred <- predict(model,dataset)
      
      if(pred_struct[1]=='vector') dataset$fitted <- as.numeric(pred) # For numeric class
      else dataset$fitted <- pred[,pred_colname] # For matrix or data.frame
    } else dataset$fitted <- fit_values
    
    # Setting minimum values for predictors (i.e. only for spend variables)
    if(!is.null(ref_values)){
      min_values <- ref_values
      names(min_values) <- spend_variables
    } else { # If panel are provided, calculate minimum by panel else use the whole dataset for minimum calculation
      if(!is.null(panel_cols)) min_values <- dataset %>% split.data.frame(.[panel_cols],sep='__') %>% map(~sapply(.[,spend_variables,drop=FALSE],min,na.rm=T)) %>% 
        do.call(rbind,.) %>% data.frame() %>% mutate(Panel = row.names(.)) %>% separate('Panel',panel_cols,sep='__')
      else min_values <- sapply(dataset[,spend_variables,drop=FALSE],min,na.rm=T) %>% t() %>% data.frame()
    }
    
    
    # Get prediction setting the spend variables to minimum
    dataset_min <- dataset
    dataset_min <- dataset_min %>% dplyr::select(-one_of(spend_variables)) %>% merge(min_values,by=panel_cols)
    
    all_min_pred <- predict(model,dataset_min)
    if(pred_struct[1]=='vector') dataset$all_min_pred <- as.numeric(all_min_pred) # For numeric class
    else dataset$all_min_pred <- all_min_pred[,pred_colname] # For matrix or data.frame
    
    
    get_column_contrib <- function(xvar){
      
      # Set the values of the test column to zero
      dataset1 <- data.frame(dataset)
      predictor_name <- testCol <- xvar 
      
      # Set all other columns apart from xvar to min value
      other_spend_vars <- setdiff(spend_variables,xvar)
      # dataset1[,other_spend_vars] <-  min_values[other_spend_vars] %>% map(~rep(.,nrow(dataset1)))
      # if(!is.null(panel_cols)) other_spend_vars <- c(panel_cols,other_spend_vars)
      dataset1 <- dataset1 %>% dplyr::select(-one_of(other_spend_vars)) %>% merge(min_values[,c(other_spend_vars,if(!is.null(panel_cols)) panel_cols)],by=panel_cols)
      
      other_min_pred <- predict(model,dataset1)
      if(pred_struct[1]=='vector') dataset1$other_min_pred <- as.numeric(other_min_pred) # For numeric class
      else dataset1$other_min_pred <- other_min_pred[,pred_colname] # For matrix or data.frame
      
      # Calculate contributions
      contrib_colname <- paste0('contrib_',xvar)
      # dataset1 <- dataset1 %>% mutate(!!contrib_colname:= (other_min_pred - all_min_pred))
      dataset1[,contrib_colname] <- other_min_pred - all_min_pred
      
      return(dataset1[,contrib_colname,drop=FALSE])
    }
    
    
    contributions_df <- do.call(cbind,spend_variables %>% map(~get_column_contrib(.)))
    
    
    # Add the contribution columns to the dataset
    dataset <- cbind(dataset,contributions_df)
    dataset$contrib_baseline <- dataset$all_min_pred
    contrib_cols <- colnames(dataset)[grep('^contrib_',colnames(dataset))]
    
    
    # Rollup the dataset to overall level i.e. sum all the observation level contribution to get total contribution 
    if(!is.null(by_cols))
      agg_dataset1 <- dataset %>% dplyr::select_at(vars(c(by_cols,contrib_cols,'fitted'))) %>% group_by_at(vars(by_cols)) %>% summarize_all(funs(do.call(aggregation,list(.))))
    else agg_dataset1 <- dataset %>% dplyr::select_at(vars(c(contrib_cols,'fitted'))) %>% summarize_all(funs(do.call(aggregation,list(.))))
    
    new_contrib_cols <- colnames(agg_dataset1)[grep('^contrib_',colnames(agg_dataset1))]
    
    # Calculate the percentage contribution 
    agg_dataset1$total <- rowSums(agg_dataset1[,new_contrib_cols,drop=FALSE])
    pct_colnames <- paste0('pct_',new_contrib_cols)
    agg_dataset1[,pct_colnames] <- agg_dataset1[,new_contrib_cols,drop=FALSE] %>% map(~./agg_dataset1$total * 100)
    
    return(agg_dataset1)
  },error=function(e) return(e))
}


## ---------------------------------------------------------------------------------------------------------------
## ------ Marginal effects plot code ----------------------------------------------------------------------------
## ---------------------------------------------------------------------------------------------------------------


get_marginal_plots2 <- function(model
                                ,transformations=list(dep=c('none','log'), indep=c('none','log'))
                                , dataset
                                , predictors
                                , seasonal_dummies = NULL
                                , level_df
                                , level1_columns
                                , plot_data_size =100
                                , pred_colname='fit'
                                , lwr_colname='lwr'
                                , upr_colname='upr'
                                , se_colname=NULL
                                , predict_params=NULL
                                , pred_struct=c('vector','other')){
  
  tryCatch({
    dataset2 <- dataset
    
    # Transform the variables based on the model type
    if(transformations$indep=='log'){
      dataset2[,predictors] <- lapply(dataset2[,predictors,drop=FALSE],function(x){
        return(log(x+1))
      })
      # dataset2[,predictors] <- dataset2[,predictors,drop=FALSE] %>% map(~log(.+1))
    }
    
    # Get the marginal plots for every predictor
    marginal_plots <- lapply(predictors,function(predictor_name){
      dataset1 <- data.frame(dataset2)
      testCol <- predictor_name
      print(paste0('Creating plot for column : ',testCol))
      
      # Calculate mean values for all other columns apart form testCol
      meancols <- setdiff(predictors,testCol)
      meancols_means <- apply(dataset1[,meancols],2,mean)
      min_value <- min(dataset1[,testCol],na.rm=T)
      max_value <- max(dataset1[,testCol],na.rm=T)
      
      # Set all other numeric predictor values to mean of the predictor
      invisible(lapply(meancols,function(x){
        dataset1[,x] <<- meancols_means[x]
      }))
      
      # Set all the seasonal dummy values to 0
      if(!is.null(seasonal_dummies)){
        invisible(lapply(seasonal_dummies,function(x){
          dataset1[,x] <<- 0
        }))
      }
      
      # dataset1 <-convertBackFactors(dataset,dataset1) # Convert columns to factors with levels similar to the model dataset
      
      # Create a sequence of values for the test column from min_value to max_value broken into 100 parts
      # dataset1 <- dataset1[1:plot_data_size,]
      dataset1 <- dataset1[1,] # Select only 1 row and then replicate it
      dataset1 <- dataset1[rep(row.names(dataset1),plot_data_size),] # Replicate the rows by freq provided in plot_data_size variable
      if(abs(min_value)==Inf) min_value <- 0
      dataset1[,testCol] <- seq(min_value,max_value,along.with=c(1:plot_data_size))
      
      if(!is.null(level_df)){
        # Expand the dataset for each level
        dataset0 <- merge(level_df,dataset1[,!colnames(dataset1) %in% colnames(level_df)],by=NULL) # cross join to get the
      } else dataset0 <- dataset1
      
      # # Convert level columns to factors
      # invisible(lapply(colnames(level_df1),function(x) 
      #   dataset0[,x] <- as.factor(dataset0[,x])))
      # 
      
      # ggplot aes concatenation function
      `+.uneval` <- function(a,b) {
        `class<-`(modifyList(a,b), "uneval")
      }
      
      predictions <-  do.call(predict,c(list(model,dataset0),predict_params))
      pClass <- class(predictions)
      
      # Tranforming variables back
      if(transformations$dep %in% c('log')){
        predictions <- exp(predictions)
        if(transformations$indep=='log') dataset0[,testCol] <- exp(dataset0[,testCol])
      } 
      
      if(pred_struct[1]=='vector'){
        dataset0$predicted <- as.numeric(predictions)
        intervals_flag <- FALSE
        
      } else {
        predictions <- data.frame(predictions)
        # Check interval names
        intervals_flag <- ifelse(is.null(lwr_colname) | is.null(upr_colname),FALSE,TRUE)
        
        
        if(is.null(se_colname)){
          predictions <- predictions[,c(pred_colname,if(intervals_flag) c(lwr_colname,upr_colname)),drop=FALSE]
          colnames(predictions) <- c('predicted',if(intervals_flag) c('lower','upper'))
        } else {
          if(intervals_flag) predictions$lwr <- predictions[,pred_colname] - (2 * predictions[,se_colname])
          if(intervals_flag) predictions$upr <- predictions[,pred_colname] + (2 * predictions[,se_colname])
          predictions <- predictions[,c(pred_colname,if(intervals_flag) c('lwr','upr')),drop=FALSE]
          colnames(predictions) <- c('predicted',if(intervals_flag) c('lower','upper'))
        }
        dataset0 <- cbind(dataset0,predictions)
      }
      
      if(!is.null(level_df)){
        dataset0$Level <- apply(dataset0[,level1_columns,drop=FALSE],1,function(x) paste(x,collapse='_'))
        # Plot the marginal effects of the testcol
        plot_data <- dataset0[,c('Level',testCol,'predicted',if(intervals_flag) c('lower','upper')),drop=FALSE]
        
        g <- ggplot(plot_data,aes(fill=Level)) + {if(intervals_flag) geom_ribbon(aes_string(x=testCol,ymin='lower',ymax='upper'),alpha=0.2)} + geom_line(aes_string(x=testCol,y='predicted') + aes(color=Level))  + labs(title = paste0('Marginal effects of ',testCol))
      } else {
        # Plot the marginal effects of the testcol
        plot_data <- dataset0[,c(testCol,'predicted',if(intervals_flag) c('lower','upper')),drop=FALSE]
        # print(head(plot_data))
        g <- ggplot(plot_data) + { if(intervals_flag) geom_ribbon(aes_string(x=testCol,ymin='lower',ymax='upper'),alpha=0.2)} + geom_line(aes_string(x=testCol,y='predicted'),color='blue')  + labs(title = paste0('Marginal effects of ',testCol))
      }
      
      # }
      
      
      return(g)
    })
    
    return(marginal_plots)
  },error=function(e) e[1])
}



# ------------------------------------------------------------------------------------------------------------------------------------
# Constant values for other variables considered to be at their min instead of mean (in get_marginal_plots2 function)
# ------------------------------------------------------------------------------------------------------------------------------------

get_marginal_plots3 <- function(model
                                , transformations=list(dep=c('none','log'), indep=c('none','log'))
                                , dataset
                                , predictors
                                , ref_agg = 'min'
                                , seasonal_dummies = NULL
                                , level_df = NULL
                                , level1_columns = NULL
                                , plot_data_size =100
                                , pred_colname='fit'
                                , lwr_colname='lwr'
                                , upr_colname='upr'
                                , se_colname=NULL
                                , predict_params=NULL
                                , pred_struct=c('vector','other')){
  
  tryCatch({
    dataset2 <- dataset
    
    # Transform the variables based on the model type
    if(transformations$indep[1]=='log'){
      # dataset2[,predictors] <- lapply(dataset2[,predictors,drop=FALSE],function(x){
      #   return(log(x+1))
      # })
      dataset2[,predictors] <- dataset2[,predictors,drop=FALSE] %>% map(~log(.+1))
    }
    
    # Get the marginal plots for every predictor
    marginal_plots <- lapply(predictors,function(predictor_name){
      dataset1 <- data.frame(dataset2)
      testCol <- predictor_name
      print(paste0('Creating plot for column : ',testCol))
      
      # Calculate mean values for all other columns apart form testCol
      meancols <- setdiff(predictors,testCol)
      meancols_means <- apply(dataset1[,meancols,drop=FALSE],2,ref_agg)
      min_value <- min(dataset1[,testCol],na.rm=T)
      max_value <- max(dataset1[,testCol],na.rm=T)
      
      # Set all other numeric predictor values to mean of the predictor
      invisible(lapply(meancols,function(x){
        dataset1[,x] <<- meancols_means[x]
      }))
      
      # Set all the seasonal dummy values to 0
      if(!is.null(seasonal_dummies)){
        invisible(lapply(seasonal_dummies,function(x){
          dataset1[,x] <<- 0
        }))
      }
      
      # dataset1 <-convertBackFactors(dataset,dataset1) # Convert columns to factors with levels similar to the model dataset
      
      # Create a sequence of values for the test column from min_value to max_value broken into 100 parts
      # dataset1 <- dataset1[1:plot_data_size,]
      dataset1 <- dataset1[1,] # Select only 1 row and then replicate it
      dataset1 <- dataset1[rep(row.names(dataset1),plot_data_size),] # Replicate the rows by freq provided in plot_data_size variable
      if(abs(min_value)==Inf) min_value <- 0
      dataset1[,testCol] <- seq(min_value,max_value,along.with=c(1:plot_data_size))
      
      if(!is.null(level_df)){
        # Expand the dataset for each level
        dataset0 <- merge(level_df,dataset1[,!colnames(dataset1) %in% colnames(level_df),drop=FALSE],by=NULL) # cross join to get the
      } else dataset0 <- dataset1
      
      # # Convert level columns to factors
      # invisible(lapply(colnames(level_df1),function(x) 
      #   dataset0[,x] <- as.factor(dataset0[,x])))
      # 
      
      # ggplot aes concatenation function
      `+.uneval` <- function(a,b) {
        `class<-`(modifyList(a,b), "uneval")
      }
      
      predictions <-  do.call(predict,c(list(model,dataset0),predict_params))
      pClass <- class(predictions)
      
      # Tranforming variables back
      if(transformations$dep[1] %in% c('log')){
        predictions <- exp(predictions)
        if(transformations$indep[1]=='log') dataset0[,testCol] <- exp(dataset0[,testCol,drop=FALSE])
      } 
      
      if(pred_struct[1]=='vector'){
        dataset0$predicted <- as.numeric(predictions)
        intervals_flag <- FALSE
        
      } else {
        predictions <- data.frame(predictions)
        # Check interval names
        intervals_flag <- ifelse(is.null(lwr_colname) | is.null(upr_colname),FALSE,TRUE)
        
        
        if(is.null(se_colname)){
          predictions <- predictions[,c(pred_colname,if(intervals_flag) c(lwr_colname,upr_colname)),drop=FALSE]
          colnames(predictions) <- c('predicted',if(intervals_flag) c('lower','upper'))
        } else {
          if(intervals_flag) predictions$lwr <- predictions[,pred_colname] - (2 * predictions[,se_colname])
          if(intervals_flag) predictions$upr <- predictions[,pred_colname] + (2 * predictions[,se_colname])
          predictions <- predictions[,c(pred_colname,if(intervals_flag) c('lwr','upr')),drop=FALSE]
          colnames(predictions) <- c('predicted',if(intervals_flag) c('lower','upper'))
        }
        dataset0 <- cbind(dataset0,predictions)
      }
      
      if(!is.null(level_df)){
        dataset0$Level <- apply(dataset0[,level1_columns,drop=FALSE],1,function(x) paste(x,collapse='_'))
        # Plot the marginal effects of the testcol
        plot_data <- dataset0[,c('Level',testCol,'predicted',if(intervals_flag) c('lower','upper')),drop=FALSE]
        
        g <- ggplot(plot_data,aes(fill=Level)) + {if(intervals_flag) geom_ribbon(aes_string(x=testCol,ymin='lower',ymax='upper'),alpha=0.2)} + geom_line(aes_string(x=testCol,y='predicted') + aes(color=Level))  + labs(title = paste0('Marginal effects of ',testCol))
      } else {
        # Plot the marginal effects of the testcol
        plot_data <- dataset0[,c(testCol,'predicted',if(intervals_flag) c('lower','upper')),drop=FALSE]
        # print(head(plot_data))
        g <- ggplot(plot_data) + { if(intervals_flag) geom_ribbon(aes_string(x=testCol,ymin='lower',ymax='upper'),alpha=0.2)} + geom_line(aes_string(x=testCol,y='predicted'),color='blue')  + labs(title = paste0('Marginal effects of ',testCol))
      }
      
      # }
      
      
      return(g)
    })
    
    return(marginal_plots)
  },error=function(e) e[1])
}


## Redefining grouping factors ----------------------------------------------------
library(purrr)
library(tidyr)

get_marginal_plots4 <- function(model
                                , transformations=list(dep=c('none','log'), indep=c('none','log'))
                                , dataset
                                , predictors
                                , ref_agg = 'min'
                                , seasonal_dummies = NULL
                                , level_df = NULL
                                , plot_data_size = 100
                                , pred_colname='fit'
                                , lwr_colname='lwr'
                                , upr_colname='upr'
                                , se_colname=NULL
                                , predict_params=NULL
                                , pred_struct=c('vector','other')){
  
  tryCatch({
    dataset2 <- dataset
    
    # Transform the variables based on the model type
    if(transformations$indep[1]=='log'){
      dataset2[,predictors] <- dataset2[,predictors] %>% map(~log(.+1))
    }
    
    # Get the marginal plots for every predictor
    get_column_marginals <- function(predictor_name){
      dataset1 <- data.frame(dataset2)
      testCol <- predictor_name
      print(paste0('Creating plot for column : ',testCol))
      
      # Calculate mean values for all other columns apart form testCol
      meancols <- setdiff(predictors,testCol)
      meancols_means <- apply(dataset1[,meancols],2,ref_agg)
      min_value <- min(dataset1[,testCol],na.rm=T)
      max_value <- max(dataset1[,testCol],na.rm=T)
      
      dataset1[,meancols] <- meancols %>% map(~meancols_means[.])
      
      # Set all the seasonal dummy values to 0
      if(!is.null(seasonal_dummies)){
        dataset1[,seasonal_dummies] <- 0
      }
      
      # Create a sequence of values for the test column from min_value to max_value broken into 100 parts
      dataset1 <- dataset1[1,] # Select only 1 row and then replicate it
      dataset1 <- dataset1[rep(row.names(dataset1),plot_data_size),] # Replicate the rows by freq provided in plot_data_size variable
      if(abs(min_value)==Inf) min_value <- 0
      dataset1[,testCol] <- seq(min_value,max_value,along.with=c(1:plot_data_size))
      
      # Replicate values for all levels in level_df
      if(!is.null(level_df)){
        if(is.vector(level_df)){
          level_df <- unique(dataset2[,level_df,drop=FALSE])
          level_names <- level_df
        } else {
          level_names <- colnames(level_df)
        }
        # Expand the dataset for each level
        dataset0 <- merge(level_df,dataset1[,!colnames(dataset1) %in% colnames(level_df)],by=NULL) # cross join to get the
      } else dataset0 <- dataset1
      
      
      # ggplot aes concatenation function
      `+.uneval` <- function(a,b) {
        `class<-`(modifyList(a,b), "uneval")
      }
      
      # Get predictions
      predictions <-  do.call(predict,c(list(model,dataset0),predict_params))
      pClass <- class(predictions)
      
      # Tranforming variables back
      if(transformations$dep[1] %in% c('log')){
        predictions <- exp(predictions)
        if(transformations$indep[1]=='log') dataset0[,testCol] <- exp(dataset0[,testCol])
      } 
      
      # Create prediction intervals if provided or calculated
      if(pred_struct[1]=='vector'){
        dataset0$predicted <- as.numeric(predictions)
        intervals_flag <- FALSE
      } else {
        predictions <- data.frame(predictions)
        # Check interval names
        intervals_flag <- ifelse(is.null(lwr_colname) | is.null(upr_colname),FALSE,TRUE)
        
        if(is.null(se_colname)){
          predictions <- predictions[,c(pred_colname,if(intervals_flag) c(lwr_colname,upr_colname)),drop=FALSE]
          colnames(predictions) <- c('predicted',if(intervals_flag) c('lower','upper'))
        } else {
          if(intervals_flag) predictions$lwr <- predictions[,pred_colname] - (2 * predictions[,se_colname])
          if(intervals_flag) predictions$upr <- predictions[,pred_colname] + (2 * predictions[,se_colname])
          predictions <- predictions[,c(pred_colname,if(intervals_flag) c('lwr','upr')),drop=FALSE]
          colnames(predictions) <- c('predicted',if(intervals_flag) c('lower','upper'))
        }
        dataset0 <- cbind(dataset0,predictions)
      }
      
      
      # Plot the marginal curves
      if(!is.null(level_df)){
        dataset0 <- dataset0 %>% unite(Level, level_names, sep='_')
        
        # Plot the marginal effects of the testcol
        plot_data <- dataset0[,c('Level',testCol,'predicted',if(intervals_flag) c('lower','upper')),drop=FALSE]
        
        g <- ggplot(plot_data,aes(fill=Level)) + {if(intervals_flag) geom_ribbon(aes_string(x=testCol,ymin='lower',ymax='upper'),alpha=0.2)} + geom_line(aes_string(x=testCol,y='predicted') + aes(color=Level))  + labs(title = paste0('Marginal effects of ',testCol))
      } else {
        # Plot the marginal effects of the testcol
        plot_data <- dataset0[,c(testCol,'predicted',if(intervals_flag) c('lower','upper')),drop=FALSE]
        g <- ggplot(plot_data) + { if(intervals_flag) geom_ribbon(aes_string(x=testCol,ymin='lower',ymax='upper'),alpha=0.2)} + geom_line(aes_string(x=testCol,y='predicted'),color='blue')  + labs(title = paste0('Marginal effects of ',testCol))
      }
      
      return(g)
    }

    
    marginal_plots <- predictors %>% map(~get_column_marginals(.))
    
    return(marginal_plots)
  },error=function(e) e[1])
}


## -----------------------------------------------------------------------------------------
## Marginal effect for plots for each channel with curves for all panels
## -----------------------------------------------------------------------------------------

get_marginal_plots5 <- function(model
                                , transformations=list(dep=c('none','log'), indep=c('none','log'))
                                , dataset
                                , predictors
                                , ref_agg = 'min'
                                , seasonal_dummies = NULL
                                , level_df = NULL
                                , plot_data_size = 100
                                , pred_colname='fit'
                                , lwr_colname='lwr'
                                , upr_colname='upr'
                                , se_colname=NULL
                                , predict_params=NULL
                                , pred_struct=c('vector','other')){
  
  tryCatch({
    dataset2 <- dataset
    
    # Transform the variables based on the model type
    if(transformations$indep[1]=='log'){
      dataset2[,predictors] <- dataset2[,predictors] %>% map(~log(.+1))
    }
    
    # Get the marginal plots for every predictor
    get_column_marginals <- function(predictor_name){
      dataset1 <- data.frame(dataset2)
      testCol <- predictor_name
      print(paste0('Creating plot for column : ',testCol))
      
      # Calculate mean values for all other columns apart form testCol
      meancols <- setdiff(predictors,testCol)
      meancols_means <- apply(dataset1[,meancols],2,ref_agg)
      min_value <- min(dataset1[,testCol],na.rm=T)
      max_value <- max(dataset1[,testCol],na.rm=T)
      
      dataset1[,meancols] <- meancols %>% map(~meancols_means[.])
      
      # Set all the seasonal dummy values to 0
      if(!is.null(seasonal_dummies)){
        dataset1[,seasonal_dummies] <- 0
      }
      
      # Create a sequence of values for the test column from min_value to max_value broken into 100 parts
      dataset1 <- dataset1[1,] # Select only 1 row and then replicate it
      dataset1 <- dataset1[rep(row.names(dataset1),plot_data_size),] # Replicate the rows by freq provided in plot_data_size variable
      if(abs(min_value)==Inf) min_value <- 0
      dataset1[,testCol] <- seq(min_value,max_value,along.with=c(1:plot_data_size))
      
      # Replicate values for all levels in level_df
      if(!is.null(level_df)){
        if(is.vector(level_df)){
          level_names <- level_df
          level_df <- unique(dataset2[,level_df,drop=FALSE])
        } else {
          level_names <- colnames(level_df)
        }
        # Expand the dataset for each level
        dataset0 <- merge(level_df,dataset1[,!colnames(dataset1) %in% colnames(level_df)],by=NULL) # cross join to get the
      } else dataset0 <- dataset1
      
      
      # ggplot aes concatenation function
      `+.uneval` <- function(a,b) {
        `class<-`(modifyList(a,b), "uneval")
      }
      
      # Get predictions
      predictions <-  do.call(predict,c(list(model,dataset0),predict_params))
      pClass <- class(predictions)
      
      # Tranforming variables back
      if(transformations$dep[1] %in% c('log')){
        predictions <- exp(predictions)
        if(transformations$indep[1]=='log') dataset0[,testCol] <- exp(dataset0[,testCol])
      } 
      
      # Create prediction intervals if provided or calculated
      if(pred_struct[1]=='vector'){
        dataset0$predicted <- as.numeric(predictions)
        intervals_flag <- FALSE
      } else {
        predictions <- data.frame(predictions)
        # Check interval names
        intervals_flag <- ifelse(is.null(lwr_colname) | is.null(upr_colname),FALSE,TRUE)
        
        if(is.null(se_colname)){
          predictions <- predictions[,c(pred_colname,if(intervals_flag) c(lwr_colname,upr_colname)),drop=FALSE]
          colnames(predictions) <- c('predicted',if(intervals_flag) c('lower','upper'))
        } else {
          if(intervals_flag) predictions$lwr <- predictions[,pred_colname] - (2 * predictions[,se_colname])
          if(intervals_flag) predictions$upr <- predictions[,pred_colname] + (2 * predictions[,se_colname])
          predictions <- predictions[,c(pred_colname,if(intervals_flag) c('lwr','upr')),drop=FALSE]
          colnames(predictions) <- c('predicted',if(intervals_flag) c('lower','upper'))
        }
        dataset0 <- cbind(dataset0,predictions)
      }
      
      
      # Plot the marginal curves
      if(!is.null(level_df)){
        dataset0 <- dataset0 %>% unite(Level, level_names, sep='_')
        
        # Plot the marginal effects of the testcol
        plot_data <- dataset0[,c('Level',testCol,'predicted',if(intervals_flag) c('lower','upper')),drop=FALSE]
        plot_data <- plot_data %>% group_by_at(vars(testCol)) %>% {if(intervals_flag) dplyr::summarize(.,lower=min(lower,na.rm=T),upper=max(upper,na.rm=T),predicted=mean(predicted,na.rm=TRUE)) else  dplyr::summarize(.,predicted = mean(predicted, na.rm=TRUE))} %>% mutate(Level='Mean') %>% rbind(plot_data[,c('Level','predicted',testCol,if(intervals_flag) c('lower','upper'))])
          
        g <- ggplot(plot_data,aes(fill=Level)) + {if(intervals_flag) geom_ribbon(aes_string(x=testCol,ymin='lower',ymax='upper'),alpha=0.2)} + geom_line(aes_string(x=testCol,y='predicted') + aes(color=Level))  + labs(title = paste0('Marginal effects of ',testCol))
      } else {
        # Plot the marginal effects of the testcol
        plot_data <- dataset0[,c(testCol,'predicted',if(intervals_flag) c('lower','upper')),drop=FALSE]
        
        g <- ggplot(plot_data) + { if(intervals_flag) geom_ribbon(aes_string(x=testCol,ymin='lower',ymax='upper'),alpha=0.2)} + geom_line(aes_string(x=testCol,y='predicted'),color='blue')  + labs(title = paste0('Marginal effects of ',testCol))
      }
      
      return(g)
    }
    
    
    marginal_plots <- predictors %>% map(~get_column_marginals(.))
    
    return(marginal_plots)
  },error=function(e) e[1])
}


get_marginal_plots6 <- function(model
                                , transformations=list(dep=c('none','log'), indep=c('none','log'))
                                , dataset
                                , predictors
                                , ref_agg = 'min'
                                , seasonal_dummies = NULL
                                , level_df = NULL
                                , plot_data_size = 100
                                , pred_colname='fit'
                                , lwr_colname='lwr'
                                , upr_colname='upr'
                                , se_colname=NULL
                                , predict_params=NULL
                                , pred_struct=c('vector','other')){
  
  tryCatch({
    dataset2 <- dataset
    
    # Transform the variables based on the model type
    if(transformations$indep[1]=='log'){
      dataset2[,predictors] <- dataset2[,predictors] %>% map(~log(.+1))
    }
    
    # Get the marginal plots for every predictor
    get_column_marginals <- function(predictor_name){
      dataset1 <- data.frame(dataset2)
      testCol <- predictor_name
      print(paste0('Creating plot for column : ',testCol))
      
      # Calculate mean values for all other columns apart form testCol
      meancols <- setdiff(predictors,testCol)
      meancols_means <- apply(dataset1[,meancols],2,ref_agg)
      min_value <- min(dataset1[,testCol],na.rm=T)
      max_value <- max(dataset1[,testCol],na.rm=T)
      
      dataset1[,meancols] <- meancols %>% map(~meancols_means[.])
      
      # Set all the seasonal dummy values to 0
      if(!is.null(seasonal_dummies)){
        dataset1[,seasonal_dummies] <- 0
      }
      
      # Create a sequence of values for the test column from min_value to max_value broken into 100 parts
      dataset1 <- dataset1[1,] # Select only 1 row and then replicate it
      dataset1 <- dataset1[rep(row.names(dataset1),plot_data_size),] # Replicate the rows by freq provided in plot_data_size variable
      if(abs(min_value)==Inf) min_value <- 0
      dataset1[,testCol] <- seq(min_value,max_value,along.with=c(1:plot_data_size))
      
      # Replicate values for all levels in level_df
      if(!is.null(level_df)){
        if(is.vector(level_df)){
          level_names <- level_df
          level_df <- unique(dataset2[,level_df,drop=FALSE])
        } else {
          level_names <- colnames(level_df)
        }
        # Expand the dataset for each level
        dataset0 <- merge(level_df,dataset1[,!colnames(dataset1) %in% colnames(level_df)],by=NULL) # cross join to get the
      } else dataset0 <- dataset1
      
      
      # ggplot aes concatenation function
      `+.uneval` <- function(a,b) {
        `class<-`(modifyList(a,b), "uneval")
      }
      
      # Get predictions
      predictions <-  do.call(predict,c(list(model,dataset0),predict_params))
      pClass <- class(predictions)
      
      # Tranforming variables back
      if(transformations$dep[1] %in% c('log')){
        predictions <- exp(predictions)
        if(transformations$indep[1]=='log') dataset0[,testCol] <- exp(dataset0[,testCol])
      } 
      
      # Create prediction intervals if provided or calculated
      if(pred_struct[1]=='vector'){
        dataset0$predicted <- as.numeric(predictions)
        intervals_flag <- FALSE
      } else {
        predictions <- data.frame(predictions)
        # Check interval names
        intervals_flag <- ifelse(is.null(lwr_colname) | is.null(upr_colname),FALSE,TRUE)
        
        if(is.null(se_colname)){
          predictions <- predictions[,c(pred_colname,if(intervals_flag) c(lwr_colname,upr_colname)),drop=FALSE]
          colnames(predictions) <- c('predicted',if(intervals_flag) c('lower','upper'))
        } else {
          if(intervals_flag) predictions$lwr <- predictions[,pred_colname] - (2 * predictions[,se_colname])
          if(intervals_flag) predictions$upr <- predictions[,pred_colname] + (2 * predictions[,se_colname])
          predictions <- predictions[,c(pred_colname,if(intervals_flag) c('lwr','upr')),drop=FALSE]
          colnames(predictions) <- c('predicted',if(intervals_flag) c('lower','upper'))
        }
        dataset0 <- cbind(dataset0,predictions)
      }
      
      
      # Plot the marginal curves
      if(!is.null(level_df)){
        dataset0 <- dataset0 %>% unite(Level, level_names, sep='_')
        
        # Plot the marginal effects of the testcol
        plot_data <- dataset0[,c('Level',testCol,'predicted',if(intervals_flag) c('lower','upper')),drop=FALSE]
        plot_data <- plot_data %>% group_by_at(vars(c('Level',testCol))) %>% {if(intervals_flag) dplyr::summarize(.,lower=min(lower,na.rm=T),upper=max(upper,na.rm=T),predicted=mean(predicted,na.rm=TRUE)) else  dplyr::summarize(.,predicted = mean(predicted, na.rm=TRUE))}
        
        g <- ggplot(plot_data,aes(fill=Level)) + {if(intervals_flag) geom_ribbon(aes_string(x=testCol,ymin='lower',ymax='upper'),alpha=0.2)} + geom_line(aes_string(x=testCol,y='predicted') + aes(color=Level))  + labs(title = paste0('Marginal effects of ',testCol))
      } else {
        # Plot the marginal effects of the testcol
        plot_data <- dataset0[,c(testCol,'predicted',if(intervals_flag) c('lower','upper')),drop=FALSE]
        
        g <- ggplot(plot_data) + { if(intervals_flag) geom_ribbon(aes_string(x=testCol,ymin='lower',ymax='upper'),alpha=0.2)} + geom_line(aes_string(x=testCol,y='predicted'),color='blue')  + labs(title = paste0('Marginal effects of ',testCol))
      }
      
      return(g)
    }
    
    
    marginal_plots <- predictors %>% map(~get_column_marginals(.))
    
    return(marginal_plots)
  },error=function(e) e[1])
}


## ---------------------------------------------------------------------------------------------------------------
## ------ Marginal effects plots with Prediction Intervals created using bootstrap method ------------------------
## ---------------------------------------------------------------------------------------------------------------


get_pi_marginal_plots <- function(model
                                  , transformations=list(dep=c('none','log'), indep=c('none','log'))
                                  , dataset
                                  , predictors
                                  , seasonal_dummies = NULL
                                  , level_df 
                                  , level1_columns
                                  , ref_agg = 'mean'
                                  , plot_data_size =100
                                  , pred_colname='fit'
                                  , lwr_colname='lwr'
                                  , upr_colname='upr'
                                  , se_colname=NULL
                                  , predict_params=NULL
                                  , pred_struct=c('vector','other')
                                  , trainFunc=NULL
                                  , bootIter = 100
                                  , intervalType = 'perc'
                                  , parallel= c('no','multicore','snow')
                                  , ncpus = (detectCores() - 1)
                                  , cl = NULL){
  
  tryCatch({
    dataset2 <- dataset
    
    # Transform the variables based on the model type
    if(transformations$indep=='log'){
      dataset2[,predictors] <- lapply(dataset2[,predictors],function(x){
        return(log(x+1))
      })
    }
    
    # Get the marginal plots for every predictor
    marginal_plots <- lapply(predictors,function(predictor_name){
      dataset1 <- data.frame(dataset2)
      testCol <- predictor_name
      print(paste0('Creating plot for column : ',testCol))
      
      # Calculate mean values for all other columns apart form testCol
      meancols <- setdiff(predictors,testCol)
      meancols_means <- apply(dataset1[,meancols],2,ref_agg)
      min_value <- min(dataset1[,testCol],na.rm=T)
      max_value <- max(dataset1[,testCol],na.rm=T)
      
      # Set all other numeric predictor values to mean of the predictor
      invisible(lapply(meancols,function(x){
        dataset1[,x] <<- meancols_means[x]
      }))
      
      # Set all the seasonal dummy values to 0
      if(!is.null(seasonal_dummies)){
        invisible(lapply(seasonal_dummies,function(x){
          dataset1[,x] <<- 0
        }))
      }
      
      # Create a sequence of values for the test column from min_value to max_value broken into 100 parts
      # dataset1 <- dataset1[1:plot_data_size,]
      dataset1 <- dataset1[1,] # Select only 1 row and then replicate it
      dataset1 <- dataset1[rep(row.names(dataset1),plot_data_size),] # Replicate the rows by freq provided in plot_data_size variable
      if(abs(min_value)==Inf) min_value <- 0
      min_value <- min_value - 0.2*min_value
      if(min_value < 0) min_value <- 0
      max_value <- max_value + 0.2*max_value
      dataset1[,testCol] <- seq(min_value,max_value,along.with=c(1:plot_data_size))
      
      if(!is.null(level_df)){
        # Expand the dataset for each level
        dataset0 <- merge(level_df,dataset1[,!colnames(dataset1) %in% colnames(level_df)],by=NULL) # cross join to get the
      } else dataset0 <- dataset1
      
      # # Convert level columns to factors
      # invisible(lapply(colnames(level_df1),function(x) 
      #   dataset0[,x] <- as.factor(dataset0[,x])))
      # 
      
      # ggplot aes concatenation function
      `+.uneval` <- function(a,b) {
        `class<-`(modifyList(a,b), "uneval")
      }
      
      # predictions <-  do.call(predict,c(list(model,dataset0),predict_params))
      # predictions <- get_bootstrap_predictions(data=dataset, modelTrainFunc=trainFunc, bootIter=bootIter, intervalType= "perc",testData=dataset0)
      predictions <- get_bootstrap_predictions(trainData=dataset2, testData=dataset0, trainFunc=trainFunc
                                               , fit.obj=model, bootIter=bootIter, intervalType = intervalType
                                               , parallel=parallel,ncpus = ncpus, cl=cl)
      pClass <- class(predictions)
      
      # Tranforming variables back
      if(transformations$dep %in% c('log')){
        predictions <- exp(predictions)
        if(transformations$indep=='log') dataset0[,testCol] <- exp(dataset0[,testCol])
      } 
      
      if(pred_struct[1]=='vector'){
        dataset0$predicted <- as.numeric(predictions)
        intervals_flag <- FALSE
        
      } else {
        predictions <- data.frame(predictions)
        # Check interval names
        intervals_flag <- ifelse(is.null(lwr_colname) | is.null(upr_colname),FALSE,TRUE)
        
        
        if(is.null(se_colname)){
          predictions <- predictions[,c(pred_colname,if(intervals_flag) c(lwr_colname,upr_colname)),drop=FALSE]
          colnames(predictions) <- c('predicted',if(intervals_flag) c('lower','upper'))
        } else {
          if(intervals_flag) predictions$lwr <- predictions[,pred_colname] - (2 * predictions[,se_colname])
          if(intervals_flag) predictions$upr <- predictions[,pred_colname] + (2 * predictions[,se_colname])
          predictions <- predictions[,c(pred_colname,if(intervals_flag) c('lwr','upr')),drop=FALSE]
          colnames(predictions) <- c('predicted',if(intervals_flag) c('lower','upper'))
        }
        dataset0 <- cbind(dataset0,predictions)
      }
      
      if(!is.null(level_df)){
        dataset0$Level <- apply(dataset0[,level1_columns,drop=FALSE],1,function(x) paste(x,collapse='_'))
        # Plot the marginal effects of the testcol
        plot_data <- dataset0[,c('Level',testCol,'predicted',if(intervals_flag) c('lower','upper')),drop=FALSE]
        
        g <- ggplot(plot_data,aes(fill=Level)) + {if(intervals_flag) geom_ribbon(aes_string(x=testCol,ymin='lower',ymax='upper'),alpha=0.2)} + geom_line(aes_string(x=testCol,y='predicted') + aes(color=Level))  + labs(title = paste0('Marginal effects of ',testCol))
      } else {
        # Plot the marginal effects of the testcol
        plot_data <- dataset0[,c(testCol,'predicted',if(intervals_flag) c('lower','upper')),drop=FALSE]
        print(head(plot_data))
        g <- ggplot(plot_data) + { if(intervals_flag) geom_ribbon(aes_string(x=testCol,ymin='lower',ymax='upper'),alpha=0.2)} + geom_line(aes_string(x=testCol,y='predicted'),color='blue')  + labs(title = paste0('Marginal effects of ',testCol))
      }
      
      # }
      
      
      return(g)
    })
    
    return(marginal_plots)
  },error=function(e) e[1])
}


get_pi_marginal_plots2 <- function(model,transformations=list(dep=c('none','log'), indep=c('none','log')), dataset, predictors, ref_agg = 'min', seasonal_dummies = NULL, level_df , level1_columns, plot_data_size =nrow(dataset), pred_colname='fit', lwr_colname='lwr', upr_colname='upr', se_colname=NULL, predict_params=NULL, pred_struct=c('vector','other')){
  # browser()
  tryCatch({
    dataset2 <- dataset
    
    # Transform the variables based on the model type
    if(transformations$indep=='log'){
      dataset2[,predictors] <- lapply(dataset2[,predictors],function(x){
        return(log(x+1))
      })
    }
    
    # Get the marginal plots for every predictor
    marginal_plots <- lapply(predictors,function(predictor_name){
      dataset1 <- data.frame(dataset2)
      testCol <- predictor_name
      print(paste0('Creating plot for column : ',testCol))
      
      # Calculate mean values for all other columns apart form testCol
      meancols <- setdiff(predictors,testCol)
      meancols_means <- apply(dataset1[,meancols],2,ref_agg)
      min_value <- min(dataset1[,testCol],na.rm=T)
      max_value <- max(dataset1[,testCol],na.rm=T)
      
      # Set all other numeric predictor values to mean of the predictor
      invisible(lapply(meancols,function(x){
        dataset1[,x] <<- meancols_means[x]
      }))
      
      # Set all the seasonal dummy values to 0
      if(!is.null(seasonal_dummies)){
        invisible(lapply(seasonal_dummies,function(x){
          dataset1[,x] <<- 0
        }))
      }
      
      # Create a sequence of values for the test column from min_value to max_value broken into 100 parts
      # dataset1 <- dataset1[1:plot_data_size,]
      dataset1 <- dataset1[1,] # Select only 1 row and then replicate it
      dataset1 <- dataset1[rep(row.names(dataset1),plot_data_size),] # Replicate the rows by freq provided in plot_data_size variable
      if(abs(min_value)==Inf) min_value <- 0
      dataset1[,testCol] <- seq(min_value,max_value,along.with=c(1:plot_data_size))
      
      if(!is.null(level_df)){
        # Expand the dataset for each level
        dataset0 <- merge(level_df,dataset1[,!colnames(dataset1) %in% colnames(level_df)],by=NULL) # cross join to get the
      } else dataset0 <- dataset1
      
      # # Convert level columns to factors
      # invisible(lapply(colnames(level_df1),function(x) 
      #   dataset0[,x] <- as.factor(dataset0[,x])))
      # 
      
      # ggplot aes concatenation function
      `+.uneval` <- function(a,b) {
        `class<-`(modifyList(a,b), "uneval")
      }
      
      # predictions <-  do.call(predict,c(list(model,dataset0),predict_params))
      predictions <- get_bootstrap_predictions(data=iris, modelTrainFunc=trainFuncLm, bootIter=100,intervalType= "perc",testData=dataset0)
      pClass <- 'matrix'
      
      # Tranforming variables back
      if(transformations$dep %in% c('log')){
        predictions <- exp(predictions)
        if(transformations$indep=='log') dataset0[,testCol] <- exp(dataset0[,testCol])
      } 
      
      if(pred_struct[1]=='vector'){
        dataset0$predicted <- as.numeric(predictions)
        intervals_flag <- FALSE
        
      } else {
        predictions <- data.frame(predictions)
        # Check interval names
        intervals_flag <- ifelse(is.null(lwr_colname) | is.null(upr_colname),FALSE,TRUE)
        
        
        if(is.null(se_colname)){
          predictions <- predictions[,c(pred_colname,if(intervals_flag) c(lwr_colname,upr_colname)),drop=FALSE]
          colnames(predictions) <- c('predicted',if(intervals_flag) c('lower','upper'))
        } else {
          if(intervals_flag) predictions$lwr <- predictions[,pred_colname] - (2 * predictions[,se_colname])
          if(intervals_flag) predictions$upr <- predictions[,pred_colname] + (2 * predictions[,se_colname])
          predictions <- predictions[,c(pred_colname,if(intervals_flag) c('lwr','upr')),drop=FALSE]
          colnames(predictions) <- c('predicted',if(intervals_flag) c('lower','upper'))
        }
        dataset0 <- cbind(dataset0,predictions)
      }
      
      if(!is.null(level_df)){
        dataset0$Level <- apply(dataset0[,level1_columns,drop=FALSE],1,function(x) paste(x,collapse='_'))
        # Plot the marginal effects of the testcol
        plot_data <- dataset0[,c('Level',testCol,'predicted',if(intervals_flag) c('lower','upper')),drop=FALSE]
        
        g <- ggplot(plot_data,aes(fill=Level)) + {if(intervals_flag) geom_ribbon(aes_string(x=testCol,ymin='lower',ymax='upper'),alpha=0.2)} + geom_line(aes_string(x=testCol,y='predicted') + aes(color=Level))  + labs(title = paste0('Marginal effects of ',testCol))
      } else {
        # Plot the marginal effects of the testcol
        plot_data <- dataset0[,c(testCol,'predicted',if(intervals_flag) c('lower','upper')),drop=FALSE]
        #print(head(plot_data))
        g <- ggplot(plot_data) + { if(intervals_flag) geom_ribbon(aes_string(x=testCol,ymin='lower',ymax='upper'),alpha=0.2)} + geom_line(aes_string(x=testCol,y='predicted'),color='blue')  + labs(title = paste0('Marginal effects of ',testCol))
      }
      
      # }
      
      
      return(g)
    })
    
    return(marginal_plots)
  },error=function(e) e[1])
}


## -----------------------------------------------------------------------------------------
## Plots for comparison of contributions ---------------------------------------------------
## -----------------------------------------------------------------------------------------


plot_contrib_comp <- function(model1_contrib,model2_contrib,xvar='variable',metric='value',remove='^intercept',data_level=c(var,'year_flag'),grid_level='year_flag',model_names=c('Model1',"Model2"),header=paste(model_names,collapse=' vs. ')){
  var <- xvar
  compared_contributions <- merge(model1_contrib,model2_contrib,by=data_level)
  cols <- grep('^value',colnames(compared_contributions))
  colnames(compared_contributions)[cols] <- model_names
  cc_data <- melt(compared_contributions,id=data_level)
  colnames(cc_data)[length(colnames(cc_data))-1] <- 'Model'
  
  cc_data1 <- cc_data  %>% filter_at(vars(xvar),any_vars(grepl('^pct_contrib_',.))) %>% { if(!is.null(remove)) filter_at(.,vars(xvar),any_vars(!grepl(remove,.))) }
  g1 <- ggplot(cc_data1,aes_string(x=xvar,y=metric)) + geom_bar(stat='identity', aes(fill = Model), position='dodge') + {if(!is.null(grid_level)) facet_grid(as.formula(paste0(".~",paste(grid_level,collapse="+"))))} + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y='Contribution (%)',title=header)
  
  cc_data3 <- compared_contributions %>% filter_at(vars(xvar),any_vars(grepl('^pct_contrib_',.))) %>% { if(!is.null(remove)) filter_at(.,vars(xvar),any_vars(!grepl(remove,.)))}
  g2 <- ggplot(cc_data3,aes_string(x=model_names[1],y=model_names[2])) + geom_point(aes_string(color=var)) + {if(!is.null(grid_level)) facet_grid(as.formula(paste0(".~",paste(grid_level,collapse="+"))))} + geom_abline(intercept=0,linetype='dashed',color='grey') + theme(axis.text.x = element_text(angle = 0, hjust = 1)) + labs(title=header)
  
  return(list(g1,g2))
}



## -----------------------------------------------------------------------
## Bootstrapping function for prediction intervals ------------------------
## -----------------------------------------------------------------------


get_bootstrap_predictions <- function(trainData, testData, trainFunc, fit.obj, bootIter=100, intervalType= c("norm","basic", "stud", "perc", "bca"), parallel= c("no", "multicore", "snow"),ncpus = getOption("boot.ncpus", 1L), cl=NULL){
  require(boot)
  
  predFunc <- function(newdata, model){
    pred <- predict(model, newdata = newdata)
    return(pred)
  }
  
  statFunc <- function(data, ind, modelTrainFunc, fit.obj, testData){
    sampData <- data[ind,]
    model <- modelTrainFunc(sampData,fit.obj)
    pr <- predFunc(testData, model)
    return(pr)
  }
  
  bootObj <- boot::boot(data = trainData, statistic = statFunc, R = bootIter
                          , modelTrainFunc=trainFunc, testData = testData, fit.obj=fit.obj, parallel=parallel,ncpus=ncpus,cl=cl)
  
  output <- do.call(rbind,lapply(c(1:nrow(testData)),function(i){
    out <- boot.ci(bootObj,type=intervalType[1],index=i)
    out <- out[[grep(paste0('^',intervalType),names(out))]] ## Get the PI matrix
    return(out)
  }))
  
  output <- output[,(ncol(output)-1):ncol(output)] # Last two columns contain the intervals
  colnames(output) <- c('lwr','upr')
  
  pred <- do.call(rbind,lapply(c(1:nrow(testData)),function(i){
    out <- boot.ci(bootObj,type=intervalType[1],index=i)
    return(out$t0)
  }))
  
  output <- cbind(output,fit=pred)
  colnames(output) <- c('lwr','upr','fit')
  return(data.frame(output))
}




## ------------------------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------------------------
## Corrected dydx function --------------
## Package : margins | function : dydx
## Correction : Modified the way to find 'fitted' column in the prediction dataset. 
## Predictions on different model objects returned different fitted colname
## --------------------------------------


dydx <- function(data, model, variable, ...) {
  UseMethod("dydx", data[[variable]])
}

#' @rdname dydx
#' @export
dydx.default <- 
  function(data, 
           model, 
           variable, 
           type = c("response", "link"), 
           change = c("dydx", "minmax", "iqr", "sd"),
           eps = 1e-7,
           as.data.frame = TRUE,
           ...) {
    
    if (is.numeric(change)) {
      stopifnot(length(change) == 2)
      lwr <- change[1]
      upr <- change[2]
      change <- "numeric"
    } else {
      change <- match.arg(change)
    }
    if (!is.numeric(data[[variable]])) {
      # return empty for unidentified variable class
      warning(paste0("Class of variable, ", variable, ", is unrecognized. Returning NA."))
      return(rep(NA_real_, nrow(data)))
    }
    
    d0 <- d1 <- data
    
    # set value of `h` based on `eps` to deal with machine precision
    setstep <- function(x) {
      x + (max(abs(x), 1, na.rm = TRUE) * sqrt(eps)) - x
    }
    
    if (change == "dydx") {
      # calculate numerical derivative
      d0[[variable]] <- d0[[variable]] - setstep(d0[[variable]])
      d1[[variable]] <- d1[[variable]] + setstep(d1[[variable]])
    } else if (change == "minmax") {
      # change from min(x) to max(x)
      d0[[variable]] <- min(d0[[variable]], na.rm = TRUE)
      d1[[variable]] <- max(d1[[variable]], na.rm = TRUE)
    } else if (change == "iqr") {
      # change from fivenum(x)[2] to fivenum(x)[4]
      fnum <- fivenum(d0[[variable]], na.rm = TRUE)
      d0[[variable]] <- fnum[2]
      d1[[variable]] <- fnum[4]
    } else if (change == "sd") {
      # change from mean(x) - sd(x) to mean(x) + sd(x)
      mn <- mean(d0[[variable]], na.rm = TRUE)
      sn <- sd(d0[[variable]], na.rm = TRUE)
      d0[[variable]] <- mn - sn
      d1[[variable]] <- mn + sn
    } else if (change == "numeric") {
      # otherwise `change` was numeric so calculate an arbitrary step
      d0[[variable]] <- lwr
      d1[[variable]] <- upr
    }
    
      
    if (!is.null(type)) {
      type <- type[1L]
      pred <- prediction(model = model, data = data.table::rbindlist(list(d0, d1)), type = type, calculate_se = FALSE, ...)
      fitted.col <- grep('^fitted',colnames(pred))
      pred <- pred[[fitted.col]]
    } else {
      pred <- prediction(model = model, data = data.table::rbindlist(list(d0, d1)), calculate_se = FALSE, ...)
      fitted.col <- grep('^fitted',colnames(pred))
      pred <- pred[[fitted.col]]
    }
    
    if (change == "dydx") {
      out <- (pred[nrow(d0) + seq_len(nrow(d0))] - pred[seq_len(nrow(d0))]) / (d1[[variable]] - d0[[variable]])
    } else {
      out <- (pred[nrow(d0) + seq_len(nrow(d0))] - pred[seq_len(nrow(d0))])
    }
    class(out) <- c("marginaleffect", "numeric")
    
    # return data.frame (or matrix) with column of derivatives
    if (isTRUE(as.data.frame)) {
      return(structure(list(out),
                       names = paste0("dydx_",variable),
                       class = c("data.frame"), 
                       row.names = seq_len(nrow(data))))
    } else {
      return(structure(matrix(out, ncol = 1L), dimnames = list(seq_len(length(out)), paste0("dydx_",variable))))
    }
  }


dydx.factor <- 
  function(data,
           model,
           variable,
           type = c("response", "link"),
           fwrap = FALSE,
           as.data.frame = TRUE,
           ...
  ) {
    
    levs <- levels(as.factor(data[[variable]]))
    base <- levs[1L]
    levs <- levs[-1L]
    
    # setup response object
    if (isTRUE(fwrap)) {
      outcolnames <- paste0("factor(", variable, ")", levs)
    } else {
      outcolnames <- paste0("dydx_", variable, levs)
    }
    
    if (isTRUE(as.data.frame)) {
      out <- structure(rep(list(list()), length(levs)), 
                       class = "data.frame", 
                       names = outcolnames, 
                       row.names = seq_len(nrow(data)))
    } else {
      out <- matrix(NA_real_, nrow = nrow(data), ncol = length(levs), dimnames = list(seq_len(nrow(data)), outcolnames))
    }
    
    # setup base data and prediction
    d0 <- d1 <- data
    d0[[variable]] <- base
    if (!is.null(type)) {
      type <- type[1L]
      pred0 <- prediction(model = model, data = d0, type = type, calculate_se = FALSE, ...)[["fitted"]]
    } else {
      pred0 <- prediction(model = model, data = d0, calculate_se = FALSE, ...)[["fitted"]]
    }
    # calculate difference for each factor level
    for (i in seq_along(levs)) {
      d1[[variable]] <- levs[i]
      if (!is.null(type)) {
        type <- type[1L]
        pred1 <- prediction(model = model, data = d1, type = type, calculate_se = FALSE, ...)[["fitted"]]
      } else {
        pred1 <- prediction(model = model, data = d1, calculate_se = FALSE, ...)[["fitted"]]
      }
      if (isTRUE(as.data.frame)) {
        out[[outcolnames[i]]] <- structure(pred1 - pred0, class = c("marginaleffect", "numeric"))
      } else {
        out[, outcolnames[i]] <- pred1 - pred0
      }
    }
    
    # return data.frame (or matrix) with column of derivatives
    return(out)
  }



#' @rdname dydx
#' @export
dydx.ordered <- dydx.factor

#' @rdname dydx
#' @export
dydx.logical <- 
  function(data,
           model,
           variable,
           type = c("response", "link"),
           as.data.frame = TRUE,
           ...
  ) {
    
    # setup base data and prediction
    d0 <- d1 <- data
    d0[[variable]] <- FALSE
    d1[[variable]] <- TRUE
    
    # calculate difference for moving FALSE to TRUE
    if (!is.null(type)) {
      type <- type[1L]
      pred <- prediction(model = model, data = data.table::rbindlist(list(d0, d1)), type = type, calculate_se = FALSE, ...)[["fitted"]]
    } else {
      pred <- prediction(model = model, data = data.table::rbindlist(list(d0, d1)), calculate_se = FALSE, ...)[["fitted"]]
    }
    out <- structure(pred[nrow(d0) + seq_len(nrow(d0))] - pred[seq_len(nrow(d0))], class = c("marginaleffect", "numeric"))
    
    # return data.frame (or matrix) with column of derivatives
    class(out) <- c("marginaleffect", "numeric")
    if (isTRUE(as.data.frame)) {
      return(structure(list(out),
                       names = paste0("dydx_",variable),
                       class = c("data.frame"), 
                       row.names = seq_len(nrow(data))))
    } else {
      return(structure(matrix(out, ncol = 1L), dimnames = list(seq_len(length(out)), paste0("dydx_",variable))))
    }
  }

## ----------------------------------------------------------------------------------
## Contribution calculations using minimum value of predictors as baseline values
## By-panel min-max baseline calculation if panel columns are provided
## ----------------------------------------------------------------------------------

# New contribution function


calculate_contributions5 <- function(model
                                     , dataset
                                     , spend_variables
                                     , fit_values = NULL
                                     , panel_cols = NULL
                                     , by_cols = NULL
                                     , pred_colname = ifelse(class(model)=='brmsfit','Estimate','fit')
                                     , pred_struct = c('vector','other')
                                     , aggregation = 'sum'
                                     , intercept_contrib = FALSE
                                     , ref_values = NULL # Dataframe with spend variables as columns and panels as rows, values being the minimum values
){
  
  tryCatch({
    
    group_by_cols <- panel_cols
    
    # Get fitted values
    if(is.null(fit_values)){
      pred <- predict(model,dataset)
      
      if(pred_struct[1]=='vector') dataset$fitted <- as.numeric(pred) # For numeric class
      else dataset$fitted <- pred[,pred_colname] # For matrix or data.frame
    } else dataset$fitted <- fit_values
    
    # Setting minimum values for predictors (i.e. only for spend variables)
    if(!is.null(ref_values)){
      min_values <- ref_values
      names(min_values) <- spend_variables
    } else { # If panel are provided, calculate minimum by panel else use the whole dataset for minimum calculation
      if(!is.null(panel_cols)) min_values <- dataset %>% split.data.frame(.[panel_cols],sep='__') %>% map(~sapply(.[,spend_variables,drop=FALSE],min,na.rm=T)) %>% 
          do.call(rbind,.) %>% data.frame() %>% mutate(Panel = row.names(.)) %>% separate('Panel',panel_cols,sep='__')
      else min_values <- sapply(dataset[,spend_variables,drop=FALSE],min,na.rm=T) %>% t() %>% data.frame()
    }
    
    
    # Get prediction setting the spend variables to minimum
    dataset_min <- dataset
    dataset_min <- dataset_min %>% dplyr::select(-one_of(spend_variables)) %>% merge(min_values,by=panel_cols)
    
    # Get baseline prediction
    all_min_pred <- predict(model,dataset_min)
    # Restructure output of predict
    if(pred_struct[1]=='vector') dataset$all_min_pred <- as.numeric(all_min_pred) # For numeric class
    else dataset$all_min_pred <- all_min_pred[,pred_colname] # For matrix or data.frame
    
    
    get_column_contrib <- function(xvar){
      
      # Set the values of the test column to zero
      dataset1 <- data.frame(dataset)
      predictor_name <- testCol <- xvar 
      
      # Set all other columns apart from xvar to min value
      other_spend_vars <- setdiff(spend_variables, xvar)
      # dataset1[,other_spend_vars] <-  min_values[other_spend_vars] %>% map(~rep(.,nrow(dataset1)))
      # if(!is.null(panel_cols)) other_spend_vars <- c(panel_cols,other_spend_vars)
      dataset1 <- dataset1 %>% dplyr::select(-one_of(other_spend_vars)) %>% merge(min_values[,c(other_spend_vars,if(!is.null(panel_cols)) panel_cols)],by=panel_cols)
      
      # Get predictions for channel in xvar with other at min
      other_min_pred <- predict(model,dataset1)
      if(pred_struct[1]=='vector') dataset1$other_min_pred <- as.numeric(other_min_pred) # For numeric class
      else dataset1$other_min_pred <- other_min_pred[,pred_colname] # For matrix or data.frame
      
      # Calculate contributions
      contrib_colname <- paste0('init_contrib_',xvar)
      # dataset1 <- dataset1 %>% mutate(!!contrib_colname:= (other_min_pred - all_min_pred))
      dataset1[,contrib_colname] <- other_min_pred - all_min_pred
      
      return(dataset1[,contrib_colname,drop=FALSE])
    }
    
    contributions_df <- do.call(cbind,spend_variables %>% map(~get_column_contrib(.)))
    
    # Add the contribution columns to the dataset
    dataset <- cbind(dataset,contributions_df)
    
    #total advertising contrib
    init_contrib_cols <- colnames(dataset)[grep('^init_contrib_',colnames(dataset))]
    dataset$total_ad_contrib <- rowSums(dataset[,init_contrib_cols,drop=FALSE])
    
    #Calculate proportion of each spend to advertising contrib
    dataset[,paste0('prop_',spend_variables)] <- spend_variables  %>% map(~{
      dataset[[paste0('init_contrib_',.)]]/dataset[['total_ad_contrib']]
    })
    
    # Add baseline contrib column
    dataset$init_contrib_baseline <- dataset$all_min_pred
    
    # Total change
    dataset$y_total_change <- dataset$fitted - dataset$init_contrib_baseline
    
    # Get absolute contrib of each channel
    dataset[,paste0('contrib_',spend_variables)] <- spend_variables %>% map(~{
      dataset[[paste0('prop_',.)]] * dataset$y_total_change
    })
    
    # Total sum of absolute contributions
    abs_contrib_cols <- colnames(dataset)[grep('^contrib_',colnames(dataset))]
    dataset$contrib_sum_prop <- rowSums(dataset[,abs_contrib_cols])
    
    # Total percentage by proportion. This should be equal to 1 for each row
    dataset$total_percent_by_prop <- (dataset$contrib_sum_prop + dataset$init_contrib_baseline)/dataset$fitted
    
    
    # Rollup the dataset to overall level i.e. sum all the observation level contribution to get total contribution 
    if(!is.null(by_cols))
      agg_dataset1 <- dataset %>% dplyr::select_at(vars(c(by_cols,init_contrib_cols,abs_contrib_cols,contrib_sum_prop,init_contrib_baseline,'fitted'))) %>% group_by_at(vars(by_cols)) %>% summarize_all(funs(do.call(aggregation,list(.))))
    else agg_dataset1 <- dataset %>% dplyr::select_at(vars(c(init_contrib_cols,abs_contrib_cols,contrib_sum_prop,init_contrib_baseline,'fitted'))) %>% summarize_all(funs(do.call(aggregation,list(.))))
    
    # new_contrib_cols <- colnames(agg_dataset1)[grep('^contrib_',colnames(agg_dataset1))]
    
    # Calculate the percentage contribution 
    # agg_dataset1$total <- rowSums(agg_dataset1[,new_contrib_cols,drop=FALSE])
    pct_colnames <- paste0('pct_',abs_contrib_cols)
    agg_dataset1[,pct_colnames] <- agg_dataset1[,abs_contrib_cols,drop=FALSE] %>% map(~./(agg_dataset1$fitted) * 100)
    agg_dataset1$pct_contrib_baseline <- agg_dataset1$init_contrib_baseline/agg_dataset1$fitted * 100
    colnames(agg_dataset1)[which(colnames(agg_dataset1)=='init_contrib_baseline')] <- 'contrib_baseline'
    
    return(agg_dataset1)
  },error=function(e) return(e))
}





# calculate_contributions5 <- function(model
#                                      , dataset
#                                      , spend_variables
#                                      , fit_values = NULL
#                                      , panel_cols = NULL
#                                      , by_cols = NULL
#                                      , pred_colname = ifelse(class(model)=='brmsfit','Estimate','fit')
#                                      , pred_struct = c('vector','other')
#                                      , aggregation = 'sum'
#                                      , ref_values = NULL # Dataframe with spend variables as columns and panels as rows, values being the minimum values
# ){
#   tryCatch({
#     
#     # Get fitted values
#     if(is.null(fit_values)){
#       pred <- predict(model,dataset)
#       
#       if(pred_struct[1]=='vector') dataset$fitted <- as.numeric(pred) # For numeric class
#       else dataset$fitted <- pred[,pred_colname] # For matrix or data.frame
#     } else dataset$fitted <- fit_values
#     
#     # Setting minimum values for predictors (i.e. only for spend variables)
#     if(!is.null(ref_values)){
#       min_values <- ref_values
#       names(min_values) <- spend_variables
#     } else { # If panel are provided, calculate minimum by panel else use the whole dataset for minimum calculation
#       if(!is.null(panel_cols)) min_values <- dataset %>% split.data.frame(.[panel_cols],sep='__') %>% map(~sapply(.[,spend_variables,drop=FALSE],min,na.rm=T)) %>% 
#           do.call(rbind,.) %>% data.frame() %>% mutate(Panel = row.names(.)) %>% separate('Panel',panel_cols,sep='__')
#       else min_values <- sapply(dataset[,spend_variables,drop=FALSE],min,na.rm=T) %>% t() %>% data.frame()
#     }
#     
#     # Get prediction setting the spend variables to minimum
#     dataset_min <- dataset
#     dataset_min <- dataset_min %>% dplyr::select(-one_of(spend_variables)) %>% merge(min_values,by=panel_cols)
#     
#     all_min_pred <- predict(model,dataset_min)
#     if(pred_struct[1]=='vector') dataset$all_min_pred <- as.numeric(all_min_pred) # For numeric class
#     else dataset$all_min_pred <- all_min_pred[,pred_colname] # For matrix or data.frame
#     
#     
#     get_column_contrib <- function(xvar){
#       # Set the values of the test column to zero
#       dataset1 <- data.frame(dataset)
#       predictor_name <- testCol <- xvar 
#       
#       # Set all other columns apart from xvar to min value
#       other_spend_vars <- setdiff(spend_variables,xvar)
#       # dataset1[,other_spend_vars] <-  min_values[other_spend_vars] %>% map(~rep(.,nrow(dataset1)))
#       # if(!is.null(panel_cols)) other_spend_vars <- c(panel_cols,other_spend_vars)
#       dataset1 <- dataset1 %>% dplyr::select(-one_of(other_spend_vars)) %>% merge(min_values[,c(other_spend_vars,if(!is.null(panel_cols)) panel_cols)],by=panel_cols)
#       
#       other_min_pred <- predict(model,dataset1)
#       if(pred_struct[1]=='vector') dataset1$other_min_pred <- as.numeric(other_min_pred) # For numeric class
#       else dataset1$other_min_pred <- other_min_pred[,pred_colname] # For matrix or data.frame
#       
#       # Calculate contributions
#       contrib_colname <- paste0('contrib_',xvar)
#       # dataset1 <- dataset1 %>% mutate(!!contrib_colname:= (other_min_pred - all_min_pred))
#       dataset1[,contrib_colname] <- other_min_pred - all_min_pred
#       
#       return(dataset1[,contrib_colname,drop=FALSE])
#     }
#     
#     
#     contributions_df <- do.call(cbind,spend_variables %>% map(~get_column_contrib(.)))
#     
#     
#     # Add the contribution columns to the dataset
#     dataset <- cbind(dataset,contributions_df)
#     dataset$contrib_baseline <- dataset$all_min_pred
#     contrib_cols <- colnames(dataset)[grep('^contrib_',colnames(dataset))]
#     
#     
#     # Rollup the dataset to overall level i.e. sum all the observation level contribution to get total contribution 
#     if(!is.null(by_cols))
#       agg_dataset1 <- dataset %>% dplyr::select_at(vars(c(by_cols,contrib_cols,'fitted'))) %>% group_by_at(vars(by_cols)) %>% summarize_all(funs(do.call(aggregation,list(.))))
#     else agg_dataset1 <- dataset %>% dplyr::select_at(vars(c(contrib_cols,'fitted'))) %>% summarize_all(funs(do.call(aggregation,list(.))))
#     
#     new_contrib_cols <- colnames(agg_dataset1)[grep('^contrib_',colnames(agg_dataset1))]
#     
#     # Calculate the percentage contribution 
#     agg_dataset1$total <- rowSums(agg_dataset1[,new_contrib_cols,drop=FALSE])
#     pct_colnames <- paste0('pct_',new_contrib_cols)
#     agg_dataset1[,pct_colnames] <- agg_dataset1[,new_contrib_cols,drop=FALSE] %>% map(~./agg_dataset1$total * 100)
#     
#     return(agg_dataset1)
#   },error=function(e) return(e))
# }