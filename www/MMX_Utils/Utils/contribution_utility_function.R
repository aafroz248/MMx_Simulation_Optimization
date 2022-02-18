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



# Testing -------------------------------------------------------

# 
# Utils
# suppressPackageStartupMessages(source('MMX_Utils/Utils/contrib_marg_utility_functions.R'))

# Input file
object_list <- readRDS('/home/vyshali/Desktop/Marketing_Mix/Data/Semi-log.RDS')

# Read revelant inputs from the input object
mix_model <- object_list$model
train_data <- object_list$train_data
spend_variables <- object_list$spend_variables
panel_cols <- object_list$panel_column



contrib_lmer_raw_1 <- calculate_contributions5(model = mix_model
                                               , dataset = data.frame(train_data)
                                               , spend_variables = spend_variables
)

pct_colnames <- colnames(contrib_lmer_raw_1)[grep('^pct_contrib_',colnames(contrib_lmer_raw_1))]
pct_contrib_lmer_raw_1 <- contrib_lmer_raw_1[pct_colnames]

pct_contrib_lmer_raw_1 <- pct_contrib_lmer_raw_1 %>% melt(.)

gm3_cb_lmer_raw_1 <- ggplot(pct_contrib_lmer_raw_1,aes(x=variable,y=value)) + geom_bar(stat='identity',color='blue',fill='dodgerblue3') + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y='Contribution (%)',title='Contributions')

pct_contrib_lmer_raw_1
gm3_cb_lmer_raw_1
