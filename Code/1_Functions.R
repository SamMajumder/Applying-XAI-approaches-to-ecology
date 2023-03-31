


##################################
###### Missing value visualize and insight function ###
########################

Missing <- function(df,y=NULL){
  
  ### 
  df <- df %>% select(-y)
  
  #### Missing values in the data
  
  Missing_values <- df %>%
    gather(key = "key", value = "val") %>%
    mutate(is.missing = is.na(val)) %>%
    group_by(key, is.missing) %>%
    summarise(number_of_missing_values = n()) %>%
    filter(is.missing==T) %>%
    select(-is.missing) %>%
    arrange(desc(number_of_missing_values)) %>%
    rename(Variables = key)

### Proportion missing from each trait    
  
  Missing_percent <- df %>%
    gather(key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    group_by(key) %>%
    mutate(total = n()) %>%
    group_by(key, total, isna) %>%
    summarise(num.isna = n()) %>%
    mutate(pct = num.isna / total * 100) %>%
    mutate(Type = case_when(isna == "FALSE" ~ "Not Missing",
                            isna == "TRUE" ~ "Missing")) %>% 
    #### remove the num.isna column because it is misleading 
    select(-num.isna)
  
  
  ## Now make a plot ## percent of missing values for variables ###
  
  p1 <- ggplot(Missing_percent,aes(fill=Type, y=pct,x=key)) +
        geom_bar(position = 'stack',stat = 'identity') +
        labs(x = "Traits", y = "Percent Missing") +
        coord_flip() +
        theme(text = element_text(size=10))
  
  
  print(p1)
  
  results <- list("Missing_values" = Missing_values,
                  "Missing_percent" = Missing_percent,
                  "Missing_Value_Plot" = p1)
  
  return(results)
  
}


#################
#######################
######## RFE ###

RFE <- function(df,subsets,params){
  features_rfe <- rfe(Ecoregions~.,data = df,
                      sizes=subsets,rfeControl=params)
  
  ## these are the predictors in the optimal subset 
  
  optimal_subset_rfe <- data.frame(Features = predictors(features_rfe))
  
  #### Importance of each variable 
  
  Rfe_Imp <- data.frame(varImp(features_rfe))
  
  Rfe_Imp <- data.frame(Features = rownames(Rfe_Imp),
                        Overall = Rfe_Imp$Overall)
  
  Rfe_Imp_best_subset <- Rfe_Imp %>%
    dplyr::filter(Features %in% optimal_subset_rfe$Features)
  
  
  ### plotting the variation of accuracy with the removal of variables
  p_1 <- ggplot(features_rfe)  
  
  p_2 <- ggplot(data = Rfe_Imp_best_subset,
                aes(x=reorder(Features,Overall), y = Overall, fill = Features)) +
    geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
    coord_flip() + 
    theme_bw() + theme(legend.position = "none") + 
    ggtitle("Optimal subset of Traits") +
    theme(text = element_text(size = 10))  
  
  list <- list("Plot of variation of accuracy with the removal of variables" = p_1,
               "Importance plot" = p_2,
               "optimal subset" = optimal_subset_rfe,
               "Best subset along with their importance values" = Rfe_Imp_best_subset
  ) 
  return(list)
  
  
}


###################
## BORUTA #############
####################
#### Now writing a function that will evaluate the strongly divergent traits ##
#### output the tables containing the importance values for each variables ###

Boruta_analysis <- function(df){
  
  Boruta <- Boruta(Ecoregions ~., df)
  
  # Putting the importance decisions in a nice table ### 
  
  Boruta_feature_analysis <- data.frame(attStats(Boruta)) 
  
  ### Making the colnames into row names ###
  
  Boruta_feature_analysis$Feature <- rownames(Boruta_feature_analysis)
  
  ### Subsetting the dataset based on the strongly relevant traits ## 
  ## i.e., Confirmed ###
  
  Boruta_feature_analysis <- Boruta_feature_analysis %>% 
    dplyr::filter(decision == "Confirmed")
  
  
  p<- ggplot(data = Boruta_feature_analysis,
             aes(x=reorder(Feature,meanImp), y = meanImp, fill = Feature)) +
    geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
    coord_flip() + 
    theme_bw() + theme(legend.position = "none") + 
    ggtitle("Strongly important variables") +
    theme(text = element_text(size = 10))   
  
  list <- list("Importance plot Boruta" = p,
               "Table with strongly divergent traits along with their importances" = Boruta_feature_analysis)  
  
  return(list)
}



#####################
## MODELING ####
################

Random_forest_analysis <- function(df,params){
  
  Rf <- train(Ecoregions~.,data=df,
              method="rf",trControl=params,
              verbose=F)  
  
  print(Rf)
  
  p_rf <- predict(Rf,test)
  
  c_rf <- confusionMatrix(p_rf,test$Ecoregions) 
  
  print(c_rf)
  
  ##############
  ### Extracting data from the confusion matrix 
  ###############    #### Random Forest ######## 
  
  per_class_metrics_RF <- data.frame(c_rf$byClass)
  
  #### Macro averaged metrics ### Random_forest ########
  
  Macro_averaged_metrics_Rf <- data.frame(Macro_averaged_metrics =
                                            apply(per_class_metrics_RF[-1],2,mean))
  
  list <- list("Rf" = Rf,"Confusion Matrix" = c_rf, 
               "predicted_rf" = p_rf,
               "Per class Metrics" = per_class_metrics_RF,
               "Macro averaged metrics" = Macro_averaged_metrics_Rf)
  
  return(list)
  
}

####### GBM ####

Gbm_analysis <- function(df,grid,params) {
  
  gbm <- train(Ecoregions~., data=df,
               method="gbm",trControl=params,
               verbose=F,tuneGrid=grid) 
  
  print(gbm)
  
  p_gbm <- predict(gbm,test)
  
  c_gbm <- confusionMatrix(p_gbm,test$Ecoregions) 
  
  print(c_gbm)
  
  ##############
  ### Extracting data from the confusion matrix 
  ###############    #### Gradient Boosting ######## 
  
  per_class_metrics_GBM <- data.frame(c_gbm$byClass)
  
  #### Macro averaged metrics ### Random_forest ########
  
  Macro_averaged_metrics_GBM <- data.frame(Macro_averaged_metrics =
                                             apply(per_class_metrics_GBM[-1],2,mean))
  
  list <- list("GBM" = gbm,"Confusion Matrix" = c_gbm, 
               "predicted_gbm" = p_gbm,
               "Per class Metrics" = per_class_metrics_GBM,
               "Macro averaged metrics" = Macro_averaged_metrics_GBM)
  
  return(list)
  
}






