

RFE <- function(df,subsets,params){
  features_rfe <- rfe(ecotype~.,data = df,
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



#####################
## MODELING ####
################

Random_forest_analysis <- function(df,params){
  set.seed(1234)
  Rf <- train(ecotype~.,data=df,
              method="rf",trControl=params,
              verbose=F)
  
  p_rf <- predict(Rf,test)
  
  c_rf <- confusionMatrix(p_rf,test$ecotype)
  
  ##############
  ### Extracting data from the confusion matrix 
  ###############    #### Random Forest ######## 
  
  per_class_metrics_RF <- data.frame(c_rf$byClass)
  
  #### Macro averaged metrics ### Random_forest ########
  
  Macro_averaged_metrics_Rf <- data.frame(Macro_averaged_metrics =
                                            apply(per_class_metrics_RF[-1],2,mean))
  
  list <- list("Rf" = Rf,"Confusion Matrix" = c_rf, 
               "Per class Metrics" = per_class_metrics_RF,
               "Macro averaged metrics" = Macro_averaged_metrics_Rf)
  
  return(list)
  
}



Gbm_analysis <- function(df,grid,params) {
  
  gbm <- train(ecotype~., data=df,
               method="gbm",trControl=params,
               verbose=F,tuneGrid=grid)
  
  p_gbm <- predict(gbm,test)
  
  c_gbm <- confusionMatrix(p_gbm,test$ecotype)
  
  ##############
  ### Extracting data from the confusion matrix 
  ###############    #### Gradient Boosting ######## 
  
  per_class_metrics_GBM <- data.frame(c_gbm$byClass)
  
  #### Macro averaged metrics ### Random_forest ########
  
  Macro_averaged_metrics_GBM <- data.frame(Macro_averaged_metrics =
                                             apply(per_class_metrics_GBM[-1],2,mean))
  
  list <- list("GBM" = gbm,"Confusion Matrix" = c_gbm, 
               "Per class Metrics" = per_class_metrics_GBM,
               "Macro averaged metrics" = Macro_averaged_metrics_GBM)
  
  return(list)
  
}