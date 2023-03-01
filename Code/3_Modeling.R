
rm(list = ls())


### Packages we require for this analysis ###
packages <- c("tidyverse","here","ggplot2","randomForest",
              "gbm","Boruta","xgboost","caret")


### Loading them up ### 
lapply(packages, require,character.only=T) 

source(here("Code","1_Functions.R"))

#### Reading in the training and the test data ### 

train <- read.csv(here("Processed_Datasets","train_imputed.csv"))

test <- read.csv(here("Processed_Datasets","test_imputed.csv"))

### converting ecotype and Darker axillae columns to factor ###
###

train$Ecoregions <- factor(train$Ecoregions)
test$Ecoregions <- factor(test$Ecoregions)

train$Darker.axillae <- factor(train$Darker.axillae)
test$Darker.axillae <- factor(test$Darker.axillae)

### renaming the column names for both training and test ### 

train <- train %>% rename(Total_Leaf_Number = TLN) %>%
                   rename(Days_to_flowering = DTF) %>%
                   rename(Specific_Leaf_Area = SLA)

test <- test %>% rename(Total_Leaf_Number = TLN) %>%
                 rename(Days_to_flowering = DTF) %>%
                 rename(Specific_Leaf_Area = SLA) 


######
subsets <- c(1:85)  ## checking all possible subsets

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                         verbose = F) 

set.seed(1234)
RFE_analysis <- RFE(train,subsets,params)

RFE_analysis

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["Best subset along with their importance values"]]


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Applying-XAI-approaches-to-ecology/Processed_Datasets/Rfe_best_subset.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

RFE_plot

ggsave("Figure 3.svg",dpi = 600)


##### Table containing the optimal subset of traits ### 
 
optimal_subset_rfe <- RFE_analysis[["optimal subset"]]  


### Table contaning best subset along with their importance values ##

Importance_values <- RFE_analysis[["Best subset along with their importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Applying-XAI-approaches-to-ecology/Processed_Datasets/Rfe_best_subset_imp.csv",
          row.names = FALSE) 


### renaming the darker axillae column in both the training and the test dataset 
## to match the trait names of the optimal subset ## RFE keeps adding a 1 after the name Darker Axillae

train  <- train %>% rename(Darker.axillae1 = Darker.axillae) 

test <- test %>% rename(Darker.axillae1 = Darker.axillae)

##### 
###########

train <- train %>% select(Ecoregions,optimal_subset_rfe$Features)

test <- test %>% select(Ecoregions,optimal_subset_rfe$Features) 

######### Renaming Darker.axillae1 to Darker.axillae ###
##### we don't want the ALE plot to show Darker.axillae1

train  <- train %>% rename(Darker.axillae = Darker.axillae1) 

test <- test %>% rename(Darker.axillae = Darker.axillae1)

########################
### Now applying Boruta to the dataset to identify strongly divergent ###
###########

set.seed(1234)

### Applying the function to the train optimal dataset ##

Boruta_results <- Boruta_analysis(train)

### Table with strongly divergent traits along with their importances

Boruta_feature_analysis <- Boruta_results[[2]] 


####### exporting the strongly relevant features as deemed by Boruta 

write.csv(Boruta_feature_analysis,
          "~/Applying-XAI-approaches-to-ecology/Processed_Datasets/Boruta_results.csv",
          row.names = FALSE)


####### Plotting the importance values ###

Boruta_importance_plots <- Boruta_results[[1]]

Boruta_importance_plots

ggsave("Figure S2.svg",dpi = 600)


####  

train <- train %>% select(Ecoregions,Boruta_feature_analysis$Feature) 

#### writing out the train file for subsequent ALE plot development##

write.csv(train, "~/Applying-XAI-approaches-to-ecology/Processed_Datasets/train_boruta.csv",
          row.names = FALSE)


test <- test %>% select(Ecoregions,Boruta_feature_analysis$Feature) 

########## Applying a Random forest classifier ##

params <- trainControl(method = "cv",number = 10)

set.seed(1234)
RF_analysis <- Random_forest_analysis(train,params)

RF_analysis 

per_class_metrics_RF <- RF_analysis[["Per class Metrics"]]

### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_RF,
          "C:/Users/samba/Documents/Applying-XAI-approaches-to-ecology/Processed_Datasets/per_class_metrics_RF.csv")


####### CREATING A PREDICTION FILE ###

### extracting prediction ## 

p_rf <- data.frame(prediction = RF_analysis[["predicted_rf"]])


### reading in the ids
test_ids <- read.csv(here("Processed_Datasets",
                          "test_ids.csv")) %>% 
            rename(Actual = Ecoregions)


## this is the prediction file 
predicted_versus_actual_Rf <- data.frame(test_ids,
                                          Predicted = p_rf$prediction)


### Add a column indicating correctly versus incorrectly predicted #

predicted_versus_actual_Rf <- predicted_versus_actual_Rf %>%
                              mutate(Outcome = ifelse(Actual == Predicted, 
                          "Correctly_Predicted",
                          "Incorrectly_Predicted"))

### move around the columns before printing it out ##
## Having predicted and actual side by side will facilitate easy viewing ##


predicted_versus_actual_Rf <- predicted_versus_actual_Rf %>% 
                              select(latitude,longitude,sitename,
                                     individual_id,population_id,
                                     genotype_id,Predicted,Actual,
                                     Outcome)


### exporting this as an excel table ###

write.csv(predicted_versus_actual_Rf,
          "~/Applying-XAI-approaches-to-ecology/Processed_Datasets/predicted_versus_actual_Rf.csv")


########################
## Applying a Gradient Boosting Machine ## 
#############

grid <- expand.grid(n.trees=c(600,1000),
                    interaction.depth=c(4,6),
                    shrinkage=0.1,
                    n.minobsinnode=10)

params <- trainControl(method = "cv",
                       number = 10)


##### Applying the function to the training data ##

set.seed(1234)

table(train$Ecoregions)

GBM_results <- Gbm_analysis(train,grid,params)


GBM_results

### a dataframe containing per class metrics 

per_class_metrics_GBM <- GBM_results[["Per class Metrics"]]


### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_GBM,
          "C:/Users/samba/Documents/Applying-XAI-approaches-to-ecology/Processed_Datasets/per_class_metrics_GBM.csv")  

### extracting prediction ## 

p_gbm <- data.frame(prediction = GBM_results[["predicted_gbm"]])

#########################
#### creating a prediction file ###
##########################

## this is the prediction file 
predicted_versus_actual_GBM <- data.frame(test_ids,
                                          Predicted = p_gbm$prediction)


### Add a column indicating correctly versus incorrectly predicted #

predicted_versus_actual_GBM <- predicted_versus_actual_GBM %>%
                               mutate(Outcome = ifelse(Actual == Predicted, 
                                                       "Correctly_Predicted",
                                                      "Incorrectly_Predicted"))

### move around the columns before printing it out ##
## Having predicted and actual side by side will facilitate easy viewing ##


predicted_versus_actual_GBM <- predicted_versus_actual_GBM %>% 
                              select(latitude,longitude,sitename,
                              individual_id,population_id,
                              genotype_id,Predicted,Actual,
                              Outcome)


### exporting this as an excel table ###

write.csv(predicted_versus_actual_GBM,
          "~/Applying-XAI-approaches-to-ecology/Processed_Datasets/predicted_versus_actual_GBM.csv")



#### Putting the models in a list so that we can plot their performance 

Rf <- RF_analysis[["Rf"]] 


## saving the trained model for further use ##

saveRDS(Rf,"Rf.rds")


gbm <- GBM_results[["GBM"]] 

### saving the trained model for further use ###

saveRDS(gbm,"GBM.rds")

### Model comparison plots 

Model_list <- resamples(list(RF=Rf,GBM=gbm))


bwplot(Model_list)








  




