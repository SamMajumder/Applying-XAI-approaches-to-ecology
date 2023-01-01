
rm(list = ls())


### Packages we require for this analysis ###
packages <- c("tidyverse","here","ggplot2","randomForest",
              "caret","gbm","iml","patchwork")


### Loading them up ### 
lapply(packages, require,character.only=T) 

source("Modeling_functions.R")

#### Reading in the training and the test data ### 

train <- read.csv(here("Datasets and Tables","train_imputed.csv"))

test <- read.csv(here("Datasets and Tables","test_imputed.csv"))

### converting ecotype and Darker axillae columns to factor ###
###

train$ecotype <- factor(train$ecotype)
test$ecotype <- factor(test$ecotype)

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
set.seed(1234)

subsets <- c(1:86)  ## checking all possible subsets

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                         verbose = F) 

RFE_analysis <- RFE(train,subsets,params)

RFE_analysis

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["Best subset along with their importance values"]]


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,"C:/Users/samba/Documents/Chapter_3_Traditional_ML_modeling/Datasets and Tables/Rfe_best_subset.csv",row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

RFE_plot

ggsave("Figure 3.svg",dpi = 300)


##### Table containing the optimal subset of traits ### 

optimal_subset_rfe <- RFE_analysis[["optimal subset"]]

### renaming the darker axillae column in both the training and the test dataset ##

train  <- train %>% rename(Darker.axillae1 = Darker.axillae) 

test <- test %>% rename(Darker.axillae1 = Darker.axillae)


###########

train <- train %>% select(ecotype,optimal_subset_rfe$Features)

test <- test %>% select(ecotype,optimal_subset_rfe$Features)

########## Applying a Random forest classifier ##

params <- trainControl(method = "cv",number = 10)

set.seed(1234)
Rf <- Random_forest_analysis(train,params)

per_class_metrics_RF <- Rf[["Per class Metrics"]]

### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_RF,"C:/Users/samba/Documents/Chapter_3_Traditional_ML_modeling/Datasets and Tables/per_class_metrics_RF.csv")



## Applying a Gradient Boosting Machine ## 

grid <- expand.grid(n.trees=c(600,1000),
                    interaction.depth=c(4,6),
                    shrinkage=0.1,
                    n.minobsinnode=10)

params <- trainControl(method = "cv",
                       number = 10)


##### Applying the function to the training data ##

set.seed(1234)

GBM_results <- Gbm_analysis(train,grid,params)

### a dataframe containing per class metrics 

per_class_metrics_GBM <- GBM_results[["Per class Metrics"]]


### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_GBM,"C:/Users/samba/Documents/Chapter_3_Traditional_ML_modeling/Datasets and Tables/per_class_metrics_GBM.csv")  




#### Putting the models in a list so that we can plot their performance 

Rf <- Rf[[1]]

gbm <- GBM_results[[1]]

### Model comparison plots 

Model_list <- resamples(list(RF=Rf,GBM=gbm))


bwplot(Model_list)


### Making four ALE plots ### 
## two for north american desert ##

class <- c("north american desert","the great plains")


Ale <- Predictor$new(gbm, data = test, type = "prob", class= class)

## Total Leaf Number ##

Total_Leaf_Number <- plot(FeatureEffect$new(Ale, 
                                            feature = "Total_Leaf_Number", 
                                            method = "ale"))


p1 <- plot(Total_Leaf_Number) +  
      labs(y = "Probability", x ="Total Leaf Number") +
      theme(text = element_text(size = 10))
  

##### Disk Diameter ####


Disk_diameter <- plot(FeatureEffect$new(Ale, 
                                        feature = "Disk.diameter", 
                                        method = "ale"))


p2 <- plot(Disk_diameter) +  
      labs(y = "Probability", x ="Disk diameter (cm)") +
      theme(text = element_text(size = 10)) 


#### Total Leaf Nitrogen 

Total_Leaf_Nitrogen <- plot(FeatureEffect$new(Ale, 
                                              feature = "Leaf.total.N", 
                                              method = "ale"))


p3 <- plot(Total_Leaf_Nitrogen) +  
      labs(y = "Probability", x ="Total Leaf Nitrogen") +
      theme(text = element_text(size = 10)) 







### Specific Leaf area ###
Specific_Leaf_Area <- plot(FeatureEffect$new(Ale, 
                                             feature = "Specific_Leaf_Area", 
                                             method = "ale"))
  


p4 <- plot(Specific_Leaf_Area) +  
      labs(y = "Probability", x ="Specific Leaf Area") +
      theme(text = element_text(size = 10))



wrap_plots(p1,p2,p3,p4)


ggsave("Figure 4.svg",dpi = 300)

