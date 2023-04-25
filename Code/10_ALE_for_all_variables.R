
packages <- c("tidyverse","here","iml")

lapply(packages, require, character.only =T)

options(scipen = 9999)

##########################

### loading in the train dataset which contains only the most divergent traits #

train <- read.csv(here::here("Processed_Datasets",
                       "train_boruta.csv"))


Model <- readRDS(here::here("Code","GBM.rds"))

## our ecoregions## 
class <- c("NORTH AMERICAN DESERTS","GREAT PLAINS")

set.seed(1234)
Ale <- Predictor$new(Model, 
                     data = train, 
                     type = "prob", 
                     class= class) 


## computing the local effects on the training dataset
ALE_train_effects <- FeatureEffects$new(Ale,
                                        method = "ale")


## extracting the local effects from the training dataset
ALE_train_values <- ALE_train_effects[["results"]]

## put all the effects in one dataframe ### train
ALE_train_values <- do.call(rbind,ALE_train_values) 

#### renaming the columns #####

ALE_train_values <- ALE_train_values %>% 
                                     dplyr::rename(Ecoregions = .class,
                                                ALE = .value,
                                                Trait_Value = .borders,
                                                Trait = .feature)

#### exporting the ALE values out ###

saveRDS(ALE_train_values,"ALE_train.RDS")








