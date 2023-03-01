
rm(list=ls())


### load the libraries ###

packages <- list("iml","randomForest",
                 "caret","gbm")

lapply(packages, require, character.only =T)

### loading in the train dataset which contains only the most divergent traits #

train <- read.csv(here("Processed_Datasets",
                       "train_boruta.csv"))


#### Rf ####
### Making ALE plots ### Top 8 variables 
## two for north american desert ##

### loading in the model ##

Rf <- readRDS(here("Code","Rf.rds"))

## our ecoregions## 
class <- c("NORTH AMERICAN DESERTS","GREAT PLAINS")

Ale <- Predictor$new(Rf, data = train, type = "prob", class= class)

############################
## Total RGB ##

Total_RGB <- plot(FeatureEffect$new(Ale, 
                                    feature = "Total.RGB", 
                                    method = "ale"))   

### extracting the information which contains the ALE values ###

ALE_RGB_RF <- Total_RGB[["data"]]

## adding a column spelling out the trait ##

ALE_RGB_RF <- ALE_RGB_RF %>% 
              mutate(Trait = "Total RGB") %>% 
              rename(Trait_Value = Total.RGB)

###########################################
### Primary branches ##
############

Primary_branches <- plot(FeatureEffect$new(Ale, 
                                           feature = "Primary.branches", 
                                           method = "ale")) 

### extracting the information which contains the ALE values ###

ALE_Primary_branches_RF <- Primary_branches[["data"]]

## adding a column spelling out the trait ##

ALE_Primary_branches_RF <- ALE_Primary_branches_RF %>% 
  mutate(Trait = "Primary Branches") %>% 
  rename(Trait_Value = Primary.branches)

################################################# 


######################## 
## RGB Proportion Red ####
##########################

RGB_proportion_red <- plot(FeatureEffect$new(Ale, 
                                             feature = "RGB.proportion.red", 
                                             method = "ale"))

### extracting the information which contains the ALE values ###

ALE_RGB_prop_Red_RF <- RGB_proportion_red[["data"]]

## adding a column spelling out the trait ##

ALE_RGB_prop_Red_RF <- ALE_RGB_prop_Red_RF %>% 
                       mutate(Trait = "RGB Proportion Red") %>% 
                       rename(Trait_Value = RGB.proportion.red)


#############
## Total Leaf Number #
#################


TLN <- plot(FeatureEffect$new(Ale, 
                              feature = "Total_Leaf_Number", 
                              method = "ale"))

### extracting the information which contains the ALE values ###

ALE_TLN_RF <- TLN[["data"]]

## adding a column spelling out the trait ##

ALE_TLN_RF <- ALE_TLN_RF %>% 
              mutate(Trait = "Leaf Number") %>% 
              rename(Trait_Value = Total_Leaf_Number)


#############
## Phyllaries length ##
############


Phyllaries_length <- plot(FeatureEffect$new(Ale, 
                                            feature = "Phyllaries.length", 
                                            method = "ale"))


### extracting the information which contains the ALE values ###

ALE_Phyllaries_RF <- Phyllaries_length[["data"]]


## adding a column spelling out the trait ##

ALE_Phyllaries_RF <- ALE_Phyllaries_RF %>% 
                    mutate(Trait = "Phyllaries Length(cm)") %>% 
                   rename(Trait_Value = Phyllaries.length)


#######################
### Leaf Total Nitrogen ##
################

Leaf_total_nitrogen <- plot(FeatureEffect$new(Ale, 
                                              feature = "Leaf.total.N", 
                                              method = "ale"))  


### extracting the information which contains the ALE values ###

ALE_Leaf_total_nitrogen_RF <- Leaf_total_nitrogen[["data"]]

## adding a column spelling out the trait ##

ALE_Leaf_total_nitrogen_RF <- ALE_Leaf_total_nitrogen_RF %>% 
                         mutate(Trait = "Leaf Nitrogen") %>% 
                         rename(Trait_Value = Leaf.total.N)


#############
## Specific Leaf Area ### 
################

Specific_leaf_area <- plot(FeatureEffect$new(Ale, 
                                             feature = "Specific_Leaf_Area", 
                                             method = "ale"))

### extracting the information which contains the ALE values ###

ALE_SLA_RF <- Specific_leaf_area[["data"]]

## adding a column spelling out the trait ##

ALE_SLA_RF <- ALE_SLA_RF %>% 
              mutate(Trait = "SLA(mm2/mg)") %>% 
              rename(Trait_Value = Specific_Leaf_Area)



################
## Leaf C:N ratio ###
##############


#### extracting the information which contains the ALE values #### 

Leaf_C_N_ratio <- plot(FeatureEffect$new(Ale, 
                                         feature = "Leaf.C.N.ratio", 
                                         method = "ale"))


### extracting the information which contains the ALE values ###

ALE_Leaf_C_N_ratio_RF <- Leaf_C_N_ratio[["data"]]

## adding a column spelling out the trait ##

ALE_Leaf_C_N_ratio_RF <- ALE_Leaf_C_N_ratio_RF %>% 
                         mutate(Trait = "Leaf C:N Ratio") %>% 
                         rename(Trait_Value = Leaf.C.N.ratio)


################ 

ALE_RF <- do.call("rbind",
                  list(ALE_Leaf_C_N_ratio_RF,
                       ALE_Phyllaries_RF,
                       ALE_Primary_branches_RF,
                       ALE_RGB_RF,
                       ALE_SLA_RF,
                       ALE_TLN_RF,
                       ALE_Leaf_total_nitrogen_RF,
                       ALE_RGB_prop_Red_RF))



### renaming the columns of this dataframe ###

ALE_RF <- ALE_RF %>% rename(Ecoregions = .class,
                            ALE = .value)



### setting levels as per the order of importance (according to Boruta) 
## plots will be faceted from left to right in order of their importances 

ALE_RF$Trait <- factor(ALE_RF$Trait, 
                       levels = c("Total RGB","Primary Branches",
                                  "SLA(mm2/mg)","Leaf Number",
                                  "RGB Proportion Red","Leaf Nitrogen",
                                  "Leaf C:N Ratio","Phyllaries Length(cm)"))



#### creating a ggplot to show the ALE values 

ggplot(ALE_RF, 
       aes(x = Trait_Value, y = ALE)) + 
  geom_line(aes(color = Ecoregions)) +
  facet_grid(Ecoregions ~ Trait,
             scales = "free_x") +
  scale_color_manual(values=c('#619CFF',
                              '#F8766D')) + 
  labs(x = "Trait Value", y = "ALE") +
  theme(text = element_text(size = 9)) +
  theme(legend.position="bottom") 
  

ggsave("Figure S4.svg",
       dpi = 600)    


#### GBM ####
### Making four ALE plots ### 
## two for north american desert ##

### loading in the model ##

gbm <- readRDS(here("Code","GBM.rds"))

class <- c("NORTH AMERICAN DESERTS","GREAT PLAINS")


Ale <- Predictor$new(gbm, data = train, type = "prob", class= class)

############################
## Total RGB ##

Total_RGB <- plot(FeatureEffect$new(Ale, 
                                    feature = "Total.RGB", 
                                    method = "ale"))   

### extracting the information which contains the ALE values ###

ALE_RGB_GBM <- Total_RGB[["data"]]

## adding a column spelling out the trait ##

ALE_RGB_GBM <- ALE_RGB_GBM %>% 
                mutate(Trait = "Total RGB") %>% 
                rename(Trait_Value = Total.RGB)

###########################################
### Primary branches ##
############

Primary_branches <- plot(FeatureEffect$new(Ale, 
                                           feature = "Primary.branches", 
                                           method = "ale")) 

### extracting the information which contains the ALE values ###

ALE_Primary_branches_GBM <- Primary_branches[["data"]]

## adding a column spelling out the trait ##

ALE_Primary_branches_GBM <- ALE_Primary_branches_GBM %>% 
                            mutate(Trait = "Primary Branches") %>% 
                            rename(Trait_Value = Primary.branches)

################################################# 


######################## 
## RGB Proportion Red ####
##########################

RGB_proportion_red <- plot(FeatureEffect$new(Ale, 
                                             feature = "RGB.proportion.red", 
                                             method = "ale"))

### extracting the information which contains the ALE values ###

ALE_RGB_prop_Red_GBM <- RGB_proportion_red[["data"]]

## adding a column spelling out the trait ##

ALE_RGB_prop_Red_GBM <- ALE_RGB_prop_Red_GBM %>% 
                        mutate(Trait = "RGB Proportion Red") %>% 
                        rename(Trait_Value = RGB.proportion.red)


#############
## Total Leaf Number #
#################


TLN <- plot(FeatureEffect$new(Ale, 
                              feature = "Total_Leaf_Number", 
                              method = "ale"))

### extracting the information which contains the ALE values ###

ALE_TLN_GBM <- TLN[["data"]]

## adding a column spelling out the trait ##

ALE_TLN_GBM <- ALE_TLN_GBM %>% 
                mutate(Trait = "Leaf Number") %>% 
                rename(Trait_Value = Total_Leaf_Number)


#############
## Phyllaries length ##
############


Phyllaries_length <- plot(FeatureEffect$new(Ale, 
                                            feature = "Phyllaries.length", 
                                            method = "ale"))


### extracting the information which contains the ALE values ###

ALE_Phyllaries_GBM <- Phyllaries_length[["data"]]


## adding a column spelling out the trait ##

ALE_Phyllaries_GBM <- ALE_Phyllaries_GBM %>% 
                      mutate(Trait = "Phyllaries Length(cm)") %>% 
                      rename(Trait_Value = Phyllaries.length)


#######################
### Leaf Total Nitrogen ##
################

Leaf_total_nitrogen <- plot(FeatureEffect$new(Ale, 
                                              feature = "Leaf.total.N", 
                                              method = "ale"))  


### extracting the information which contains the ALE values ###

ALE_Leaf_total_nitrogen_GBM <- Leaf_total_nitrogen[["data"]]

## adding a column spelling out the trait ##

ALE_Leaf_total_nitrogen_GBM <- ALE_Leaf_total_nitrogen_GBM %>% 
                               mutate(Trait = "Leaf Nitrogen") %>% 
                               rename(Trait_Value = Leaf.total.N)


#############
## Specific Leaf Area ### 
################

Specific_leaf_area <- plot(FeatureEffect$new(Ale, 
                                             feature = "Specific_Leaf_Area", 
                                             method = "ale"))

### extracting the information which contains the ALE values ###

ALE_SLA_GBM <- Specific_leaf_area[["data"]]

## adding a column spelling out the trait ##

ALE_SLA_GBM <- ALE_SLA_GBM %>% 
               mutate(Trait = "SLA(mm2/mg)") %>% 
               rename(Trait_Value = Specific_Leaf_Area)



################
## Leaf C:N ratio ###
##############


#### extracting the information which contains the ALE values #### 

Leaf_C_N_ratio <- plot(FeatureEffect$new(Ale, 
                                         feature = "Leaf.C.N.ratio", 
                                         method = "ale"))


### extracting the information which contains the ALE values ###

ALE_Leaf_C_N_ratio_GBM <- Leaf_C_N_ratio[["data"]]

## adding a column spelling out the trait ##

ALE_Leaf_C_N_ratio_GBM <- ALE_Leaf_C_N_ratio_GBM %>% 
                          mutate(Trait = "Leaf C:N Ratio") %>% 
                          rename(Trait_Value = Leaf.C.N.ratio)


################ 

ALE_GBM <- do.call("rbind",
                   list(ALE_Leaf_C_N_ratio_GBM,
                        ALE_Phyllaries_GBM,
                        ALE_Primary_branches_GBM,
                        ALE_RGB_GBM,
                        ALE_SLA_GBM,
                        ALE_TLN_GBM,
                        ALE_Leaf_total_nitrogen_GBM,
                        ALE_RGB_prop_Red_GBM))



### renaming the columns of this dataframe ###

ALE_GBM <- ALE_GBM %>% rename(Ecoregions = .class,
                              ALE = .value)



### setting levels as per the order of importance (according to Boruta) 
## plots will be faceted from left to right in order of their importances 

ALE_GBM$Trait <- factor(ALE_GBM$Trait, 
                       levels = c("Total RGB","Primary Branches",
                                  "SLA(mm2/mg)","Leaf Number",
                                  "RGB Proportion Red","Leaf Nitrogen",
                                  "Leaf C:N Ratio","Phyllaries Length(cm)"))



#### creating a ggplot to show the ALE values 

ggplot(ALE_GBM, 
       aes(x = Trait_Value, y = ALE)) + 
  geom_line(aes(color = Ecoregions)) +
  facet_grid(Ecoregions ~ Trait,
             scales = "free_x") +
  scale_color_manual(values=c('#619CFF',
                              '#F8766D')) + 
  labs(x = "Trait Value", y = "ALE") +
  theme(text = element_text(size = 9)) +
  theme(legend.position="bottom")



ggsave("Figure 5.svg",
       dpi = 600)    

