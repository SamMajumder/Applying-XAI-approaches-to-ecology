

rm(list=ls())

### these are the packages we need ##
packages <- list("jsonlite","tidyverse","here","randomForest",
                 "caret")


## lets load all the packages ###
lapply(packages, require,character.only=T)

source("Preprocess_functions.R")

### read in the names of flowers and leaves in our study ###

#### Adding the flower file names ##
### WHEN RECREATING THIS ANALYSIS, PLEASE FEEL FREE TO RESET THE RELATIVE PATHS FOR THE IMAGES ###

## but first we have to set a few argument values for our custom function
path = 'D:/Images/HeliantOME/flowertop_Sam_Curated_11_18_2022' 
pattern = "*.JPG"
full.names =TRUE

## Applying the function and creating a dataframe with the flower file names
Names_flowers <- Repeated_files_aggregate(path,pattern,full.names)

### Doing the same thing with leaf file images
path = 'D:/Images/HeliantOME/leaftop_Sam_curated'
pattern = "*.png"

Names_leaves <- Single_files_aggregate(path,pattern,full.names)


### Now Only keeping the ones that are common in both flowers and leaf folders ##

Names <- inner_join(Names_flowers,Names_leaves)

###### Now reading in the file containing the Genotype and corresponding population info 
## and only retaining the files that is in our study ###

Population_Genotype <- read.csv(here("Datasets and Tables","Population_Genotype.csv"))

Population_Genotype <- inner_join(Names,Population_Genotype)

### Only keeping Helianthus Annuus 
## because our study focuses only on this species ##

Population_Genotype <- Population_Genotype %>% 
                       filter(species == "Helianthus annuus")


#####
##### Adding the trait data to the file ##
###### 

Trait_data <- read.csv(here("Datasets and Tables","Trait_data.csv"))

Data <- inner_join(Population_Genotype,Trait_data)

#################
## Now we need to know what ecotypes these individuals belong to ###
#####################


### For that first we need is the coordinates of the populations ####

### download the json file ## This file contains populations 
## along with some climate and soil variables ###

Location <- fromJSON("http://www.helianthome.org/rest/population/list.json")

#### Only keeping the Helianthus annuus in this file ###

Location <- Location %>%
            filter(species == "Helianthus annuus")


### Extracting the coordinates ###

Population_coordinates <- Location %>% 
                          select(population_id,species,longitude,sitename,
                                 latitude)

### writing out this file as a csv ##
## we will then open this csv on a map using ArcGIS and figure out what ecotypes each populations belong to ###
#######

write.csv(Population_coordinates,"C:/Users/samba/Documents/Chapter_3_Traditional_ML_modeling/Datasets and Tables/Population_coordinates.csv",row.names = FALSE)

###############
#### Now reading in the file that contains the Population, coordinates and ecotype info ###
#######

Ecotypes <- read.csv(here("Datasets and Tables","Population_Ecotype.csv"))

### Joining this to our dataset containing the functional traits, population and genotype info ##
### 

Data <- inner_join(Ecotypes,Data)

#### filtering out the Data based on the ecotypes in our study ###

our_ecotypes <- c("north american desert","the great plains")

Data <- Data %>% filter(ecotype %in% our_ecotypes)

###############
#### Now only keeping the columns which we require ####
#############

Data <- Data %>% select(-c(species,genotype_id,
                           num_phenotypes,longitude,sitename,latitude))


#######################
#### Exploring the missing data ###
################## 

#### Viewing the missing  values in the dataframe ##

Missing_data_results <- Missing(Data,"ecotype")

## Extracting the plot ###

Missing_data_results[["Percent missing plot"]]

## saving it ###

ggsave("Figure S1.svg",dpi = 300)


### Extracting the table which contains percent missing values for each variable

Missing_percent <- Missing_data_results[["Missing_percent"]]

write.csv(Missing_percent,"C:/Users/samba/Documents/Chapter_3_Traditional_ML_modeling/Datasets and Tables/Missing_percent_table.csv",row.names = F)

#### removing the variable which doesn't have any data ##

Data <- Data %>% select(-Peduncle.length.of.first.flower)

### Writing out this dataframe ###

write.csv(Data,"C:/Users/samba/Documents/Chapter_3_Traditional_ML_modeling/Datasets and Tables/Total_Data.csv",row.names = F)

#### Now getting the names of the training and test data for both classes 
### 

### Converting the Darker axillae and ecotype to factor ###

Data$ecotype <- factor(Data$ecotype)
Data$Darker.axillae <- factor(Data$Darker.axillae)


##### Divide the data into training and test ##

s <- createDataPartition(y=Data$ecotype,p=0.70,list=F)
train <- Data[s,]
test <- Data[-s,]

#### now extracting the individual ids for both training and test along with what ecotype region they belong to##

train_ids <- train %>% select(individual_id,ecotype)

test_ids <- test %>% select(individual_id,ecotype)

train_desert_ids <- train_ids %>% filter(ecotype == "north american desert")

test_desert_ids <- test_ids %>% filter(ecotype == "north american desert")

train_great_plains_ids <- train_ids %>% filter(ecotype == "the great plains")

test_great_plains_ids <- test_ids %>% filter(ecotype == "the great plains")

#### Now writing out the training and test names 

write.csv(train_desert_ids,"C:/Users/samba/Documents/Chapter_3_Traditional_ML_modeling/Datasets and Tables/train_desert_names.csv",row.names = F)

write.csv(test_desert_ids,"C:/Users/samba/Documents/Chapter_3_Traditional_ML_modeling/Datasets and Tables/test_desert_names.csv",row.names = F)


write.csv(train_great_plains_ids,"C:/Users/samba/Documents/Chapter_3_Traditional_ML_modeling/Datasets and Tables/train_great_plains_names.csv",row.names = F)

write.csv(test_great_plains_ids,"C:/Users/samba/Documents/Chapter_3_Traditional_ML_modeling/Datasets and Tables/test_great_plains_names.csv",row.names = F)


###################
###### There is some missing data ## lets impute it ###
############

### first removing population_id and individual id from training and test dataframe ##
###

train <- train %>% select(-c(population_id,individual_id))

test <- test %>% select(-c(population_id,individual_id))


####### Imputing the values ###

set.seed(1234)
train_imputed <- rfImpute(ecotype~.,train)
test_imputed <- rfImpute(ecotype~.,test)

#### Now exporting the train and the test imputed data as csv files ###
##########

write.csv(train_imputed,"C:/Users/samba/Documents/Chapter_3_Traditional_ML_modeling/Datasets and Tables/train_imputed.csv",row.names = F)

write.csv(test_imputed,"C:/Users/samba/Documents/Chapter_3_Traditional_ML_modeling/Datasets and Tables/test_imputed.csv",row.names = F)


############### AS COOL EXTRA STEP ### 
############### I WILL ALSO BE EXTRACTING THE SOIL AND THE CLIMATE DATA FROM THE JSON file ###
############## 

### In this work we are removing population ANN 71 and PET 21 because they don't have soil data associated with them ## 
### SAM is cultivated and doesn't have soil or climate data associayted with it

names_to_remove <- c("ANN_71","PET_21","SAM")


#### applying the functions on the dataframe and removing the soil and climate variable list columns ##


Location_climate_soil <- Data_preprocess(Location,names_to_remove=names_to_remove) %>%
                         select(-c(climate_variables,soil_variables))



#### removing the variable Peduncle length of first flower ##
## because it has all its values missing values ### 

### Extracting the metadata about climate and soil ##

Climate <- Location[[10]][[1]]

Climate <- Climate %>% select(name,description)

Soil <- Location[[11]][[1]] 

Soil <- Soil %>% select(name,description)

### Writing out the metadata and the dataset ###

write.csv(Location_climate_soil,"C:/Users/samba/Documents/Chapter_3_Traditional_ML_modeling/Datasets and Tables/Combined_Data.csv",row.names = FALSE)

write.csv(Climate,"C:/Users/samba/Documents/Chapter_3_Traditional_ML_modeling/Datasets and Tables/Climate_variables.csv", row.names = FALSE)

write.csv(Soil,"C:/Users/samba/Documents/Chapter_3_Traditional_ML_modeling/Datasets and Tables/Soil_variables.csv", row.names = FALSE)















