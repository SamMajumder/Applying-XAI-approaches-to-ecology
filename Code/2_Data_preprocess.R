

rm(list=ls())


### these are the packages we need ##
packages <- list("jsonlite","tidyverse","here","randomForest",
                 "caret","readxl","ggspatial","sf")


## lets load all the packages ###
lapply(packages, require,character.only=T)

source(here("Code","1_Functions.R"))

###### Now reading in the file containing the Genotype and corresponding population info 
## and only retaining the files that is in our study ###

Population_Genotype <- read.csv(here("Raw_Datasets_and_Tables",
                                     "Population_Genotype.csv"))

### Only keeping Helianthus Annuus 
## because our study focuses only on this species ##

Population_Genotype <- Population_Genotype %>% 
                       filter(species == "Helianthus annuus")


## writing this file out ###

write.csv(Population_Genotype,
          "~/Applying-XAI-approaches-to-ecology/Processed_Datasets/Population_coordinates.csv",
          row.names = FALSE)

#####
##### Adding the trait data to the file ##
###### 

Trait_data <- read.csv(here("Raw_Datasets_and_Tables","Trait_data.csv"))

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


#### writing the coordinate file out as a csv ###

write.csv(Population_coordinates,
      "~/Applying-XAI-approaches-to-ecology/Processed_Datasets/Population_coordinates.csv",
      row.names=FALSE)

############## experiment ### this worked ##

### Convert the Population coordinates into an sf object ###

Population_coordinates_sf <- st_as_sf(Population_coordinates,
                             coords = c("longitude","latitude"), crs = 4326) %>% 
                             inner_join(Population_coordinates)
  
## read in the ecoregion shape file ###
Ecoregion_shape <- st_read(here("Shape_files","na_cec_eco_l1",
                                "NA_CEC_Eco_Level1.shp"))



## set coordinate system of the point locations to the coordinate system of the ecoregions ##

Population_coordinates_sf <- st_transform(Population_coordinates_sf,
                                          crs = st_crs(Ecoregion_shape))

###perform a spatial join ### 


### trying to perform a spatial join ### 

Population_ecoregions <- Population_coordinates_sf %>% 
                         st_join(Ecoregion_shape,
                         join = st_intersects,
                         left = TRUE)



### converting the Population_ecoregions to a dataframe ###

Study_ecoregions <- Population_ecoregions %>% st_drop_geometry() %>% 
                    select(-c(NA_L1CODE,NA_L1KEY,Shape_Leng,Shape_Area)) %>% 
                    rename(Ecoregions = NA_L1NAME)



### Now filtering out the ecoregions that are not part of this dataset ##

our_ecotypes <- c("NORTH AMERICAN DESERTS","GREAT PLAINS")

Study_ecoregions <- Study_ecoregions %>% filter(Ecoregions %in% our_ecotypes)

### Joining this to our dataset containing the functional traits, population and genotype info ##
### 

Data <- inner_join(Study_ecoregions,Data)

###############
#### Now only keeping the columns which we require ####
#############

Data <- Data %>%select(-c(species,num_phenotypes))


###################################
####### Trying to visualize the points on the map ### 
#################

### First lets filter the shapefile to only contain the ecoregions we need ##


Ecoregion_shape_our_ecotype <- Ecoregion_shape %>% 
                               rename(Ecoregion = NA_L1NAME) %>% 
                               filter(Ecoregion %in% our_ecotypes)


### Convert the Study ecoregions into an sf object ###

Study_ecoregions_sf <- st_as_sf(Study_ecoregions,
                                      coords = c("longitude","latitude"), 
                                      crs = 4326) %>% 
                                      inner_join(Study_ecoregions)


#### Plotting it on a map ### ## use aes(color = Ecoregion for outline) 
### or aes(fill = Ecoregion) to fill the polygon

ggplot() +
  geom_sf(data = Ecoregion_shape_our_ecotype, 
          mapping = aes(fill = Ecoregion)) + 
  geom_sf(data = Study_ecoregions_sf) + 
  coord_sf(datum = st_crs(Study_ecoregions_sf)) +
  ggtitle("Wild Helianthus annus populations within the Great Plains and North American Deserts") +
  labs(x='Longitude',
       y='Latitude') +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  theme(plot.title = element_text(face = "bold")) +
  theme(panel.background = element_blank()) +
  theme(text = element_text(size = 10))



ggsave("~/Applying-XAI-approaches-to-ecology/Figure 1.svg",
       dpi=1000)

#######################
#### Exploring the missing data ###
################## 

#### Viewing the missing  values in the dataframe ##


columns_to_remove <- c("Ecoregions","individual_id","genotype_id",
                       "population_id","latitude","longitude","sitename")

Missing_data_results <- Missing(Data,columns_to_remove)

## Extracting the plot ###

Missing_data_results[["Percent missing plot"]]

## saving it ###

ggsave("Figure S1.svg",dpi = 1000)


### Extracting the table which contains percent missing values for each variable

Missing_percent <- Missing_data_results[["Missing_percent"]]

write.csv(Missing_percent,
          "C:/Users/samba/Documents/Applying-XAI-approaches-to-ecology/Processed_Datasets/Missing_percent_table.csv",
          row.names = F)

#### removing the variable which doesn't have any data ##

Data <- Data %>% select(-Peduncle.length.of.first.flower)

### Writing out this dataframe ###

write.csv(Data,"C:/Users/samba/Documents/Applying-XAI-approaches-to-ecology/Processed_Datasets/Total_Data.csv",
          row.names = F)

#### Now getting the names of the training and test data for both classes 
### 

### Converting the Darker axillae and ecotype to factor ###

Data$Ecoregions <- factor(Data$Ecoregions)
Data$Darker.axillae <- factor(Data$Darker.axillae)


##### Divide the data into training and test ##

s <- createDataPartition(y=Data$Ecoregions,p=0.70,list=F)
train <- Data[s,]
test <- Data[-s,]


###################
###### There is some missing data ## lets impute it ###
############

### extracting the individual id, population and genotype id column
### along with latitude, longitude and sitename ###

train_ids <- train %>% select(individual_id,Ecoregions,population_id,
                              genotype_id,latitude,longitude,sitename) 

test_ids <- test %>% select(individual_id,Ecoregions,population_id,
                            genotype_id,latitude,longitude,sitename)


### Remove the individual id from training and test dataframe ##
###

train <- train %>% select(-c(individual_id,population_id,
                             genotype_id,latitude,longitude,sitename))

test <- test %>% select(-c(individual_id,population_id,
                           genotype_id,latitude,longitude,sitename))


####### Imputing the values ###

set.seed(1234)
train_imputed <- rfImpute(Ecoregions~.,train)
test_imputed <- rfImpute(Ecoregions~.,test)

#### Now exporting the train and the test imputed data as csv files ###
##########

write.csv(train_imputed,
          "C:/Users/samba/Documents/Applying-XAI-approaches-to-ecology/Processed_Datasets/train_imputed.csv",
          row.names = F)

write.csv(test_imputed,
          "C:/Users/samba/Documents/Applying-XAI-approaches-to-ecology/Processed_Datasets/test_imputed.csv",
          row.names = F)


######### Now exporting the train ids and the test ids ###

write.csv(train_ids,
          "C:/Users/samba/Documents/Applying-XAI-approaches-to-ecology/Processed_Datasets/train_ids.csv",
          row.names = FALSE)  


write.csv(test_ids,
          "C:/Users/samba/Documents/Applying-XAI-approaches-to-ecology/Processed_Datasets/test_ids.csv",
          row.names = FALSE)












