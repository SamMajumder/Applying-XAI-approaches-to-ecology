

rm(list = ls())

packages <- list("iml","patchwork","FRACTION",
                 "sf","jsonlite","ggspatial",
                 "here","tidyverse")


lapply(packages, require,character.only =T)


################################
####  #####
############################

### reading in the predicted versus actual table for random forest 


predicted_versus_actual_Rf <- read.csv(here("Processed_Datasets",
                                            "predicted_versus_actual_Rf.csv"))


### removing the index column ###

predicted_versus_actual_Rf <- predicted_versus_actual_Rf %>% 
                              select(-X)


###############
##### RF ####
########

C_v_I_predicted_RF <- predicted_versus_actual_Rf %>%
                      group_by(population_id) %>%
                      count(Outcome)  

#### pivot wider to create separate columns for correctly 
### and incorrectly predicted ##
C_v_I_predicted_RF <- C_v_I_predicted_RF %>% 
                     pivot_wider(names_from = Outcome,
                     values_from = n,
                     values_fill = 0)

### Adding a another column which expresses how many were correctly predicted 
### in a fraction ### this will be important for mapping ##
### making sure we are recoding the 1/1 and 0/1 into 1 and 0 respectively 


C_v_I_predicted_RF <- C_v_I_predicted_RF %>% 
  mutate(Prop_correct = fra.m(Correctly_Predicted/(Correctly_Predicted + Incorrectly_Predicted))) %>% 
  mutate(Prop_correct = recode(Prop_correct,
                               "1 / 1" = "All",
                               "0 / 1" = "None"))


#### Arranging the order of Prop correct so that we can get the facets right ##

C_v_I_predicted_RF$Prop_correct <- factor(C_v_I_predicted_RF$Prop_correct,
                                          levels = c("All","4 / 5","3 / 4",
                                                     "2 / 3", "1 / 2","1 / 4",
                                                     "None")) 







###### Add the coordinates ###

### download the json file ## This file contains populations 
## along with some climate and soil variables ###

Location <- fromJSON("http://www.helianthome.org/rest/population/list.json")

#### Only keeping the Helianthus annuus in this file ###

Location <- Location %>%
  filter(species == "Helianthus annuus")


### Extracting the coordinates ###

Population_coordinates <- Location %>% 
  dplyr::select(population_id,longitude,latitude,
                sitename)


### Adding the coordinates ###

C_v_I_predicted_RF <- C_v_I_predicted_RF %>% 
  left_join(Population_coordinates) 


#### write out the file containing the predictions and coordinates for mapping

write.csv(C_v_I_predicted_RF,
          "~/Applying-XAI-approaches-to-ecology/Processed_Datasets/C_v_I_predicted__RF.csv",
          row.names = FALSE)



#### see where the model got it wrong according to RF ### 
## and indicate that with either a correctly predicted versus incorrectly predicted 

## Preparing to visualize these points on a map ## 

### Convert the C_v_I_predicted file into an sf object ###

C_v_I_predicted_RF_sf <- st_as_sf(C_v_I_predicted_RF,
                                  coords = c("longitude","latitude"), 
                                  crs = 4326) %>% 
                            ### this keeps the coordinates in the dataframe
                              inner_join(C_v_I_predicted_RF)



## read in the ecoregion shape file ### 
## and select specific columns ## 
Ecoregions_shape <- st_read(here("Shape_files","na_cec_eco_l1",
                                 "NA_CEC_Eco_Level1.shp")) %>% 
                                 rename(Ecoregions = NA_L1NAME) 


### filtering out by Ecoregions ###
### keeping only North American Deserts and Great Plains ecoregions ##

our_ecotypes <- c("NORTH AMERICAN DESERTS","GREAT PLAINS")

Ecoregions_shape <- Ecoregions_shape %>% 
                    filter(Ecoregions %in% our_ecotypes)



### plotting the points according to the coordinate system of the ecoregions ###
### Visualizinhg on the map ## get rid of the background ###
### add labels ## the title needs to be bold ##

ggplot() +
  geom_sf(data = Ecoregions_shape,
          mapping = aes(color=Ecoregions)) +
  geom_sf(data = C_v_I_predicted_RF_sf) +
  coord_sf(datum = st_crs(C_v_I_predicted_RF_sf)) +
  facet_wrap(~Prop_correct) +
  labs(x='Longitude',
       y='Latitude') +
  theme(plot.title = element_text(face = "bold")) +
  theme(panel.background = element_blank()) +
  theme(text = element_text(size = 10)) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave("~/Applying-XAI-approaches-to-ecology/Figure S2.svg",
       dpi=1000)




####### GBM ####

### reading in the predicted versus actual table for random forest 

predicted_versus_actual_GBM <- read.csv(here("Processed_Datasets",
                                            "predicted_versus_actual_GBM.csv"))


### removing the index column ###

predicted_versus_actual_GBM <- predicted_versus_actual_GBM %>% 
                              select(-X)


### Creating a table that shows how many individuals were correctly predicted 
### versus how many were incorrectly predicted ### per population ### 

### grouping them by population_id
C_v_I_predicted_GBM <- predicted_versus_actual_GBM %>%
                       group_by(population_id) %>%
                       count(Outcome)  

#### pivot wider to create separate columns for correctly 
### and incorrectly predicted ##
C_v_I_predicted_GBM <- C_v_I_predicted_GBM %>% 
                      pivot_wider(names_from = Outcome,
                      values_from = n,
                      values_fill = 0)

### Adding a another column which expresses how many were correctly predicted 
### in a fraction ### this will be important for mapping ##
### making sure we are recoding the 1/1 and 0/1 into 1 and 0 respectively 


C_v_I_predicted_GBM <- C_v_I_predicted_GBM %>% 
  mutate(Prop_correct = fra.m(Correctly_Predicted/(Correctly_Predicted + Incorrectly_Predicted))) %>% 
  mutate(Prop_correct = recode(Prop_correct,
                               "1 / 1" = "All",
                               "0 / 1" = "None"))  


#### Arranging the order of Prop correct so that we can get the facets right ##

C_v_I_predicted_GBM$Prop_correct <- factor(C_v_I_predicted_GBM$Prop_correct,
                                          levels = c("All","4 / 5","3 / 4",
                                                     "2 / 3", "1 / 2","1 / 4",
                                                     "None")) 



###### Add the coordinates ###

C_v_I_predicted_GBM <- C_v_I_predicted_GBM %>% 
                      left_join(Population_coordinates) 


#### write out the file containing the predictions and coordinates for mapping ##

write.csv(C_v_I_predicted_GBM,
          "~/Applying-XAI-approaches-to-ecology/Processed_Datasets/C_v_I_predicted_GBM.csv",
          row.names = FALSE)



#### see where the model got it wrong ### 
## and indicate that with either a correctly predicted versus incorrectly predicted 

## Preparing to visualize these points on a map ## 

### Convert the C_v_I_predicted file into an sf object ###

C_v_I_predicted_GBM_sf <- st_as_sf(C_v_I_predicted_GBM,
                                   coords = c("longitude","latitude"), 
                                   crs = 4326) %>% 
                         ### this keeps the coordinates in the dataframe
                            inner_join(C_v_I_predicted_GBM)



### plotting the points according to the coordinate system of the ecoregions ###
### Visualizing on the map ## get rid of the background ###
### add labels ## the title needs to be bold ##

ggplot() +
  geom_sf(data = Ecoregions_shape,
          mapping = aes(color=Ecoregions)) +
  geom_sf(data = C_v_I_predicted_GBM_sf) +
  coord_sf(datum = st_crs(C_v_I_predicted_GBM_sf)) +
  facet_wrap(~Prop_correct) +
  labs(x='Longitude',
       y='Latitude') +
  theme(plot.title = element_text(face = "bold")) +
  theme(panel.background = element_blank()) +
  theme(text = element_text(size = 10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave("~/Applying-XAI-approaches-to-ecology/Figure 4.svg",
       dpi=1000)



