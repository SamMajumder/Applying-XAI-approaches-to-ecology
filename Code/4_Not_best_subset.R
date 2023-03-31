
rm(list=ls())

library(tidyverse)
library(here)
## Traits that weren't a part of the optimal subset of relevant traits ##
# According to RFE ###

Rfe_best_subset <- read.csv(here("Processed_Datasets","Rfe_best_subset.csv"))

Rfe_best_subset$Features[Rfe_best_subset$Features == "Darker.axillae1"] <- "Darker.axillae"

Best_subset <- data.frame(Features = Rfe_best_subset$Features)

Trait_data <- read.csv(here("Raw_Datasets_and_Tables","Trait_data.csv"))

### removing the column individual id ###

Trait_data <- Trait_data %>% select(-c(individual_id))


### renaming the names of the columns to match the Rfe_best_subset ##

Trait_data <- Trait_data %>% 
              rename(Total_Leaf_Number = TLN) %>%
              rename(Days_to_flowering = DTF) %>%
              rename(Specific_Leaf_Area = SLA)


### Now extracting the column names ###

Traits <- data.frame(Features = colnames(Trait_data))


### Not part of the optimal subset ##

Not_best_subset <- anti_join(Traits,Best_subset)

### exporting this out as a .csv file ###

write.csv(Not_best_subset,
          "C:/Users/samba/Documents/Applying-XAI-approaches-to-ecology/Processed_Datasets/Not_best_subset.csv",
          row.names = F)




