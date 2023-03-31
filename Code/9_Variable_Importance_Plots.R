
rm(list=ls())

packages <- list("tidyverse","here","ggplot2","RColorBrewer")

lapply(packages, require, character.only=T)

Optimal_variables <- read.csv(here("Processed_Datasets",
                                   "Rfe_best_subset_imp.csv"))


Optimal_variables <- Optimal_variables %>% 
        mutate(Features = str_replace_all(Features,"\\."," ")) %>% 
        mutate(Features = str_replace_all(Features, "_"," ")) %>% 
        mutate(Features = recode(Features, "Days to flowering" = "DTF")) %>% 
        mutate(Features = recode(Features, "Darker axillae1" = "Darker axillae")) %>% 
        mutate(Features = recode(Features, "Total Leaf Number" = "TLN")) %>% 
        mutate(Features = recode(Features, "Specific Leaf Area" = "SLA")) %>% 
        mutate(Trait_type = case_when(str_detect(Features,"Leaf") ~ "Leaf",
                                      str_detect(Features, "Seed") ~ "Seed",
                                      str_detect(Features, "RGB") ~ "Leaf",
                                      str_detect(Features, "SLA") ~ "Leaf",
                                      str_detect(Features, "Primary branches") ~ "Plant architecture",
                                      str_detect(Features, "Distance of first branching from ground") ~ "Plant architecture",
                                      str_detect(Features, "TLN") ~ "Plant architecture",
                                      str_detect(Features, "Stem") ~ "Stem",
                                      str_detect(Features, "Flower") ~ "Floral",
                                      str_detect(Features, "Ligule") ~ "Floral",
                                      str_detect(Features, "axillae") ~ "Floral",
                                      str_detect(Features, "phyllaries") ~ "Floral",
                                      str_detect(Features, "Phyllaries") ~ "Floral",
                                      str_detect(Features, "Disk") ~ "Floral",
                                      str_detect(Features, "DTF") ~ "Phenology",
                                      str_detect(Features, "Plant height at flowering") ~ "Phenology",
                                      str_detect(Features, "Days to budding") ~ "Phenology"))
        
          

Optimal_variables$Trait_type <- factor(Optimal_variables$Trait_type, 
                                   levels = c("Leaf","Plant architecture",
                                              "Floral","Seed","Phenology",
                                              "Stem"))


#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(8, 'Set2') ## displaying the colors


brewer.pal(8, 'Set2') ## displaying the hex codes 


####### Create a variable importance plot ####

ggplot(data = Optimal_variables,
                 aes(x=reorder(Features,Overall), 
                     y = Overall, 
                     fill = Trait_type)) +
               geom_bar(stat = "identity",
                        color ="black") + 
               scale_fill_manual(values = c("#A6D854",
                                            "#8DA0CB",
                                            "#FFD92F" ,
                                            "#E5C494",
                                            "#E78AC3",
                                            "#FC8D62"),
                                 name= 'Trait type') +
               labs(x= "HeliantHOME Traits",
                    y= "Variable Importance(Mean Decrease of Accuracy)") +
               coord_flip() + 
               theme_bw() + theme(legend.position = "right") +
               theme(text = element_text(size = 10))
               



ggsave("Figure 3.svg",dpi = 1000)






