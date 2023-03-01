
### 
rm(list = ls())

##############
########## CREATE RAINCLOUD PLOTS ########
######## VISUALIZING RAW TRAIT VALUES ######

##library(tidyquant)

library(here)
library(tidyverse)
library(ggdist)
library(cowplot)

#### reading in the data ### 

train <- read.csv(here("Processed_Datasets",
                       "train_boruta.csv"))

## Selecting the subset of the data we need ###
## top 8 ## according to Boruta ###

train_raincloud <- train %>% 
                   select(Ecoregions,
                    Specific_Leaf_Area,
                   Total_Leaf_Number,
                    Leaf.C.N.ratio,
                    Leaf.total.N,
                    Phyllaries.length,
                   RGB.proportion.red,
                   Primary.branches,
                   Total.RGB)


### Maybe recode the names to be more terse 
### the title can be bold #### 
### edit the titles and stuff ###

SLA <- ggplot(data = train_raincloud, 
             aes(x = Ecoregions, y = Specific_Leaf_Area, 
                 fill=Ecoregions)) + 
  labs(x = "Ecoregions", 
       y = "SLA (mm2/mg)") +
  stat_halfeye(adjust = 0.5,
               justification = -.2,
               .width = 0,
               point_colour=NA) + 
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #scale_fill_tq() +
  #theme_tq() +
  coord_flip() + 
  #theme(plot.title = element_text(face = "bold")) +
  theme(text = element_text(size = 10)) + 
  theme(legend.position = "None") +
  theme(axis.text.y = element_blank()) 
  

SLA

TLN <- ggplot(data = train_raincloud, 
             aes(x = Ecoregions, y = Total_Leaf_Number, 
                 fill=Ecoregions)) + 
  labs(x = "Ecoregions", 
       y = "Leaf Number") +
  stat_halfeye(adjust = 0.5,
               justification = -.2,
               .width = 0,
               point_colour=NA) + 
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #scale_fill_tq() +
  #theme_tq()  +
  coord_flip() + 
  #theme(plot.title = element_text(face = "bold")) +
  theme(text = element_text(size = 10)) +
  theme(legend.position = "None") +
  theme(axis.text.y = element_blank())
  

TLN

C_N_ratio <- ggplot(data = train_raincloud, 
             aes(x = Ecoregions, y = Leaf.C.N.ratio, 
                 fill=Ecoregions)) + 
  labs(x = "Ecoregions", 
       y = "Leaf C:N Ratio") +
  stat_halfeye(adjust = 0.5,
               justification = -.2,
               .width = 0,
               point_colour=NA) + 
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #scale_fill_tq() +
  #theme_tq() +
  coord_flip() + 
  #theme(plot.title = element_text(face = "bold")) +
  theme(text = element_text(size = 10)) + 
  theme(legend.position = "None") +
  theme(axis.text.y = element_blank()) 
  
C_N_ratio

Total_N <- ggplot(data = train_raincloud, 
                  aes(x = Ecoregions, 
                      y = Leaf.total.N, 
                      fill=Ecoregions)) + 
  labs(x = "Ecoregions", 
       y = "Leaf Nitrogen") +
  stat_halfeye(adjust = 0.5,
               justification = -.2,
               .width = 0,
               point_colour=NA) + 
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #scale_fill_tq() +
  #theme_tq() +
  coord_flip() + 
  #theme(plot.title = element_text(face = "bold")) +
  theme(text = element_text(size = 10)) + 
  theme(legend.position = "None") +
  theme(axis.text.y = element_blank()) 

  
Total_N 

Phyllaries_length <- ggplot(data = train_raincloud, 
             aes(x = Ecoregions, 
                 y = Phyllaries.length, 
                 fill=Ecoregions)) + 
  labs(x = "Ecoregions", 
       y = "Phyllaries Length(cm)") +
  stat_halfeye(adjust = 0.5,
               justification = -.2,
               .width = 0,
               point_colour=NA) + 
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #scale_fill_tq() +
  #theme_tq() +
  coord_flip() + 
  #theme(plot.title = element_text(face = "bold")) +
  theme(text = element_text(size = 10)) + 
  theme(legend.position = "None") +
  theme(axis.text.y = element_blank()) 

Phyllaries_length

RGB_proportion_red <- ggplot(data = train_raincloud, 
             aes(x = Ecoregions, 
                 y = RGB.proportion.red, 
                 fill=Ecoregions)) + 
  labs(x = "Ecoregions", 
       y = "RGB Proportion Red") +
  stat_halfeye(adjust = 0.5,
               justification = -.2,
               .width = 0,
               point_colour=NA) + 
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #scale_fill_tq() +
  #theme_tq() +
  coord_flip() + 
  #theme(plot.title = element_text(face = "bold")) +
  theme(text = element_text(size = 10)) + 
  theme(legend.position = "None") +
  theme(axis.text.y = element_blank()) 


RGB_proportion_red


Primary_Branches <- ggplot(data = train_raincloud, 
             aes(x = Ecoregions, 
                 y = Primary.branches, 
                 fill=Ecoregions)) + 
  labs(x = "Ecoregions", 
       y = "Primary Branches") +
  stat_halfeye(adjust = 0.5,
               justification = -.2,
               .width = 0,
               point_colour=NA) + 
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #scale_fill_tq() +
  #theme_tq() +
  coord_flip() + 
  #theme(plot.title = element_text(face = "bold")) +
  theme(text = element_text(size = 10)) + 
  theme(legend.position = "None") +
  theme(axis.text.y = element_blank()) 


Primary_Branches

Total_RGB <- ggplot(data = train_raincloud, 
             aes(x = Ecoregions, 
                 y = Total.RGB, 
                 fill=Ecoregions)) + 
  labs(x = "Ecoregions", 
       y = "Total RGB") +
  stat_halfeye(adjust = 0.5,
               justification = -.2,
               .width = 0,
               point_colour=NA) + 
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #scale_fill_tq() +
  #theme_tq() +
  coord_flip() + 
  #theme(plot.title = element_text(face = "bold")) +
  theme(text = element_text(size = 10)) + 
  theme(legend.position = "None") +
  theme(axis.text.y = element_blank()) 



### legend ##

legend_b <- get_legend(Total_RGB + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom"))

############ 

rain_cloud <- plot_grid(Total_RGB,Primary_Branches,SLA,
              TLN,RGB_proportion_red,Total_N,
              C_N_ratio,Phyllaries_length,
              legend_b, ncol = 3)


rain_cloud


ggsave("Figure 6.svg",
       dpi = 600)



