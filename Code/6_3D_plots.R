rm(list = ls())

packages <- list("tidyverse","plotly","here","htmlwidgets")


lapply(packages, require,character.only=T)


### 3D PLOTS ### GENUS ###

### reading in data #### 

train_imputed <- read_csv("Datasets and Tables","train_imputed.csv")

colnames(train_imputed)

### 3D PLOTLY PLOTS ###

plot <- plot_ly(train_imputed,x= ~TLN,y= ~Disk.diameter,z= ~Primary.branches, color = ~ecotype
) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = 'Total Leaf Number'),
                 yaxis = list(title = 'Flower disk diameter'),
                 zaxis = list(title = 'Number of primary branches'))
  )

plot


saveWidget(plot,"Figure 5.html")
