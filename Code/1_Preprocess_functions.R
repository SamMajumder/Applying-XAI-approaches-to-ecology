

### writing a function that reshapes the data 
## removes the description column and makes it a wide table from a long table

Data_aggregate <- function(df){
  df <- df %>% select(-description)
  df <- df %>% pivot_wider(names_from = name,
                           values_from = value)
  
  return(df)
}


## writing a function that applies the Data aggregate function to each dataframe 
## in the column of climate data and soil data ##
## aggregates everything and presents the final version

Data_preprocess <- function(df,names_to_remove=NULL) {
  df <- df %>% filter(!population_id %in% names_to_remove) ## removes the populations we don't need
  climate_list <- df$climate_variables ### puts the climate data into a list ## a list of dataframes ##
  soil_list <- df$soil_variables ## same with soil data ###
  df_2 <- map(climate_list,Data_aggregate) ## maps the function Data_aggregate to each df in the list
  df_3 <- do.call(rbind,df_2) ### row binds all the dataframes into one dataframe
  df_4 <- cbind(df,df_3)  ### column binds the original location data to the climate data
  
  
  ## doing the samw operations on the soil data ##
  df_5 <- map(soil_list,Data_aggregate)
  df_6 <- do.call(rbind,df_5)
  df_7 <- cbind(df,df_6)
  df_8 <- df_7 
  df_9 <- inner_join(df_4,df_8)
  
  return(df_9)
}



Dataframe_aggregate <- function(df_1,df_2,df_3,df_4){
  df_5 <- inner_join(df_2,df_1)
  df_6 <- inner_join(df_3,df_5)
  df_7 <- inner_join(df_6,df_4) 
  return(df_7)
}

## this function writes all the file names, puts them in the dataframe and 
## collapses the repeated row names to one ### 

Repeated_files_aggregate <- function(path = path,
                                    pattern= pattern ,full.names = TRUE) {
  #### Now reading in the names of the files in our flower folder ###
  ##### 
  all_paths <- list.files(path = path,
                          pattern = pattern,      ### extension
                          full.names = full.names) 
  
  ### read file names 
  
  all_filenames <- all_paths %>% 
    basename() %>%
    as.list()
  
  #### put them in a nice dataframe ###
  
  Names_flowers <- data.frame(individual_id=unlist(all_filenames))   
  
  ##### Getting rid of part of the name that doesn't relay the genotype info about the plant ##
  ### i.e. removing flowertop_1. or 2 or 3 and the file extension
  
  Names_flowers <- Names_flowers %>% 
    separate(col = individual_id,
             into = c("individual_id","delete"))
  
  ###### delete the "delete column"'
  
  Names_flowers <- Names_flowers %>% 
    select(-c(delete))
  
  ### Nw collapsing the columns with repeated names into one ##
  
  Names_flowers <- Names_flowers %>%
    group_by(individual_id) %>%
    summarise_all(sum)
  
  return(Names_flowers)
}

## this function writes in all the file names and puts them in a dataframe ##

Single_files_aggregate <- function(path = path,
                                   pattern = pattern,full.names = TRUE) {
  ########
  ### Now reading in the names of the leaf image files ###
  ##### 
  
  all_paths <- list.files(path = path,
                          pattern = pattern,      ### extension
                          full.names = full.names)
  
  ### read file names 
  
  all_filenames <- all_paths %>% 
    basename() %>%
    as.list()
  
  #### put them in a nice dataframe ###
  
  Names_leaves <- data.frame(individual_id=unlist(all_filenames))   
  
  ##### Getting rid of part of the name that doesn't relay the genotype info about the plant ##
  ### i.e. removing leaftop and the file extension
  
  Names_leaves <- Names_leaves %>% 
    separate(col = individual_id,into = c("individual_id","delete"))
  
  ###### delete the "delete column"'
  
  Names_leaves <- Names_leaves %>% 
    select(-c(delete))
  
  return(Names_leaves)
  
}

###### Missing value visualize and insight function ###


Missing <- function(df,y){
  ### 
  
  total_cells <- prod(dim(df %>% select(-y))) 
  
  missing_cells <- sum(is.na(df %>% select(-y)))
  
  # calculating percentage of missing values
  percentage_missing <- (missing_cells * 100 )/(total_cells)
  
  print(percentage_missing) 
  
  #### Missing values in the data
  
  Missing_values <- df %>%
    gather(key = "key", value = "val") %>%
    mutate(is.missing = is.na(val)) %>%
    group_by(key, is.missing) %>%
    summarise(number_of_missing_values = n()) %>%
    filter(is.missing==T) %>%
    select(-is.missing) %>%
    arrange(desc(number_of_missing_values)) %>%
    rename(Variables = key)
  
  ### Percent missing for each trait ###
  
  Missing_percent <- Missing_values %>%
                     mutate(proportion_missing = number_of_missing_values/nrow(df)) %>%
                     mutate(percent_missing = proportion_missing * 100)
                     
  
  ## Now make a plot ## percent of missing values for variables ###
  
  p1 <- ggplot(data = Missing_percent,
         aes(x=reorder(Variables,percent_missing), y = percent_missing, 
             fill = "red")) +
         geom_bar(stat = "identity") + 
         labs(x= "Variables", y = "Percent missing") +
         coord_flip() + 
         theme_bw() + theme(legend.position = "none") + 
         ggtitle("Percent Missing") +
         theme(text = element_text(size = 10)) 
  
  print(p1)
  
  results <- list("Missing_values" = Missing_values,
                  "Missing_percent" = Missing_percent,
                  "Percent missing plot" = p1,
                  "Total_missing" = percentage_missing)
  
  return(results)
  
}













