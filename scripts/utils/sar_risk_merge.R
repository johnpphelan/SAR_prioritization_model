library(tidyverse)
library(sf)
library(ggplot2)

get_status<-function(){
  
  risk_data = read.csv(file.path(here::here(), "../AEB_CCzembor_SAR_Data_Scraper/output/risk_status_merged.csv"))
  
  sara_locs = st_read(here::here("data","sara_filtered_frasColum.gpkg")) |> st_drop_geometry()
  
  
  overlap = risk_data |> 
    filter(Scientific.name %in% sara_locs$Scientific_Name)
  
  
  # unique(overlap$Legal.population)
  # unique(sara_locs$Population_EN)
  
  #overlap[grep(sara_locs$Population_EN[37], overlap$Legal.population), ]
  
  
  sara_locs = sara_locs |> 
    dplyr::mutate(Population_EN = dplyr::if_else(
      is.na(Population_EN), 
      "NA", 
      Population_EN
    ))
  
  overlap = overlap |> 
    dplyr::mutate(Legal.population = dplyr::if_else(
      is.na(Legal.population), 
      "NA", 
      Legal.population
    ))
  
  
  overlap_selected =  overlap |> 
    filter(map_lgl(Legal.population, ~ any(str_detect(.x, sara_locs$Population_EN))) | Scientific.name %in% "Gonidea angulata") |> 
    select(COSEWIC.common.name, COSEWIC.status, Scientific.name, Legal.population)
  
  
  return(overlap_selected)
}