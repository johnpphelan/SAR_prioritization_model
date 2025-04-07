library(tidyverse)
library(sf)
library(ggplot2)

get_status<-function(){

risk_data = read.csv("../SAR_scraper/output/risk_status_merged.csv")

sara_locs = st_read("data/sara_filtered_frasColum.gpkg") |> st_drop_geometry()


overlap = risk_data |> 
        filter(Scientific.name %in% sara_locs$Scientific_Name)


# unique(overlap$Legal.population)
# unique(sara_locs$Population_EN)

#overlap[grep(sara_locs$Population_EN[37], overlap$Legal.population), ]



overlap_selected =  overlap |> 
  filter(map_lgl(Legal.population, ~ any(str_detect(.x, sara_locs$Population_EN))) | Scientific.name %in% "Gonidea angulata") |> 
  select(COSEWIC.common.name, COSEWIC.status, Scientific.name, Legal.population)


return(overlap_selected)
}