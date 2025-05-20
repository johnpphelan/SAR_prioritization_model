library(openxlsx)
library(readxl)
library(tidyverse)
library(bcdata)
library(sf)
library(DBI)
library(bcinvadeR)
library(terra)
library(geodata)
library(predicts)
library(ggpubr)
library(dismo)
library(rJava)
library(patchwork)
library(ecospat)
library(fwa.connect)
library(tidyterra)
library(tidyr)
library(BAMMtools) # Used to find natural breaks ("jenks")
library(basemaps)
library(bcmaps)
library(dplyr)
library(stringr)
library(ggplot2)

source("scripts/utils/load_data.R")

# Try making some plots!
dfo_sar_4326 = sf::st_transform(dfo_sar, 4326)

dfo_extent = sf::st_bbox(dfo_sar_4326) |>
  sf::st_as_sfc() |>
  sf::st_sf() |>
  sf::st_set_crs(4326)

species_maps = dfo_sar_4326 |>
  dplyr::group_by(Common_Name_EN) |>
  dplyr::group_split() |>
  purrr::map( ~ {
 
    
    buffered_bbox = sf::st_bbox(.x) |>
      sf::st_as_sfc() |>
      sf::st_as_sf() |>
      sf::st_transform(3005) |>
      sf::st_buffer(dist = 500)
    
    the_ocean_bm = basemaps::basemap_terra(ext = buffered_bbox,
                                           map_service = "carto",
                                           map_type = "light")
    
    
    ## arrow for the small polygon - calculate the size for RMRM?
    # and add a label geom_sf_label()
    ggplot() +
      tidyterra::geom_spatraster_rgb(data = the_ocean_bm) +
      geom_sf(data = .x,
              aes(fill = Common_Name_EN), alpha = 0.8, linewidth = 1) +
      ggtitle(case_when(
        .x$Common_Name_EN[1] == "Cultus Pygmy Sculpin" ~ "Coastrange Sculpin - Cultus",
        .x$Common_Name_EN[1] == "Westslope Cutthroat Trout" ~ "Westslope Cutthroat Trout",
        .x$Common_Name_EN[1] == "Bull Trout" ~ "Bull Trout - South Coast",
        .x$Common_Name_EN[1] == "Rocky Mountain Ridged Mussel" ~ "Rocky Mountain Ridged Mussel",
        .x$Common_Name_EN[1] == "Sockeye Salmon" ~ "Sockeye Salmon - Cultus",
        TRUE ~ .x$Common_Name_EN[1]
      ))+
      ggthemes::theme_map() +
      theme(legend.position = 'none',
            plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
  })

all_my_maps = Reduce(`+`, species_maps)


inset_map = ggplot() +
  geom_sf(data = bcmaps::bc_bound(), fill = "white") +
  geom_sf(data = dfo_extent, color = "red", fill = NA, linewidth = 1) +
  ggthemes::theme_map()

all_my_maps + inset_map
