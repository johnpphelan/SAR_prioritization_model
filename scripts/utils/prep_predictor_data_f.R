prep_predictor_data = function(proj_path,
                               onedrive_path,
                               ext_vect){
  
  print("Reading in rasters...")
  
  # Make {terra} vector of BC.
  bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
  
  # Pull in climate variables
  ph_NAM = terra::rast(paste0(onedrive_path,"CNF/ph-KR-208784-median_10km_ZN.tif")) |> terra::project("EPSG:4326")
  names(ph_NAM) <- "pH"
  Calc_NAM = terra::rast(paste0(onedrive_path,"CNF/calcium-KR-97648-median-10km-ZN.tif")) |> terra::project("EPSG:4326")
  names(Calc_NAM) <- "calc"
  
  # Pull in distance to road network
  roads = terra::rast(paste0(onedrive_path,"CNF/distance_to_numbered_highway_raster.tif"))
  names(roads) <- "dist_to_highways"
  
  # And paved roads too!
  roads = terra::rast(paste0(onedrive_path,"CNF/distance_to_paved_road_raster.tif"))
  names(roads) <- "dist_to_paved_roads"
  
  # Pull in population density raster
  pop_dens = terra::rast(paste0(onedrive_path,"CNF/population_density_raster.tif"))
  names(pop_dens) = "population_density"
  
  # Pull in watercraft destination waterbodies (from Invasive Mussel Defence Program inspections)
  boat_dests = terra::rast(paste0(onedrive_path,"CNF/watercraft_visits_all_years.tif"))
  
  # Pull in DFO 2023 Angler Survey data - Sum of Days Fished
  days_fished = terra::rast(paste0(onedrive_path,"CNF/DFO_angling_survey_days_fished_raster.tif"))
  
  slope_bc<- terra::rast(paste0(onedrive_path,"CNF/slope_bc.tif"))
  
  # Pull in stream order (of streams with stream order 3+), 2km resolution.
  # stream_ord = terra::rast(paste0(onedrive_path,"fwa_streams/stream_order_three_plus_2km_res.tif"))
  # names(stream_ord) = "stream_order"
  # stream_ord[] = factor(terra::values())
  # Read in variable names for the worldclim variables.
  renames<-c("Annual Mean Temperature", 
             "Mean Diurnal Range (temp)", 
             "Isothermality", 
             "Temperature Seasonality", 
             "Max Temperature of Warmest Month", 
             "Min Temperature of Coldest Month", 
             "Temperature Annual Range", 
             "Mean Temperature of Wettest Quarter",
             "Mean Temperature of Driest Quarter", 
             "Mean Temperature of Warmest Quarter", 
             "Mean Temperature of Coldest Quarter", 
             "Annual Precipitation", 
             "Precipitation of Wettest Month",
             "Precipitation of Driest Month", 
             "Precipitation Seasonality", 
             "Precipitation of Wettest Quarter", 
             "Precipitation of Driest Quarter",
             "Precipitation of Warmest Quarter", 
             "Precipitation of Coldest Quarter")
  renames<-gsub(" ", "_", renames)
  
  cmidata<-geodata::cmip6_world("ACCESS-CM2", ssp = "585", var = "bioc", res = 5, time = "2021-2040",path= paste0(proj_path,"/CMI/"))
  names(cmidata)<-renames
  
  # Elevation
  elev = terra::rast(paste0(onedrive_path,"CNF/elevation_BC.tif"))
  
  names(elev) = "elev"
  
  # Slope 
  slope = terra::rast(paste0(onedrive_path,"CNF/slope_BC.tif"))
  
  names(slope) = 'slope'
  
  # Bring in waterbody connectivity
  wb_conn = terra::rast(paste0(onedrive_path,"CNF/stream_order_three_plus_raster.tif"))
  
  
  
  interpolated_raster_limits_filepaths = list.files(path = paste0(onedrive_path,"raster/limits/"),
                                                    pattern = ".*limits.tif$",
                                                    full.names = T)
  interpolated_rasts_limits = interpolated_raster_limits_filepaths |> 
    lapply(terra::rast)
  
  raster_names = list.files(path = paste0(onedrive_path,"raster/limits/"),
                            pattern = ".*limits.tif$") %>%
                                    gsub(".tif", "", .) 
 
  names(interpolated_rasts_limits)<-raster_names
  
  interpolated_rasts_limits = purrr::map2(interpolated_rasts_limits, names(interpolated_rasts_limits), ~ {
    names(.x) = .y
    .x
  })
  
  rm(raster_names)
  
  
  
  # Bring in masked rasters from the 'raster/' data folder.
  interpolated_raster_filepaths = list.files(path = paste0(onedrive_path,"raster/"),
             pattern = ".*masked_krig",
             full.names = T)
  
  interpolated_rasts = interpolated_raster_filepaths |> 
    lapply(terra::rast)
  
  raster_names = list.files(path = paste0(onedrive_path,"raster/"),
                            pattern = ".*masked_krig") |> 
    stringr::str_remove_all("_All.*")
  
  # Replace 'Temperature' with 'Water Temperature'
  raster_names[raster_names == 'Temperature'] = 'Water_Temperature'
  
  # Replace any instances of '+' with 'plus'
  raster_names = stringr::str_replace_all(raster_names,"\\+","plus")
  
  names(interpolated_rasts) = raster_names
  
  # Make sure the actual variable name inside each raster is the same
  # name as the raster... instead of being 'var1.predict' or whatever it starts out life as.
  interpolated_rasts = purrr::map2(interpolated_rasts, names(interpolated_rasts), ~ {
    names(.x) = .y
    .x
  })
  
  seasonTemperatures_fp = list.files(path = paste0(onedrive_path,"raster/"),
                                             pattern = ".*raster_temp",
                                             full.names = T)
  
  seasonTemps = seasonTemperatures_fp |> 
    lapply(terra::rast)
  
  raster_names = list.files(path = paste0(onedrive_path,"raster/"),
                            pattern = ".*raster_temp") |> 
    stringr::str_remove_all("raster_") |> 
    stringr::str_remove_all(".tif")
  
  names(seasonTemps) = raster_names
  
  seasonTemps = purrr::map2(seasonTemps, names(seasonTemps), ~ {
    names(.x) = .y
    .x
  })
  
  rasters = list(cmidata$Annual_Mean_Temperature,
                 cmidata$Annual_Precipitation,

                 ph_NAM,Calc_NAM,roads,elev,pop_dens,
                 boat_dests,days_fished, slope_bc)
  names(rasters)<-c("Annual Mean Temperature", "Precipitation", "pH", "Calcium", "roads", "elevation",
                    "popn_density", "boats_destination", "days_fished", "slope")
  rasters = append(rasters, interpolated_rasts)
  rasters<- append(rasters, interpolated_rasts_limits)
  rasters<- append(rasters, seasonTemps)
  
  # Cut our rasters down to just BC.
  rasters = rasters |>
    lapply(\(x) {
      terra::mask(terra::crop(x, ext_vect), ext_vect)
    })
  
  # Resample to ensure same resolution as bioclim variables.
  rasters = rasters |> 
    lapply(\(x) {
      terra::resample(x, rasters[[8]])
    })
  
  rasters_c = Reduce(x = rasters, f = 'c')
  
  return(rasters_c)
}
