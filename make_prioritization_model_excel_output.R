# Use this 'for' loop to run the entire script.
for(m in 1){
  
  library(openxlsx)
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
  # If you don't have this package installed, grab it from Chris' github page:
  #devtools::install_github('chrispmad/fwa.connect')
  library(fwa.connect)
  # library(ENMeval)
  
  # =========================================
  
  # Get / set file paths
  proj_wd = getwd()
  onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
  lan_root = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/"
  output_folder = paste0(lan_root,"2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Work Planning and modelling/")
  maxent_output_folder = paste0(output_folder,"MaxEnt_predictions/")
  
  ## Remove rows where SARA overlaps are lacking?
  remove_zero_sara = TRUE
  
  # Read in utility scripts for functions
  source('scripts/utils/gather_AIS_data.R')
  source('scripts/utils/native_CDC_COSEWIC_SARA_species_occurrence_counter.R')
  source('scripts/utils/read_in_inputs_and_clean_for_model_excel_output.R')
  source('scripts/utils/add_occurrence_record_fields.R')
  source("scripts/utils/native_CDC_COSEWIC_SARA_species_occurrence_counter.R")
  source("scripts/utils/add_first_nations_territories_and_wildlife_habitat_areas_overlaps.R")
  source("scripts/utils/add_introduction_risk.R")
  source("scripts/utils/add_maxent_prediction_vars.R")
  source("scripts/utils/summarise_columns_and_produce_excel_output_file.R")
  source("scripts/utils/add_sara_overlap_wbs_in_10_km_variable.R")
  source("scripts/utils/count_trailing_zeros.R")
  source("scripts/utils/find_downstream_waterbody.R")
  source("scripts/utils/join_sara_cdc_to_wb.R")
  # list of invasive species on our watch list.
  pr_sp = gather_ais_data(data = 'species list', lan_root = lan_root, onedrive_wd = onedrive_wd)
  
  # Make BC shapefile.
  bc = bcmaps::bc_bound() |> 
    sf::st_transform(4326) |> 
    terra::vect()
  
  # Grab regions of BC. These will be useful for finding waterbodies.
  regs = bcmaps::nr_regions() |> sf::st_transform(4326)
  
  # =========================================
  # Read in data and clean it up a bit
  data_files = read_in_inputs_and_clean_for_model_excel_output(onedrive_wd)
  d = data_files[[1]]
  unique_wbs = data_files[[2]]
  wbs_overlap_sara_cdc = data_files[[3]]
  
  # =========================================
  
  # File that tracks previously performed queries / runs of these script, to save time.
  if(file.exists(paste0(onedrive_wd,"AIS_previous_query_results.rds"))){
    previous_results = readRDS(file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
  } else {
    previous_results = d |> 
      sf::st_drop_geometry() |> 
      dplyr::select(Region:FWA_WATERSHED_CODE) |> 
      dplyr::mutate(query_date = Sys.Date()) |> 
      dplyr::select(query_date, dplyr::everything())
    saveRDS(previous_results, paste0(onedrive_wd,"AIS_previous_query_results.rds"))
  }
  
  # ================================
  # Bring in / calculate variables
  
  # 1. Calculate number of inflows and outflows using fwa.connect package!!
  tbl_of_fwa_junctions = fwa.connect::stream_conn_tbl()
  # The following function takes about 1-2 minutes.
  d = d |> 
    rowwise() |> 
    dplyr::mutate(number_outflows = nrow(tbl_of_fwa_junctions[tbl_of_fwa_junctions$upstream_fwa_code == FWA_WATERSHED_CODE,]),
                  number_inflows = nrow(tbl_of_fwa_junctions[tbl_of_fwa_junctions$downstream_fwa_code == FWA_WATERSHED_CODE,])) |> 
    dplyr::ungroup()
  
  # Additional test - look for waterbodies within x KM that are connected
  # to focal waterbody and that have SAR overlaps
  print("Adding in overlaps with SAR waterbodies that are within 10 km of target waterbodies")
  sara_overlap_wbs = sf::read_sf(paste0(onedrive_wd,"waterbodies_overlapping_with_SARA_and_CDC_occs.gpkg"))
  d = add_sara_overlap_wbs_in_10_km(d,sara_overlap_wbs,previous_results)
  
  # 2. Number of records in waterbody
  print("Adding in occurrence record fields")
  d = add_occurrence_record_fields(d,lan_root)
  
  # 3. Distinct DFO SARA species, COSEWIC and CDC species in waterbody.
  # previous_results = readRDS(file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
  print("Pulling in native, CDC, SARA and COSEWIC occurrences")
  #View(readRDS(paste0(onedrive_wd,"AIS_previous_query_results.rds")))
  d = native_CDC_COSEWIC_SARA_species_occurrence_counter(d,unique_wbs,wbs_overlap_sara_cdc,onedrive_wd)
  
  # 4. MaxEnt Predictions
  print("Adding maxent prediction variables")
  d = add_maxent_prediction_vars(d,onedrive_wd,maxent_output_folder)
  
  # 5. Overlaps with First Nations Territories and also Wildlife Habitat Areas
  previous_results = readRDS(file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
  print("Doing overlap with First Nations Territories and Wildlife Habitat Areas")
  d = add_first_nations_territories_and_wildlife_habitat_areas_overlaps(d,unique_wbs,onedrive_wd)
  
  # previous_results = readRDS(file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
  print("Adding introduction Risk variables")
  d = add_introduction_risk(d, lan_root)
  # =========================================
  # Make bins for variables
  print("Summarising output to Excel file!")
  summarise_columns_and_produce_excel_output_file(d,output_folder,maxent_output_folder,remove_zero_sara)
}
