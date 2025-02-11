# Use this 'for' loop to run the entire script.

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

# 1 - LOAD DATA -----------------------------
# Get / set file paths
source("scripts/utils/load_data.R")

# SAR stuff
if(!file.exists("data/dfo_sar_w_wb_no_dups.rds")){

  dfo_sar_frascol = dfo_sar |>
    sf::st_filter(frascol)

  rm(dfo_sar)

  dfo_sar_w_wb = dfo_sar_frascol |>
    dplyr::select(-Waterbody) |>
    sf::st_join(named_wbs)

  # Drop geometries!
  dfo_sar_no_geom = dfo_sar_w_wb |>
    sf::st_drop_geometry()

  # Some duplications of SAR by waterbody.
  dfo_sar_w_wb_no_dups = dfo_sar_no_geom |>
    dplyr::mutate(Population_EN = tidyr::replace_na(Population_EN, "NA")) |>
    dplyr::group_by(waterbody, BLUE_LINE_KEY, watershed, FWA_WATERSHED_CODE, wb_type) |>
    dplyr::summarise(Common_Name_EN = paste0(Common_Name_EN, collapse = ', '),
                     Population_EN = paste0(na.omit(Population_EN), collapse = ', '),
                     Scientific_Name = paste0(Scientific_Name, collapse = ', '),
                     .groups = 'drop')

  # We've got some repeated species in a single waterbody! Drop those.
  dfo_sar_w_wb_no_dups = dfo_sar_w_wb_no_dups |>
    tidyr::separate_longer_delim(cols = c(Common_Name_EN:Scientific_Name), delim = ', ') |>
    dplyr::group_by(waterbody, BLUE_LINE_KEY, watershed, FWA_WATERSHED_CODE, wb_type) |>
    dplyr::filter(!duplicated(Common_Name_EN)) |>
    dplyr::summarise(Common_Name_EN = paste0(Common_Name_EN, collapse = ', '),
                     Population_EN = paste0(na.omit(Population_EN), collapse = ', '),
                     Scientific_Name = paste0(Scientific_Name, collapse = ', '),
                     .groups = 'drop')

  saveRDS(dfo_sar_w_wb_no_dups, "data/dfo_sar_w_wb_no_dups.rds")

} else {
  dfo_sar_w_wb_no_dups = readRDS("data/dfo_sar_w_wb_no_dups.rds")
}

# AIS stuff
ais_occ_frascol = ais_occs |>
  sf::st_filter(frascol)
rm(ais_occs)
ais_occs_w_wb = ais_occ_frascol |>
  sf::st_join(named_wbs) |>
  dplyr::filter(!is.na(waterbody))
# Drop geometries!
ais_occs_no_geom = ais_occs_w_wb |>
  sf::st_drop_geometry()
rm(ais_occs_w_wb)


# Some duplications of AIS by waterbody.
ais_occs_no_dups = ais_occs_no_geom |>
  dplyr::group_by(waterbody, BLUE_LINE_KEY, watershed, FWA_WATERSHED_CODE, wb_type) |>
  dplyr::summarise(Species = paste0(Species, collapse = ', '),
                   .groups = 'drop')

# We've got some repeated species in a single waterbody! Drop those.
ais_occs_no_dups = ais_occs_no_dups |>
  tidyr::separate_longer_delim(cols = Species, delim = ', ') |>
  dplyr::group_by(waterbody, BLUE_LINE_KEY, watershed, FWA_WATERSHED_CODE, wb_type) |>
  dplyr::filter(!duplicated(Species)) |>
  dplyr::summarise(Species = paste0(Species, collapse = ', '),
                   .groups = 'drop')

# 2 - Overlap SAR with AIS ======================
dfo_sar_w_ais = dfo_sar_w_wb_no_dups |>
  dplyr::left_join(ais_occs_no_dups, by = join_by(waterbody,BLUE_LINE_KEY,watershed,FWA_WATERSHED_CODE, wb_type))

# 3. Calculate some columns =====================

dfo_sar_w_ais = dfo_sar_w_ais |>
  dplyr::mutate(ais_present_in_wb = !is.na(Species)) |>
  dplyr::rename(ais_present_names = Species) |>
  dplyr::mutate(number_ais_present = stringr::str_count(ais_present_names,", ") + 1) |>
  dplyr::mutate(number_ais_present = tidyr::replace_na(number_ais_present, 0))

dfo_sar_w_ais$ais_upstream = FALSE
dfo_sar_w_ais$ais_upstream_names = ""
dfo_sar_w_ais$ais_upstream_number = 0
dfo_sar_w_ais$sum_of_maxent_hab_not_hab = 0

for(i in 1:nrow(dfo_sar_w_ais)){
  print(i)
  row_name = dfo_sar_w_ais$waterbody[i]
  row_fwa = dfo_sar_w_ais$FWA_WATERSHED_CODE[i]
  row_fwa_prefix = str_remove(row_fwa, "000000.*")
  row_wb_type = dfo_sar_w_ais$wb_type[i]
  row_watershed = dfo_sar_w_ais$watershed[i]

  # Filter named waterbodies for just those upstream of the_wb.
  # Then, do AIS overlap with just those upstream wb(s).
  upstream_named_wbs = named_wbs |>
    dplyr::filter(str_detect(FWA_WATERSHED_CODE,paste0(row_fwa_prefix))) |>
    dplyr::filter(FWA_WATERSHED_CODE != row_fwa) |>
    dplyr::filter(str_detect(FWA_WATERSHED_CODE,paste0(row_fwa_prefix,"(?!000000)"))) |>
    dplyr::filter(str_detect(FWA_WATERSHED_CODE,paste0(row_fwa_prefix,"[0-9]{6}-000000"))) |>
    sf::st_drop_geometry()

  upstream_named_wbs = upstream_named_wbs |>
    dplyr::filter(watershed == row_watershed)

  ais_upstream = upstream_named_wbs |>
    dplyr::select(-wb_type) |>
    dplyr::inner_join(ais_occs_no_dups, by = join_by(waterbody,BLUE_LINE_KEY,watershed,FWA_WATERSHED_CODE)) |>
    dplyr::select(-FWA_WATERSHED_CODE) |>
    tidyr::separate_longer_delim(cols = Species, delim = ', ') |>
    dplyr::select(Species) |>
    dplyr::distinct()

  dfo_sar_w_ais$ais_upstream[i] = as.logical(nrow(ais_upstream) > 0)
  dfo_sar_w_ais$ais_upstream_names[i] = paste0(ais_upstream$Species, collapse = ', ')
  dfo_sar_w_ais$ais_upstream_number[i] = nrow(ais_upstream)
}

# Drop rows where we have no AIS in waterbody and also no AIS upstream!
dfo_sar_w_ais = dfo_sar_w_ais |>
  dplyr::filter(ais_present_in_wb | ais_upstream)

# Drop some columns that we don't need for the Excel output
dfo_output = dfo_sar_w_ais |>
  dplyr::select(-c(FWA_WATERSHED_CODE,BLUE_LINE_KEY,wb_type))

my_wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(my_wb, "output")
openxlsx::writeData(my_wb, "output", dfo_output)
openxlsx::saveWorkbook(my_wb, file = 'output/SARA_prioritization_model_output.xlsx', overwrite = T)
