proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"
output_folder = paste0(lan_root,"2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Work Planning and modelling/")
maxent_output_folder = paste0(output_folder,"MaxEnt_predictions/")
fras = sf::read_sf(paste0(onedrive_wd,"CNF/fraser_watershed_priority_area.gpkg"))
col = sf::read_sf(paste0(onedrive_wd,"CNF/columbia_watershed_priority_area.gpkg"))
frascol = dplyr::bind_rows(fras,col) |> dplyr::summarise() |> sf::st_transform(4326)
# Read in SARA-listed species plus Sockeye Salmon (for all of BC)
dfo_sar = sf::read_sf(paste0(onedrive_wd,"CNF/dfo_sara_and_crit_hab_bulltrout_and_sockeye_data.gpkg")) |> sf::st_transform(4326)
# list of invasive species on our watch list.
ais_occs = sf::read_sf(paste0(lan_root, "2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/AIS_occurrences_all_sources.gpkg")) |>
  sf::st_transform(4326)
# Make BC shapefile.
bc = bcmaps::bc_bound() |>
  sf::st_transform(4326) |>
  terra::vect()
# Grab regions of BC. These will be useful for finding waterbodies.
regs = bcmaps::nr_regions() |> sf::st_transform(4326)
# ggplot() + geom_sf(data = dfo_sar, aes(fill = Common_Name_EN, col = Common_Name_EN))
# named_wbs = sf::read_sf("W:/CMadsen/shared_data_sets/summarized_bc_waterbodies.shp") |>
#   dplyr::select(waterbody = GNIS_NAME_, watershed = WATERSHED_, BLUE_LINE_KEY = BLUE_LI) |>
#   sf::st_transform(4326)

if(!file.exists("data/named_lakes_and_rivers.rds")){
  named_lakes = bcdc_query_geodata('freshwater-atlas-lakes') |>
    filter(!is.na(GNIS_NAME_1)) |>
    collect() |>
    dplyr::select(waterbody = GNIS_NAME_1, watershed = WATERSHED_GROUP_ID, BLUE_LINE_KEY, FWA_WATERSHED_CODE) |>
    dplyr::group_by(waterbody, watershed, BLUE_LINE_KEY, FWA_WATERSHED_CODE) |>
    dplyr::summarise(.groups = 'drop') |>
    dplyr::mutate(wb_type = "lake")

  named_rivers = bcdc_query_geodata('freshwater-atlas-rivers') |>
    filter(!is.na(GNIS_NAME_1)) |>
    collect() |>
    dplyr::select(waterbody = GNIS_NAME_1, watershed = WATERSHED_GROUP_ID, BLUE_LINE_KEY, FWA_WATERSHED_CODE) |>
    dplyr::group_by(waterbody, watershed, BLUE_LINE_KEY, FWA_WATERSHED_CODE) |>
    dplyr::summarise(.groups = 'drop') |>
    dplyr::mutate(wb_type = "river")

  named_wbs = dplyr::bind_rows(
    named_lakes,
    named_rivers
  ) |>
    sf::st_transform(4326)

  saveRDS(named_wbs, "data/named_lakes_and_rivers.rds")
} else {
  named_wbs = readRDS("data/named_lakes_and_rivers.rds")
}
