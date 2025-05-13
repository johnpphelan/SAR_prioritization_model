# Use this 'for' loop to run the entire script.

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
# If you don't have this package installed, grab it from Chris' github page:
#devtools::install_github('chrispmad/fwa.connect')
library(fwa.connect)
library(tidyterra)
library(tidyr)
library(BAMMtools) # Used to find natural breaks ("jenks")
library(basemaps)

remake_hab_images = FALSE

# library(ENMeval)

# =========================================

# 1 - LOAD DATA -----------------------------
# Get / set file paths
source("scripts/utils/load_data.R")
source("scripts/utils/prep_predictor_data_f.R")

<<<<<<< HEAD
# Try making some plots!
dfo_sar_4326 = sf::st_transform(dfo_sar, 4326)

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
                                           map_service = "esri",
                                           map_type = "world_street_map")

    ggplot() +
      tidyterra::geom_spatraster_rgb(data = the_ocean_bm) +
      geom_sf(data = .x,
              aes(fill = Common_Name_EN), alpha = 0.5) +
      ggthemes::theme_map() +
      theme(legend.position = 'none')

  })

all_my_maps = Reduce(`+`, species_maps)

inset_map = ggplot() +
  geom_sf(data = bcmaps::bc_bound())

all_my_maps + inset_map
=======


>>>>>>> 2139a1c28684d40782d5ce954dfdf49627803a27

# SAR stuff
if(!file.exists("data/dfo_sar_w_wb_no_dups.rds")){

  # Filter the SARA-listed species polygons we got from DFO by
  # the Fraser and Columbia River watersheds (very large!)
  dfo_sar_frascol = dfo_sar |>
    sf::st_filter(frascol)

  rm(dfo_sar)

  # Drop the 'Waterbody' column in the dfo SARA object, then join on
  # waterbody names etc. that spatially overlap, drawing on a spatial
  # file that consists of all named lakes and rivers in BC.
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

# AIS stuff - filter to Fraser / Columbia River watersheds
ais_occ_frascol = ais_occs |>
  sf::st_filter(frascol)

rm(ais_occs)

# As above for SAR - inform the waterbody name for spatial overlaps
# with named lakes / rivers.
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

# 3. Enumerate Upstream AIS =====================
dfo_sar_w_ais = dfo_sar_w_ais |>
  dplyr::mutate(ais_present_in_wb = !is.na(Species)) |>
  dplyr::rename(ais_present_names = Species) |>
  dplyr::mutate(number_ais_present = stringr::str_count(ais_present_names,", ") + 1) |>
  dplyr::mutate(number_ais_present = tidyr::replace_na(number_ais_present, 0))

dfo_sar_w_ais$ais_upstream = FALSE
dfo_sar_w_ais$ais_upstream_names = ""
dfo_sar_w_ais$ais_upstream_number = 0
dfo_sar_w_ais$number_upstream_waterbodies = 0
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

  # Record the number of named upstream waterbodies detected.
  dfo_sar_w_ais$number_upstream_waterbodies[i] = nrow(upstream_named_wbs)
  # if(nrow(upstream_named_wbs) > 0) print(paste0("row ",i, " has at least one upstream named waterbody."))

  # Check if there are any named lakes that are touching the 1+ upstream
  # waterbody/waterbodies.
  upstream_wbs_streams = upstream_named_wbs |>
    dplyr::filter(stringr::str_detect(waterbody,"(Stream|Creek|River)"))

  if(nrow(upstream_wbs_streams) > 0){
    for(y in 1:nrow(upstream_wbs_streams)){
      upstream_wbs_stream = upstream_wbs_streams[y,]
      upstream_wbs_stream_fwa = upstream_wbs_stream$FWA_WATERSHED_CODE
      upstream_wbs_stream_fwa_prefix = str_remove(upstream_wbs_stream_fwa, "000000.*")
      additional_upstream_wbs = named_wbs |>
        dplyr::filter(str_detect(FWA_WATERSHED_CODE,paste0(upstream_wbs_stream_fwa_prefix))) |>
        dplyr::filter(FWA_WATERSHED_CODE != upstream_wbs_stream_fwa) |>
        dplyr::filter(str_detect(FWA_WATERSHED_CODE,paste0(upstream_wbs_stream_fwa_prefix,"(?!000000)"))) |>
        dplyr::filter(str_detect(FWA_WATERSHED_CODE,paste0(upstream_wbs_stream_fwa_prefix,"[0-9]{6}-000000"))) |>
        dplyr::filter(str_detect(waterbody,"Lake")) |>
        sf::st_drop_geometry()
      if(nrow(additional_upstream_wbs) > 0){
        print("An additional upstream lake that was touching the network of upstream waterbodies was added to the dataframe of upstream waterbodies!")
        upstream_named_wbs = dplyr::bind_rows(
          upstream_named_wbs
        )
      }
    }

  }

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

hab_suit<-list()

#######################################################

dfo_sar_w_ais$sum_of_maxent_hab_not_hab = 0
dfo_sar_w_ais$mean_of_maxent_hab_not_hab = 0

# 4. Get maxent for the AIS upstream
for(i in 1:nrow(dfo_sar_w_ais)){
  # species upstream of target waterbody
  sp_upstream_of_wb = dfo_sar_w_ais[i,]$ais_upstream_names
  # species in the target waterbody
  sp_in_wb = dfo_sar_w_ais[i,]$ais_present_names
  # separate the species
  sp_upstream_of_wb_split <- strsplit(sp_upstream_of_wb, ", ")[[1]]
  sp_in_wb_split <- strsplit(sp_in_wb, ", ")[[1]]
  # if they are in the waterbody, don't bother with calculating maxent upstream
  sp_upstream_of_wb_split <- setdiff(sp_upstream_of_wb_split, sp_in_wb_split)

  if(sp_upstream_of_wb == ""){
    next
  }

  ## The waterbodies no longer have geometries, so we have to get them from the freshwater atlas

  #get code and type of wb
  wb_id<-dfo_sar_w_ais$FWA_WATERSHED_CODE[i]; wb_type<-dfo_sar_w_ais$wb_type[i]; blk<-dfo_sar_w_ais$BLUE_LINE_KEY[i]

  if(wb_type == "lake"){
    the_wb = bcdc_query_geodata('freshwater-atlas-lakes') |>
      filter(FWA_WATERSHED_CODE == wb_id, BLUE_LINE_KEY == blk) |>
      collect() |>
      st_transform(4326) |>
      summarise(.groups = 'drop')
  }else{
    the_wb = bcdc_query_geodata('freshwater-atlas-rivers') |>
      filter(FWA_WATERSHED_CODE == wb_id, BLUE_LINE_KEY == blk) |>
      collect() |>
      st_transform(4326) |>
      summarise(.groups = 'drop')
  }

  for (ais_spp in sp_upstream_of_wb_split) {

    the_species_snake = snakecase::to_snake_case(ais_spp)
    species_folder = paste0(onedrive_wd,"MaxEnt_predictions/",the_species_snake,"/")

    #we want the habitat/not for this work
    if(!file.exists(paste0(species_folder,"MaxEnt_prediction_habitat_or_not.tif"))){
      percentage = 0
      warning(paste0("On loop run ",i,", MaxEnt not found for ",ais_spp, " (searched for ",species_folder,")"))
    }else{

      the_pred_r = terra::rast(paste0(species_folder,"MaxEnt_prediction_habitat_or_not.tif"))

      the_pred_about_wb = terra::crop(the_pred_r, sf::st_buffer(the_wb, 5000))
      #waterbody only
      the_pred_wb_masked <- mask(the_pred_about_wb, the_wb)
      pred_plot<-the_pred_wb_masked
      pred_plot[pred_plot != 1] <- NA
      # save the raster to a folder, so they can be linked in the excel file
      output_path <- paste0("output/spp_wb_overlap/", dfo_sar_w_ais$waterbody[i], "/",the_species_snake,"_masked_habOrNot.jpeg")
      dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
      # spp_hab_plot <- ggplot() +
      #   geom_spatraster(data = pred_plot) +  # Add raster layer+
      #   labs(title = paste0("Waterbody: ", dfo_sar_w_ais$waterbody[i], "\nSpecies: ",ais_spp))+
      #   geom_sf(data = the_wb, fill = "transparent", color = "black") +  # Plot waterbodies
      #   scale_fill_gradient(low = "lightgreen", high = "lightgreen", na.value = "transparent") +  # Ensure habitat areas are blue
      #   theme_minimal()+
      #   theme(plot.title = element_text(face = "bold", size = 14))
      #
      # #spp_hab_plot
      #
      # inset_bc <- ggplot() +
      #   geom_sf(data = bc, fill = "lightgray", color = "black") +  # Full BC map
      #   geom_sf(data = the_wb, fill = "red", color = "red") +  # Highlight the waterbody
      #   theme_void()  # Remove axes and grid
      #
      # #inset_grob <- ggplotGrob(inset_bc)
      #
      # final_plot <- ggarrange(spp_hab_plot, inset_bc,
      #                         ncol = 2, nrow = 1, heights = c(3, 1))
      #
      # ggexport(final_plot, filename = output_path, width = 1200, height = 600)

      # Count the total number of pixels in the waterbody
      total_pixels <- sum(!is.na(values(the_pred_wb_masked)))
      ones_count <- sum(values(the_pred_wb_masked) == 1, na.rm = TRUE)
      percentage <- (ones_count / total_pixels) * 100

      print(i)
      # the_pred_wb_masked <- as.factor(the_pred_wb_masked)
      # levels(the_pred_wb_masked) <- list(data.frame(ID = c(0, 1), fc.LQ_rm.1 = c("FALSE", "TRUE")))
      # ggplot() +
      #   geom_sf(data = the_wb) +
      #   geom_spatraster(data = the_pred_wb_masked, alpha = 0.6) +
      #   coord_sf(crs = st_crs(the_wb)) +
      #   scale_fill_manual(
      #     values = c("FALSE" = "lightgreen", "TRUE" = "purple"),  # Discrete color scale
      #     na.value = "transparent"  # Handle NA values
      #   ) +
      #   labs(title = "Prediction around Waterbody") +
      #   theme_bw()
    }
    # how to gather all the percentages?
    # Keep them separate for each species, then sum them up for final presentation

    dfo_sar_w_ais$sum_of_maxent_hab_not_hab[i] = dfo_sar_w_ais$sum_of_maxent_hab_not_hab[i] + percentage
    #now we want to store the percentage for each species in habitat_suitablities
    # save the names of the waterbodies, the species and the percentage
    # hab_suit[[length(hab_suit)+1]] <- list(wb_name = dfo_sar_w_ais$waterbody[i], watershed = dfo_sar_w_ais$watershed[i], species = the_species_snake, percentage = percentage)
    hab_suit[[length(hab_suit)+1]] <- data.frame(wb_name = dfo_sar_w_ais$waterbody[i], watershed = dfo_sar_w_ais$watershed[i], species = the_species_snake, percentage = percentage)

  }
  dfo_sar_w_ais$mean_of_maxent_hab_not_hab[i] = dfo_sar_w_ais$sum_of_maxent_hab_not_hab[i] / length(sp_upstream_of_wb_split)
  if(is.nan(dfo_sar_w_ais$mean_of_maxent_hab_not_hab[i])) {
    dfo_sar_w_ais$mean_of_maxent_hab_not_hab[i] = 0
  }
}

hab_suit_df = dplyr::bind_rows(hab_suit) |>
  tidyr::as_tibble()

# Add categories of action based on AIS presence
# AIS in wb and not upstream = eradication? (moderate priority of all the possible actions?)
# AIS in wb and upstream = monitoring (lowest priority of the possible actions?)
# AIS not in wb but upstream = monitoring and prevention (highest priority, as it's most feasible?)
dfo_sar_w_ais = dfo_sar_w_ais |>
  dplyr::mutate(status = dplyr::case_when(
    ais_present_in_wb & ais_upstream ~ "AIS in WB and upstream",
    ais_present_in_wb & !ais_upstream ~ "AIS in WB only",
    !ais_present_in_wb & ais_upstream ~ "AIS upstream only",
    T ~ "Unknown"
  ))

# Final - write to excel
# Drop some columns that we don't need for the Excel output
dfo_output = dfo_sar_w_ais |>
  dplyr::select(-c(FWA_WATERSHED_CODE,BLUE_LINE_KEY,wb_type, sum_of_maxent_hab_not_hab))


# The following chunk of code finds all the pairings of AIS and SARA
# to inform an excel sheet that John made for Amalis and Martina to
# fill in how impactful a given AIS might be for a given SAR species.
if(!file.exists("output/SARA_prioritization_model_output.xlsx")){

  # get unique combinations
  sar_ais_combo<-data.frame(dfo_sar_w_ais$Common_Name_EN, dfo_sar_w_ais$ais_present_names, dfo_sar_w_ais$ais_upstream_names)

  sar_ais_combo_test<- sar_ais_combo |>
    separate_longer_delim(dfo_sar_w_ais.Common_Name_EN, delim = ",") |>
    separate_longer_delim(dfo_sar_w_ais.ais_present_names, delim = ",") |>
    separate_longer_delim(dfo_sar_w_ais.ais_upstream_names, delim = ",")

  unique_combos<- sar_ais_combo_test |>
    pivot_longer(cols = 2:3, names_to = "ais_type", values_to = "ais_name") |>
    filter(!is.na(ais_name)) |>
    distinct() |>
    select(-ais_type) |>
    filter(ais_name != "") |>
    rename(sar_name = dfo_sar_w_ais.Common_Name_EN) |>
    mutate(sar_name = trimws(sar_name), ais_name = trimws(ais_name)) |>
    distinct() |>
    arrange(sar_name, ais_name)


  #separate_longer_delim(across(everything()), delim = ",")
  #separate_rows(sar_ais_combo, dfo_sar_w_ais.Common_Name_EN, delim = ",")

  unique_ais<-unique_combos |>
    distinct(ais_name)

  my_wb = openxlsx::createWorkbook()
  openxlsx::addWorksheet(my_wb, "output")
  openxlsx::writeData(my_wb, "output", dfo_output)
  openxlsx::addWorksheet(my_wb, "habitat_suitabilities")
  openxlsx::writeData(my_wb, "habitat_suitabilities", hab_suit_df)
  openxlsx::addWorksheet(my_wb, "SAR and AIS combintations")
  openxlsx::writeData(my_wb, "SAR and AIS combintations", unique_combos)
  openxlsx::addWorksheet(my_wb, "AIS present")
  openxlsx::writeData(my_wb, "AIS present", unique_ais)

  openxlsx::saveWorkbook(my_wb, file = 'output/SARA_prioritization_model_output.xlsx', overwrite = T)
}

# Add these effects
sar_fish = c('Westslope Cutthroat Trout, Bull Trout, Cultus Pygmy Sculpin, Sockeye Salmon')
sar_inverts = c('Rocky Mountain Ridged Mussel')

# Read in the AIS-SAR effect matrix.
ais_effects_on_fish = read_excel("data/ais_effects.xlsx", sheet = 'fish')
ais_effects_on_inverts = read_excel("data/ais_effects.xlsx", sheet = 'invertebrates')

# Accordion this out so that we have a row for all AIS-SARA combos. This will
# faciltate left_joining this table on to our results.
ais_effects = dplyr::bind_rows(
  ais_effects_on_fish |> mutate(Common_Name_EN = sar_fish),
  ais_effects_on_inverts |> mutate(Common_Name_EN = "Rocky Mountain Ridged Mussel")
) |>
  dplyr::select(ais_present_names = Species,
                Common_Name_EN,
                dplyr::everything()) |>
  tidyr::separate_longer_delim(Common_Name_EN, delim = ", ")

# Separate SARA and AIS species name lists that have more than one element into
# separate rows.
dfo_output_long = dfo_output |>
  tidyr::separate_longer_delim(cols = c(Common_Name_EN,Population_EN,Scientific_Name), delim = ", ") |>
  tidyr::separate_longer_delim(cols = c(ais_present_names), delim = ", ") |>
  dplyr::mutate(ais_present_names = str_to_title(ais_present_names)) |>
  dplyr::left_join(ais_effects, by = join_by(Common_Name_EN, ais_present_names))

# To estimate the negative effect of a given AIS on a given SAR, we take the
# mean of the effect columns! If there are multiple AIS affecting a given SAR
# in a given waterbody, we will SUM the mean effects.
dfo_output_long = dfo_output_long |>
  tidyr::pivot_longer(cols = c(predation:uncertainty)) |>
  dplyr::group_by(waterbody, watershed, Common_Name_EN, Population_EN, ais_present_names) |>
  mutate(mean_ais_effect = mean(value,na.rm=T)) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider()

# Add First Nations cultural significance column, Expert Opinions for SAR and AIS,
# and population importance.
dfo_output_long = dfo_output_long |>
  dplyr::mutate(FN_cultural_significance = NA,
                expert_opinion_AIS = NA,
                expert_opinion_SARA = NA)

#####################
# We are looking for the risk levels from COSEWIC for each of the SARA species
# get rocky mountain ridged mussel, not western, for matching purposes - change later?

source("scripts/utils/sar_risk_merge.R")
risk_levels<-get_status()

#remove the Western Ridged Mussel if Rocky Mountain Ridged Mussel is present in risk_levels
risk_levels<-risk_levels |>
  filter(!(COSEWIC.common.name == "Western Ridged Mussel" &
             "Rocky Mountain Ridged Mussel" %in% COSEWIC.common.name))

# we will match the risk levels to the dfo_output_long common names and the we also need to string detect
# Population_EN to the risk_levels Legal.population. This will be done similarly to the sar_risk_merge.r script

dfo_output_long <- dfo_output_long |>
  left_join(risk_levels, by = c("Scientific_Name" = "Scientific.name"), relationship = "many-to-many") |>
  filter(Scientific_Name == "Gonidea angulata" |
           str_detect(Legal.population, regex(Population_EN, ignore_case = TRUE))) |>
  select(-c(COSEWIC.common.name,Legal.population))

# Resummarise by waterbody and SARA species!
dfo_output = dfo_output_long |>
  dplyr::mutate(ais_sp_in_wb_mean_effect = paste0(ais_present_names, " - ", round(mean_ais_effect,3)," (Uncert. ",uncertainty,")")) |>
  dplyr::group_by(waterbody, watershed, Common_Name_EN, Population_EN, Scientific_Name,
                  ais_present_in_wb, number_ais_present, ais_upstream, ais_upstream_number,
                  number_upstream_waterbodies, mean_of_maxent_hab_not_hab, FN_cultural_significance,
                  expert_opinion_AIS,expert_opinion_SARA, COSEWIC.status) |>
  dplyr::reframe(across(c(ais_present_names,ais_upstream_names,ais_sp_in_wb_mean_effect), \(x) paste0(unique(x), collapse = ', ')),
                 summed_ais_in_wb_effects = sum(mean_ais_effect,na.rm=T),
                 summed_in_wb_uncertainties = sum(uncertainty,na.rm=T),
                 mean_in_wb_uncertainty = mean(uncertainty,na.rm=T),
                 max_ais_in_wb_on_sara_effect = max(mean_ais_effect,na.rm=T)) |>
  # Replace -Inf for max AIS on SARA effect with NA
  dplyr::mutate(max_ais_in_wb_on_sara_effect = ifelse(max_ais_in_wb_on_sara_effect == -Inf, NA, max_ais_in_wb_on_sara_effect))


# ------------------------------------------------------
# As above, but for AIS upstream!
dfo_output_long = dfo_output |>
  tidyr::separate_longer_delim(cols = c(ais_upstream_names), delim = ", ") |>
  dplyr::mutate(ais_upstream_names = str_to_title(ais_upstream_names)) |>
  # Where the same species is upstream AND in the wb, don't worry about upstream effects.
  dplyr::filter(!str_detect(ais_present_names, ais_upstream_names)) |>
  dplyr::left_join(
    ais_effects |> dplyr::rename(ais_upstream_names = ais_present_names),
    by = join_by(Common_Name_EN, ais_upstream_names)
  )

# To estimate the negative effect of a given AIS on a given SAR, we take the
# mean of the effect columns! If there are multiple AIS affecting a given SAR
# in a given waterbody, we will SUM the mean effects.
dfo_output_long = dfo_output_long |>
  tidyr::pivot_longer(cols = c(predation:uncertainty)) |>
  dplyr::group_by(waterbody, watershed, Common_Name_EN, Population_EN, ais_upstream_names) |>
  mutate(mean_ais_upstream_effect = mean(value,na.rm=T)) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider()

# Resummarise by waterbody and SARA species!
dfo_output_upstream = dfo_output_long |>
  dplyr::mutate(upstream_ais_sp_mean_effect = paste0(ais_upstream_names, " - ", round(mean_ais_upstream_effect,3)," (Uncert. ",uncertainty,")")) |>
  dplyr::group_by(waterbody, watershed, Common_Name_EN, Population_EN, Scientific_Name,
                  ais_present_in_wb, number_ais_present,
                  ais_upstream, ais_upstream_number, ais_present_names,
                  number_upstream_waterbodies, mean_of_maxent_hab_not_hab, FN_cultural_significance,
                  expert_opinion_AIS, expert_opinion_SARA,
                  summed_ais_in_wb_effects, summed_in_wb_uncertainties, ais_sp_in_wb_mean_effect, mean_in_wb_uncertainty,
                  max_ais_in_wb_on_sara_effect, COSEWIC.status) |>
  dplyr::reframe(across(c(ais_upstream_names,upstream_ais_sp_mean_effect), \(x) paste0(unique(x), collapse = ', ')),
                 summed_upstream_ais_effects = sum(mean_ais_upstream_effect,na.rm=T),
                 summed_upstream_uncertainties = sum(uncertainty,na.rm=T),
                 mean_upstream_uncertainty = mean(uncertainty,na.rm=T),
                 max_upstream_ais_on_sara_effect = max(mean_ais_upstream_effect,na.rm=T)) |>
  # Replace -Inf for max AIS on SARA effect with NA
  dplyr::mutate(max_upstream_ais_on_sara_effect = ifelse(max_upstream_ais_on_sara_effect == -Inf, NA, max_upstream_ais_on_sara_effect))

dfo_output_final = dfo_output |>
  # We've changed the capitalization scheme for upstream ais names - it is now title case for upstream fragment table!
  # So, let's make it title case for the dfo_output table that we are joining that upstream fragment table onto.
  dplyr::mutate(ais_upstream_names = str_to_title(ais_upstream_names)) |>
  dplyr::left_join(
    dfo_output_upstream |> dplyr::select(waterbody,watershed,Common_Name_EN,ais_present_names,
                                         summed_upstream_ais_effects:max_upstream_ais_on_sara_effect)
  )

dfo_output_final = dfo_output_final |>
  mutate(
    cosewic_numeric = case_when(
      COSEWIC.status == "Not at Risk"     ~ 0,
      COSEWIC.status == "Data Deficient"  ~ 1,
      COSEWIC.status == "Special Concern" ~ 1,
      COSEWIC.status == "Threatened"      ~ 2,
      COSEWIC.status == "Endangered"      ~ 3,
      TRUE ~ NA_real_
    )#,
    # summed_ais_in_wb_effects = if_else(
    #   is.na(summed_ais_in_wb_effects) | summed_ais_in_wb_effects == 0,
    #   summed_ais_in_wb_effects,
    #   summed_ais_in_wb_effects + cosewic_numeric
    # ),
    # summed_upstream_ais_effects = if_else(
    #   is.na(summed_upstream_ais_effects) | summed_upstream_ais_effects == 0,
    #   summed_upstream_ais_effects,
    #   summed_upstream_ais_effects + cosewic_numeric
    # )
  ) |>
  # Multiple COSEWIC status by the max effect of AIS (1) in wb and (2) upstream
  # Also, downscale the weighting of the upstream effect
  dplyr::mutate(
    max_ais_ef_in_wb_on_status = max_ais_in_wb_on_sara_effect + cosewic_numeric,
    max_ais_ef_upstream_on_status = 0.5 * (max_upstream_ais_on_sara_effect + cosewic_numeric)
  ) |>
  dplyr::mutate(max_ais_ef_in_wb_on_status = tidyr::replace_na(max_ais_ef_in_wb_on_status, 0),
                max_ais_ef_upstream_on_status = tidyr::replace_na(max_ais_ef_upstream_on_status, 0)) #|>
  # dplyr::mutate(final_risk = max_ais_ef_in_wb_on_status + max_ais_ef_upstream_on_status)

# Add binned versions of our key columns.
# natural_breaks_ais_in_wb = BAMMtools::getJenksBreaks(dfo_output_final$max_ais_ef_in_wb_on_status,k=4)
# natural_breaks_ais_upstream = BAMMtools::getJenksBreaks(as.numeric(dfo_output_final$max_ais_ef_upstream_on_status), k = 4)

# If the natural breaks are doubling up on 0, remove the first break so we don't have duplicates!
# if(natural_breaks_ais_in_wb[2] == 0){
#   natural_breaks_ais_in_wb = natural_breaks_ais_in_wb[-1]
# }
# if(natural_breaks_ais_upstream[2] == 0){
#   natural_breaks_ais_upstream = natural_breaks_ais_upstream[-1]
# }

# dfo_output_final = dfo_output_final |>
#   dplyr::mutate(max_ais_ef_in_wb_on_status_b = as.numeric(cut(max_ais_ef_in_wb_on_status, natural_breaks_ais_in_wb))) |>
#   mutate(max_ais_ef_in_wb_on_status_b = replace_na(max_ais_ef_in_wb_on_status_b, 0)) |>
#   dplyr::mutate(max_ais_ef_upstream_on_status_b = as.numeric(cut(as.numeric(max_ais_ef_upstream_on_status), natural_breaks_ais_upstream))) |>
#   mutate(max_ais_ef_upstream_on_status_b = replace_na(max_ais_ef_upstream_on_status_b, 0))

dfo_output_final = dfo_output_final |>
  mutate(final_risk = max_ais_ef_in_wb_on_status + max_ais_ef_upstream_on_status)
# dfo_output_final = dfo_output_final |>
#   mutate(final_risk = max_ais_ef_in_wb_on_status + max_ais_ef_upstream_on_status)



# Bin the final risk column
final_risk_bins = BAMMtools::getJenksBreaks(dfo_output_final$final_risk, k = 4)

dfo_output_final = dfo_output_final |>
  dplyr::mutate(final_risk_b = as.numeric(cut(final_risk, final_risk_bins))) |>
  dplyr::mutate(final_risk_b = factor(final_risk_b,
                                      levels = c(1:3),
                                      labels = c(
                                        paste0("Low (<",final_risk_bins[2],")"),
                                        paste0("Moderate (",final_risk_bins[2],"-",final_risk_bins[3],")"),
                                        paste0("High (",final_risk_bins[3],"+)"))))

dfo_output_final = dfo_output_final |>
  dplyr::mutate(` ` = NA) |>
  dplyr::select(waterbody, watershed, Common_Name_EN, Population_EN, COSEWIC.status,
                # Subjective valuation columns
                FN_cultural_significance, expert_opinion_AIS, expert_opinion_SARA,
                # Sum of effects of AIS in waterbody
                max_ais_ef_in_wb_on_status,
                #summed_ais_in_wb_effects, summed_in_wb_uncertainties,
                # Sum of effecs of AIS upstream
                max_ais_ef_upstream_on_status,
                #summed_upstream_ais_effects, summed_upstream_uncertainties,
                # binned values,
                # summed_ais_in_wb_effects_b, summed_ais_upstream_effects_b, final_risk,
                final_risk, final_risk_b,
                # White space column
                ` `,
                everything()
                )

dfo_output_final = dfo_output_final |>
  dplyr::arrange(desc(final_risk), desc(cosewic_numeric))
# create factors for all the types of cosewic status - data deficient is 1, not at risk is 0, special concern 1, threatened is 2, endangered is 3.

# # Convert binned ais effect variables to 'low','med','high'
# dfo_output_final = dfo_output_final |>
#   dplyr::mutate(summed_ais_in_wb_effects_b = dplyr::case_when(
#     summed_ais_in_wb_effects_b == 1 ~ "Low (1)",
#     summed_ais_in_wb_effects_b == 2 ~ "Moderate (2)",
#     summed_ais_in_wb_effects_b == 3 ~ "High (3)",
#     T ~ "None"
#   )) |>
#   dplyr::mutate(summed_ais_upstream_effects_b = dplyr::case_when(
#     summed_ais_upstream_effects_b == 1 ~ "Low (1)",
#     summed_ais_upstream_effects_b == 2 ~ "Moderate (2)",
#     summed_ais_upstream_effects_b == 3 ~ "High (3)",
#     T ~ "None"
#   ))

# dfo_output_final = dfo_output_final |>
#   dplyr::mutate(pop_importance = dplyr::case_when(
#     Population_EN == 'Cultus' ~ 1.5,
#     T ~ 1
#   )) |>
#   dplyr::mutate(
#     summed_ais_in_wb_effects = summed_ais_in_wb_effects * pop_importance,
#     summed_upstream_ais_effects = summed_upstream_ais_effects * pop_importance
#   )

# Prepare results for Excel file.

# Round some digits and adjust column placement!
dfo_output_final = dfo_output_final |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x,2)))

# Replace NA / Nulls / weird stuff with "NA" (except for 'White Space' column!)
dfo_output_final = dfo_output_final |>
  dplyr::mutate(dplyr::across(dplyr::everything(), \(x) ifelse(is.na(x) | is.nan(x) | x == "NA - NaN (Uncert. NA)", "NA", x))) |>
  dplyr::mutate(` ` = NA)

my_wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(my_wb, "output")
openxlsx::writeData(my_wb, "output", dfo_output_final)
openxlsx::setColWidths(my_wb, "output", cols = 1:ncol(dfo_output_final), widths = 18)
# Reduce width of certain hand-picked columns that really need not be so wide.
openxlsx::setColWidths(my_wb, "output", cols = c(1,2,4,6:13), widths = 10)
openxlsx::addWorksheet(my_wb, "habitat_suitabilities")
openxlsx::writeData(my_wb, "habitat_suitabilities", hab_suit_df)
openxlsx::saveWorkbook(my_wb, paste0("output/SARA_prioritization_model_",Sys.Date(),".xlsx"), overwrite = T)
