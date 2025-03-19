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


# library(ENMeval)

# =========================================

# 1 - LOAD DATA -----------------------------
# Get / set file paths
source("scripts/utils/load_data.R")
source("scripts/utils/prep_predictor_data_f.R")

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
      spp_hab_plot <- ggplot() +
        geom_spatraster(data = pred_plot) +  # Add raster layer+
        labs(title = paste0("Waterbody: ", dfo_sar_w_ais$waterbody[i], "\nSpecies: ",ais_spp))+
        geom_sf(data = the_wb, fill = "transparent", color = "black") +  # Plot waterbodies
        scale_fill_gradient(low = "lightgreen", high = "lightgreen", na.value = "transparent") +  # Ensure habitat areas are blue
        theme_minimal()+
        theme(plot.title = element_text(face = "bold", size = 14))

      #spp_hab_plot

      inset_bc <- ggplot() +
        geom_sf(data = bc, fill = "lightgray", color = "black") +  # Full BC map
        geom_sf(data = the_wb, fill = "red", color = "red") +  # Highlight the waterbody
        theme_void()  # Remove axes and grid

      #inset_grob <- ggplotGrob(inset_bc)

      final_plot <- ggarrange(spp_hab_plot, inset_bc,
                              ncol = 2, nrow = 1, heights = c(3, 1))

      ggexport(final_plot, filename = output_path, width = 1200, height = 600)

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
  dplyr::mutate(action = dplyr::case_when(
    ais_present_in_wb & ais_upstream ~ "Monitoring",
    ais_present_in_wb & !ais_upstream ~ "Eradication",
    !ais_present_in_wb & ais_upstream ~ "Monitoring and Prevention",
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
  tidyr::separate_longer_delim(cols = c(Common_Name_EN), delim = ", ") |>
  tidyr::separate_longer_delim(cols = c(ais_present_names), delim = ", ") |>
  dplyr::left_join(ais_effects, by = join_by(Common_Name_EN, ais_present_names)) |>
  # For now (?), fill in wholes with a value of 3 IF there's an AIS species there.
  # If not (because the AIS is only upstream), replace with a value of 0.
  dplyr::mutate(dplyr::across(predation:`disease/parasite transmission`, \(x) ifelse(
    !is.na(ais_present_names),
    tidyr::replace_na(x, 3),
    tidyr::replace_na(x, 0)))
  )

# To estimate the negative effect of a given AIS on a given SAR, we take the
# mean of the effect columns! If there are multiple AIS affecting a given SAR
# in a given waterbody, we will SUM the mean effects.
dfo_output_long = dfo_output_long |>
  tidyr::pivot_longer(cols = c(predation:`disease/parasite transmission`)) |>
  dplyr::group_by(waterbody, watershed, Common_Name_EN, Population_EN, ais_present_names) |>
  mutate(mean_ais_effect = mean(value)) |>
  # dplyr::filter(mean_ais_effect == 3.4) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider()

# Resummarise by waterbody!
dfo_output = dfo_output_long |>
  dplyr::mutate(ais_sp_and_mean_effect = paste0(ais_present_names, " (", mean_ais_effect,")")) |>
  dplyr::group_by(waterbody, watershed, ais_present_in_wb, number_ais_present, ais_upstream, ais_upstream_number, number_upstream_waterbodies, mean_of_maxent_hab_not_hab) |>
  dplyr::reframe(across(c(Common_Name_EN:ais_present_names,ais_upstream_names,ais_sp_and_mean_effect), \(x) paste0(unique(x), collapse = ', ')),
                 summed_ais_effects = sum(mean_ais_effect)) |>
  dplyr::select(names(dfo_output), summed_ais_effects, ais_sp_and_mean_effect)

# Prepare results for Excel file.

my_wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(my_wb, "output")
openxlsx::writeData(my_wb, "output", dfo_output)
openxlsx::addWorksheet(my_wb, "habitat_suitabilities")
openxlsx::writeData(my_wb, "habitat_suitabilities", hab_suit_df)
openxlsx::saveWorkbook(my_wb, paste0("output/SARA_prioritization_model_",Sys.Date(),".xlsx"), overwrite = T)
