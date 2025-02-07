run_maxent = function(species,
                      predictor_data,
                      vars_to_use = c(),
                      show_plots_in_R = FALSE,
                      onedrive_path,
                      seed = 12345,
                      number_pseudoabsences = 5000,
                      number_folds = 5,
                      regularisation_levels = c(1:2),
                      feature_classes = c("L","LQ"),
                      habitat_threshold_var = "equal_training_sensitivity_and_specificity_cloglog_threshold",
                      output_folder = NULL
){
  
  if(is.null(output_folder)) output_folder = getwd()
  
  if(!is.null(seed)) set.seed(seed)
  
  if(is.data.frame(species)){
    # User fed in dataframe of species occurrences.
    if('geom' %in% names(species)){
      dat = species |> 
        dplyr::mutate(x = sf::st_coordinates(geom)[,1],
                      y = sf::st_coordinates(geom)[,2]) |> 
        # Rename geom column to geometry.
        dplyr::rename(geometry = geom)
    } else {
      dat = species |> 
        dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
                      y = sf::st_coordinates(geometry)[,2])
    }
    species_name = dat$Species[1]
  } else {
    if(is.character(species)){
      # User fed in species common name. Search for records.
      dat = suppressMessages(grab_aq_occ_data(species))
      
      species_name = species
      
      dat = dat |> 
        dplyr::select(-c(DataSource,Date,Species,Location,iNat_user,iNat_report_id))
      
      dat$x = sf::st_coordinates(dat$geometry)[,1]
      dat$y = sf::st_coordinates(dat$geometry)[,2]
      
      #dat = sf::st_drop_geometry(dat)
    }
  }
  ## If the species is northern pike, then remove the native northern pike from the data returned
  source("scripts/utils/native_northern_pike_f.R")
  if("Northern pike" %in% pr_sp$Species){
    occ_dat_res_b = remove_native_nPike(occ_dat_res_b)
  }
  # Check for output folder; if it exists already, delete contents.
  output_fn = paste0(output_folder,snakecase::to_snake_case(species_name),"/")
  
  if(is.null(predictor_data)){
    stop("No predictor data entered; please supply at least one {terra} spatRaster.")
  }
  
  # Record the number of occurrence points at this stage in the function; we'll use this
  # in the metadata file that is produced.
  number_occurrences_at_outset = nrow(dat)
  
  # Make {terra} vector of BC.
  bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
  
  # Bring in watercourses to constrain randomly sampled pseudoabsences to biologically meaningful locations
  # for aquatic organisms.
  watercourses = terra::rast(paste0(onedrive_path,"fwa_streams/stream_order_three_plus_2km_res.tif")) 
  
  spatial_extent_resampled = terra::resample(predictor_data[[1]], watercourses)
  # Crop and mask the watercourses raster to fit our spatial extent.
  watercourses = terra::mask(terra::crop(watercourses, spatial_extent_resampled),spatial_extent_resampled)
  
  # If the user has specified a list of predictor variables to use, just keep those.
  if(!is.null(vars_to_use)){
    print(paste0("Constraining predictor raster variables to just: ",paste0(vars_to_use, collapse = ', ')))
    predictor_data = predictor_data[[c(vars_to_use)]]
  }
  
  cat("\nPulling predictor raster values for presence points...\n")
  
  ### move to the nearest raster cell
  
  dat_orig<-dat
  dat<-sf::st_transform(dat, crs(predictor_data))
  dat<-rSDM::points2nearestcell(dat, predictor_data, layer = 1, move = TRUE, distance = 2000)
  
  # locations<- ggplot2::ggplot()+
  #   geom_sf(data=bc_vect, color = "darkgrey")+
  #   geom_sf(data = dat, color = "green")+
  #   ggtitle("Presence locations")+
  #   theme_minimal()
  
  
  
  
   for(raster_var in unique(names(predictor_data))){
    dat[[raster_var]] <- terra::extract(predictor_data[[raster_var]], 
                                        dat[,c("x","y")], ID = FALSE)[[raster_var]]
  }
  
  # Ensure we have a folder to throw images into!
  if (!dir.exists(output_fn)) {
    dir.create(output_fn)
  } else {
    old_files <- list.files(path = output_fn, full.names = TRUE)
    # old_files_to_remove <- old_files[!grepl("\\.jpg$", old_files)]
    # old_files_to_remove <- old_files_to_remove[!grepl("MaxEnt_console_output.txt", old_files_to_remove)]
    # old_files_to_remove <- old_files_to_remove[!grepl("pres_bg_boxplot.png", old_files_to_remove)]
    
    if (length(old_files) > 0) {
      cat("\nDeleting old contents of results folder...\n")
      file.remove(old_files)
    }
  }
  
  # ggplot2::ggsave(filename = paste0(output_fn,species_name, "_locations.jpg"),
  #        plot = locations,
  #        dpi = 300, width = 8, height = 8)
  
  dat_just_pred_vars = sf::st_drop_geometry(dat[,c(names(predictor_data))])
  
  
  tryCatch(
    expr = {
      cor <- ENMTools::raster.cor.matrix(predictor_data)
      thresh<-0.6 # this could be causing problems - how else could this be done? the value could be too low now...
      dists<-as.dist(1-abs(cor))
      clust <- hclust(dists, method = "single")
      groups <- cutree(clust, h = 1 - thresh)
      jpeg(paste0(output_fn,"clusterDendogram.jpg"), width = 1200, height = 800)
      ## Visualize groups
      plot(clust, hang = -1)
      rect.hclust(clust, h = 1 - thresh)
      dev.off()
      
      unique_groups <- unique(groups)
      selected_predictors <- sapply(unique_groups, function(group_num) {
        group_indices <- which(groups == group_num)
        return(names(groups)[group_indices[1]])
      })
      predictor_data_full<-predictor_data
      predictor_data<-predictor_data_full[[selected_predictors]]
    },
    error = function(e) NULL
  )
  
  
  # cor<-raster.cor.matrix(predictor_data)
  # thresh<-0.5
  # dists<-as.dist(1-abs(cor))
  # clust <- hclust(dists, method = "single")
  # groups <- cutree(clust, h = 1 - thresh)
  # jpeg(paste0(output_fn,"clusterDendogram_reduced.jpg"), width = 1200, height = 800)
  # ## Visualize groups:
  # plot(clust, hang = -1)
  # rect.hclust(clust, h = 1 - thresh)
  # dev.off()
  
  # Remove samples lacking predictor raster values?
  keep_ind = complete.cases(dat_just_pred_vars)
  dat = dat[keep_ind,]
  
  # Test collinearity
  # pred_vals = dat[,c(names(predictor_data))]
  
  if(show_plots_in_R){
    pairs(dat_just_pred_vars, lower.panel = panel.smooth2, upper.panel = panel.cor, diag.panel = panel.hist)
  }
  
  cor_res = cor(dat_just_pred_vars |> dplyr::select(dplyr::where(is.numeric))) |>
   as.data.frame()
  
  # # Pull out highly correlated variables.
  # highly_correlated_vars = cor_res |>   
  #   tidyr::as_tibble() |> 
  #   dplyr::mutate(var_2  = row.names(cor_res)) |> 
  #   # Gather table long so we have a column for each of the two-variable comparisons
  #   tidyr::pivot_longer(cols = -c(var_2)) |> 
  #   # Drop rows where a variable was being compared with itself
  #   dplyr::filter(var_2 != name) |> 
  #   # Filter by some arbitrary cut-off - what is 'too' correlated??
  #   dplyr::filter(abs(value) >= 0.8) |> 
  #   # Switch into 'rowwise' mode - this is like looping by row.
  #   dplyr::rowwise() |> 
  #   # Make a column that lists which two variables are being compared, 
  #   # sorted alphabetically.
  #   dplyr::mutate(variable_combo = list(c(var_2, name))) |>
  #   dplyr::mutate(variable_combo = paste0(variable_combo[order(variable_combo)], collapse = '-')) |> 
  #   # Exit 'rowwise' mode with an ungroup()
  #   dplyr::ungroup() |> 
  #   # Filter using that alphabetical var name column; this prevents duplication of results.
  #   dplyr::filter(duplicated(variable_combo))
  # 
  # # predictor_data_low_cor = predictor_data[[names(dat)[-c(1,2)]]]
  # predictor_data_low_cor = predictor_data
  
  # predictor_data
  # Pull out x and y coordinates for presences
  presences = sf::st_drop_geometry(dat[,c('x','y')])
  
  # Make sure presences are distinct.
  presences = dplyr::distinct(presences)
  
  # Sample watercourses' locations for a collection of pseudoabsences; combine with data and then split into testing and training.
  pseudoabsences <- predicts::backgroundSample(watercourses, p = terra::vect(dat), n = number_pseudoabsences,
                                extf = 0.9) %>% 
    as.data.frame()
  
  
  
  presencedat<- dat |> 
    dplyr::select(x, y, Species)
  
  for(raster_var in unique(names(predictor_data))){
    presencedat[[raster_var]] <- terra::extract(predictor_data[[raster_var]], 
                                                presencedat[,c("x","y")], ID = FALSE)[[raster_var]]
  }
  
  presData<-presencedat[, 5:ncol(presencedat)]
  presData$presence <- 1
  pseudoDat<-pseudoabsences
  pseudoDat$fill<-0
  pseudoDat$long<-pseudoDat$x
  pseudoDat$lat<-pseudoDat$y
  pseudoDat<-st_as_sf(pseudoDat, coords = (c("long", "lat")), crs = 4326)
  #not getting variables - why?
  for(raster_var in unique(names(predictor_data))){
    pseudoDat[[raster_var]] <- terra::extract(predictor_data[[raster_var]], 
                                              pseudoDat[,c("x","y")], ID = FALSE)[[raster_var]]
  }
  
  
  pDat<-pseudoDat[, 5:ncol(pseudoDat)]
  pDat$presence<-0
  pDat <- pDat |> st_drop_geometry()
  presData <- presData |> st_drop_geometry()
  
  tot_box <-rbind(presData, pDat)
  tot_box$index<- 1:nrow(tot_box)
  
  data.table::setDT(tot_box)
  melt_box <- data.table::melt(tot_box, id.vars = c("presence", "index"), measure.vars = c(1:(ncol(tot_box)-2)))
  
  # levels(melt_box$variable) <- c(
  #   "pH" = "pH",
  #   "calc" = "Calcium",
  #   "population_density" = "Population Density",
  #   "elev" = "Elevation",
  #   "TotalInspections" = "Total Inspections",
  #   "days_fished" = "Days Fished",
  #   "Carbon_Dissolved_Organic" = "Dissolved Organic Carbon",
  #   "Chlorophyll" = "Chlorophyll",
  #   "Conductivity" = "Conductivity",
  #   "Oxygen_Dissolved" = "Dissolved Oxygen",
  #   "Turbidity" = "Turbidity",
  #   "temp_Summer" = "Summer Temperature",
  #   "temp_Winter" = "Winter Temperature",
  #   "Water_Temperature" = "Water Temperature",
  #   "pres" = "Pressure",
  #   "slope" = "slope"
  # )
  
  melt_box$presence <- factor(melt_box$presence, levels = c(1, 0), labels = c("Present", "Absence"))
  
  # Make sure value is numeric.
  melt_box$value = as.numeric(melt_box$value)
  
  predictor_box<-ggplot(melt_box, aes(x = as.factor(presence), y = value, fill = variable)) +
    geom_boxplot() +
    facet_wrap(~ variable, scales = "free_y") +
    labs(title = "Absence and Presence by Predictor") +
    ylab("")+
    xlab("")+
    theme_minimal()+
    theme(
      strip.text = element_text(size = 12, face = "bold"), 
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
      axis.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
      legend.position = 'none'
    )
  ggplot2::ggsave(filename = paste0(output_fn,"pres_bg_boxplot.jpg"),
                  plot = predictor_box,
                  dpi = 300, width = 8, height = 8)
  
  # Make MaxEnt model
  cat("\nMaking MaxEnt Model...")
  
  me = ENMevaluate(occs = presences,
                   envs = predictor_data,
                   bg = pseudoabsences,
                   algorithm = 'maxent.jar',
                   partitions = 'block',
                   tune.args = list(fc = feature_classes,
                                    rm = regularisation_levels))
  
  
  
  top5 <- me@results |> 
    filter(!is.na(AICc)) |> 
    mutate(auc_cbi_mix = (as.numeric(auc.train) + as.numeric(cbi.train)) / 2) |> 
    arrange(desc(auc_cbi_mix)) |> 
    slice_head(n = 5)

  write.table(top5, paste0(output_fn, "top_5_models.txt"), row.names = F)
  topfc<-as.character(top5[1,]$fc)
  toprm<-as.character(top5[1,]$rm)
  
  opt.aicc<- eval.results(me) |> 
    dplyr::filter(fc == topfc & rm == toprm)
  
  # Find which model had the lowest AIC; we'll use this for now.
  # opt.aicc = eval.results(me) |> dplyr::filter(delta.AICc == 0)
  
  var_importance = me@variable.importance[[opt.aicc$tune.args]]
  
  predictions = terra::rast(eval.predictions(me)[[opt.aicc$tune.args]])
  
  # eval_model<- eval.models(me)[[opt.aicc$tune.args]]
  
  # eval_plot<-eval_model
  # Pull out maxent's predictions for occurrence locations.

  # Check out results - this dataframe could be simplified to just hone in 
  # on the particular metrics we are curious about!
  maxent_results = me@results |> 
    dplyr::filter(tune.args == opt.aicc$tune.args) |> 
    tidyr::as_tibble() |> 
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) as.character(x))) |> 
    tidyr::pivot_longer(cols = dplyr::everything()) |> 
    dplyr::add_row(name = "regularisation_levels_tested", value = paste0(regularisation_levels, collapse = ', ')) |> 
    dplyr::add_row(name = "feature_classes_tested", value = paste0(feature_classes, collapse = ', '))
  
  maxent_results.partitions = me@results.partitions |> 
    dplyr::filter(tune.args == opt.aicc$tune.args) |> 
    tidyr::as_tibble()
  
  maxent_html = me@models[[opt.aicc$tune.args]]@html
  
  single_model_metrics = me@models[[opt.aicc$tune.args]]@results[,1] |> 
    as.matrix() |> 
    as.data.frame()
  
  single_model_metrics = single_model_metrics |> 
    dplyr::mutate(metric = snakecase::to_snake_case(rownames(single_model_metrics))) |>
    dplyr::rename(value = V1) |>
    tidyr::as_tibble() |>
    dplyr::select(metric, value)
  
  key_metrics = single_model_metrics |>
    dplyr::filter(metric %in% c("x_training_samples","training_auc",habitat_threshold_var) | str_detect(metric,".*_contribution") | str_detect(metric,".*permutation_imp.*"))
  
  pres_sf = sf::st_as_sf(me@occs, coords = c("x","y"), crs = 4326)
  pres_sf$groups = me@occs.grp
  absences_sf = sf::st_as_sf(me@bg, coords = c("x","y"), crs = 4326)
  
  points_sf = dplyr::bind_rows(
    pres_sf |> dplyr::mutate(type = 'presence'), 
    absences_sf |> dplyr::mutate(type = 'pseudoabsence')
  )
  
  # Calculate some values to use as labels and captions in the figure.
  train_samp = key_metrics[key_metrics$metric == 'x_training_samples',]$value
  maxent_results<-as.data.frame(maxent_results)
  train_auc = maxent_results[maxent_results$name == 'train.auc',]$value
  
  metrics_caption = var_importance |> 
    dplyr::select(variable, percent.contribution) |> 
    dplyr::arrange(dplyr::desc(percent.contribution)) |> 
    dplyr::filter(percent.contribution >= 0.01) |> 
    dplyr::mutate(title_metric = stringr::str_replace_all(variable,"_"," ")) |>
    dplyr::rowwise() |>
    dplyr::summarise(v = paste0(stringr::str_to_title(title_metric),': ',round(percent.contribution,2),"%")) |>
    dplyr::ungroup() |>
    dplyr::summarise(paste0(v, collapse = '<br>'))
  
  predictions_plot = ggplot() +
    tidyterra::geom_spatraster(data = predictions) +
    geom_sf(data = points_sf, aes(col = type, alpha = type, shape = type)) +
    scale_colour_manual(values = c('presence' = "red", 'pseudoabsence' = "purple")) +
    scale_alpha_manual(values = c('presence' = 1, 'pseudoabsence' = 0.4), guide = 'none') +
    scale_shape_manual(values = c('presence' = 19, 'pseudoabsence' = 4)) +  
    scale_fill_viridis_c() +
    labs(title = paste0(stringr::str_to_title(species_name)),
         subtitle = paste0("Number of Training Data Points: ",train_samp,
                           "<br>Training Area-Under-Curve: ",round(as.numeric(train_auc),4)),
         caption = metrics_caption,
         fill = "Predicted \nRelative \nSuitability",
         color = "Sample Type",
         shape = "Sample Type") +  # Add shape to the legend
    theme(
      plot.subtitle = ggtext::element_markdown(),
      plot.caption = ggtext::element_markdown()
    )
  
  predictions_plot_blank <- ggplot() +
    tidyterra::geom_spatraster(data = predictions) +
    scale_fill_viridis_c(option = "D", na.value = NA)+
    labs(fill = paste0("Habitat suitability: \n",dat$Species[1]))+
    theme_minimal()
  
  # Predicted habitat vs. not habitat plot, using 
  # whichever threshold approach selected in function call.
  habitat_or_not = me@predictions[[opt.aicc$tune.args]]
  
  threshold_value = key_metrics |> 
    dplyr::filter(metric == habitat_threshold_var) |> 
    dplyr::pull(value)
  
  habitat_or_not[habitat_or_not < threshold_value] = FALSE
  habitat_or_not[habitat_or_not >= threshold_value] = TRUE
  
  fit = terra::extract(
    predictions,
    points_sf |>
    terra::vect()
    )
  
  names(fit)[2] = 'maxent'
  
  obs = terra::extract(
    predictions,
    points_sf |>
      dplyr::filter(type == "presence") |> 
      terra::vect()
  )
  
  # Make a masked version of the predictions, masked by watercourses.
  predictions_aq = terra::mask(terra::crop(terra::resample(predictions, watercourses),watercourses),watercourses)
  
  file_version_csv = data.frame(
    run_date = lubridate::ymd(Sys.Date()),
    number_occurrences = number_occurrences_at_outset
  )
  
  # save to standalone file - to local file, then copy to lan
  file.copy(from = maxent_html, to = paste0(output_fn,"MaxEnt_results.html"))
  write.csv(key_metrics, paste0(output_fn,"MaxEnt_key_metrics.csv"), row.names = F)
  write.csv(maxent_results, paste0(output_fn,"MaxEnt_Detailed_Model_Fitting_results.csv"), row.names = F)
  write.csv(file_version_csv, paste0(output_fn,"MaxEnt_model_run_metadata.csv"), row.names = F)
  terra::writeRaster(predictions, paste0(output_fn,"MaxEnt_prediction_raster.tif"))
  terra::writeRaster(predictions_aq, paste0(output_fn,"MaxEnt_prediction_raster_masked_by_watercourses.tif"))
  terra::writeRaster(habitat_or_not, paste0(output_fn,"MaxEnt_prediction_habitat_or_not.tif"))
  ggplot2::ggsave(filename = paste0(output_fn,"MaxEnt_prediction_plot.png"),
                  plot = predictions_plot,
                  dpi = 300, width = 8, height = 8)
  ggplot2::ggsave(filename = paste0(output_fn,"MaxEnt_prediction_plot_no_occ.jpg"),
                  plot = predictions_plot_blank,
                  dpi = 300, width = 8, height = 8)

  cat("\nFiles written to output folder...\n")
  
  return(
    list(model_fit = me,
         maxent_results = maxent_results,
         key_metrics = key_metrics,
         predictions_r = predictions,
         predictions_plot = predictions_plot,
         eval_plot = eval_plot,
         habitat_predictions = habitat_or_not
    )
  )
}

