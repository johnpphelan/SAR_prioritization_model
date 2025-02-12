find_downstream_waterbody = function(i,the_wb,d,TO_string){
  
  print(paste0("Working on waterbody ",i,", which is ",the_wb$Waterbody))
  
  geom_and_fwa_for_wb = d |> 
    dplyr::filter(Waterbody == the_wb$Waterbody, Region == the_wb$Region) |> 
    dplyr::select(Waterbody,Region,FWA_WATERSHED_CODE,geometry) |> 
    dplyr::distinct()
  
  if(sf::st_is_empty(geom_and_fwa_for_wb)){
    print(paste0("Warning: ", the_wb$Waterbody, " in ", the_wb$Region," has empty geometry - you should fix this!"))
    next
  }
  
  if(nrow(geom_and_fwa_for_wb) > 1){
    warning(paste0("More than one waterbody found for: ",the_wb$Waterbody, " ",the_wb$Region),
            ". The highest order waterbody will be selected.")
    geom_and_fwa_for_wb<-geom_and_fwa_for_wb |> 
      rowwise() |> 
      mutate(trailing_zeroes = count_trailing_zeroes(FWA_WATERSHED_CODE)) |> 
      ungroup() |> 
      arrange(desc(trailing_zeroes)) %>%  # Sort descending by trailing zeroes
      slice(1)                            # Select the first row (highest order)
  }
  
  the_wb = the_wb |> dplyr::left_join(geom_and_fwa_for_wb, by = join_by(Waterbody, Region))
  the_wb = sf::st_set_geometry(the_wb, "geometry")
  
  # Find downstream waterbody
  # If the chosen waterbody is one of the Thompson string, don't look downstream.
  
  if(the_wb$Waterbody %in% TO_string){
    ds_wb = the_wb |> dplyr::select(geometry)
    ds_wb = ds_wb[0,]
  } else {
    # Waterbodies downstream, up to 5 kilometers away.
    buffer_5km = sf::st_buffer(sf::st_as_sfc(sf::st_bbox(the_wb)),5000)
    wb_fwa_code = the_wb$FWA_WATERSHED_CODE
    ds_wb_fwa_code = stringr::str_replace(wb_fwa_code,"[0-9]{6}(?=\\-000000)","000000")
    ds_river = bcdata::bcdc_query_geodata('freshwater-atlas-rivers') |> filter(FWA_WATERSHED_CODE == ds_wb_fwa_code) |> collect() |> sf::st_transform(4326)
    if(nrow(ds_river) > 0){
      # Find the longest / largest river. Keep that name, in cases where there's multiple.
      largest_river = ds_river |> 
        dplyr::filter(!is.na(GNIS_NAME_1)) |> 
        dplyr::arrange(dplyr::desc(AREA_HA)) |> 
        dplyr::slice(1) |> 
        dplyr::pull(GNIS_NAME_1)
      
      ds_river = ds_river |> 
        dplyr::filter(!is.na(GNIS_NAME_1)) |> 
        dplyr::rename(Waterbody = GNIS_NAME_1) |> 
        dplyr::filter(Waterbody == largest_river) |> 
        dplyr::group_by(Waterbody,FWA_WATERSHED_CODE) |> 
        dplyr::summarise(.groups = 'drop')
    }
    
    ds_lakes = bcdata::bcdc_query_geodata('freshwater-atlas-lakes') |> 
      filter(FWA_WATERSHED_CODE == ds_wb_fwa_code) |> 
      collect() |> sf::st_transform(4326)
    # Check for streams too, if necessary.
    if(nrow(ds_lakes) == 0 & nrow(ds_river) == 0){
      ds_streams = bcdata::bcdc_query_geodata('freshwater-atlas-stream-network') |> 
        filter(FWA_WATERSHED_CODE == ds_wb_fwa_code) |> 
        collect() |> sf::st_zm() |> 
        sf::st_transform(4326)
      
      if(nrow(ds_streams) > 0){
        ds_streams = ds_streams |> 
          dplyr::rename(Waterbody = GNIS_NAME) |> 
          dplyr::group_by(FWA_WATERSHED_CODE, Waterbody) |> 
          dplyr::summarise(.groups = "drop")
        
        ds_streams = ds_streams |> 
          dplyr::arrange(Waterbody) |> 
          dplyr::slice(1)
      }
    }
    
    #modified, removed summary and added a select
    if(nrow(ds_lakes) > 0){
      ds_lakes = ds_lakes |> 
        dplyr::filter(!is.na(GNIS_NAME_1)) |> 
        dplyr::rename(Waterbody = GNIS_NAME_1) |> 
        # dplyr::group_by(Waterbody,FWA_WATERSHED_CODE) |> 
        dplyr::select(Waterbody, FWA_WATERSHED_CODE, AREA_HA, geometry)
    }
    # Some big edge cases: 
    
    # 1. Is the downstream the Columbia River?
    # Since it dips out of BC, then back in, things get very complicated, and 
    # lakes that seem implicated perhaps should not be; in this case, just take
    # the Columbia River, trim it to 5 km, and be done with it!
    
    if("Columbia River" %in% ds_river$Waterbody){
      ds_lakes = ds_lakes[0,]
    }
    
    # 2. is the Thompson-Okanagan string of connected lakes
    # present in the downstream names? If so, those get priority and we 
    # can set the geometry to be the merged lakes.
    
    if("Okanagan Lake" %in% unique(ds_lakes$Waterbody)){
      # Downstream waterbody is the Thompson-Okanagan string!! Assign that to wb_ds
      wbs_rivers = bcdc_query_geodata('freshwater-atlas-rivers') |> 
        filter(GNIS_NAME_1 %in% TO_string) |> 
        collect() |> 
        dplyr::summarise()
      
      wbs_lakes = bcdc_query_geodata('freshwater-atlas-lakes') |> 
        filter(GNIS_NAME_1 %in% TO_string) |> 
        collect() |> 
        dplyr::summarise()
      
      wbs = dplyr::bind_rows(wbs_rivers,wbs_lakes) |> 
        dplyr::summarise() |> 
        dplyr::mutate(Waterbody = 'Okanagan_Lake_System') |> 
        dplyr::mutate(FWA_WATERSHED_CODE = "300-432687-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000") |> 
        sf::st_transform(4326)
      
      ds_lake = wbs
    } else {
      # If there are multiple lakes with the same name and FWA code, merge them
      if(nrow(ds_lakes) > 1 & length(unique(ds_lakes$Waterbody)==1) & length(unique(ds_lakes$FWA_WATERSHED_CODE)==1)){
        ds_lakes = ds_lakes |> 
          dplyr::group_by(Waterbody,FWA_WATERSHED_CODE) |> 
          dplyr::mutate(AREA_HA = sum(AREA_HA,na.rm=T)) |> 
          dplyr::group_by(Waterbody,FWA_WATERSHED_CODE,AREA_HA) |> 
          dplyr::summarise(.groups = 'drop')
      }
      # If there are multiple lakes, take the largest one.
      if(nrow(ds_lakes) > 1){
        chosen_lake = ds_lakes |> 
          dplyr::ungroup() |>                        
          dplyr::arrange(dplyr::desc(AREA_HA)) |>    
          dplyr::slice(1) |>                         
          dplyr::pull(Waterbody)                    
        chosen_lake = chosen_lake[1]
        
        ds_lake = ds_lakes |> 
          dplyr::filter(Waterbody == chosen_lake)
      } else {
        ds_lake = ds_lakes
      }
      # ds_lake = ds_lakes
      # rm(ds_lakes)
    }
    if(nrow(ds_lake) > 0){
      if(ds_lake$Waterbody != the_wb$Waterbody){
        ds_wb = ds_lake
      } else {
        ds_wb = the_wb[0,]
      }
    } else {
      ds_wb = the_wb[0,]
      if(nrow(ds_river) > 0){
        ds_wb = ds_river |> 
          sf::st_intersection(buffer_5km)
      } else {
        if(exists("ds_streams")){
          if(nrow(ds_streams) > 0){
            # No river or lake found! We're using a stream!
            ds_wb = ds_streams |> 
              sf::st_intersection(buffer_5km)
          } else {
            ds_wb = the_wb[0,]
          }
        } else {
          ds_wb = the_wb[0,]
        }
      }
    }
  }
  return(list(the_wb, ds_wb))
}



