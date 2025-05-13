##### Making a plot - not done - ignore or delete

# library(ggspatial)
# library(leaflet)
# 
# # Transform the data to EPSG:3857
# dfo_sar_4326 <- st_transform(dfo_sar, 4326)
# 
# dfo_sar_points <- dfo_sar_4326[st_geometry_type(dfo_sar_4326) %in% c("POINT", "MULTIPOINT"), ]
# dfo_sar_polygons <- dfo_sar_4326[!st_geometry_type(dfo_sar_4326) %in% c("POINT", "MULTIPOINT"), ]
# 
# p <- leaflet() %>%
#   # Add a base map.  You can change the provider.
#   addTiles()
# 
# # Add polygons if there are any
# if (nrow(dfo_sar_polygons) > 0) {
#   p <- p %>%
#     addPolygons(
#       data = dfo_sar_polygons,
#       fillColor = ~ colorFactor("Set1", Common_Name_EN)(Common_Name_EN),
#       color = "black",
#       weight = 1,
#       opacity = 1,
#       fillOpacity = 0.7,
#       popup = ~paste0("Species: ", Common_Name_EN)
#     ) %>%
#     # Add a layer control to toggle species on/off
#     addLegend(
#       "bottomright",
#       pal = colorFactor("Set1", dfo_sar_polygons$Common_Name_EN),
#       values = dfo_sar_polygons$Common_Name_EN,
#       title = "Species"
#     )
# }
# 
# # Add points if there are any
# if (nrow(dfo_sar_points) > 0) {
#   p <- p %>%
#     addMarkers(
#       data = dfo_sar_points,
#       popup = ~paste0("Species: ", Common_Name_EN),
#       clusterOptions = TRUE # Add this for point clustering
#     )
# }
# # Print the map
# print(p)