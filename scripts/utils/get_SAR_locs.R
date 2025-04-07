library(tidyverse)
library(sf)
library(bcmaps)
# open the data/cnf folder 
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/CNF/")


init_sara <-sf::st_read(paste0(onedrive_wd,"dfo_occurrences_in_BC_all_species.gpkg"))
bull_sock <- sf::st_read(paste0(onedrive_wd,"dfo_sara_and_crit_hab_bulltrout_and_sockeye_data.gpkg"))
init_sara<- sf::st_transform(init_sara, 4326)
bull_sock<-sf::st_transform(bull_sock, 4326)
sara_locs = dplyr::bind_rows(init_sara, bull_sock)



ggplot()+
  geom_sf(data = bull_sock)

sar_spp = c('Westslope Cutthroat Trout', 'Bull Trout', 'Cultus Pygmy Sculpin',
'Sockeye Salmon','Rocky Mountain Ridged Mussel')

sara_filtered<- sara_locs |> 
  dplyr::filter(tolower(Common_Name_EN) %in% tolower(sar_spp))

ggplot()+
  geom_sf(data = sara_filtered, aes(color = Common_Name_EN))



rd<-regional_districts()
rd$ADMIN_AREA_NAME

frasColum<- rd %>% 
  filter(str_detect(ADMIN_AREA_NAME, "\\Fraser") | 
           str_detect(ADMIN_AREA_NAME, "\\Columbia"))
nrR<-bcmaps::nr_regions()
frasColum<- nrR %>% 
  filter(str_detect(ORG_UNIT_NAME, "\\Fraser") | 
           str_detect(ORG_UNIT_NAME, "\\Columbia")) |> 
  st_transform(4326)

bc<-bc_bound()
southcoast = suppressMessages(bcmaps::nr_regions())
frasColum<- southcoast %>% 
  filter(REGION_NAME == "South Coast Natural Resource Region" |
           REGION_NAME == "Thompson-Okanagan Natural Resource Region" |
           REGION_NAME == "Kootenay-Boundary Natural Resource Region" |
           REGION_NAME == "Cariboo Natural Resource Region" 
  )


# ggplot()+
#   geom_sf(data = bc)+
#   geom_sf(data = frasColum)
sara_filtered <- sf::st_transform(sara_filtered, crs = 3005)
frasColum <- sf::st_transform(frasColum, crs = 3005)
sara_filtered_frasColum <- sf::st_intersection(sara_filtered, frasColum)

ggplot() +
  geom_sf(data = sara_filtered_frasColum, aes(color = Common_Name_EN))+
  facet_wrap(~Common_Name_EN)

st_write(sara_filtered_frasColum, "data/sara_filtered_frasColum.gpkg",
         delete_dsn = TRUE, quiet = TRUE)
  
