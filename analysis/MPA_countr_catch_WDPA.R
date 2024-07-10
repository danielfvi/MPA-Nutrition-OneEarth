library(tidyverse)
library(readxl)
library(raster)
library(sf)
library(leaflet)

# Clear workspace
rm(list = ls())

#saup_data_2014 <- readRDS("~/MPA_Nutrition/data/saup_data_2014.rds")
cell_id_lookup <- read_excel("data/cell_id_lookup.xlsx")
#ChrisGolden_202212151042 <- read_csv("data/ChrisGolden_202212151042.csv")
cellID_dta <- read_csv("Outputs/cellID_dta.csv")

cell_dta = cell_id_lookup %>% 
  left_join(cellID_dta)

#MPAtlas_us <- read_excel("data/MPAtlas_United States (national waters)_MPAs_2024-01-11.xlsx")
MPAs1 <- read_sf('data/WDPA', layer = 'WDPA_WDOECM_Jan2024_Public_marine_shp-polygons') 
MPAs2 <- read_sf('data/WDPA', layer = 'WDPA_WDOECM_Jan2024_Public_marine_shp-polygons(1)') 
MPAs3 <- read_sf('data/WDPA', layer = 'WDPA_WDOECM_Jan2024_Public_marine_shp-polygons(2)') 

MPAs = rbind(MPAs1, MPAs2, MPAs3)

MPAs = MPAs %>% 
  mutate(MPA_category = if_else(NO_TAKE=="All", "NT", "MU")) %>% 
  filter(MPA_category == "MU")

sf_use_s2(FALSE)

##EEZ
eez <- read_sf('spatial data/World_EEZ_v12_20231025', layer = 'eez_v12') %>% st_transform(4326)

eez = eez %>% 
  mutate(iso3c = if_else(is.na(ISO_TER1), ISO_SOV1, ISO_TER1))

eez_countries = unique(eez$iso3c)


##Create raster
firstRaster <- raster(xmn = -180,   # set minimum x coordinate
                      xmx = 180,    # set maximum x coordinate
                      ymn = -90,     # set minimum y coordinate
                      ymx = 90,     # set maximum y coordinate
                      res = c(0.5,0.5)) # resolution in c(x,y) direction

#firstRaster[] <- seq(from = 1, to = ncell(firstRaster),by = 1)
#firstRaster

#plot(r)

# leaflet() %>%
#   addTiles() %>%
#   #addPolygons(data = eez_country) %>%
#   addPolygons(data = x, color = "red") #%>%
  #addRasterImage(r)
# plot(ras2, add=T)
# plot(eez_country)
# plot(MPA_final, col = "red")

MPA_countries = data.frame(country = unique(MPAs$ISO3)) %>% 
  filter(!country %in% c("MNP;UMI", 
                         "FIN;SWE",
                         "MNP;GUM",
                         "FRA;ITA;MCO",
                         "BLM;GLP;MAF;MTQ",
                         "ABNJ",
                         "NLD;DEU;DNK",
                         "SGP",
                         "HKG",
                         "ALA",
                         "IOT",
                         "NIU",
                         "ATA",
                         "PYF",
                         "BVT",
                         "BHR")) %>% 
  drop_na(country)

MPA_countries = unique(MPA_countries$country)


###############MPA catch per country
#Catch per cell
cell = cell_dta %>% 
  drop_na(catch)

r <- rasterize(cbind(cell$lon, cell$lat), firstRaster, cell$catch)
#plot(r)

##MPA Catch for each country
for(k in 1:length(MPA_countries)){
  
  print(k)
  print(MPA_countries[k])
  
  country_MPA = MPAs %>% 
    filter(str_detect(ISO3, MPA_countries[k]))
  
  country_MPA_dta = country_MPA %>% 
    st_drop_geometry()
  
  eez_country = eez %>% 
    filter(iso3c == MPA_countries[k]) %>% 
    st_union()
  
  MPA_inter = st_intersection(country_MPA, eez_country)
  
  ##EEZ
  eez_final = as(eez_country, 'Spatial')
  
  ras <- mask(r, eez_final)
  
  eez_catch = as.data.frame(ras, xy=TRUE) %>% 
    drop_na(layer)
  
  eez_catch = sum(eez_catch$layer)
  
  ##MPA
  MPA_union = st_union(MPA_inter)
  
  MPA_final = as(MPA_union, 'Spatial')
  
  ras2 <- mask(r, MPA_final)
  
  mpa_catch = as.data.frame(ras2, xy=TRUE) %>% 
    drop_na(layer)
  
  mpa_catch = sum(mpa_catch$layer)
  
  if(k==1){
    dta = data.frame(country = MPA_countries[k],
                     catch_mpa = mpa_catch,
                     catch_eez = eez_catch)
  }else{
    dta = rbind(dta, 
                data.frame(country = MPA_countries[k],
                           catch_mpa = mpa_catch,
                           catch_eez = eez_catch))
    
  }
}

write.csv(dta, "Outputs/MPA_country_catch_WDPA.csv", row.names = F)

###############MPA catch value per country
#Catch per cell
cell = cell_dta %>% 
  drop_na(value)

r <- rasterize(cbind(cell$lon, cell$lat), firstRaster, cell$value)
#plot(r)

##MPA Catch for each country
for(k in 1:length(MPA_countries)){
  
  print(k)
  print(MPA_countries[k])
  
  country_MPA = MPAs %>% 
    filter(str_detect(ISO3, MPA_countries[k]))
  
  eez_country = eez %>% 
    filter(iso3c == MPA_countries[k]) %>% 
    st_union()
  
  MPA_inter = st_intersection(country_MPA, eez_country)
  
  ##EEZ
  eez_final = as(eez_country, 'Spatial')
  
  ras <- mask(r, eez_final)
  
  eez_catch = as.data.frame(ras, xy=TRUE) %>% 
    drop_na(layer)
  
  eez_catch = sum(eez_catch$layer)
  
  ##MPA
  MPA_union = st_union(MPA_inter)
  
  MPA_final = as(MPA_union, 'Spatial')
  
  ras2 <- mask(r, MPA_final)
  
  mpa_catch = as.data.frame(ras2, xy=TRUE) %>% 
    drop_na(layer)
  
  mpa_catch = sum(mpa_catch$layer)
  
  if(k==1){
    dta = data.frame(country = MPA_countries[k],
                     catch_mpa = mpa_catch,
                     catch_eez = eez_catch)
  }else{
    dta = rbind(dta, 
                data.frame(country = MPA_countries[k],
                           catch_mpa = mpa_catch,
                           catch_eez = eez_catch))
    
  }
}


write.csv(dta, "Outputs/MPA_country_value_WDPA.csv", row.names = F)

###############MPA nutrient supply per country
#Catch per cell
cell = cell_dta %>% 
  drop_na(n_people)

r <- rasterize(cbind(cell$lon, cell$lat), firstRaster, cell$nut_supply)
#plot(r)

##MPA Catch for each country
for(k in 1:length(MPA_countries)){
  
  print(MPA_countries[k])
  
  country_MPA = MPAs %>% 
    filter(str_detect(country, MPA_countries[k]))
  
  eez_country = eez %>% 
    filter(iso3c == MPA_countries[k]) %>% 
    st_union()
  
  MPA_inter = st_intersection(country_MPA, eez_country)
  
  ##EEZ
  eez_final = as(eez_country, 'Spatial')
  
  ras <- mask(r, eez_final)
  
  eez_catch = as.data.frame(ras, xy=TRUE) %>% 
    drop_na(layer)
  
  eez_catch = sum(eez_catch$layer)
  
  ##MPA
  MPA_union = st_union(MPA_inter)
  
  MPA_final = as(MPA_union, 'Spatial')
  
  ras2 <- mask(r, MPA_final)
  
  mpa_catch = as.data.frame(ras2, xy=TRUE) %>% 
    drop_na(layer)
  
  mpa_catch = sum(mpa_catch$layer)
  
  if(k==1){
    dta = data.frame(country = MPA_countries[k],
                     catch_mpa = mpa_catch,
                     catch_eez = eez_catch)
  }else{
    dta = rbind(dta, 
                data.frame(country = MPA_countries[k],
                           catch_mpa = mpa_catch,
                           catch_eez = eez_catch))
    
  }
}


write.csv(dta, "Outputs/MPA_country_nut.csv", row.names = F)
