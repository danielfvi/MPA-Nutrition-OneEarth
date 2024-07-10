library(tidyverse)
library(readxl)
library(raster)
library(sf)

# Clear workspace
rm(list = ls())

saup_data_2014 <- readRDS("~/MPA_Nutrition/data/saup_data_2014.rds")
cell_id_lookup <- read_excel("data/cell_id_lookup.xlsx")
ChrisGolden_202212151042 <- read_csv("data/ChrisGolden_202212151042.csv")

##Price
price = ChrisGolden_202212151042 %>%
  mutate(price = value/catch) %>% 
  group_by(fishing_entity, scientific_name, sector_type) %>% 
  summarise(price = mean(price, na.rm = T)) %>% 
  rename(fishing_entity_name = fishing_entity,
         taxon_scientific_name = scientific_name,
         sector_type_name = sector_type)

saup_data_2014_val = saup_data_2014 %>% 
  left_join(price)

missing = saup_data_2014_val %>% 
  filter(is.na(price))

price_sci = price %>% 
  group_by(taxon_scientific_name) %>% 
  summarise(price = mean(price, na.rm = T))

missing2 = missing %>% 
  dplyr::select(-price) %>% 
  left_join(price_sci)

missing3 = missing2 %>% 
  filter(is.na(price))

price_func = ChrisGolden_202212151042 %>%
  mutate(price = value/catch) %>% 
  group_by(functional_group) %>% 
  summarise(price = mean(price, na.rm = T)) %>% 
  rename(functional_group_name = functional_group)

missing4 = missing3 %>% 
  dplyr::select(-price) %>% 
  left_join(price_func)

final_value = rbind(saup_data_2014_val %>% 
                      filter(!is.na(price)),
                    missing2 %>% 
                      filter(!is.na(price)),
                    missing4) %>% 
  mutate(value = catch_sum*price)

final_dta = final_value %>% 
  group_by(cell_id) %>% 
  summarise(catch = sum(catch_sum, na.rm = T),
            value = sum(value, na.rm = T))

write.csv(final_dta, "Outputs/cellID_dta_v2.csv", row.names = F)

##Calculate nutrient
spp_nutrient <- read_csv("data/spp_nutrient_final.csv") %>% 
  dplyr::select(species, nutrient, value) %>% 
  rename(value_nut = value,
         taxon_scientific_name = species)

nut = unique(spp_nutrient$nutrient)

for(n in 1:length(nut)){
  
  print(n)
  print(nut[n])
  spp_nut = spp_nutrient %>% 
    filter(nutrient == nut[n])
  
  saup_data_2014_nut = final_value %>%
    mutate(taxon_scientific_name = tolower(taxon_scientific_name)) %>% 
    left_join(spp_nut)
  
  missing = saup_data_2014_nut %>% 
    filter(is.na(value_nut))
  
  nut_func = saup_data_2014_nut %>% 
    group_by(nutrient, functional_group_name) %>% 
    summarise(value_nut = mean(value_nut, na.rm = T))
  
  missing2 = missing %>% 
    dplyr::select(-value_nut) %>% 
    left_join(nut_func)
  
  final_value_nut = rbind(saup_data_2014_nut %>% 
                            filter(!is.na(value_nut)),
                          missing2) %>% 
    mutate(nut_supply = value_nut*catch_sum*0.7*10000) %>% 
    group_by(cell_id, nutrient) %>% 
    summarise(nut_supply = sum(nut_supply, na.rm = T)) %>% 
    drop_na(nutrient)
  
  if(n==1){
    dta = final_value_nut
  }else{
    dta = rbind(dta, final_value_nut)
  }
}

write.csv(dta, "Outputs/cellID_dta_nut.csv", row.names = F)






###########Old code
##Add nutrient
spp_nutrient <- read_csv("data/spp_nutrient_final.csv") %>% 
  filter(nutrient == "Vitamin B12") %>% 
  dplyr::select(species, value) %>% 
  rename(value_nut = value,
         taxon_scientific_name = species)

saup_data_2014_nut = final_value %>%
  mutate(taxon_scientific_name = tolower(taxon_scientific_name)) %>% 
  left_join(spp_nutrient)

missing = saup_data_2014_nut %>% 
  filter(is.na(value_nut))

nut_func = saup_data_2014_nut %>% 
  group_by(functional_group_name) %>% 
  summarise(value_nut = mean(value_nut, na.rm = T))

missing2 = missing %>% 
  dplyr::select(-value_nut) %>% 
  left_join(nut_func)

EAR <- read_csv("data/EAR.csv")
##EAR for vit B12 is 2, 25% of EAR is 0.5

final_value_nut = rbind(saup_data_2014_nut %>% 
                          filter(!is.na(value_nut)),
                        missing2) %>% 
  mutate(nut_supply = value_nut*catch_sum*0.7*10000,
         n_people = nut_supply/0.5)

final_dta = final_value_nut %>% 
  group_by(cell_id) %>% 
  summarise(catch = sum(catch_sum, na.rm = T),
            value = sum(value, na.rm = T),
            nut_supply = sum(nut_supply, na.rm = T),
            n_people = sum(n_people, na.rm = T))