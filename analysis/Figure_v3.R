#Figure
library(tidyverse)
library(ggpubr)
library(sf)
library(ggnewscale)
library(countrycode)
library(ggridges)
library(viridis)

# Clear workspace
rm(list = ls())

MPA_country_catch <- read_csv("Outputs/MPA_country_catch_WDPA.csv")
MPA_country_value <- read_csv("Outputs/MPA_country_value_WDPA.csv")
MPA_country_nut <- read_csv("Outputs/MPA_country_nut_WDPA.csv")
perc_pop_deficient <- read_csv("data/2017_perc_pop_deficient.csv") %>% 
  group_by(iso3) %>% 
  summarise(perc_deficient = mean(perc_deficient)) %>% 
  mutate(is_def = if_else(perc_deficient>25, "yes", "no")) %>% 
  rename(country = iso3)

##Map 1 - % catch within MPAs and total catch per MPA

perc_catch = MPA_country_catch %>% 
  mutate(perc_catch = 100*catch_mpa/catch_eez,
         var = "Catch",
         fig = "figA") %>% 
  drop_na(perc_catch) 
  
perc_value = MPA_country_value %>% 
  mutate(perc_catch = 100*catch_mpa/catch_eez,
         var = "Revenue",
         fig = "figA") %>% 
  drop_na(perc_catch)

perc_nut = MPA_country_nut %>% 
  mutate(perc_catch = 100*catch_mpa/catch_eez,
         fig = "figB") %>% 
  drop_na(perc_catch) %>% 
  rename(var = nut_2)

dta_all = rbind(perc_catch, perc_value, perc_nut)

#Load world map
world <- rnaturalearth::ne_countries("small", returnclass = "sf")

# Extract French Guiana
fguiana <-world %>% 
  sf::st_cast(to="POLYGON") %>% 
  filter(gu_a3=="FRA") %>% 
  mutate(id=1:n()) %>% 
  select(id) %>% 
  filter(id==1)

sf_use_s2(FALSE)

#World centroids
world_lg <- rnaturalearth::ne_countries(scale="large", returnclass = "sf") %>% 
  mutate(area_sqkm=sf::st_area(.)/(1000*1000)) %>%
  mutate(area_sqkm=as.numeric(area_sqkm)) %>% 
  sf::st_centroid() %>% 
  select(continent, subunit, su_a3, area_sqkm) %>% 
  rename(country=subunit, iso3=su_a3) 

# Small nation centroids
world_tiny <- rnaturalearth::ne_countries(type="tiny_countries", returnclass = "sf") %>% 
  select(continent, subunit, su_a3) %>% 
  rename(country=subunit, iso3=su_a3) %>% 
  mutate(area_sqkm=10)

# Merge centroids
world_centers <- bind_rows(world_lg, world_tiny)

# Base theme
base_theme <- theme(axis.text=element_blank(),
                    axis.title=element_blank(),
                    legend.text=element_text(size=15),
                    legend.title=element_text(size=15),
                    strip.text=element_blank(),
                    plot.title=element_text(size=7),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.position=c(0.11,0.35),
                    legend.background = element_rect(fill=alpha('blue', 0)))


#################################################################Percent of catch


# Format data
mpa_dta_sf <- world %>% 
  left_join(perc_catch, by=c("gu_a3"="country"))

##Spatialize tiny
sdata_pt <- world_centers %>%
  left_join(perc_catch, by=c("iso3"="country")) %>%
  # Reduce to ones with data
  filter(!is.na(perc_catch)) %>%
  arrange(area_sqkm) %>%
  # Reduce to small
  filter(area_sqkm<=2.5*10^4 & continent!="Europe")

p_map_catch <- ggplot(mpa_dta_sf) +
  geom_sf(mapping=aes(fill=perc_catch), lwd=0, colour = "black") +
  # Plot small places
  geom_sf(data=sdata_pt, mapping=aes(fill=perc_catch), shape=21, size=3, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Legend and labels
  scale_fill_gradientn(name = "Catch from\nMPAs (%)",
                       colors = RColorBrewer::brewer.pal(9, "YlOrRd"), 
                       na.value = "grey87") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 4)) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  #labs(title = "A") +
  theme_bw() + base_theme +
  theme(legend.position="left",
        legend.box="vertical",
        legend.key.width = unit(10, "mm"),
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.text = element_text(hjust = 0),
        plot.title = element_text(face = "bold", size = 13),
        plot.margin = margin(t = 0, r = 0.7, l = 1, b = 0, "cm"))
p_map_catch

#######################################Global percentages
#Global percent
perc_catch_total = perc_catch %>% 
  summarise(catch_mpa = sum(catch_mpa),
            catch_eez = sum(catch_eez)) %>% 
  mutate(perc_catch = 100*catch_mpa/catch_eez,
         var = "Catch",
         fig = "figA") 
  

perc_value_total = perc_value %>% 
  summarise(catch_mpa = sum(catch_mpa),
            catch_eez = sum(catch_eez)) %>% 
  mutate(perc_catch = 100*catch_mpa/catch_eez,
         var = "Revenue",
         fig = "figA")

perc_nut_total = perc_nut %>%
  group_by(var) %>% 
  summarise(catch_mpa = sum(catch_mpa),
            catch_eez = sum(catch_eez)) %>% 
  mutate(perc_catch = 100*catch_mpa/catch_eez,
         fig = "figB")

dta = rbind(perc_catch_total, perc_value_total, perc_nut_total) %>% 
  select(var, fig, perc_catch)

dta$var = factor(dta$var, levels=c("DHA+EPA",
                                   "Calcium",
                                   "Zinc",
                                   "Iron",
                                   "Vitamin A",
                                   "Vitamin B12",
                                   "Revenue",
                                   "Catch"))

dta_all$var = factor(dta_all$var, levels=c("DHA+EPA",
                                   "Calcium",
                                   "Zinc",
                                   "Iron",
                                   "Vitamin A",
                                   "Vitamin B12",
                                   "Revenue",
                                   "Catch"))

##Total catch plot
p_tot_catch = ggplot(data = dta, aes(x=perc_catch, y=var)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.margin = margin(t = 0.5, r = 0.7, l = 1.05, b = 0, "cm"),
        plot.title = element_text(size = 15, hjust = 0.3))
p_tot_catch

##Two plots - Barplots
p_tot_catch_1 = ggplot(data = dta %>% filter(fig == "figA"), aes(x=perc_catch, y=var)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.text.x = element_blank(),
        plot.margin = margin(t = 0.5, r = 0.7, l = 1.8, b = -0.5, "cm"),
        plot.title = element_text(size = 15, hjust = 0.3))
p_tot_catch_1

p_tot_catch_2 = ggplot(data = dta %>% filter(fig == "figB"), aes(x=perc_catch, y=var)) +
  geom_bar(stat = "identity") +
  labs(x = "Sourced from MPAs (%)", y = "Nutrients") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.margin = margin(t = 0, r = 0.7, l = 1.05, b = 0, "cm"),
        plot.title = element_text(size = 15, hjust = 0.3))
p_tot_catch_2

p2 = ggarrange(p_tot_catch_1, p_tot_catch_2, ncol = 1, heights = c(1,3))
p2


##Two plots - Point segment
p_tot_catch_1 = ggplot(data = dta %>% filter(fig == "figA"), aes(x=perc_catch, y=var)) +
  geom_segment(aes(y = var, yend=var, x=0, xend=perc_catch), linewidth = 1) +
  geom_point(size=5, color = "#00AFBB") +
  labs(x = "", y = "") +
  xlim(0, 8) +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.text.x = element_blank(),
        plot.margin = margin(t = 0.5, r = 0.7, l = 1.8, b = -0.5, "cm"),
        plot.title = element_text(size = 15, hjust = 0.3))
p_tot_catch_1

p_tot_catch_2 = ggplot(data = dta %>% filter(fig == "figB"), aes(x=perc_catch, y=var)) +
  geom_segment(aes(y = var, yend=var, x=0, xend=perc_catch), linewidth = 1) +
  geom_point(size=5, color = "#00AFBB") +
  xlim(0, 8) +
  labs(x = "Percent from MPAs", y = "Nutrients") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.margin = margin(t = 0, r = 0.7, l = 1.05, b = 0, "cm"),
        plot.title = element_text(size = 15, hjust = 0.3))
p_tot_catch_2

p2 = ggarrange(p_tot_catch_1, p_tot_catch_2, ncol = 1, heights = c(1,3))
p2


##Two plots - Boxplots
p_tot_catch_1 = ggplot(data = dta_all %>% filter(fig == "figA"), aes(x=perc_catch, y=var, fill = var)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option = "magma") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position="none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.text.x = element_blank(),
        plot.margin = margin(t = 0.5, r = 0.7, l = 1.8, b = -0.5, "cm"),
        plot.title = element_text(size = 15, hjust = 0.3))
p_tot_catch_1

p_tot_catch_2 = ggplot(data = dta_all %>% filter(fig == "figB"), aes(x=perc_catch, y=var, fill = var)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(x = "Percent from MPAs", y = "Nutrients") +
  theme_bw() +
  theme(legend.position="none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.margin = margin(t = 0, r = 0.7, l = 1.05, b = 0, "cm"),
        plot.title = element_text(size = 15, hjust = 0.3))
p_tot_catch_2

p2 = ggarrange(p_tot_catch_1, p_tot_catch_2, ncol = 1, heights = c(1,3))
p2

###########Combine plots
p = ggarrange(p_map_catch, p2, 
              ncol=2, 
              widths = c(2, 0.8),
              labels = c("A", "B"), 
              font.label = list(size = 20, color = "black", face = "bold"),
              label.y = c(0.98), 
              label.x = c(0.04))
p

ggsave(filename = "figures/Figure3.jpeg", 
       plot = p,
       height = 3, 
       width = 12.5,
       dpi=600)

ggsave(filename = "figures/Figure3.pdf", 
       plot = p,
       height = 3.6, 
       width = 12.5, dpi=600, 
       device=cairo_pdf)







## Show distribution
p_tot_catch_1 = ggplot(data = dta_all %>% filter(fig == "figA"), aes(x=perc_catch, y=var, group = var)) +
  geom_violin() +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        #axis.text.x = element_blank(),
        plot.margin = margin(t = 0.5, r = 0.7, l = 1.8, b = -0.5, "cm"),
        plot.title = element_text(size = 15, hjust = 0.3))
p_tot_catch_1


p_tot_catch_1 = ggplot(data = dta_all %>% filter(fig == "figA"), aes(x=perc_catch, y=var, group = var)) +
  geom_density_ridges(fill = "#00AFBB", scale = 0.95) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        #axis.text.x = element_blank(),
        plot.margin = margin(t = 0.5, r = 0.7, l = 1.8, b = -0.5, "cm"),
        plot.title = element_text(size = 15, hjust = 0.3))
p_tot_catch_1

p_tot_catch_2 = ggplot(data = dta %>% filter(fig == "figB"), aes(x=perc_catch, y=var)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Nutrients") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.margin = margin(t = 0, r = 0.7, l = 1.05, b = 0, "cm"),
        plot.title = element_text(size = 15, hjust = 0.3))
p_tot_catch_2

p2 = ggarrange(p_tot_catch_1, p_tot_catch_2, ncol = 1, heights = c(1,3))
p2



p_tot_catch = ggplot(data = dta, aes(x=perc_catch, y=var)) +
  geom_segment(aes(y = var, yend=var, x=0, xend=perc_catch), linewidth = 1) +
  geom_point(size=5) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.margin = margin(t = 0.5, r = 0.7, l = 1.05, b = 0, "cm"),
        plot.title = element_text(size = 15, hjust = 0.3))
p_tot_catch

##Regional plot
p_reg_catch = ggplot(data = region_catch, aes(x=total_catch_mpa, y=region)) +
  geom_segment(aes(y = region, yend = region, x=0, xend=total_catch_mpa), linewidth = 1) +
  geom_point(size=5) +
  labs(x = "Total MPA catch (million tonnes)", y = "") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.margin = margin(t = 0, r = 0.7, l = 0, b = 0, "cm"))

p_catch = ggarrange(p_tot_catch, p_reg_catch, p_map_catch, 
                    ncol = 1, heights = c(1.2, 1.7, 2))
p_catch
############################################################################Percent of value

##Total revenue
total_catch = perc_value %>% 
  summarise(total_catch_mpa = sum(catch_mpa),
            total_catch_eez = sum(catch_eez)) %>% 
  mutate(perc_catch = 100*total_catch_mpa/total_catch_eez)

def_catch = perc_value %>% 
  left_join(perc_pop_deficient) %>% 
  group_by(is_def) %>% 
  summarise(total_catch_mpa = sum(catch_mpa),
            total_catch_eez = sum(catch_eez)) %>% 
  mutate(perc_catch = 100*total_catch_mpa/total_catch_eez) %>% 
  filter(is_def == "yes")

dta = data.frame(group = c("All countries", "Nutritionally vulnerable"),
                 catch = c(total_catch$total_catch_mpa/1000000, def_catch$total_catch_mpa/1000000))

dta$group = factor(dta$group, levels = c("Nutritionally vulnerable", "All countries"))

region_catch = perc_value %>% 
  mutate(region = countrycode(country, 'iso3c', 'region'),
         region = recode(region, "East Asia & Pacific" = "Southeast Asia & Pacific")) %>% 
  group_by(region) %>% 
  summarise(total_catch_mpa = sum(catch_mpa)/1000000,
            total_catch_eez = sum(catch_eez)/1000000) %>% 
  mutate(perc_catch = 100*total_catch_mpa/total_catch_eez) %>% 
  drop_na(region)

region_catch$region = with(region_catch, reorder(region, total_catch_mpa, median))

# Format data
mpa_dta_sf <- world %>% 
  left_join(perc_value, by=c("gu_a3"="country"))

##Spatialize tiny
sdata_pt <- world_centers %>%
  left_join(perc_value, by=c("iso3"="country")) %>%
  # Reduce to ones with data
  filter(!is.na(perc_catch)) %>%
  arrange(area_sqkm) %>%
  # Reduce to small
  filter(area_sqkm<=2.5*10^4 & continent!="Europe")

##Plot
p_map_value <- ggplot(mpa_dta_sf) +
  geom_sf(mapping=aes(fill=perc_catch), lwd=0, colour = "black") +
  # Plot small places
  geom_sf(data=sdata_pt, mapping=aes(fill=perc_catch), shape=21, size=3, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Legend and labels
  scale_fill_gradientn(name = "Revenue from\nMPAs (%)",
                       colors = RColorBrewer::brewer.pal(9, "YlOrRd"), 
                       na.value = "grey87") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 4)) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  #labs(title = "A") +
  theme_bw() + base_theme +
  theme(legend.position="left",
        legend.box="vertical",
        legend.key.width = unit(10, "mm"),
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.text = element_text(hjust = 0),
        plot.title = element_text(face = "bold", size = 13),
        plot.margin = margin(t = 0, r = 0.7, l = 1, b = 0, "cm"))
#p_map_value

p_tot_value = ggplot(data = dta, aes(x=catch, y=group)) +
  geom_segment(aes(y = group, yend=group, x=0, xend=catch), linewidth = 1) +
  geom_point(size=5) +
  labs(x = "", y = "", title = "Revenue") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.margin = margin(t = 0.5, r = 0.7, l = 1.05, b = 0, "cm"),
        plot.title = element_text(size = 15, hjust = 0.3))

p_reg_value = ggplot(data = region_catch, aes(x=total_catch_mpa, y=region)) +
  geom_segment(aes(y = region, yend = region, x=0, xend=total_catch_mpa), linewidth = 1) +
  geom_point(size=5) +
  labs(x = "Total MPA revenue (million US$)", y = "") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.margin = margin(t = 0, r = 0.7, l = 0, b = 0, "cm"))

p_value = ggarrange(p_tot_value, p_reg_value, p_map_value, 
                    ncol = 1, heights = c(1.2, 1.7, 2))
p_value
############################################################################Percent of nutrient (VitB12)

##Total nutrient
total_catch = perc_nut %>% 
  summarise(total_catch_mpa = sum(catch_mpa),
            total_catch_eez = sum(catch_eez)) %>% 
  mutate(perc_catch = 100*total_catch_mpa/total_catch_eez)

def_catch = perc_nut %>% 
  left_join(perc_pop_deficient) %>% 
  group_by(is_def) %>% 
  summarise(total_catch_mpa = sum(catch_mpa),
            total_catch_eez = sum(catch_eez)) %>% 
  mutate(perc_catch = 100*total_catch_mpa/total_catch_eez) %>% 
  filter(is_def == "yes")

dta = data.frame(group = c("All countries", "Nutritionally vulnerable"),
                 catch = c(total_catch$total_catch_mpa/2/1000000000, def_catch$total_catch_mpa/2/1000000000))

dta$group = factor(dta$group, levels = c("Nutritionally vulnerable", "All countries"))

region_nut = perc_nut %>% 
  mutate(region = countrycode(country, 'iso3c', 'region'),
         region = recode(region, "East Asia & Pacific" = "Southeast Asia & Pacific")) %>% 
  group_by(region) %>% 
  summarise(total_catch_mpa = sum(catch_mpa)/2/1000000000,
            total_catch_eez = sum(catch_eez)/2/1000000000) %>% 
  mutate(perc_catch = 100*total_catch_mpa/total_catch_eez) %>% 
  drop_na(region)

region_nut$region = with(region_nut, reorder(region, total_catch_mpa, median))

# Format data
mpa_dta_sf <- world %>% 
  left_join(perc_nut, by=c("gu_a3"="country"))

##Spatialize tiny
sdata_pt <- world_centers %>%
  left_join(perc_nut, by=c("iso3"="country")) %>%
  # Reduce to ones with data
  filter(!is.na(perc_catch)) %>%
  arrange(area_sqkm) %>%
  # Reduce to small
  filter(area_sqkm<=2.5*10^4 & continent!="Europe")

##Plot
p_map_nut <- ggplot(mpa_dta_sf) +
  geom_sf(mapping=aes(fill=perc_catch), lwd=0, colour = "black") +
  # Plot small places
  geom_sf(data=sdata_pt, mapping=aes(fill=perc_catch), shape=21, size=3, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Legend and labels
  scale_fill_gradientn(name = "Vitamin B12\nfrom MPAs (%)",
                       colors = RColorBrewer::brewer.pal(9, "YlOrRd"), 
                       na.value = "grey87") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 4)) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  #labs(title = "A") +
  theme_bw() + base_theme +
  theme(legend.position="left",
        legend.box="vertical",
        legend.key.width = unit(10, "mm"),
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.text = element_text(hjust = 0),
        plot.title = element_text(face = "bold", size = 13),
        plot.margin = margin(t = 0, r = 0.7, l = 1, b = 0, "cm"))

#p_map_nut

p_tot_nut = ggplot(data = dta, aes(x=catch, y=group)) +
  geom_segment(aes(y = group, yend=group, x=0, xend=catch), linewidth = 1) +
  geom_point(size=5) +
  labs(x = "Billions of people", y = "", title = "Vitamin B12") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.margin = margin(t = 0.5, r = 0.7, l = 1.05, b = 0, "cm"),
        plot.title = element_text(size = 15, hjust = 0.3))

p_reg_nut = ggplot(data = region_nut, aes(x=total_catch_mpa, y=region)) +
  geom_segment(aes(y = region, yend = region, x=0, xend=total_catch_mpa), linewidth = 1) +
  geom_point(size=5) +
  labs(x = "Billions of people", y = "") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.margin = margin(t = 0, r = 0.7, l = 0, b = 0, "cm"))

p_nut = ggarrange(p_tot_nut, p_reg_nut, p_map_nut, 
                    ncol = 1, heights = c(1.2, 1.7, 2))
p_nut

###Combine plots

p_final = ggarrange(p_catch, p_nut, p_value, ncol = 3)

p_final

p_final2 = ggarrange(p_map_catch, p_map_nut, p_map_value, ncol = 1)
p_final2

ggsave(p_final, "figures/P_final.jpeg")













##Simple plot prod per EEEZ
map1 = ggplot() +
  geom_sf(data = world_shp,
          fill = "grey85", 
          color = "grey90",
          size = 0.05) +
  geom_sf(data = RLS_sites_st_OA,
          shape=21, 
          alpha = 0.6,
          aes(fill=bio_log),
          size = 2) +
  scale_fill_gradientn(name=expression(paste("Biomass\nlog (kg ha"^-1,")")), 
                       colors = RColorBrewer::brewer.pal(9, "PuBu"), 
                       na.value = "grey87",
                       breaks = c(2, 6, 9),
                       labels = c("2", "6", "9")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 1, barheight = 3)) +
  labs(title = "Non-MPA sites") +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.title = element_text(hjust = 0.5,face = "bold", size = 15),
        legend.key = element_rect(fill = NA),
        title = element_text(size = 10),
        legend.title = element_text(size=15),
        legend.text = element_text(size = 15),
        plot.margin = ggplot2::margin(t = -3, r = 0, b = 0, l = 0.04, "cm")) +
  coord_sf(y=c(-28, 30))