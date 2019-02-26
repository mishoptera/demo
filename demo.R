# Author: Misha Leong
# Date: February 2019
# Project: Demo of some methods used with City Nature Challenge iNaturalist data

# *************************************************************
# FIRST THINGS FIRST
# *************************************************************
# load libraries
library(tidyverse)
library(vegan)
library(ggmap)
library(rinat)
library(FedData)
library(sf)
library(ggrepel)
library(ggpubr)
library(stringr)
library(cooccur)

# load files
load('data/all_inat.Rdata')
load('data/nlcd_codes.Rdata')
load('data/cities.Rdata')

# source files
source('functions/isp_functions.r')
source('functions/cc_functions.r')
source('keys.R')

# keys
register_google(key = personal_google_api_key) 

# *************************************************************
# QUICK GLIMPSE INTO HOW OBSERVATIONS ARE ACCESSED AND MATCHED
# *************************************************************
# set up bounding box for US only (optional)
bounds <- c(25, -125.1, 49.5, -66.7) #all US

# retrieve observations for a given species
sp_all <- get_inat_obs(taxon_name = "apis mellifera", 
                       quality = "research", 
                       maxresults = 500,
                       bounds = bounds)

# map observations
sp_map <- inat_map(sp_all, plot = FALSE)
sp_map + borders("state") + theme_bw()

# match an nlcd value to an observation
city <- get_map("San Francisco", zoom = 10)
bb<-attr(city, 'bb')
extentB <- polygon_from_extent(raster::extent(bb$ll.lon, bb$ur.lon, bb$ll.lat, bb$ur.lat), proj4string = "+proj=longlat +ellps=GRS80   +datum=NAD83 +no_defs")
city_nlcd <- get_nlcd (template = (extentB), label = "San Francisco")
sp_city <- sp_all %>%
    filter(latitude > bb$ll.lat & latitude < bb$ur.lat &
             longitude > bb$ll.lon & longitude < bb$ur.lon)
sp_points <- data.frame(sp_city) 
coordinates(sp_points) = ~longitude + latitude
proj4string(sp_points) <- CRS("+init=epsg:4326") 
sp_points2 <- spTransform(sp_points, proj4string(city_nlcd))
sp_points3 <- raster::extract(city_nlcd, sp_points2, sp=TRUE)
sp_city_wNLCD <- spTransform(sp_points3, CRS("+init=epsg:4326")) %>%
    as.tibble() %>%
    rename("nlcd_code" = !!names(.[37])) %>%
    left_join(nlcd_codes, by = "nlcd_code")
View(sp_city_wNLCD)
    

# *************************************************************
# TO SAVE TIME, ALL ALL CNC DATA ALREADY SAVED
# *************************************************************
# as above, but for every 2016-2018 CNC obs once filtered. N=66209.
head(all_inat)

# what are the total species numbers and observations in the dataset?
totals <- all_inat %>%
  summarise (num_species = n_distinct (scientific_name),
             num_obs = n())
totals$num_species
totals$num_obs

# adding taxon labels
all_inat <- all_inat %>%
  mutate(taxon = if_else (taxon_class_name == "Magnoliopsida", "dicots", 
                  if_else (taxon_class_name == "Liliopsida", "monocots",  
                  if_else (taxon_class_name == "Polypodiopsida", "ferns", 
                  if_else (taxon_class_name == "Pinopsida", "conifers",
                  if_else (taxon_class_name == "Aves", "birds",
                  if_else (taxon_class_name == "Insecta", "insects",
                  if_else (taxon_class_name == "Reptilia", "reptiles",
                  if_else (taxon_class_name == "Amphibia", "amphibians",
                  if_else (taxon_class_name == "Gastropoda", "gastropods",
                  if_else (taxon_class_name == "Mammalia", "mammals","other")))))))))))

# some last minute file cleaning
all_inat$scientific_name <- str_replace(all_inat$scientific_name,"Columba livia domestica", "Columba livia")
all_inat$scientific_name <- as.factor(all_inat$scientific_name)

# data subsets for later use
plants <- all_inat %>% filter(taxon_class_name %in% c("Magnoliopsida", "Liliopsida", "Polypodiopsida", "Pinopsida", "Agaricomycetes", "Lecanoromycetes"))
animals <- all_inat %>% filter(taxon_class_name %in% c("Arachnida", "Aves", "Gastropoda", "Insecta", "Amphibia", "Reptilia", "Mammalia"))


# *************************************************************
# MAP OF CNC CITIES (Figure 1)
# *************************************************************
# 'cities' is a summary file I created in advance that is a summary of 
# number of species and observations for each city, with additional useful
# stuff for later analyses (region, central lat/lon, and better formatting
# for hometown)
head(cities)

map <- get_googlemap(center = c(-98, 38), zoom = 4,
                     color = "bw",
                     style = "feature:road|visibility:off&style=element:labels|visibility:off&style=feature:administrative|visibility:off")

# Plot cities onto map basic
ggmap(map) +
  geom_point(data = cities, aes(x = lon, y = lat))

# Plot cities onto map with colors, sizes, and labels
ggmap(map) +
  geom_point(data = cities, aes(x = lon, y = lat, size = num_obs, color = region)) +
  labs(colour = "Regions", size = "Records")+
  geom_text_repel(data = cities, aes(x = lon, y = lat, label = official_hometown))


# *************************************************************
# EDA OF URBAN HOMOGENIZATION BETWEEN CITIES
# *************************************************************
# // GENERAL STATS
# how many cities does each species appear in? (14 possible cities)
total_cities <- all_inat %>%
  group_by (scientific_name) %>%
  summarise (num_cities = n_distinct(hometown)) %>%
  select(scientific_name, num_cities) %>%
  arrange(desc(num_cities))
total_cities

# for later use to match common names to scientific names
names <- all_inat %>%
  select(scientific_name:common_name) %>%
  unique()

# Table of all species found in > 8 cities
over8 <- all_inat %>%
  group_by (taxon, scientific_name) %>%
  summarise (num_cities = n_distinct(hometown), num_obs = n()) %>%
  left_join(names, by="scientific_name") %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  select(taxon, common_name, scientific_name, num_cities, num_obs) %>%
  filter(num_cities > 7) %>%
  arrange(desc(num_cities))
over8

# BY TAXON, overall number of observations and and number of species to use for taxon_over8
all_stats <- all_inat %>%
  group_by (taxon) %>%
  summarise (total_species = n_distinct(scientific_name), total_obs = n())
all_stats

# this is to create blank variables for the taxon groups that do not have any species in >8 cities
blanks <- tibble(taxon = c("gastropods", "ferns"), 
                 over8_species = c(0, 0), 
                 over8_obs = c(0, 0))

# Table by taxon, showing number of species and observations of cosmopolitan species
taxon_over8 <- over8 %>% 
  group_by(taxon) %>%
  summarise (over8_species = n_distinct(scientific_name), over8_obs = sum(num_obs)) %>%
  bind_rows(blanks) %>%
  left_join(all_stats, by = "taxon") %>%
  mutate(prop_sp = (over8_species/total_species)*100, prop_obs = (over8_obs/total_obs)*100) %>%
  arrange(taxon)
taxon_over8
write.csv(taxon_over8, "figures_n_tables/taxon_over8.csv") 

# // TABLE LOOKING AT OVERALL SPECIES RANKS BETEEEN CITIES
taxa_names <- c("dicots", "monocots", "ferns", "conifers", "birds", "insects", "reptiles", "amphibians", "mammals", "gastropods")

# create simple ranking tables for each taxa (landcover collapsed)
lapply(taxa_names, function(i){
  assign(paste0("simple_", i) , create_big_table_simple(all_inat %>% filter (taxon == i), i), 
         envir = .GlobalEnv)
})
# example
simple_birds

big_simple_counts <- simple_birds %>%
  bind_rows(simple_mammals, simple_reptiles, simple_amphibians, simple_gastropods, simple_insects, simple_dicots, simple_monocots, simple_ferns, simple_conifers) %>%
  left_join(names, by="scientific_name") %>%
  left_join(total_cities, by="scientific_name") %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  filter(num_cities>=4) %>%
  select(taxon, common_name, scientific_name, count, num_cities, contains("count")) 
big_simple_counts

big_simple_ranks <- simple_birds %>%
  bind_rows(simple_mammals, simple_reptiles, simple_amphibians, simple_gastropods, simple_insects, simple_dicots, simple_monocots, simple_ferns, simple_conifers) %>%
  left_join(names, by="scientific_name") %>%
  left_join(total_cities, by="scientific_name") %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  filter(num_cities>=4) %>%
  select(taxon, common_name, scientific_name, count, num_cities, contains("rank")) 
big_simple_ranks

# *************************************************************
# WITHIN CITY - COMMUNITY COMPOSITION 
# *************************************************************
source('functions/cc_functions.r')

# All Taxa
cc_all <- cc_matrix(all_inat)
cc_all_env <- cc_env(cc_all)
plot_cc_us(cc_all, cc_all_env, "All taxa")          
plot_cc_region_4(all_inat, "All taxa")             

# All Plants
cc_plants <- cc_matrix(plants) 
cc_plants_env <- cc_env(cc_plants)
plot_cc_us(cc_plants, cc_plants_env, "Plants")      
plot_cc_region_4(plants, "Plants")

# All Animals
cc_animals <- cc_matrix(animals) 
cc_animals_env <- cc_env(cc_animals)
plot_cc_us(cc_animals, cc_animals_env, "Animals")
plot_cc_region_4(animals, "Animals")

# Create a table of PERMANOVA results for all taxa in all regions, nested by hometown.
tab_all <- adonis.table.hometown(all_wfreq) %>% mutate (taxon = "all")
tab_plants <- adonis.table.hometown(plants) %>% mutate (taxon = "plants")
tab_animals <- adonis.table.hometown(animals) %>% mutate (taxon = "animals")
tab <- bind_rows(tab_all, tab_plants, tab_animals)
write.csv(tab, "figures_n_tables/permanova_results_hometown.csv")       # Table 3

# Create a table of PERMANOVA results for all taxa in all regions, nested by land cover type.
tab_all <- adonis.table.lc(all_wfreq) %>% mutate (taxon = "all")
tab_plants <- adonis.table.lc(plants) %>% mutate (taxon = "plants")
tab_animals <- adonis.table.lc(animals) %>% mutate (taxon = "animals")
tab <- bind_rows(tab_all, tab_plants, tab_animals)
write.csv(tab, "figures_n_tables/permanova_results_lc.csv")       # Table 4


# *************************************************************
# WITHIN CITY - INDIVIDUAL SPECIES PATTERNS (Supplementary Table 1)
# *************************************************************
source('functions/isp_functions.r')
taxa_names <- c("dicots", "monocots", "ferns", "conifers", "birds", "insects", "reptiles", "amphibians", "mammals", "gastropods")

# create big ranking tables for each taxa
lapply(taxa_names, function(i){
  assign(paste0("ranks_", i) , create_big_table(all_inat %>% filter (taxon == i)), 
         envir = .GlobalEnv)
})

# create city aggregation metric tables
lapply(taxa_names, function(i){
  ranks_table <- eval(as.name(paste0("ranks_", i)))
  assign(paste0("cam_", i), small_table3(ranks_table), envir = .GlobalEnv)
})

# create averaged ranking metric tables
lapply(taxa_names, function(i){
  ranks_table <- eval(as.name(paste0("ranks_", i)))
  assign(paste0("arm_", i), small_table(ranks_table), envir = .GlobalEnv)
})

# one table to bind them all
lapply(taxa_names, function(i){
  assign((paste0("big_", i)), bigify(eval(as.name(paste0("cam_", i))), 
                                     eval(as.name(paste0("arm_", i))), i),
         envir = .GlobalEnv)
  
})


# to be able to add common names to table
names <- all_inat%>%
  select(scientific_name:common_name) %>%
  unique()

# how many cities does each species appear in?
total_cities <- all_inat %>%
  group_by (scientific_name) %>%
  summarise (num_cities = n_distinct(hometown)) %>%
  select(scientific_name, num_cities)

# creating a single table with all of the above
big_everything <- big_birds %>%
  bind_rows(big_mammals, big_reptiles, big_amphibians, big_gastropods, big_insects, big_dicots, big_monocots, big_ferns, big_conifers) %>%
  left_join(names, by="scientific_name") %>%
  left_join(total_cities, by="scientific_name") %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  mutate (diff_cam = (n+d1)-(d3+d4)) %>%
  mutate (diff_arm = (d3.mean+d4.mean)-(n.mean+d1.mean)) %>%
  select(taxon, common_name, scientific_name, count, num_cities, 
         diff_cam, diff_arm, everything())

# alternatives to make it a bit manageable to share in paper as a table
big_top10s <- big_everything %>%
  filter(rank<=10)
big_over4cities <- big_everything %>%
  filter(num_cities>=4)
big_over100obs <- big_everything %>%
  filter(count>=100)


# *************************************************************
# CALCULATING AND PLOTTING SLOPES BASED ON CAM AND ARM (Supplementary table)
# *************************************************************

# a function to calculate the slope of the n:d4 points!
get_slope <- function(n, d1, d2, d3, d4) { 
  x <- c(1:5)
  y <- c(n, d1, d2, d3, d4)
  slope_result <- lm(y~x, na.action=na.exclude)$coeff[[2]]
  return(slope_result)
}

# creating a table of the arm and cam slopes
slopes <- big_over100obs %>%
  rowwise() %>%  #this is such an important thing!!!
  mutate(slope_cam = get_slope(n,d1, d2, d3, d4)) %>%
  mutate(slope_arm = get_slope(n.mean,d1.mean, d2.mean, d3.mean, d4.mean)) %>%
  ungroup() 

write.csv(slopes, "figures_n_tables/big_over100obs_slopes.csv") # Supplementary Table

# plotting the cam slopes
cam_labels_over <- slopes %>%
  filter(slope_cam >0) %>%
  filter(num_cities>7)
cam_labels_under <- slopes %>%
  filter(slope_cam < -1) %>%
  filter(num_cities>7)
plot_cam <- ggplot(data=slopes,aes(x=num_cities,y=slope_cam, colour=taxon))+
  geom_point() + 
  labs(title = "evaluated with City Aggregation Metric", x = "number of cities", y = "slope of CAM") +
  geom_text_repel(data = cam_labels_over, aes(x=num_cities, y=slope_cam, label = common_name)) + 
  geom_text_repel(data = cam_labels_under, aes(x=num_cities, y=slope_cam, label = common_name)) + 
  theme_bw() 
plot_cam

# plotting the arm slopes
arm_labels_over <- slopes %>%
  filter(slope_arm < -2) %>%
  filter(num_cities>7)
arm_labels_under <- slopes %>%
  filter(slope_arm > 0 ) %>%
  filter(num_cities>7)
plot_arm <- ggplot(data=arm_slopes,aes(x=num_cities,y=slope_arm, colour=taxon))+
  geom_point() + 
  geom_text_repel(data = arm_labels_over, aes(x=num_cities, y=slope_arm, label = common_name)) + 
  geom_text_repel(data = arm_labels_under, aes(x=num_cities, y=slope_arm, label = common_name)) + 
  labs(title = "evaluated with Averaged Ranking Metric", x = "number of cities", y = "slope of ARM") +
  theme_bw()+
  scale_y_reverse(lim=c(8, -8))
plot_arm

# Combine the cam and arm slopes into one lovely figure and save
plots <- ggarrange(plot_cam, plot_arm, labels = c("A", "B"), ncol = 1, nrow = 2)
plots <- annotate_figure(plots,
                         top = text_grob("Biotic homogenization with urbanization intensity", face = "bold", size = 18))
ggsave(plot = plots, filename = "figures_n_tables/bh_CAM_ARM.png", height = 24, width = 20, units = "cm")



