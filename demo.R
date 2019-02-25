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
load('data/cities.Rdata')

# source files
source('functions/isp_functions.r')
source('functions/cc_functions.r')
source('functions/keys.R')

# keys
register_google(key = personal_google_api_key) 

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

# Save it for export
ggsave("figures_n_tables/cnc_map.png", width = 20, height = 15, units = "cm")


# *************************************************************
# URBAN HOMOGENIZATION BETWEEN CITIES (Table 2)
# *************************************************************
# // GENERAL STATS
# how many cities does each species appear in?
total_cities <- all_inat %>%
  group_by (scientific_name) %>%
  summarise (num_cities = n_distinct(hometown)) %>%
  select(scientific_name, num_cities)

# what are the total species numbers and observations in the dataset?
totals <- all_inat %>%
  summarise (num_species = n_distinct (scientific_name),
             num_obs = n())
totals$num_species
totals$num_obs



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
write.csv(over8, "figures_n_tables/over8.csv")

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
write.csv(taxon_over8, "figures_n_tables/taxon_over8.csv")    # TABLE 2

# To compare to overall totals, a subset of only those species with more than 100 observations
subset100 <- all_inat %>%
  group_by(scientific_name)%>%
  mutate (count = n()) %>%
  filter(count>=100) %>%
  ungroup() %>%
  summarise (num_species = n_distinct (scientific_name),
             num_obs = n())
subset100$num_species
subset100$num_obs


# creating a table of the 10 taxon classes that looks at how frequently
# species from these groups have at least 100 observations.  For example,
# birds are over represented in this frequently observed group compared to insects
over100 <- all_inat %>%
  group_by(scientific_name)%>%
  mutate (count = n()) %>%
  group_by(taxon)%>%
  filter(count>=100) %>%
  summarise (subset_num_species =  n_distinct(scientific_name),
             subset_num_obs = n(), 
             subset_ratio_species = subset_num_species / subset100$num_species,
             subset_ratio_obs = subset_num_obs / subset100$num_obs) 

everything <-all_inat %>%
  group_by(taxon)%>%
  summarise (all_num_species =  n_distinct(scientific_name),
             all_num_obs = n(), 
             all_ratio_species = all_num_species / totals$num_species, 
             all_ratio_obs = all_num_obs / totals$num_obs) %>%
  arrange(desc(all_num_species)) %>%
  left_join(over100, by = "taxon") %>%
  mutate (diff_species = all_ratio_species - subset_ratio_species,
          diff_obs = all_ratio_obs - subset_ratio_obs) 

everything    # Birds and dicots get overrepresented in the top 100, while insects get underrepresented
write.csv(everything, "figures_n_tables/summary_over100obs.csv")  # Table 5

# Top10 lists for all cities
top10_knit(plants)
top10_knit(animals)