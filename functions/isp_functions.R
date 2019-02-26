# Author: Misha Leong
# Date: October 2018
# Project: Exploring urban biodiversity patterns with City Nature Challenge iNaturalist data
# Specificly: These are the functions used to make the individual species patterns tables


# *************************************************************
# FUNCTION THAT CREATES A TOP 10 LIST FOR EACH CITY
# *************************************************************

top10 <- function (city, taxon) {
  taxon %>%
    filter(hometown == city) %>%
    group_by(common_name) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(rank = rank(desc(count), ties.method="average")) %>%
    filter (rank < 11) %>%
    select (common_name) %>% #this line can be deleted if needing more info
    rename(!!city := common_name)
}

top10_knit <- function(taxon) {
  taxa_name <-deparse(substitute(taxon))
  
  t10 <- top10("austin", taxon) %>%
    bind_cols (top10("boston", taxon)) %>%
    bind_cols (top10("chicago", taxon)) %>%
    bind_cols (top10("dallas", taxon)) %>%
    bind_cols (top10("houston", taxon)) %>%
    bind_cols (top10("losangeles", taxon)) %>%
    bind_cols (top10("miami", taxon)) %>%
    bind_cols (top10("minneapolis", taxon)) %>%
    bind_cols (top10("newyork", taxon)) %>%
    bind_cols (top10("raleigh", taxon)) %>%
    bind_cols (top10("saltlakecity", taxon)) %>%
    bind_cols (top10("sanfrancisco", taxon)) %>%
    bind_cols (top10("seattle", taxon)) %>%
    bind_cols (top10("washingtondc", taxon)) %>%
    mutate (subgroup = taxa_name) %>%
    select (subgroup, everything())
  
  filename <- paste("figures_n_tables/t10_", taxa_name, ".csv", sep = "")
  write.csv(t10, filename)
}


# *************************************************************
# FUNCTIONS TO CREATE A GIANT TABLE OF RANKS FOR EACH TAXA
# *************************************************************

# Creates a ranked list of the most common species for each city (land cover types lumped together)
process_city_simple <- function (hometown1, taxa) {
 
  city_rank = paste0(hometown1, "_rank")
  city_count = paste0(hometown1, "_count")
  
  city_taxa <- taxa %>%
    filter(hometown == hometown1) %>%
    group_by(scientific_name) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(rank = rank(desc(count), ties.method="average")) %>%
    rename(!!city_rank := rank) %>%
    rename(!!city_count := count)
  
  return(city_taxa)
}

# ////////////////////
# knit simple city tables together!
create_big_table_simple <- function(taxa, i)  {
  
  total  <- taxa %>%
    group_by(scientific_name) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(rank = rank(desc(count), ties.method="average"))
  
  Austin  <- process_city_simple("austin", taxa)
  Boston  <- process_city_simple("boston", taxa)
  Chicago  <- process_city_simple("chicago", taxa)
  Dallas  <- process_city_simple('dallas', taxa)
  Houston  <- process_city_simple('houston', taxa)
  Los_Angeles  <- process_city_simple('losangeles', taxa)
  Miami  <- process_city_simple('miami', taxa)
  Minneapolis  <- process_city_simple('minneapolis', taxa)
  New_York  <- process_city_simple('newyork', taxa)
  Raleigh  <- process_city_simple('raleigh', taxa)
  Salt_Lake_City  <- process_city_simple('saltlakecity', taxa)
  San_Francisco  <- process_city_simple('sanfrancisco', taxa)
  Seattle  <- process_city_simple('seattle', taxa)
  Washington_DC <- process_city_simple('washingtondc', taxa)
  
  big_table <- total %>%
    left_join(Austin, by="scientific_name") %>%
    left_join(Boston, by="scientific_name") %>%
    left_join(Chicago, by="scientific_name") %>%
    full_join(Dallas, by="scientific_name") %>%
    full_join(Houston, by="scientific_name") %>%
    full_join(Los_Angeles, by="scientific_name") %>%
    full_join(Miami, by="scientific_name") %>%
    full_join(Minneapolis, by="scientific_name") %>%
    full_join(New_York, by="scientific_name") %>%
    full_join(Raleigh, by="scientific_name") %>%
    full_join(Salt_Lake_City, by="scientific_name") %>%
    full_join(San_Francisco, by="scientific_name") %>%
    full_join(Seattle, by="scientific_name") %>%
    full_join(Washington_DC, by="scientific_name")%>%
    distinct() %>%
    mutate (taxon=i) 
}

# ////////////////////
# Creates a ranked list of the most common species for each city for each land cover type
process_city <- function (hometown1, taxa, nlcd) {
  name_rank = paste(if_else (nlcd == "natural", "n", 
                             if_else (nlcd == "developed1_open_space", "d1",  
                                      if_else (nlcd == "developed2_low_intensity", "d2", 
                                               if_else (nlcd == "developed3_medium_intensity", "d3","d4")))), hometown1, sep = "_")
  
  city_taxa <- taxa %>%
    filter(hometown == hometown1) %>%
    filter(nlcd_group2 == nlcd) %>%
    group_by(scientific_name) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(rank = rank(desc(count), ties.method="average")) %>%
    select(-count) %>%
    rename(!!name_rank := rank)
  
  return(city_taxa)
}



# ////////////////////
# mini table of all the different land use types for each city
create_table_hometown <- function(hometown1, taxa)  {
  n <- process_city(hometown1, taxa, "natural")
  d1 <- process_city(hometown1, taxa, "developed1_open_space")
  d2 <- process_city(hometown1, taxa, "developed2_low_intensity")
  d3 <- process_city(hometown1, taxa, "developed3_medium_intensity")
  d4 <- process_city(hometown1, taxa, "developed4_high_intensity")
  
  
  dummy <- taxa %>%
    filter(hometown == hometown1) %>%
    group_by(scientific_name) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(rank = rank(desc(count), ties.method="average"))
  
  hometown_table <- dummy %>%
    left_join(n, by="scientific_name") %>%
    left_join(d1, by="scientific_name") %>%
    left_join(d2, by="scientific_name") %>%
    left_join(d3, by="scientific_name") %>%
    full_join(d4, by="scientific_name") %>%
    select(-count) %>%
    select(-rank) %>%
    distinct()
}

# ////////////////////
# knit all mini city tables together!
create_big_table <- function(taxa)  {
  
  total  <- taxa %>%
    group_by(scientific_name) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(rank = rank(desc(count), ties.method="average"))
  
  Austin  <- create_table_hometown("austin", taxa)
  Boston  <- create_table_hometown("boston", taxa)
  Chicago  <- create_table_hometown("chicago", taxa)
  Dallas  <- create_table_hometown('dallas', taxa)
  Houston  <- create_table_hometown('houston', taxa)
  Los_Angeles  <- create_table_hometown('losangeles', taxa)
  Miami  <- create_table_hometown('miami', taxa)
  Minneapolis  <- create_table_hometown('minneapolis', taxa)
  New_York  <- create_table_hometown('newyork', taxa)
  Raleigh  <- create_table_hometown('raleigh', taxa)
  Salt_Lake_City  <- create_table_hometown('saltlakecity', taxa)
  San_Francisco  <- create_table_hometown('sanfrancisco', taxa)
  Seattle  <- create_table_hometown('seattle', taxa)
  Washington_DC <- create_table_hometown('washingtondc', taxa)
  
  big_table <- total %>%
    left_join(Austin, by="scientific_name") %>%
    left_join(Boston, by="scientific_name") %>%
    left_join(Chicago, by="scientific_name") %>%
    full_join(Dallas, by="scientific_name") %>%
    full_join(Houston, by="scientific_name") %>%
    full_join(Los_Angeles, by="scientific_name") %>%
    full_join(Miami, by="scientific_name") %>%
    full_join(Minneapolis, by="scientific_name") %>%
    full_join(New_York, by="scientific_name") %>%
    full_join(Raleigh, by="scientific_name") %>%
    full_join(Salt_Lake_City, by="scientific_name") %>%
    full_join(San_Francisco, by="scientific_name") %>%
    full_join(Seattle, by="scientific_name") %>%
    full_join(Washington_DC, by="scientific_name")%>%
    distinct()
}


# *************************************************************
# FUNCTIONS FOR CITY AGGREGATION METRIC
# *************************************************************

count_high <- function(temp_vector) {
  length(which(temp_vector < 10000))
}

small_table3 <- function(big_table) {
  n.app <- big_table %>% select(contains("n_")) %>% apply(., 1, count_high)
  d1.app <- big_table %>% select(contains("d1_")) %>% apply(., 1, count_high)
  d2.app <- big_table %>% select(contains("d2_")) %>% apply(., 1, count_high)
  d3.app <- big_table %>% select(contains("d3_")) %>% apply(., 1, count_high)
  d4.app <- big_table %>% select(contains("d4_")) %>% apply(., 1, count_high)
  
  temp <- big_table %>% 
    select(scientific_name, count, rank) %>%
    cbind(n.app, d1.app, d2.app, d3.app, d4.app) %>%
    rename(n=n.app, d1=d1.app, d2=d2.app, d3=d3.app, d4=d4.app) %>%
    mutate(total=n+d1+d2+d3+d4) %>%
    arrange(rank)
  
  
  return(temp)
}


# *************************************************************
# FUNCTIONS FOR AVERAGED RANKING METRIC
# *************************************************************

small_table <- function(big_table) {
  n.mean <- big_table %>% select(contains("n_")) %>% rowMeans(., na.rm = TRUE)
  d1.mean <- big_table %>% select(contains("d1_")) %>% rowMeans(., na.rm = TRUE)
  d2.mean <- big_table %>% select(contains("d2_")) %>% rowMeans(., na.rm = TRUE)
  d3.mean <- big_table %>% select(contains("d3_")) %>% rowMeans(., na.rm = TRUE)
  d4.mean <- big_table %>% select(contains("d4_")) %>% rowMeans(., na.rm = TRUE)
  
  temp <- big_table %>% 
    select(scientific_name, count, rank) %>%
    cbind(n.mean, d1.mean, d2.mean, d3.mean, d4.mean) %>%
    arrange((rank))
  
  return(temp)
}



# *************************************************************
# FUNCTIONS TO COMBINE INTO ONE MAIN TABLE
# *************************************************************
bigify <- function(cam, arm, i){
  cam %>%
    left_join(select(arm, -c(count, rank)), by="scientific_name") %>%
    mutate (taxon=i)
}


