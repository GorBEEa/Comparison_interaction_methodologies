#Script for manipulating and processing 2023 interaction transcet flower count data

#unblock these packages if not running after interaction_data (Recommended to run first)
#library(readr)
#library(here)
#library(tidyverse)
#library(tidyr)
#library(tidyselect)

#Load Data -----

flower.count.2023 <- read_csv(here("Data/flower_count_2023_int_trnsect_clean.csv")) #ty Estefanía for cleaning this already
flower.count.2023 <- flower.count.2023 %>% 
  mutate(flower_genus = sapply(strsplit(as.character(Planta), " "), `[`, 1)) %>% 
  rename(period = Periodo) %>% 
  rename(site = Sitio) %>% 
  filter(flower_genus != "SI") %>% 
  mutate(period = as.integer(period)) %>% 
  mutate(site = as.integer(site))

flower.count.genera <- flower.count.2023 %>% distinct(flower_genus) 

flower.genus.by.period <-  flower.count.2023 %>% 
  group_by(period) %>% 
  summarise(n.flower.count.genera = as.integer(n_distinct(flower_genus)))

flower.genus.by.site <-  flower.count.2023 %>% 
  group_by(site) %>% 
  summarise(n.flower.count.genera = as.integer(n_distinct(flower_genus)))



#organize flower count data for all in 1 analysis with interaction and mb data ------
bp23.fc4stats <- flower.count.2023 %>% #isolate the data of interest from all fc data
  select(period, site, flower_genus) 
bp23.fc4stats.wide <- bp23.fc4stats %>% #transpose to match the mb data format
  group_by(period, site, flower_genus) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  pivot_wider(
    id_cols = c(period, site),
    names_from = flower_genus,
    values_from = count,
    values_fill = 0
  )

bp23.fc4stats.wide.binary <- bp23.fc4stats.wide %>% #make binary version
  mutate(across(Bellis:last_col(), ~ifelse(. > 0, 1, 0))) %>% 
  mutate(method = rep("count")) %>% #add a methodology identifier for next analysis
  relocate(method, .after = "site")

