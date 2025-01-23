#Script for manipulating and processing 2023 interaction transcet flower count data

library(readr)
library(here)
library(tidyverse)
library(tidyr)
library(tidyselect)

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
