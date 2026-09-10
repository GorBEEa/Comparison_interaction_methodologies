#Script for manipulating and processing 2023 B. pascuorum interaction data

#If you want to clear environment ->...
#rm(list = ls()) 

#things u need
library(readr)
library(here)
library(tidyverse)
library(tidyr)
library(tidyselect)
library(easystats)
library(visreg)
library(vegan)

#load interaction transect data ------
Interaction_transect <- read_csv(here("Data/Interaction_transect.csv"), show_col_types = FALSE) 

#extract Bombus pascuorum interactions
bp.interactions.2023 <- Interaction_transect %>% filter(Polinizador == "Bombus pascuorum")


#clean plant species names -----
source(here("Scripts/plant_taxa_names_cleaning/BP_flower_w_interactions_cleaning_2023.R"))
#You should now see bp.interactions.clean in environment


# Observed species interactions -----
#visualize by number of observed interactions
title.bpasc.int <- expression(paste("Total ", italic("B. pascuorum"), " interactions with flower species 2023 season")) #new plot, give it a title

ggplot(data = bp.interactions.clean, aes(x = reorder(Planta, -table(Planta)[Planta]))) + geom_bar() + 
  labs(x = "Floral species", y = "Interaction count 2023 Season", title = title.bpasc.int) + 
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),plot.title = element_text(hjust=0.5))

#interactions by genus -----

# with metabarcoding it looks like everything has to be done at genus level
# redo all of this just for genus level

title.int.genus <- expression(paste("Total ", italic("B. pascuorum"), " flower interactions by plant genus 2023 season")) #new plot, give it a title

bp23.genus.int <- bp.interactions.clean %>% 
  mutate(genus = sapply(strsplit(as.character(Planta), " "), `[`, 1)) %>% 
  relocate(genus, .after = Planta) %>% 
  filter(genus != "SI") %>% 
  rename(period = Periodo) %>% 
  rename(site = Sitio)
  

df.int.genus <- data.frame(genus = sapply(strsplit(as.character(bp.interactions.clean$Planta), " "), `[`, 1))
df.int.genus <- df.int.genus %>% filter(genus != "SI")

fig.z <- df.int.genus %>% 
  count(genus) %>% 
  arrange(n) %>% 
  ggplot(aes(x = reorder(genus, -n), y = n)) + geom_bar(stat = "identity") + 
  labs(x = "Floral genus", y = "Interaction count 2023 Season", title = title.int.genus) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),plot.title = element_text(hjust=0.5))



#analyze BP interactions by periods, later the same can be done using gut data ------

#Summarize the number of occurences of the genera by period and site
gen.ev <- bp23.genus.int %>% 
  group_by(site, period) %>% #can call just one of Period or Site also
  summarise(n.genera = n_distinct(genus))

int.genus.by.period <-  bp23.genus.int %>% 
  group_by(period) %>% 
  mutate(period = as.integer(period)) %>% 
  summarise(n.genera.int = as.integer(n_distinct(genus)))

#what about mean genera per sampling day by period?
int.mean.gnra.by.period <- bp23.genus.int %>% 
  group_by(period, site) %>% 
  summarise(
    n_taxa = n_distinct(genus),
    .groups = "drop"
  ) %>% 
  group_by(period) %>% 
  summarise(
    mean_taxa = mean(n_taxa),
    se_taxa = sd(n_taxa) / sqrt(n()),
    .groups = "drop"
  ) %>% 
  mutate(type = "Interaction transect")



int.genus.by.site <-  bp23.genus.int %>% 
  group_by(site) %>% 
  mutate(site = as.integer(site)) %>% 
  summarise(n.genera = n_distinct(genus))


#organize interaction data for all in 1 analysis with metabarcoding and flower count data ------

bp23.int4stats <- bp23.genus.int %>% #isolate the data of interest from all clean BP interaction data
  select(period, site, genus) 
bp23.int4stats.wide <- bp23.int4stats %>% #transpose to match the mb data format
  group_by(period, site, genus) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  pivot_wider(
    id_cols = c(period, site),
    names_from = genus,
    values_from = count,
    values_fill = 0
  )

bp23.int4stats.wide.binary <- bp23.int4stats.wide %>% #make binary version
  mutate(across(Lathyrus:last_col(), ~ifelse(. > 0, 1, 0))) %>% 
  mutate(method = rep("interaction")) %>% #add a methodology identifier for next analysis
  relocate(method, .after = "site")

#going to look at sampling completeness for interaction data ----------------------------------------

#format for iNEXT
int23_inext <- bp23.int4stats.wide %>% 
  mutate(
    ps = paste0("p", period, "s", site)
  ) %>%
  select(ps, everything(), -period, -site)

int23_inext_list <- apply(int23_inext, 1, function(x) as.numeric(x))
#`apply` returns a matrix for numeric input, so convert to list
int23_inext_list <- split(int23_inext_list, seq(nrow(int23_inext)))
# But `split` won’t work properly here; better do:
int23_inext_list <- lapply(1:nrow(int23_inext), function(i) as.numeric(int23_inext[i, ]))
names(int23_inext_list) <- rownames(int23_inext)
#NA control
int23_inext_list <- lapply(int23_inext_list, function(x) {
  x[is.na(x)] <- 0
  x})

# Now run iNEXT
int23_inext_run <- iNEXT(int23_inext_list, q = 0, datatype = "abundance", size = NULL)
int23_covg <- estimateD(int23_inext_list, datatype = "abundance") 
int23_covg_obs <- int23_covg[int23_covg$Method == "Rarefaction", ]
median(int23_covg_obs$SC) #median sampling completeness value








save(bp23.int4stats.wide.binary, file = here("Data/gbp23.interaction.data4analysis.RData") )

