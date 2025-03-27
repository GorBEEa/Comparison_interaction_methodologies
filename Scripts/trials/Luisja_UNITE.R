#Script for viewing Luisja's UNITE assign_tax results

library(tidyverse)
library(here)
library(dplyr)

#upload data and check if they match mine
LJU.genus.hits <- read_csv(here("Data/UNITE_genus_hits.csv"))
colnames(LJU.genus.hits) <- c("tax","count")
LJU.genus.hits$genus <- sapply(strsplit(as.character(LJU.genus.hits$tax), "__"), `[`, 2)
LJU.genus <- LJU.genus.hits[,-1]

#Vale tenemos los mismos resultados, bien

bp.2023 <- read_tsv(here("Data/2023_BP_metab_sample_info.tsv"))
bp.asv.counts.2023 <- read_tsv(here("Data/LJU_2023_plant_GorBEEa_prj_ASVs_counts.tsv"))
bp.asv.tax.2023 <- read_tsv(here("Data/LJU_2023_plant_GorBEEa_prj_ASVs_taxonomy.tsv"))

