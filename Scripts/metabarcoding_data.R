#Analyzing metabarcoding data by Bombus factors (Period, Site, Size/age, etc.)

library(readr)
library(here)
library(tidyverse)
library(tidyr)
library(tidyselect)
library(easystats)
library(visreg)
library(vegan)


#Load Data -----
#For now we are going to work with Bell db analyzed results @ genus level

bp.2023 <- read_tsv(here("Data/2023_BP_metab_sample_info.tsv")) #bp sample info
bp.2023 <- bp.2023 %>% rename(sample = sample_name)

bp.asv.counts.2023 <- read_tsv(here("Data/2023_plantv2_bell_ASVs_counts.tsv")) #asv counts
bp.asv.counts.2023 <- bp.asv.counts.2023 %>% rename(asv_id = ...1)

bp.asv.tax.2023 <- read_tsv(here("Data/2023_plantv2_bell_ASVs_taxonomy.tsv")) #taxa associated with asvs
bp.asv.tax.2023 <- bp.asv.tax.2023 %>% 
  rename(asv_id = ...1) %>% 
  rename(genus = Genus)
bp.asv.genus.2023 <- bp.asv.tax.2023 %>% 
    select(asv_id,genus)

#for unite or other db results where the tax lvel is written as k__,p__ ....
#bp.asv.genus.2023 <- bp.asv.tax.2023 %>% 
#  select(asv_id,genus) %>% 
#  mutate(genus = str_remove(genus, "g__"))


#condense two of these to have genera names assigned to asvs in samples
bp.plant.asvNs.w.genus.2023 <- right_join(bp.asv.counts.2023,bp.asv.genus.2023, by = "asv_id")
bp.plant.asvNs.w.genus.2023 %>% relocate(genus, .after = asv_id) #this + use head() just to make sure it worked

#transpose to be able to combine with other BP data
bp.plant.asvgn.wide.2023 <- bp.plant.asvNs.w.genus.2023 %>%
  pivot_longer(cols = starts_with(c("GBP","neg")), names_to = "sample", values_to = "Count") %>%
  group_by(sample, genus) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  pivot_wider(names_from = genus, values_from = Count, values_fill = 0)

#combine all
bp23.genomic.specs <- right_join(bp.2023, bp.plant.asvgn.wide.2023, by = "sample")
#21.01.2025 the 2023 bp data are missing intertegular length


#manipulate and analyze data ------


#get total number of genera by sample column
genera <- bp23.genomic.specs %>%
  select(where(is.numeric,-c(year,quant_reading))) %>%
  names()

bp23.genomic.analys <- bp23.genomic.specs %>% 
  rowwise() %>% 
  mutate(gen_diversity = sum(c_across(all_of(genera)) > 0)) %>% 
  ungroup() %>% 
  relocate(gen_diversity, .after = quant_reading)

#get number of genera detected by period, should be replicable for site if needed
bp23.genomic.periods <- bp23.genomic.analys %>% 
  pivot_longer(cols = all_of(genera), names_to = "genus", values_to = "count") %>% #easier with re-tranposed data
  filter(count > 0) %>% #filter for detected genera
  group_by(period, genus) %>% 
  slice(1) %>%  #COUNT ONLY 1 DETECTION PER PERIOD
  group_by(period) %>% 
  summarise(n_samples = n_distinct(sample),
            n_genera_detected = n_distinct(genus))
  

#extract genus hits from here

genus.hits.23 <- bp.plant.asvNs.w.genus.2023 %>%
  select(!asv_id) %>% #simplify working data
  group_by(genus) %>% 
  summarise(across(where(is.numeric), sum)) %>% #here we already see that 160 genera were detected in 2023
  rowwise() %>% 
  filter(if_any(where(is.numeric), ~. > 0)) %>% #should already be the difinitive list, but if any genera still have 0 detections, remove them
  ungroup %>% 
  select(genus) #create just a list of the genera


  







