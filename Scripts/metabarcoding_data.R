#Script for manipulating and processing 2023 B. pascuorum ITS2 plant metabarcoding data

library(readr)
library(here)
library(tidyverse)
library(tidyr)
library(tidyselect)
library(easystats)
library(visreg)
library(vegan)
library(viridis)
library(report)


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
bp.plant.asvNs.w.genus.2023 %>% relocate(genus, .after = asv_id) #this is just to look and make sure it worked
binary.plant.asvNs.w.genus.2023 <- bp.plant.asvNs.w.genus.2023 %>% 
  relocate(genus, .after = asv_id) %>% 
  mutate(across(GBP23010301M_ITS_P48:last_col(), ~ifelse(. > 0, 1, 0))) %>% 
  mutate(indiv.w.signal = rowSums(across(GBP23010301M_ITS_P48:GBP23041104M_ITS_P48))) %>% 
  relocate(indiv.w.signal, .after = genus)

#transpose to be able to combine with other BP data
bp.plant.asvgn.wide.2023 <- bp.plant.asvNs.w.genus.2023 %>%
  pivot_longer(cols = starts_with(c("GBP","neg")), names_to = "sample", values_to = "Count") %>%
  group_by(sample, genus) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  pivot_wider(names_from = genus, values_from = Count, values_fill = 0)

#combine all
bp23.genomic.specs <- right_join(bp.2023, bp.plant.asvgn.wide.2023, by = "sample")
bp23.genomic.specs <- bp23.genomic.specs %>%  
  filter(type != "negative") %>% #only sample data
  mutate(period = str_extract(period, "\\d+"), period = as.integer(period)) %>% 
  mutate(site = str_extract(site, "\\d+"), site = as.integer(site))
#21.01.2025 the 2023 bp data are missing intertegular length


#manipulate and analyze data ------

#extract genus hits from here

genus.hits.23 <- bp.plant.asvNs.w.genus.2023 %>%
  select(!asv_id) %>% #simplify working data
  group_by(genus) %>% 
  summarise(across(where(is.numeric), sum)) %>% #here we already see that 160 genera were detected in 2023
  rowwise() %>% 
  filter(if_any(where(is.numeric), ~. > 0)) %>% #should already be the difinitive list, but if any genera still have 0 detections, remove them
  ungroup %>% 
  select(genus) #create just a list of the genera


bp23.genomic.analys <- bp23.genomic.specs #a copy for manipulation
bp23.genomic.analys$sample <- str_split_fixed(bp23.genomic.analys$sample, "_", 2)[,1] #remove sequencing plate labels
bp23.genomic.analys <- left_join(bp23.genomic.analys, bp23.size, by = "sample")
bp23.genomic.analys <- bp23.genomic.analys %>% relocate(intertegular_dist_mm, .after = quant_reading) %>% 
  relocate(abdomen_length_mm, .after = intertegular_dist_mm) %>% 
  relocate(total_length_mm, .after = abdomen_length_mm)
#NOTE: NAs exist for bombus measurements
#use df %>% na.replace(0) if needed


bp23.genomic.binary <- bp23.genomic.analys %>% 
  mutate(across(Abelmoschus:last_col(), ~ifelse(. > 0, 1, 0))) %>% #read count data to presence absence 1s and 0s
  mutate(genera.by.indiv = rowSums(across(Abelmoschus:last_col()))) %>%  #add a sum of genera for diversity by inv sample
  relocate(genera.by.indiv, .after = quant_reading)


#Analyses by period, site, bombus size, etc. ----

#get number of genera detected by period
bp23.genomic.periods <- bp23.genomic.analys %>% 
  pivot_longer(cols = Abelmoschus:last_col(), names_to = "genus", values_to = "count") %>%
  filter(count > 0) %>% 
  group_by(period, genus) %>% 
  slice(1) %>%  # Count only 1 detection per period
  group_by(period) %>% 
  summarise(n_samples = n_distinct(sample),
            n.genera = n_distinct(genus),
            .groups = 'drop')
  

#get number of genera detected by site
bp23.genomic.sites <- bp23.genomic.analys %>% 
       pivot_longer(cols = Abelmoschus:last_col(), names_to = "genus", values_to = "count") %>%
       filter(count > 0) %>% 
       group_by(site, genus) %>% 
       slice(1) %>%  # Count only 1 detection per site
       group_by(site) %>% 
       summarise(n_samples = n_distinct(sample),
                 n.genera = n_distinct(genus),
                 .groups = 'drop')



#plant DNA diversity by bombus size (using intertegular distance)
ggplot(data = bp23.genomic.binary, aes(period, genera.by.indiv, size = intertegular_dist_mm)) +
  geom_point(alpha = 0.6, color = "skyblue4") +
  theme(legend.key.size = unit(1, 'cm')) +
  scale_x_continuous(breaks = 1:6, labels = 1:6) +
  scale_size_continuous(range = c(1, 6)) +
  labs(x = "Period",
       y = "Detected plant genera",
       size = "Intertegular Distance")

#This isn't as informative as I had hoped

ggplot(data = bp23.genomic.binary, aes(intertegular_dist_mm, genera.by.indiv)) +
  geom_point(alpha = 0.6, color = "skyblue4") +
  geom_smooth(method = "lm")

#model.diversity.x.size <- lm(genera.by.indiv ~ intertegular_dist_mm, data = bp23.genomic.binary)
#summary(model.diversity.x.size) #in this model a bee with intertegular of 0 would have
#37 plant genera in its gut. nah. Center.

c.bp23.genomic.binary <-  bp23.genomic.binary %>% mutate(c_intertegular_dist_mm = intertegular_dist_mm - 4.91) #4.91 is the mean intertegular distance for 2023
c.model.diversity.x.size <- lm(genera.by.indiv ~ c_intertegular_dist_mm, data = c.bp23.genomic.binary)
report(c.model.diversity.x.size) #intercept corresponding to an int_dist of 4.91 in reality
#relationship weak but significant, but is the model worthy?
check_model(c.model.diversity.x.size) #Looks decent, but maybe there are other factors...




#organize metabarcoding data for all in 1 analysis with interaction and flower count data ------

bp23.genomic.binary4stats <- bp23.genomic.binary %>% 
  select(period, site, Abelmoschus:last_col()) %>% 
  mutate(method = rep("metabarcoding")) %>% #add a methodology identifier for next analysis
  relocate(method, .after = "site")
  







