#Script for manipulating and processing 2023 B. pascuorum ITS2 plant metabarcoding data

#unblock these packages if not running after interaction_data (Recommended to run first)
#library(readr)
#library(here)
#library(tidyverse)
#library(tidyr)
#library(tidyselect)
#library(easystats)
#library(visreg)
#library(vegan)

library(viridis)
library(report)
library(DHARMa)
library(phyloseq) ; packageVersion("phyloseq")


#Load Data ----

# Original load data flow directly from dada2 CSVs:

#bp.2023 <- read_tsv(here("Data/2023_BP_metab_sample_info.tsv")) #bp sample info
#bp.2023 <- bp.2023 %>% rename(sample = sample_name)

#bp.asv.counts.2023 <- read_tsv(here("Data/2023_plant_ASVs_counts.tsv")) #asv counts
#bp.asv.counts.2023 <- bp.asv.counts.2023 %>% rename(asv_id = ...1)

#bp.asv.tax.2023 <- read_tsv(here("Data/2023_plant_ASVs_taxonomy.tsv")) #taxa associated with asvs
#bp.asv.tax.2023 <- bp.asv.tax.2023 %>% 
 # rename(asv_id = ...1) %>% 
  #rename(genus = Genus)
#bp.asv.genus.2023 <- bp.asv.tax.2023 %>% 
    #select(asv_id,genus)


#Now what we actually do it bring the data into R in the decontam step
#the data used in this script are outputs from decontam_gut_plant_2023.R

plant.decontam0.5 <- readRDS(here("Data/gbp23.plant.decontam.0.5.RDS")) #all data in phyloseq format from strict decontam filter (th = 0.5) THESE ARE THE DATA USED IN THE FINAL ANALYSIS
plant.decontam0.1 <- readRDS(here("Data/gbp23.plant.decontam.0.1.RDS")) #all data in phyloseq format from less strict decontam filter (th = 0.1)
long.plant.decontam0.5 <- readRDS(here("Data/long.gbp23.plant.decontam.0.5.RDS")) #all data from ITS2 specific dada2 analysis in phyloseq format from strict decontam filter (th = 0.5)



#split decontam output of choice into dataframes used in this analysis

bp.asv.counts.2023 <- as.data.frame(otu_table(plant.decontam0.5))
asvs <- rownames(bp.asv.counts.2023)
bp.asv.counts.2023 <- bp.asv.counts.2023 %>% mutate(asv_id = asvs) %>% relocate(asv_id)

sample_data_tab.cl <- sample_data(plant.decontam0.5)
sample_data_tab.cl <- as.data.frame(sample_data_tab.cl)
samples <- rownames(sample_data_tab.cl)
bp.2023 <- data.frame(sample = samples, sample_data_tab.cl, stringsAsFactors = FALSE)

tax_table.cl <- as.data.frame(tax_table(plant.decontam0.5))
asvs2 <- rownames(tax_table.cl)
bp.asv.tax.2023 <- tax_table.cl%>% 
  rename(genus = Genus) %>% 
  mutate(asv_id = asvs2) %>% 
  relocate(asv_id)
bp.asv.genus.2023 <- bp.asv.tax.2023 %>% 
  select(asv_id,genus)

#Look for genera that only correspond to one ASV. These could be contaminants/misidentified sequences
#These can be confirmed by BLASTing the ASV sequence
ASVs.per.Genus <- as.data.frame(table(bp.asv.genus.2023$genus))
single.ASVs <- ASVs.per.Genus %>% 
  filter(Freq == 1) %>% 
  rename(genus = Var1)
single.ASVs <- left_join(single.ASVs, bp.asv.genus.2023, by = 'genus')


#condense two of these to have genera names assigned to asvs in samples
bp.plant.asvNs.w.genus.2023 <- right_join(bp.asv.counts.2023,bp.asv.genus.2023, by = "asv_id")

#screen even more for contaminats based on expert knowledge reveiw of results
load(file = here("Data/known.misIDs.RData")) #list of taxa that are either known contaminants or mistakenly identified ASVs
bp.plant.asvNs.w.genus.2023 <- bp.plant.asvNs.w.genus.2023 %>% relocate(genus, .after = asv_id) %>%  #this is just to look and make sure it worked
  filter(!genus %in% known.misIDs) #remove taxa from the loaded list 

#create binary version
binary.plant.asvNs.w.genus.2023 <- bp.plant.asvNs.w.genus.2023 %>% 
  relocate(genus, .after = asv_id) %>% 
  mutate(across(GBP23010301M_ITS_P48:last_col(), ~ifelse(. > 0, 1, 0))) %>% 
  mutate(indiv.w.signal = rowSums(across(GBP23010301M_ITS_P48:GBP23061405M_ITS_P48))) %>% 
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
  mutate(site = str_extract(site, "\\d+"), site = as.integer(site)) %>% 
  select(-last_col()) #Check that this is removing the NA column and not something important


#manipulate and analyze data ------

#extract all interaction genera from gut metabarcoding
genus.hits.23 <- bp.plant.asvNs.w.genus.2023 %>%
  #select(!asv_id) %>% #simplify working data
  group_by(genus) %>% 
  summarise(across(where(is.numeric), sum)) %>% #here we already see that 160 genera were detected in 2023
  rowwise() %>% 
  filter(if_any(where(is.numeric), ~. > 0)) %>% #should already be the difinitive list, but if any genera still have 0 detections, remove them
  ungroup %>% 
  select(genus) %>%  #create just a list of the genera
  filter(genus != "NA")

paste("Metabarcoding detected", nrow(genus.hits.23),"plant genera across all of the 2023 gut samples")



bp23.genomic.analys <- bp23.genomic.specs #a copy for manipulation
bp23.genomic.analys$sample <- str_split_fixed(bp23.genomic.analys$sample, "_", 2)[,1] #remove sequencing plate labels


#create a dataset aggregated by sampling day
bp23.genomic.xday <- bp23.genomic.analys %>%
  group_by(period, site) %>%
  summarise(across(Abelmoschus:last_col(), sum), .groups = "drop") %>% 
  mutate(day = paste0("P", period, "S", site)) %>% 
  relocate(day)
  

#make presence/absence reduced version of 2023 data
bp23.genomic.binary <- bp23.genomic.analys %>% 
  mutate(across(Abelmoschus:last_col(), ~ifelse(. > 0, 1, 0))) %>% #read count data to presence absence 1s and 0s
  mutate(genera.by.indiv = rowSums(across(Abelmoschus:last_col()))) %>%  #add a sum of genera for diversity by inv sample
  relocate(genera.by.indiv, .after = quant_reading)

#day aggregated
bp23.genomic.xday.binary <- bp23.genomic.xday %>% 
  mutate(across(Abelmoschus:last_col(), ~ifelse(. > 0, 1, 0))) %>% #read count data to presence absence 1s and 0s
  mutate(genera.xday = rowSums(across(Abelmoschus:last_col()))) %>%  #add a sum of genera for diversity by inv sample
  relocate(genera.xday, .after = site)




#Quickly visualize MB results by SAMPLES
mb.genus.detections <- as.data.frame(colSums(bp23.genomic.binary[16:ncol(bp23.genomic.binary)])) %>% #THR COLUMNS SELECTED HERE ARE IMPORTANT FOR THE RESULTS YOU SEE. Make sure that they include all taxa
  rownames_to_column(var = "genus") %>% 
  rename(n.sample.detections = "colSums(bp23.genomic.binary[16:ncol(bp23.genomic.binary)])")
mb.genus.detections <- mb.genus.detections[order(mb.genus.detections$n.sample.detections, decreasing = TRUE) , ]
top.mb.genus.detections <- mb.genus.detections[1:30,]

  
fig.mb.title <- expression(paste("Top plant genera detected in", italic(" B. pascuorum "), "genetic sampling 2023"))
ggplot(top.mb.genus.detections, aes(x = reorder(genus, -n.sample.detections)
                                                 , y = n.sample.detections)) +
  geom_col(alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust=0.5)) +
  labs(x = "Plant Genus", y = "Positive Detections in Gut Samples") +
  ggtitle(fig.mb.title) 

#Quickly visualize MB results by SAMPLING DAYS
mb.genus.detections.xday <- as.data.frame(colSums(bp23.genomic.xday.binary[5:ncol(bp23.genomic.xday.binary)])) %>% 
  rownames_to_column(var = "genus") %>% 
  rename(n.day.detections = "colSums(bp23.genomic.xday.binary[5:ncol(bp23.genomic.xday.binary)])")
mb.genus.detections.xday <- mb.genus.detections.xday[order(mb.genus.detections.xday$n.day.detections, decreasing = TRUE) , ]
top.mb.genus.detections.xday <- mb.genus.detections.xday[1:32,]
top.mb.genus.detections.xday <- top.mb.genus.detections.xday %>% 
  filter(genus != "Dioscorea") %>% 
  filter(genus != "Spondias") %>% 
  filter(genus != "Pleuropterus")

fig.mb.days.title <- expression(paste("Top plant genera detected in", italic(" B. pascuorum "), "genetic sampling 2023 (sampling days aggregated)"))
ggplot(top.mb.genus.detections.xday, aes(x = reorder(genus, -n.day.detections), y = n.day.detections)) +
  geom_col(alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust=0.5)) +
  labs(x = "Plant Genus", y = "Positive metabarcoding detection days") +
  ggtitle(fig.mb.days.title)



#c.bp23.genomic.binary <-  bp23.genomic.binary %>% mutate(c_intertegular_dist_mm = intertegular_dist_mm - 4.91) #4.91 is the mean intertegular distance for 2023

#organize metabarcoding data for all in 1 analysis with interaction and flower count data

bp23.genomic.binary4stats <- bp23.genomic.binary %>% 
  select(period, site, Abelmoschus:last_col()) %>% 
  mutate(method = rep("gut.metabarcoding")) %>% #add a methodology identifier for next analysis
  relocate(method, .after = "site")

bp23.genomic.binary4stats.xday <- bp23.genomic.xday.binary %>% 
  select(-c(day, genera.xday)) %>%
  mutate(method = rep("gut.metabarcoding")) %>%
  relocate(method, .after = "site")









#Analyses by period, site, bombus size, etc. ----

#Analysis by period -----

#get number of genera detected by period
bp23.genomic.periods <- bp23.genomic.analys %>% 
  pivot_longer(cols = Abelmoschus:last_col(), names_to = "genus", values_to = "count") %>%
  filter(count > 0) %>% 
  group_by(period, genus) %>% 
  slice(1) %>%  # Count only 1 detection per period
  group_by(period) %>% 
  summarise(n_samples = n_distinct(sample),
            n.genera.gmb = n_distinct(genus),
            .groups = 'drop')

#Analysis by site -----

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
