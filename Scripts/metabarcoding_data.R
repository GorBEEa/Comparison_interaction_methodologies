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
library(DHARMa)
library(phyloseq) ; packageVersion("phyloseq")


#Load Data -----
#For now we are going to work with Bell db analyzed results @ genus level

# Original load data flow directly from dada2 CSVs: ------

#bp.2023 <- read_tsv(here("Data/2023_BP_metab_sample_info.tsv")) #bp sample info
#bp.2023 <- bp.2023 %>% rename(sample = sample_name)

#bp.asv.counts.2023 <- read_tsv(here("Data/2023_plant_GorBEEa_ASVs_counts.tsv")) #asv counts
#bp.asv.counts.2023 <- bp.asv.counts.2023 %>% rename(asv_id = ...1)

#bp.asv.tax.2023 <- read_tsv(here("Data/2023_plant_GorBEEa_ASVs_taxonomy.tsv")) #taxa associated with asvs
#bp.asv.tax.2023 <- bp.asv.tax.2023 %>% 
 # rename(asv_id = ...1) %>% 
  #rename(genus = Genus)
#bp.asv.genus.2023 <- bp.asv.tax.2023 %>% 
 #   select(asv_id,genus)

#Working with data cleaned with decontam

plant.decontam.05 <- readRDS(here("Data/gbp23.plant.decontam.0.5.RDS"))

count_tab.cl <- otu_table(plant.decontam.05)
#count_tab.cl <- as.data.frame(plant.decontam.05) #can't? do this?

sample_data_tab.cl <- sample_data(plant.decontam.05)
sample_data_tab.cl <- as.data.frame(sample_data_tab.cl)
samples <- rownames(sample_data_tab.cl)
bp.2023 <- sample_data_tab.cl %>% mutate(sample_name = samples)

tax_table.cl <- tax_table(plant.decontam.05)




bp23.size <- read_csv(here("Data/2023_bombus_size_data.csv"))

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
  mutate(site = str_extract(site, "\\d+"), site = as.integer(site))
#this dataset does not contain Bombus size, see bp23.genomic.analys below


#manipulate and analyze data ------

#extract genus hits from here

genus.hits.23 <- bp.plant.asvNs.w.genus.2023 %>%
  #select(!asv_id) %>% #simplify working data
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



#Analysis by period -----

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
  
#is there a significant diference in genera by period detected by stats?
boxplot(genera.by.indiv ~ period, data = bp23.genomic.binary)
dist_alphadiv <- vegdist(bp23.genomic.binary$genera.by.indiv, method = 'bray')
disp_anova_a_period <- betadisper(d = dist_alphadiv, group = bp23.genomic.binary$period)
anova_a_period <- anova(disp_anova_a_period)
#report(anova_a_period)
#looks like the answer is no, but composition yes could change between periods right? 
#(ie. LJ's figure from ecoflor poster)
#explore with permanova


#simplify factors/data involved THIS SHOULD BE IN INTERACTIONS x METABARCODING (bp23.all.binary is from there)
site <- as.factor(bp23.all.binary$site)
period <- as.factor(bp23.all.binary$period)
all.plants <- bp23.all.binary %>% 
  select(!c(site, period, method))  
all.plants <- all.plants %>% replace(is.na(all.plants), 0)

#prepare NMDS data
dist.all.plants <- vegdist(all.plants, method = "bray") #calc distance between communities for later stat analysis
all.plant.mds <- metaMDS(all.plants, distance = "bray")

a_period_nmds_points <- as.data.frame(all.plant.mds$points)
colnames(a_period_nmds_points) <- c("NMDS1", "NMDS2")
a_period_nmds_points$period <- period

#prep polgons
polygon_data <- a_period_nmds_points %>%
       group_by(period) %>%
       slice(chull(NMDS1, NMDS2))

#NMDS plot
expression(paste("NMDS visualization of plant DNA diversity by period"))

fig.NMDS.gutplant.period <- ggplot(nmds_points, aes(x = NMDS1, y = NMDS2, color = period)) +
       geom_polygon(data = polygon_data, 
                                       aes(fill = period, color = NULL), 
                                       alpha = 0.2, 
                                       show.legend = FALSE) +
       geom_point(size = 3) +
  theme_classic() +  
       labs(
             title = NMDS.title,
             x = "NMDS1",
             y = "NMDS2",
             color = "period"
         ) +
       theme(plot.title = element_text(hjust=0.5),
                         legend.position = c(0.13, 0.85)
                   ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d(alpha = 0.2)

#permanova test of this comparison
permanova.alpha.periods <- adonis2(all.plants ~ period, permutations = 9999, method = "bray", by = "terms")
permanova.alpha.periods %>% 
  kbl(caption = "PERMANOVA analysis of temporal effects on observed plant DNA diversity from Bombus gut extracts") %>% 
  kable_minimal(full_width = F, html_font = "Cambria")

#weak but significant change across periods




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








#plant DNA diversity by bombus size (using intertegular distance)
fig.diversity.pd.intglr <- ggplot(data = bp23.genomic.binary, aes(period, genera.by.indiv, size = intertegular_dist_mm)) +
  geom_point(alpha = 0.6, color = "skyblue4") +
  theme(legend.key.size = unit(1, 'cm')) +
  scale_x_continuous(breaks = 1:6, labels = 1:6) +
  scale_size_continuous(range = c(1, 6)) +
  labs(x = "Period",
       y = "Detected plant genera",
       size = "Intertegular Distance 
       (mm)")

#This isn't as informative as I had hoped

fig.diversity.x.intglr <- ggplot(data = bp23.genomic.binary, aes(intertegular_dist_mm, genera.by.indiv)) +
  geom_point(alpha = 0.6, color = "skyblue4") +
  geom_smooth(method = "lm", color = "forestgreen") +
  theme_classic()

#model.diversity.x.size <- lm(genera.by.indiv ~ intertegular_dist_mm, data = bp23.genomic.binary)
#summary(model.diversity.x.size) #in this model a bee with intertegular of 0 would have
#37 plant genera in its gut. Biologically doesn't make sense - center data

c.bp23.genomic.binary <-  bp23.genomic.binary %>% mutate(c_intertegular_dist_mm = intertegular_dist_mm - 4.91) #4.91 is the mean intertegular distance for 2023
c.model.diversity.x.size <- lm(genera.by.indiv ~ c_intertegular_dist_mm, data = c.bp23.genomic.binary)
report(c.model.diversity.x.size) #intercept corresponding to an int_dist of 4.91 in reality
#relationship weak but significant, but is the model worthy?
check_model(c.model.diversity.x.size) #Looks decent, but maybe there are other factors...
DHARMa::testResiduals(c.model.diversity.x.size)



#organize metabarcoding data for all in 1 analysis with interaction and flower count data ------

bp23.genomic.binary4stats <- bp23.genomic.binary %>% 
  select(period, site, Abelmoschus:last_col()) %>% 
  mutate(method = rep("metabarcoding")) %>% #add a methodology identifier for next analysis
  relocate(method, .after = "site")
  







