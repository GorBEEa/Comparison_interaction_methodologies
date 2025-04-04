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
    #select(asv_id,genus)

#Working with data cleaned with decontam

plant.decontam0.5 <- readRDS(here("Data/gbp23.plant.decontam.0.5.RDS")) #all data in phyloseq format from strict decontam filter (th = 0.5)
plant.decontam0.1 <- readRDS(here("Data/gbp23.plant.decontam.0.1.RDS")) #all data in phyloseq format from less strict decontam filter (th = 0.1)
long.plant.decontam0.5 <- readRDS(here("Data/long.gbp23.plant.decontam.0.5.RDS")) #all data from ITS2 specific dada2 analysis in phyloseq format from strict decontam filter (th = 0.5)


#split into dataframes used in this analysis

bp.asv.counts.2023 <- as.data.frame(otu_table(long.plant.decontam0.5))
asvs <- rownames(bp.asv.counts.2023)
bp.asv.counts.2023 <- bp.asv.counts.2023 %>% mutate(asv_id = asvs) %>% relocate(asv_id)

sample_data_tab.cl <- sample_data(long.plant.decontam0.5)
sample_data_tab.cl <- as.data.frame(sample_data_tab.cl)
samples <- rownames(sample_data_tab.cl)
bp.2023 <- data.frame(sample = samples, sample_data_tab.cl, stringsAsFactors = FALSE)

tax_table.cl <- as.data.frame(tax_table(long.plant.decontam0.5))
asvs2 <- rownames(tax_table.cl)
bp.asv.tax.2023 <- tax_table.cl%>% 
  rename(genus = Genus) %>% 
  mutate(asv_id = asvs2) %>% 
  relocate(asv_id)
bp.asv.genus.2023 <- bp.asv.tax.2023 %>% 
  select(asv_id,genus)


#add intertegular distances to data 

bp23.size <- read_csv(here("Data/2023_bombus_size_data.csv")) #this has values for samples that were not in metabarcoding
bp23.size <- bp23.size %>% filter(sample != is.na(sample)) #clean up

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
paste("Metabarcoding detected", nrow(genus.hits.23),"plant genera across all of the 2023 gut samples")


bp23.genomic.analys <- bp23.genomic.specs #a copy for manipulation
bp23.genomic.analys$sample <- str_split_fixed(bp23.genomic.analys$sample, "_", 2)[,1] #remove sequencing plate labels
bp23.genomic.analys <- right_join(bp23.genomic.analys, bp23.size, by = "sample") #I think this step is adding 3 rows that are not real samples
bp23.genomic.analys <- bp23.genomic.analys %>% relocate(intertegular_dist_mm, .after = quant_reading) %>% 
  relocate(abdomen_length_mm, .after = intertegular_dist_mm) %>% 
  relocate(total_length_mm, .after = abdomen_length_mm) %>% 
  filter(specimen != is.na(specimen))
#NOTE: NAs exist for bombus measurements
#use df %>% na.replace(0) if needed

#make binary versions of 2023 data

bp23.genomic.binary <- bp23.genomic.analys %>% 
  mutate(across(Abelmoschus:last_col(), ~ifelse(. > 0, 1, 0))) %>% #read count data to presence absence 1s and 0s
  mutate(genera.by.indiv = rowSums(across(Abelmoschus:last_col()))) %>%  #add a sum of genera for diversity by inv sample
  relocate(genera.by.indiv, .after = quant_reading)


#Quickly visualize MB results
mb.genus.detections <- as.data.frame(colSums(bp23.genomic.binary[16:136])) %>% #THR COLUMNS SELECTED HERE ARE IMPORTANT FOR THE RESULTS YOU SEE. Make sure that they include all taxa
  rownames_to_column(var = "genus") %>% 
  rename(n.sample.detections = "colSums(bp23.genomic.binary[16:136])")
mb.genus.detections <- mb.genus.detections[order(mb.genus.detections$n.sample.detections, decreasing = TRUE) , ]
mb.genus.detections <- mb.genus.detections[-4, ] #NA row
top.mb.genus.detections <- mb.genus.detections[1:34,]
top.mb.genus.detections <- top.mb.genus.detections %>% 
  filter(genus != "Dioscorea") %>% 
  filter(genus != "Spondias") %>% 
  filter(genus != "Leptospermum")
  

fig.mb.title <- expression(paste("Top plant genera detected in", italic(" B. pascuorum "), "genetic sampling 2023"))
ggplot(top.mb.genus.detections, aes(x = reorder(genus, -n.sample.detections)
                                                 , y = n.sample.detections)) +
  geom_col(alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust=0.5)) +
  labs(x = "Plant Genus", y = "Positive Detections in Gut Samples") +
  ggtitle(fig.mb.title) 



c.bp23.genomic.binary <-  bp23.genomic.binary %>% mutate(c_intertegular_dist_mm = intertegular_dist_mm - 4.91) #4.91 is the mean intertegular distance for 2023

#organize metabarcoding data for all in 1 analysis with interaction and flower count data

bp23.genomic.binary4stats <- bp23.genomic.binary %>% 
  select(period, site, Abelmoschus:last_col()) %>% 
  mutate(method = rep("metabarcoding")) %>% #add a methodology identifier for next analysis
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
            n.genera = n_distinct(genus),
            .groups = 'drop')
  
#is there a significant diference in genera by period detected by stats?
boxplot(genera.by.indiv ~ period, data = bp23.genomic.binary)
dist_alphadiv <- vegdist(bp23.genomic.binary$genera.by.indiv, method = 'jaccard') #there may be two rows that sum to zero from the samples with 0 reads
disp_anova_a_period <- betadisper(d = dist_alphadiv, group = bp23.genomic.binary$period)
anova_a_period <- anova(disp_anova_a_period)
#report(anova_a_period)
#looks like the answer is no, but composition yes could change between periods right? 
#(ie. LJ's figure from ecoflor poster)
#could explore with permanova/NMDS:





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




#Analysis by period and site using various stat methods 
#Analyses of metabarcoding community data by site/period using nMDS, PERMANOVA, manyGLM  --------

#This appears to be an analysis just of metabarcoding data and is maybe also done in the corresponding script


#can do for data with read counts (bp23.genomic.analys) or presence absence (bp23.genomic.binary)
#just change these three lines
#simplify factors/data involved
stat.clean.bp23.genomic.binary <- bp23.genomic.binary[rowSums(bp23.genomic.binary[, 16:ncol(bp23.genomic.binary)], na.rm = TRUE) > 0, ]
site <- as.factor(stat.clean.bp23.genomic.binary$site)
period <- as.factor(stat.clean.bp23.genomic.binary$period)
gut.plants <- stat.clean.bp23.genomic.binary %>%
  select(Abelmoschus:last_col()) 

#There is a problematic sample that makes an outlier point because it only has read counts for the ASV of Iberis. Take it out.
gut.plants <- gut.plants[-c(39),]
gut.plants <- gut.plants %>% 
  select(!Iberis)

#NMDS visualization
dist.gut.plants <- vegdist(gut.plants, method = "raup", binary = TRUE) #calc distance between communities for later stat analysis
gut.plants.mds <- metaMDS(gut.plants, distance = "raup", binary = TRUE)
plot(gut.plants.mds$points, col = site, pch = 16, main = "Plant taxa in GBP23 gut DNA visualized by site")
plot(gut.plants.mds$points, col = period, pch = 16, main = "Plant taxa in GBP23 gut DNA visualized by period")


#permanova test (play with ~ variables to understand more)
permanova.gut.plants <- adonis2(gut.plants ~ period*site, permutations = 9999, method = "jaccard", by = "terms")
summary(permanova.gut.plants)
permanova.gut.plantss


# Alternative analysis: many glm 
#extracting effect of site or period for each species using multiple glm
#CSG: did this in sevilla as a test of using our data with mult glm, would have to revise to do anything with this

#gut.plants.spp <- mvabund(gut.plants)
#mglm.gut.flowers <- manyglm(gut.flowers.spp ~ bp23.genomic.analys$period, family = "")
#anova(mglm.gut.flowers, p.uni="adjusted") #this takes a lot of computing power
#should show deviation and probable significance of effect for each species



#Analysis by bee size ---------

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

#c.bp23.genomic.binary <-  bp23.genomic.binary %>% mutate(c_intertegular_dist_mm = intertegular_dist_mm - 4.91) #4.91 is the mean intertegular distance for 2023
#above step moved up in code
c.model.diversity.x.size <- lm(genera.by.indiv ~ c_intertegular_dist_mm, data = c.bp23.genomic.binary)
report(c.model.diversity.x.size) #intercept corresponding to an int_dist of 4.91 in reality
#relationship weak but significant, but is the model worthy?
check_model(c.model.diversity.x.size) #Looks decent, but maybe there are other factors...
DHARMa::testResiduals(c.model.diversity.x.size)

  







