#Script for manipulating and processing 2023 B. pascuorum ITS2 plant metabarcoding data

#unblock these packages if not running after interaction_data (Recommended to run first)
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
library(iNEXT)


#Load Data -----

#Working with data cleaned with decontam

pollen.decontam0.5 <- readRDS(here("Data/pollen23.24.decontam.0.5.RDS")) #all data in phyloseq format from 2023 and 2024 Pollen sequence data


#split into dataframes used in this analysis

poln.asv.counts.2023.24 <- as.data.frame(otu_table(pollen.decontam0.5))
poln.asvs <- rownames(poln.asv.counts.2023.24)
poln.asv.counts.2023.24 <- poln.asv.counts.2023.24 %>% mutate(asv_id = poln.asvs) %>% relocate(asv_id)

poln.sample_data_tab.cl <- sample_data(pollen.decontam0.5)
poln.sample_data_tab.cl <- as.data.frame(poln.sample_data_tab.cl)
poln.samples <- rownames(poln.sample_data_tab.cl)
poln.2023.24 <- data.frame(sample = poln.samples, poln.sample_data_tab.cl, stringsAsFactors = FALSE)

poln.tax_table.cl <- as.data.frame(tax_table(pollen.decontam0.5))
poln.tax_table.cl$Genus[poln.tax_table.cl$Genus == "Chaetopogon"] <- "Agrostis" #these are the same, we don't want to double count
poln.asvs2 <- rownames(poln.tax_table.cl)
poln.asv.tax.2023.24 <- poln.tax_table.cl%>% 
  rename(genus = Genus) %>% 
  mutate(asv_id = poln.asvs2) %>% 
  relocate(asv_id)
poln.asv.genus.2023.24 <- poln.asv.tax.2023.24 %>% 
  select(asv_id,genus)


#General sequencing data stats and information
poln.reads.23 <- read_tsv(here("Data/dada2_outputs/2023_pollen_R1_read_summary.tsv"), show_col_types = FALSE)
poln.reads.23 <- as.data.frame(poln.reads.23[1:25,])


fig.poln.read.coverage <- ggplot(poln.reads.23, aes(x = sample, y = reads)) +
  geom_col(fill = "goldenrod1", alpha = 0.8, width = 0.7) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x = "Samples (N = 25)", y = "Reads") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("B.")

#Look for genera that only correspond to one ASV. These could be contaminants/misidentified sequences
#These can be confirmed by BLASTing the ASV sequence
poln.ASVs.per.Genus <- as.data.frame(table(poln.asv.genus.2023.24$genus))
poln.single.ASVs <- poln.ASVs.per.Genus %>% 
  filter(Freq == 1) %>% 
  rename(genus = Var1)
poln.single.ASVs <- left_join(poln.single.ASVs, poln.asv.genus.2023.24, by = 'genus')



#create a table of ASV counts corresponding to genera names 
poln.asvNs.w.genus.2023.24 <- right_join(poln.asv.counts.2023.24,poln.asv.genus.2023.24, by = "asv_id")
poln.asvNs.w.genus.2023.24 <- poln.asvNs.w.genus.2023.24 %>% relocate(genus, .after = asv_id) %>%  #this is just to look and make sure it worked
  filter(!genus %in% known.misIDs)
poln.asvNs.w.genus.2023.24$genus[poln.asvNs.w.genus.2023.24$genus == "Chaetopogon"] <- "Agrostis"
#Just 2023
poln.asvNs.w.genus.2023 <- poln.asvNs.w.genus.2023.24 %>% select(asv_id, genus, starts_with("Y23"))

#create a binary version of the above table
#basically a genus occurence table organized by samples
binary.poln.asvNs.w.genus.2023.24 <- poln.asvNs.w.genus.2023.24 %>% 
  relocate(genus, .after = asv_id) %>% 
  mutate(across(Y23010501PI_ITS:last_col(), ~ifelse(. > 0, 1, 0))) %>% 
  mutate(indiv.w.signal = rowSums(across(Y23010501PI_ITS:last_col()))) %>% 
  relocate(indiv.w.signal, .after = genus) 

#transpose to be able to combine with other BP data
poln.asvgn.wide.2023.24 <- poln.asvNs.w.genus.2023.24 %>%
  pivot_longer(cols = starts_with(c("Y2")), names_to = "sample", values_to = "Count") %>%
  group_by(sample, genus) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  pivot_wider(names_from = genus, values_from = Count, values_fill = 0)

#version of above for sampling completeness
poln.asvgn.wide.2023 <- poln.asvNs.w.genus.2023 %>%
  pivot_longer(cols = starts_with(c("Y2")), names_to = "sample", values_to = "Count") %>%
  group_by(sample, asv_id) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  pivot_wider(names_from = asv_id, values_from = Count, values_fill = 0)

#combine all
poln23.24.genomic.specs <- right_join(poln.2023.24, poln.asvgn.wide.2023.24, by = "sample")
poln23.24.genomic.specs <- poln23.24.genomic.specs %>% 
  mutate(
    period = as.integer(str_sub(sample, 4, 5)),
    site   = as.integer(str_sub(sample, 6, 7)),
    year = case_when(str_sub(sample, 2, 3) == "23" ~ 2023L, str_sub(sample, 2, 3) == "24" ~ 2024L, TRUE ~ NA_integer_) 
    ) %>% 
  select(-last_col()) #Check that this is removing the NA column and not something important

#Just 2023
poln.2023.genomic.specs <- poln23.24.genomic.specs %>% 
  filter(year == 2023)


#look at sampling completeness ------------------------------------

poln.asv.list <- apply(poln.asvgn.wide.2023, 1, function(x) as.numeric(x))
#`apply` returns a matrix for numeric input, so convert to list
poln.asv.list <- split(poln.asv.list, seq(nrow(poln.asvgn.wide.2023)))
# But `split` won’t work properly here; better do:
poln.asv.list <- lapply(1:nrow(poln.asvgn.wide.2023), function(i) as.numeric(poln.asvgn.wide.2023[i, ]))
names(poln.asv.list) <- rownames(poln.asvgn.wide.2023)
#NA control
poln.asv.list <- lapply(poln.asv.list, function(x) {
  x[is.na(x)] <- 0
  x})
# Now run iNEXT
#poln.asv.inext <- iNEXT(poln.asv.list, q = 0, datatype = "abundance", size = NULL)
#saveRDS(poln.asv.inext, file = here("Data/poln_inext_out")) #better to just save and reload if it is ok
poln.asv.inext <- readRDS(here("Data/poln_inext_out"))

# Plot rarefaction curves

poln23.rarefaction <- ggiNEXT(poln.asv.inext, type = 1) +
  xlab("read count") + 
  ylab("") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_viridis_d(option = "D", end = 0.9)+
  ggtitle("B.")



poln23.rarefaction$layers <- lapply(poln23.rarefaction$layers, function(layer) {
  if ("GeomLine" %in% class(layer$geom)) {
    layer$aes_params$linewidth <- 1
    }
  layer})

poln23.rarefaction$layers <- poln23.rarefaction$layers[
  !sapply(poln23.rarefaction$layers, function(l) inherits(l$geom, "GeomPoint"))
]

#poln23.rarefaction #peek

ggsave(here("results/poln23_sampling_completeness.png"),
       plot = poln23.rarefaction,
       width = 4,
       height = 5,
       dpi = 400)




#manipulate and analyze data ------

#Identify all interaction genera from pollen metabarcoding
poln.genus.hits.23.24 <- poln.asvNs.w.genus.2023.24 %>%
  #select(-starts_with("Y24")) %>% #only 2023
  group_by(genus) %>% 
  summarise(across(where(is.numeric), sum)) %>% #here we already see that 160 genera were detected in 2023
  rowwise() %>% 
  filter(if_any(where(is.numeric), ~. > 0)) %>% #should already be the difinitive list, but if any genera still have 0 detections, remove them
  ungroup %>% 
  select(genus) %>%  #create just a list of the genera
  filter(genus != "NA")

paste("Metabarcoding detected", nrow(poln.genus.hits.23.24),"plant genera across all of the 2023 and 2024 corbicular pollen samples")

#Just 2023
poln.genus.hits.2023 <- poln.asvNs.w.genus.2023 %>%
  #select(-starts_with("Y24")) %>% #only 2023
  group_by(genus) %>% 
  summarise(across(where(is.numeric), sum)) %>% #here we already see that 160 genera were detected in 2023
  rowwise() %>% 
  filter(if_any(where(is.numeric), ~. > 0)) %>% #should already be the difinitive list, but if any genera still have 0 detections, remove them
  ungroup %>% 
  select(genus) %>%  #create just a list of the genera
  filter(genus != "NA")

paste("Metabarcoding detected", nrow(poln.genus.hits.2023),"plant genera across all of the 2023 corbicular pollen samples")


#From here I am going to only work with 2023 samples---------------------------


#Aggregate by sampling days
poln.genomic.xday.2023 <- poln.2023.genomic.specs %>%
  group_by(period, site) %>%
  summarise(across(Achillea:last_col(), sum), .groups = "drop") %>% 
  mutate(day = paste0("P", period, "S", site)) %>% 
  relocate(day)


#make binary versions of 2023 data

poln.genomic.binary.2023 <- poln.2023.genomic.specs %>% 
  mutate(across(Achillea:last_col(), ~ifelse(. > 0, 1, 0))) %>% #read count data to presence absence 1s and 0s
  mutate(genera.by.indiv = rowSums(across(Achillea:last_col()))) %>%  #add a sum of genera for diversity by inv sample
  relocate(genera.by.indiv, .after = quant_reading)

poln.2023.genomic.xday.binary <- poln.genomic.xday.2023 %>% 
  mutate(across(Achillea:last_col(), ~ifelse(. > 0, 1, 0))) %>% #read count data to presence absence 1s and 0s
  mutate(genera.xday = rowSums(across(Achillea:last_col()))) %>%  #add a sum of genera for diversity by inv sample
  relocate(genera.xday, .after = site)





#Quickly visualize MB results by pollen samples
poln.mb.genus.detections <- as.data.frame(colSums(poln.genomic.binary.2023[13:ncol(poln.genomic.binary.2023)])) %>% #THR COLUMNS SELECTED HERE ARE IMPORTANT FOR THE RESULTS YOU SEE. Make sure that they include all taxa
  rownames_to_column(var = "genus") %>% 
  rename(n.sample.detections = "colSums(poln.genomic.binary.2023[13:ncol(poln.genomic.binary.2023)])")
poln.mb.genus.detections <- poln.mb.genus.detections[order(poln.mb.genus.detections$n.sample.detections, decreasing = TRUE) , ]
top.poln.mb.genus.detections <- poln.mb.genus.detections[1:30,]


poln.fig.mb.title <- expression(paste("Top plant genera detected by metabarcoding in", italic(" B. pascuorum "), "corbicular pollen samples 2023"))
ggplot(top.poln.mb.genus.detections, aes(x = reorder(genus, -n.sample.detections)
                                    , y = n.sample.detections)) +
  geom_col(alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust=0.5)) +
  labs(x = "Plant Genus", y = "Positive detections in pollen samples") +
  ggtitle(poln.fig.mb.title) 

#Quickly visualize MB results by SAMPLING DAYS
poln.mb.genus.detections.xday <- as.data.frame(colSums(poln.2023.genomic.xday.binary[5:ncol(poln.2023.genomic.xday.binary)])) %>% 
  rownames_to_column(var = "genus") %>% 
  rename(n.day.detections = "colSums(poln.2023.genomic.xday.binary[5:ncol(poln.2023.genomic.xday.binary)])")
poln.mb.genus.detections.xday <- poln.mb.genus.detections.xday[order(poln.mb.genus.detections.xday$n.day.detections, decreasing = TRUE) , ]
top.poln.mb.genus.detections.xday <- poln.mb.genus.detections.xday[1:32,]
top.poln.mb.genus.detections.xday <- top.poln.mb.genus.detections.xday 

poln.fig.mb.days.title <- expression(paste("Top plant genera detected in", italic(" B. pascuorum "), "genetic sampling 2023 (sampling days aggregated)"))
ggplot(top.poln.mb.genus.detections.xday, aes(x = reorder(genus, -n.day.detections), y = n.day.detections)) +
  geom_col(alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust=0.5)) +
  labs(x = "Plant Genus", y = "Positive metabarcoding detection days") +
  ggtitle(poln.fig.mb.days.title)



#organize metabarcoding data for all in 1 analysis with interaction and flower count data

poln.genomic.binary.2023.4stats <- poln.genomic.binary.2023 %>% 
  select(period, site, Achillea:last_col()) %>% 
  mutate(method = rep("pollen.metabarcoding")) %>% #add a methodology identifier for next analysis
  relocate(method, .after = "site")

poln.genomic.binary.2023.xday.4stats <- poln.2023.genomic.xday.binary %>% 
  select(-c(day, genera.xday)) %>%
  mutate(method = rep("pollen.metabarcoding")) %>%
  relocate(method, .after = "site")














#Analyses by period, site, etc. ----

#Will be interesting, but not prioritized for methdology comparisons

#Analysis by period -----

#get number of genera detected by period
poln.2023.genomic.periods <- poln.2023.genomic.specs %>% 
  pivot_longer(cols = Achillea:last_col(), names_to = "genus", values_to = "count") %>%
  filter(count > 0) %>% 
  group_by(period, genus) %>% 
  slice(1) %>%  # Count only 1 detection per period
  group_by(period) %>% 
  summarise(n_poln.samples = n_distinct(sample),
            n.genera.pmb = n_distinct(genus),
            .groups = 'drop')

#is there a significant diference in genera by period detected by stats?
#boxplot(genera.by.indiv ~ period, data = poln.genomic.binary.23.24)
#dist_alphadiv <- vegdist(poln.genomic.binary.23.24$genera.by.indiv, method = 'jaccard') #there may be two rows that sum to zero from the poln.samples with 0 reads
#disp_anova_a_period <- betadisper(d = dist_alphadiv, group = poln.genomic.binary.23.24$period)
#anova_a_period <- anova(disp_anova_a_period)
#report(anova_a_period)
#looks like the answer is no, but composition yes could change between periods right? 
#(ie. LJ's figure from ecoflor poster)
#could explore with permanova/NMDS:
#I repeated this for poln.samples grouped by day and there is no change to the conclusion

#Analysis by site -----

#get number of genera detected by site
poln23.genomic.sites <- poln.2023.genomic.specs %>% 
  pivot_longer(cols = Achillea:last_col(), names_to = "genus", values_to = "count") %>%
  filter(count > 0) %>% 
  group_by(site, genus) %>% 
  slice(1) %>%  # Count only 1 detection per site
  group_by(site) %>% 
  summarise(n_poln.samples = n_distinct(sample),
            n.genera.poln = n_distinct(genus),
            .groups = 'drop')




