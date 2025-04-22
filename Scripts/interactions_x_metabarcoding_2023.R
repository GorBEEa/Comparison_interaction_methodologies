#Analyzing interaction methodologies and floral diversity data together 
#Interaction_Data.R, flower_count_data.R, and metabarcoding_data.R should already be sourced
#(In that order)


#unblock these packages if not running after _data.R scripts
#library(readr)
#library(here)
#library(tidyverse)
#library(tidyr)
#library(tidyselect)
#library(easystats)
#library(visreg)
#library(vegan)

library(mvabund)
library(easystats)
library(kableExtra)


#Overlap in methodology communities ----------

#Quick visual check of which interaction transect species were detected by gut metabarcoding 
detected.int.genus <- df.int.genus %>% distinct(genus) #Clean list of genera (27) from BP interactions
detected.int.genus$mb.detected <- as.integer(detected.int.genus$genus %in% genus.hits.23$genus) #presence absence comparison
int.genus.occur.md.detect <- full_join(detected.int.genus, df.int.genus %>% count(genus)) #table with total interaction counts for 2023 by genus (n) and their binary value for detection y/n with mb

fig.zz <- int.genus.occur.md.detect %>% 
  arrange(n) %>% 
  ggplot(aes(x = reorder(genus, -n), y = n, fill = factor(mb.detected))) + 
  geom_bar(stat = "identity") + 
  labs(x = "Floral genus", y = "Interaction count 2023 Season", title = title.int.genus) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),plot.title = element_text(hjust=0.5)) + 
  scale_fill_manual(values = c("0" = "grey", "1" = "lightblue"), name = "Detected by ITS2 Metabarcoding", labels = c("No", "Yes"))
#because there are no cases where mb did not detect a genus, the legend is confused and thinks that
#result: all 27 interaction taxa were observed by MB


#Inverse analysis of above - which genera observed in metabarcoding were observed in interactions -----
observed.mb.genus <- genus.hits.23
observed.mb.genus$int.detected <- as.integer(observed.mb.genus$genus %in% df.int.genus$genus)
observed.mb.genus %>% filter(int.detected == 1)
#result: Interactions did not detect any species beyond those detected by metabarcoding


#which detected by metabarcoding were observed in flower counts?
observed.mb.genus$flower.count.detected <- as.integer(observed.mb.genus$genus %in% flower.count.genera$flower_genus)
paste("Of the", nrow(genus.hits.23),"taxa detected by metabarcoding,", 
      nrow(observed.mb.genus %>% filter(flower.count.detected == 1)),
      "were observed in the flower counts")

#inverse of above - how many flower count genera were not in metabarcoding results?
cp.flower.count.genera <- flower.count.genera
cp.flower.count.genera$in.mb <- as.integer(flower.count.genera$flower_genus %in% genus.hits.23$genus)
in.fc.not.mb <- cp.flower.count.genera %>% filter(in.mb == 0)
paste("Of the", nrow(flower.count.genera),"taxa detected in flower counts,", nrow(in.fc.not.mb),
      "were unique to this survey")


#Diversity by periods ----- 

method.colors <- c("c_n_genera_flower_count" ="slategrey",
                   "a_n_genera_interactions" = "lightblue",
                   "b_n_genera_metabarcoding" = "forestgreen") #set some universal colors for this project

compare.gen.by.periods <- right_join(int.genus.by.period, bp23.genomic.periods, by = "period") %>% 
  right_join(., flower.genus.by.period, by = "period")
compare.gen.by.periods <- compare.gen.by.periods %>% 
  select(c(period, n.genera.x, n.genera.y, n.flower.count.genera)) %>% 
  rename(b_n_genera_metabarcoding = n.genera.y) %>% #the abc is for organizing bar on my plot by alphaetical order
  rename(a_n_genera_interactions = n.genera.x) %>% 
  rename(c_n_genera_flower_count = n.flower.count.genera)
  

long.gen.by.periods <- compare.gen.by.periods %>% 
  pivot_longer(!period) %>% 
  rename(method = name) %>% 
  rename(n.genera = value)

ggplot(long.gen.by.periods, aes(period, n.genera, fill = method)) +
  geom_col(position = "Dodge") +
  scale_x_continuous(breaks = 1:6, labels = 1:6) +
  scale_fill_manual(values = method.colors) +
  theme(axis.ticks.x = element_blank())




#Diversity by site -----

compare.gen.by.sites <- right_join(int.genus.by.site, bp23.genomic.sites, by = "site") %>% 
  right_join(., flower.genus.by.site, by = "site")
compare.gen.by.sites <- compare.gen.by.sites %>% 
  rename(b_n_genera_metabarcoding = n.genera.y) %>% 
  rename(a_n_genera_interactions = n.genera.x) %>% 
  rename(c_n_genera_flower_count = n.flower.count.genera) %>% 
  select(c(site, b_n_genera_metabarcoding, a_n_genera_interactions, c_n_genera_flower_count))

long.gen.by.sites <- compare.gen.by.sites %>% 
  pivot_longer(!site) %>% 
  rename(method = name) %>% 
  rename(n.genera = value)

ggplot(long.gen.by.sites, aes(site, n.genera, fill = method)) +
  geom_col(position = "Dodge") +
  scale_x_continuous(breaks = 1:16, labels = 1:16) +
  scale_fill_manual(values = method.colors) +
  theme(axis.ticks.x = element_blank())

#ok these analyses are interesting for context at least
#could later go deeper and look at diversity by period across sites




#Another basic analysis of diveristy by method (over space and time) ------
#could do linear models relating diversity (n.genera) to method*site*period



#Statistical analysis of methodologies ------

#bring together binary presence absence data from interactions and metabarcoding into one table

bp23.all.binary <- full_join(bp23.int4stats.wide.binary, bp23.genomic.binary4stats) %>% 
  full_join(.,bp23.fc4stats.wide.binary) 

#This part gets messy....

#vegan outputs do not like a few of the "samples" that have no species detections at all
#make a new version of all of the binary data that is "cleaned" of these lines
#but first see what they are/what they mean

#clean out zero sum columns in binary data for statistical analyses
#for.stats.bp23.all.binary <- bp23.all.binary #a copy for cleaning
clean4stats.bp23.all.binary <- bp23.all.binary #another copy for alternative cleaning
#clean4stats.bp23.all.binary <- for.stats.bp23.all.binary[rowSums(for.stats.bp23.all.binary[, 4:ncol(for.stats.bp23.all.binary)], na.rm = TRUE) > 0, ] #keeps only the rows that have greater than 0 sums in binary presence absence


clean4stats.bp23.all.binary <- clean4stats.bp23.all.binary[-c(8,73),] #these samples causes (caused?) nMDS issues as an outlier
clean4stats.bp23.all.binary <- clean4stats.bp23.all.binary %>% 
  select(!c(Iberis,"NA")) #remove Iberis, which was only in problem sample, and NA column
clean4stats.bp23.all.binary <- clean4stats.bp23.all.binary %>% 
  replace(is.na(clean4stats.bp23.all.binary), 0) %>% 
  filter(rowSums(pick(4:221)) != 0)

#simplify factors for nMDS 
site.all <- as.factor(clean4stats.bp23.all.binary$site)
period.all <- as.factor(clean4stats.bp23.all.binary$period)
methodology <- as.factor(clean4stats.bp23.all.binary$method)
#length(factor) #to count/check factor lengths (should ll be the same and same as row # in clean4stats.bp23.all.binary and all.plants)
all.plants <- clean4stats.bp23.all.binary %>% 
  select(!c(site, period, method))

#NMDS visualiztion
#prepare NMDS data

dist.all.plants <- vegdist(all.plants, method = "raup") #calc distance between communities for later stat analysis
all.plant.mds <- metaMDS(all.plants, distance = "raup") 

#Quick plot option
plot(all.plant.mds$points, col = methodology, pch = 16)
#legend("topleft", legend = levels(methodology), col = method.colors, pch = 16, title = "Methodology")






#cool, this works again up to here. Nice plot option not changed yet. First I will change to aggregate Bombus data by day

#Nice plot option - used in EcoFlor poster... when

nmds_points <- as.data.frame(all.flower.mds$points)
colnames(nmds_points) <- c("NMDS1", "NMDS2")
nmds_points$Methodology <- methodology
method.colors2 <- c("count" ="slategrey",
                    "interaction" = "lightblue",
                    "metabarcoding" = "forestgreen")

polygon_data <- nmds_points %>%
  group_by(Methodology) %>%
  slice(chull(NMDS1, NMDS2))

NMDS.title <- expression(paste("NMDS visualization of community composition by methodology"))
ggplot(nmds_points, aes(x = NMDS1, y = NMDS2, color = Methodology)) +
  geom_polygon(data = polygon_data, 
               aes(fill = Methodology, color = NULL), 
               alpha = 0.2, 
               show.legend = FALSE) +
  geom_point(size = 3) + 
  scale_color_manual(values = method.colors2,
                     labels = c(
                      "count" = "Floral diversity survey",
                      "interaction" = "Interaction observations",
                      "metabarcoding" = "Metabarcoding detections")) +
  scale_fill_manual(values = method.colors2) +
  theme_classic() +  
  labs(
    title = NMDS.title,
    x = "NMDS1",
    y = "NMDS2",
    color = "Methodology"
  ) +
  theme(plot.title = element_text(hjust=0.5),
    legend.position = c(0.13, 0.85)
  )


#statistical analysis using PERMANOVA
#silenced to run script faster
#permanova.all.data <- adonis2(all.flowers ~ site*period*methodology, permutations = 9999, method = "bray", by = "terms")

permanova.all.data %>% 
  kbl(caption = "PERMANOVA analysis of spatiotemporal effects on observed plant diversity across three field survey methodologies:
      Bombus gut DNA content, Interaction transects, and Floral diversity") %>% 
  kable_minimal(full_width = F, html_font = "Cambria")








#Figure: metabarcoding results and co-occurence in other methods ------
#figure used in EcoFlor poster

detects.by.genus <- as.data.frame(colSums(bp23.genomic.binary[16:136])) %>% #THR COLUMNS SELECTED HERE ARE IMPORTANT FOR THE RESULTS YOU SEE. Make sure that they include all taxa
  rownames_to_column(var = "genus") %>% 
  rename(n.sample.detections = "colSums(bp23.genomic.binary[16:136])")

detects.comparison <- right_join(detects.by.genus,observed.mb.genus, by = "genus") 
detects.comparison <- detects.comparison[-120,] %>% #last row is an NA
  mutate(detected.fc.int = int.detected + flower.count.detected)
detects.comparison <- detects.comparison[order(detects.comparison$n.sample.detections, decreasing = TRUE) , ] %>% 
  mutate(color_group = case_when(
    detected.fc.int == 2 ~ "Both",
    detected.fc.int == 1 & int.detected == 1 ~ "Int Only",
    detected.fc.int == 1 & flower.count.detected == 1 ~ "Flower Only",
    detected.fc.int == 0 ~ "Neither"
  ))

#select top occurrences
top.detects.comparison <- detects.comparison[1:32,] #two false IDs below are removed, so 1:32 instead of 1:30
top.detects.comparison <- top.detects.comparison %>% 
 filter(genus != "Dioscorea") %>% 
 filter(genus != "Spondias")

#plot
fig.poster.title <- expression(paste("Top plant genera detected in", italic(" B. pascuorum "), "genetic sampling 2023"))
fig.poster <- ggplot(top.detects.comparison, aes(x = reorder(genus, -n.sample.detections)
                                                 , y = n.sample.detections, fill = color_group)) +
  geom_col(alpha = 0.7) +
  scale_fill_manual(values = c("Both" = "skyblue4",
                               "Int Only" = "lightblue",
                               "Flower Only" = "skyblue2",
                               "Neither" = "grey80"),
                    labels = c(
                      "Both" = "Interactions + floral resource survey",
                      "Int Only" = "Interactions only",
                      "Flower Only" = "Floral resource survey only",
                      "Neither" = "Metabarcoding only")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust=0.5),
        legend.position = c(1, 1),
        legend.justification = c(1, 1)) +
  labs(x = "Plant Genus", y = "Positive Detections in Gut Samples", fill = "Detection Method Overlap") +
  ggtitle(fig.poster.title) 

fig.poster



