#Analyzing interaction methodologies and floral diversity data together 

library(readr)
library(here)
library(tidyverse)
library(tidyr)
library(tidyselect)
library(easystats)
library(visreg)
library(vegan)
library(easystats)
library(mvabund)
library(kableExtra)
library(ggvenn)



#Load data from methodologies (Scripts 01-04)
load(file = here("Data/methodologies_data.RData"))
#or run 00_quickstart script or all scripts 01-04, 1 by 1





#Overlap in methodology communities ------------------------------------------------------------

#Which interaction transect species were detected by gut metabarcoding 
gut.detected.int.genus <- df.int.genus %>% distinct(genus) #Clean list of genera (27) from BP interactions
gut.detected.int.genus$mb.detected <- as.integer(gut.detected.int.genus$genus %in% genus.hits.23$genus) #presence absence comparison
int.genus.occur.gut.mb.detect <- full_join(gut.detected.int.genus, df.int.genus %>% count(genus)) #table with total interaction counts for 2023 by genus (n) and their binary value for detection y/n with mb
#result: all 27 interaction taxa were observed by MB

#Which interaction transect species were detected by pollen metabarcoding 
poln.detected.int.genus <- df.int.genus %>% distinct(genus) #Clean list of genera (27) from BP interactions
poln.detected.int.genus$mb.detected <- as.integer(poln.detected.int.genus$genus %in% poln.genus.hits.2023$genus) #presence absence comparison
poln.genus.occur.gut.mb.detect <- full_join(poln.detected.int.genus, df.int.genus %>% count(genus)) #table with total interaction counts for 2023 by genus (n) and their binary value for detection y/n with mb
#result: 24 of 27 interaction taxa were observed by MB, missing taxa: Anemone, Jasione, Arabis


#Inverse analysis of above - which genera observed in gut metabarcoding were observed in interactions -----
observed.mb.genus <- genus.hits.23
observed.mb.genus$int.detected <- as.integer(observed.mb.genus$genus %in% df.int.genus$genus)
observed.mb.genus %>% filter(int.detected == 1)
#result: Interactions did not detect any species beyond those detected by gut metabarcoding

#Which genera observed in pollen metabarcoding were observed in interactions -----
observed.pmb.genus <- poln.genus.hits.2023
observed.pmb.genus$int.detected <- as.integer(observed.pmb.genus$genus %in% df.int.genus$genus)
observed.pmb.genus %>% filter(int.detected == 1)
#result: Interactions did not detect any species beyond those detected by pollen metabarcoding


#which detected by gut metabarcoding were observed in flower counts?
observed.mb.genus$flower.count.detected <- as.integer(observed.mb.genus$genus %in% flower.count.genera$flower_genus)
paste("Of the", nrow(genus.hits.23),"taxa detected by gut metabarcoding,", 
      nrow(observed.mb.genus %>% filter(flower.count.detected == 1)),
      "were observed in the flower counts")

#which detected by pollen metabarcoding were observed in flower counts?
observed.pmb.genus$flower.count.detected <- as.integer(observed.pmb.genus$genus %in% flower.count.genera$flower_genus)
paste("Of the", nrow(poln.genus.hits.2023),"taxa detected by pollen metabarcoding,", 
      nrow(observed.mb.genus %>% filter(flower.count.detected == 1)),
      "were observed in the flower counts")

#inverse of above - how many flower count genera were not in gut metabarcoding results?
cp.flower.count.genera <- flower.count.genera
cp.flower.count.genera$in.mb <- as.integer(flower.count.genera$flower_genus %in% genus.hits.23$genus)
in.fc.not.gmb <- cp.flower.count.genera %>% filter(in.mb == 0)
paste("Of the", nrow(flower.count.genera),"taxa detected in flower counts,", nrow(in.fc.not.gmb),
      "were unique to this survey when compared with gut metabarcoding")

#How many flower count genera were not in pollen metabarcoding results?
cp.flower.count.genera$in.pmb <- as.integer(flower.count.genera$flower_genus %in% poln.genus.hits.2023$genus)
in.fc.not.pmb <- cp.flower.count.genera %>% filter(in.pmb == 0)
paste("Of the", nrow(flower.count.genera),"taxa detected in flower counts,", nrow(in.fc.not.pmb),
      "were unique to this survey when compared with pollen metabarcoding")

#How many flower count genera were not in any metabarcoding results?
in.fc.not.mb <- cp.flower.count.genera %>% filter(in.mb == 0 & in.pmb == 0)
paste("Of the", nrow(flower.count.genera),"taxa detected in flower counts,", nrow(in.fc.not.mb),
      "were unique to this survey when compared with all metabarcoding")


#Which taxa observed in gut metabarcoding were undetected by pollen metabarcoding?
observed.mb.genus$pmb.detected <- as.integer(observed.mb.genus$genus %in% poln.genus.hits.2023$genus)
in.gmb.not.pmb <-  observed.mb.genus %>% filter(pmb.detected == 0)
paste("Of the", nrow(genus.hits.23),"taxa detected in gut metabarcoding,", nrow(in.gmb.not.pmb),
      "were uniquely detected when compared with pollen metabarcoding")

#Inverse of above - Which taxa observed in pollen metabarcoding were undetected by gut metabarcoding?
observed.pmb.genus$gmb.detected <- as.integer(poln.genus.hits.2023$genus %in% observed.mb.genus$genus)
in.pmb.not.gmb <-  observed.pmb.genus %>% filter(gmb.detected == 0)
paste("Of the", nrow(poln.genus.hits.2023),"taxa detected in pollen metabarcoding,", nrow(in.pmb.not.gmb),
      "were uniquely detected when compared with gut metabarcoding")





#Venn diagram visualization of detection overlap ------------------------------------------------------


#just add another part to the list once we have pollen
taxa.all.methodologies <- list(
  "Gut content\nMetabarcoding\nN = 131" = genus.hits.23$genus,
  "Interactions\nN = 27" = gut.detected.int.genus$genus, #this works to give the correct N, but it's sketchy. There is probably a better way
  "Flower Count\nN = 117" = flower.count.genera$flower_genus,
  "Pollen\nMetabarcoding\nN = 123" = poln.genus.hits.2023$genus)

venn.cap <- expression(paste(bold("Figure 1: "),"Number of plant genera detected within interation networks 
                             constructed for", italic(" B. pascuorum "), "using three interaction observation methodologies:
                             interaction field transects, corbicular pollen metabarcoding, and gut content metabarcoding
                             Flower count floral diversity survey results are represented within the analysis to provide 
                             environmental context. All results are aggregated from samples and surveys taken from April to 
                             August of 2023. Degree of ", italic(" B. pascuorum "), " as a network node is represented by the
                             total number of genera detected by each methodology (N = ). Overlap with other methodologies, and
                             uniquely detected genera are shown within the venn diagram."))

fig.venn <- ggvenn(taxa.all.methodologies,
               show_percentage = FALSE,
               fill_color = c("forestgreen","lightblue","slategrey","goldenrod1"),
               stroke_size = 0.5,
               set_name_size = 5,
               text_size = 8) + 
  coord_cartesian(clip = "off") +
  ggtitle("Interaction network degree and
    overlap by methodology") +
 # labs(caption = venn.cap) +
  theme(plot.title = element_text(face="bold", vjust = 2, hjust = 0.01))

ggsave(here("results/venn.figure.jpeg"),fig.venn, width=10, height=9, units="in", dpi=300)





#Diversity by periods --------------------------------------------------------------------------------

method.colors <- c("n.genera.int" = "lightblue",
                   "n.genera.fc" ="slategrey",
                   "n.genera.pmb" = "goldenrod1",
                   "n.genera.gmb" = "forestgreen") #set some universal colors for this project

compare.gen.by.periods <- right_join(
  int.genus.by.period, flower.genus.by.period, by = "period") %>% 
  right_join(., bp23.genomic.periods, by = "period") %>% 
  right_join(., poln.2023.genomic.periods, by = "period")
  
compare.gen.by.periods <- compare.gen.by.periods %>% 
  select(c(period, n.genera.int, n.genera.fc, n.genera.pmb, n.genera.gmb))

long.gen.by.periods <- compare.gen.by.periods %>%
  pivot_longer(!period) %>% rename(method = name) %>% 
  rename(n.genera = value)

long.gen.by.periods$method <- factor(
  long.gen.by.periods$method, 
  levels = c("n.genera.int", "n.genera.fc", "n.genera.pmb", "n.genera.gmb")
)

#Means across methodologies by period

#Some data prep for making an informative bar chart
mean.taxa.periods <- long.gen.by.periods %>% 
  group_by(period) %>% 
  summarise(mean.genera = mean(n.genera))

mean.gut.taxa.periods <- long.gen.by.periods %>% 
  filter(method == "n.genera.gmb") %>% 
  group_by(period) %>% 
  summarise(mean.genera = mean(n.genera))

mean.fc.taxa.periods <- long.gen.by.periods %>% 
  filter(method == "n.genera.fc") %>% 
  group_by(period) %>% 
  summarise(mean.genera = mean(n.genera))

mean.gut.taxa.periods$LineType <- "Gut Content Metabarcoding"
mean.fc.taxa.periods$LineType <- "Flower Count"
mean.lines <- rbind(mean.gut.taxa.periods, mean.fc.taxa.periods)


fig.methods.x.periods <- ggplot(long.gen.by.periods, aes(period, n.genera, fill = method)) + 
  geom_col(position = "Dodge", alpha = 0.8) + 
  theme_minimal() + 
  xlab("Sampling Period") +
  ylab("Detected Plant Genera") +
  scale_x_continuous(breaks = 1:6, labels = 1:6) + 
  scale_fill_manual(values = method.colors, labels = c(
    "n.genera.int" = "Interactions Transects",
    "n.genera.fc" = "Flower Count",
    "n.genera.pmb" = "Pollen Metabarcoding",
    "n.genera.gmb" = "Gut Content Metabarcoding")) +
  labs(fill = "Methodology", color = NULL , linetype = NULL) +
  theme(plot.title = element_text(hjust = 0.7),
    axis.ticks.x = element_blank()) +
  ggtitle("Detected plant genera by methodology across 2023 field sampling periods") +
  geom_line(
    data = mean.lines,
    aes(x = period, y = mean.genera, color = LineType, linetype = LineType),
    linewidth = 1,
    inherit.aes = FALSE) +
 scale_color_manual(values = c("Gut Content Metabarcoding" = "forestgreen", "Flower Count" = "slategrey")) +
  scale_linetype_manual(values = c("Gut Content Metabarcoding" = "dashed", "Flower Count" = "dotdash")) +
  guides(
    fill = guide_legend(order = 1),
    color = guide_legend(order = 2),
    linetype = guide_legend(order = 2)
  )
  

fig.methods.x.periods


#Diversity by site ------------------------------------------------------------------------------

#compare.gen.by.sites <- right_join(int.genus.by.site, bp23.genomic.sites, by = "site") %>%right_join(., flower.genus.by.site, by = "site") %>% right_join(., poln23.genomic.sites, by = "site") 
#compare.gen.by.sites <- compare.gen.by.sites %>% rename(b_n_genera_gut_metabarcoding = n.genera.y) %>% rename(a_n_genera_interactions = n.genera.x) %>% rename(c_n_genera_flower_count = n.flower.count.genera) %>% rename(d_n_genera_pollen_metabarcoding = n.genera.poln) %>% select(c(site, b_n_genera_gut_metabarcoding, a_n_genera_interactions, c_n_genera_flower_count, d_n_genera_pollen_metabarcoding))
#long.gen.by.sites <- compare.gen.by.sites %>% pivot_longer(!site) %>% rename(method = name) %>% rename(n.genera = value)
#ggplot(long.gen.by.sites, aes(site, n.genera, fill = method)) + geom_col(position = "Dodge") + scale_x_continuous(breaks = 1:16, labels = 1:16) + scale_fill_manual(values = method.colors) + theme(axis.ticks.x = element_blank())
#looks like sites missing pollen samples hav just been omitted from the analysis in general, coiuld fix but not a priority





#Another basic analysis of diveristy by method (over space and time) ------
#could do linear models relating diversity (n.genera) to method*site*period



#Statistical analysis of methodologies --------------------------------------------------------------

#bring together binary presence absence data from interactions and metabarcoding into one table

bp23.all.binary <- full_join(bp23.int4stats.wide.binary, bp23.genomic.binary4stats.xday) %>% 
  full_join(.,bp23.fc4stats.wide.binary) %>% 
  full_join(.,poln.genomic.binary.2023.xday.4stats)
bp23.all.binary[is.na(bp23.all.binary)] <- 0 #Just do this now, later it's a disaster


#Here, if interested in only analyzing data from the days with the full set of methodologies you can jump to the script here("Scripts/trials/05.3_community_comparison_full_2023.R")



#vegan outputs sometimes do not like a few of the "samples" that have no species detections at all
#make a new version of all of the binary data that is "cleaned" of these lines
#but first see what they are/what they mean

#remove NAs and clean out zero sum rows and columns in binary data for statistical analyses. The next two commands do the same and are redundant, but why not do both
clean4stats.bp23.all.binary <- bp23.all.binary[rowSums(bp23.all.binary[, 4:ncol(bp23.all.binary)], na.rm = TRUE) > 0, ] #keeps only the rows that have greater than 0 sums in binary presence absence
clean4stats.bp23.all.binary <- clean4stats.bp23.all.binary %>% 
  select(1:3, # keep metadata columns unchanged
         where(~ is.numeric(.) && sum(., na.rm = TRUE) > 0)) #remove 0 sum columns
#removes Avenella and Mentha



#simplify factors and data for nMDS 
site.all <- as.factor(clean4stats.bp23.all.binary$site)
period.all <- as.factor(clean4stats.bp23.all.binary$period)
methodology <- as.factor(clean4stats.bp23.all.binary$method)
#length(factor) #to count/check factor lengths (should all be the same and same as row # in clean4stats.bp23.all.binary and all.plants)
all.plants <- clean4stats.bp23.all.binary %>% 
  select(!c(site, period, method))


#NMDS visualization of data

#prepare NMDS data with vegan

dist.all.plants <- vegdist(all.plants, method = "raup", binary = TRUE) #calc distance between communities for later stat analysis
set.seed(123) #this should make it so that the nmds results are always the same despite permutations
all.plant.mds <- metaMDS(all.plants, distance = "raup", trace = FALSE) 


#Quick plot option - the colors are probably deceiving right now
#plot(all.plant.mds$points, col = method.colors, pch = 16)
#legend("topleft", legend = levels(methodology), col = method.colors, pch = 16, title = "Methodology")
#that outlier point is from interactions, P2S14


#Nice plot option - as used in EcoFlor poster
nmds_points <- as.data.frame(all.plant.mds$points)
nmds_points <- nmds_points %>% 
  mutate(methodology = methodology) # %>% 
  #slice(-8) #if you want to remove the outlier for whatever reason
method.colors2 <- c("count" ="slategrey",
                    "interaction" = "lightblue",
                    "gut.metabarcoding" = "forestgreen",
                    "pollen.metabarcoding" = "goldenrod1")

polygon_data <- nmds_points %>%
  group_by(methodology) %>%
  slice(chull(MDS1, MDS2))

NMDS.title <- expression(paste("Composition of interaction plant community by methodology"))
NMDS.cap <- expression(paste(
  bold("Figure3: "), "Non-metric dimensional scaling of interaction plant communities for", italic(" Bombus pascuorum "),"as detected by three interaction observation methodologies and a floral diversity survey. ",
  "Interaction methodologies included floral diversity surveys and ITS2 metabarcoding of DNA extracted from bumblebee gut contents and corbicular pollen loads. ",
  "Observation data are aggregated by sampling day, and the binary presence/absence list for the plant genera observed by each methodology on each sampling day are compared in ordination space. ",
  "PERMANOVA comparisons of the community compositions represented by NMDS show no significant differences between interaction observations, gut content metabarcoding, and corbicular pollen metabarcoding. ",
  "Floral diversity survey results were significantly different (P < 0.001) from each of the interaction methodologies."
))
nmds.cap.wrap <- str_wrap(NMDS.cap, width = 150)

NMDS.method.comparisons <- ggplot(nmds_points, aes(x = MDS1, y = MDS2, color = methodology, shape = methodology)) +
  geom_polygon(data = polygon_data, 
               aes(fill = methodology, color = NULL), 
               alpha = 0.2, 
               show.legend = FALSE) +
  geom_point(size = 3) + 
  scale_color_manual(values = method.colors2,
                     labels = c(
                      "count" = "Floral diversity survey",
                      "interaction" = "Interaction observations",
                      "gut.metabarcoding" = "Gut metabarcoding",
                      "pollen.metabarcoding" = "Pollen metabarcoding")
                     ) + 
  scale_shape_manual(values = c(
    "count" = 16,               
    "gut.metabarcoding" = 17,   
    "interaction" = 15,         
    "pollen.metabarcoding" = 3),
    labels = c(
      "count" = "Floral diversity survey",
      "interaction" = "Interaction observations",
      "gut.metabarcoding" = "Gut metabarcoding",
      "pollen.metabarcoding" = "Pollen metabarcoding")) + 
  scale_fill_manual(values = method.colors2) +
  theme_classic() +  
  labs(
    title = NMDS.title,
    x = "NMDS1",
    y = "NMDS2",
    color = "Methodology",
    shape = "Methodology") +
  theme(plot.title = element_text(hjust=0.5),
        legend.position.inside = c(0.13, 0.85))

NMDS.method.comparisons

#statistical analysis using PERMANOVA
#Are the patterns observed withing NMDS real?

permanova.all.data <- adonis2(all.plants ~ methodology, permutations = 9999, method = "raup", pairwise = TRUE)

permanova.kbl <- permanova.all.data %>% 
  kbl(caption = "PERMANOVA analysis of methodology's effect on observed plant community") %>% 
  kable_minimal(full_width = F, html_font = "Cambria")

#Output probability that F statistic is significant, meaning that the model explains R2 *100% (Model/TotaL FOR sUMoFsQS) of the observed variation (SumOfSqs) between groups 

#second check: are the groups really different in terms of how they are independently dispersed?
metodology.disp <- betadisper(dist.all.plants, clean4stats.bp23.all.binary$method)
#metodology.disp #observe average distances to mean between groups, do they look different?
#interaction definitely looks more dispersed thatn the others, then gut mb
disp.anova <- anova(metodology.disp) #are the differences in dispersal significantly different?
#disp.anova %>% kbl(caption = "ANOVA analysis of methodology's effect on dispersal of plant-pollinator 
#                   interaction network composition data in ordination") %>% 
#  kable_minimal(full_width = F, html_font = "Cambria")

#looks like yes









#Figure: metabarcoding results and co-occurence in other methods ------
#figure used in EcoFlor poster
#should this use the detections by samples and not by days? I think so... here we want resolution and we don't need it to match for comparison

#maybe I have to do the average detections between MB methods and 
detects.by.genus <- as.data.frame(colSums(bp23.genomic.binary[16:ncol(bp23.genomic.binary)])) %>% #THR COLUMNS SELECTED HERE ARE IMPORTANT FOR THE RESULTS YOU SEE. Make sure that they include all taxa
  rownames_to_column(var = "genus") %>% 
  rename(n.sample.detections = "colSums(bp23.genomic.binary[16:ncol(bp23.genomic.binary)])")

detects.comparison <- right_join(detects.by.genus,observed.mb.genus, by = "genus") %>% 
  mutate(detected.fc.int = int.detected + flower.count.detected)
detects.comparison <- detects.comparison[order(detects.comparison$n.sample.detections, decreasing = TRUE) , ] %>% 
  mutate(color_group = case_when(
    detected.fc.int == 2 ~ "Both",
    detected.fc.int == 1 & int.detected == 1 ~ "Int Only",
    detected.fc.int == 1 & flower.count.detected == 1 ~ "Flower Only",
    detected.fc.int == 0 ~ "Neither",
    detected.fc.int == 0 & pmb.detected == 1 ~ "poln Only",
  ))
#this system depends on the fact that any interaction observed will have included a plant species already documented in the flower count. Which should always be the case.


#select top occurrences
top.detects.comparison <- detects.comparison[1:30,] #30 is a detailed but not overwhelming number for visualization
#contaminant/misidentified species should have been removed in metabarcoding_data after importing said data

#plot
fig.poster.title <- expression(paste("Top plant genera detected in", italic(" B. pascuorum "), "genetic sampling 2023"))
fig.poster <- ggplot(top.detects.comparison, aes(x = reorder(genus, -n.sample.detections)
                                                 , y = n.sample.detections, fill = color_group)) +
  geom_col(alpha = 0.7) +
  scale_fill_manual(values = c("Both" = "skyblue4",
                               "Int Only" = "lightblue",
                               "Flower Only" = "skyblue2",
                               "Neither" = "grey80",
                               "poln only" = "forestgreen"),
                    labels = c(
                      "Both" = "Interactions + floral resource survey",
                      "Int Only" = "Interactions only",
                      "Flower Only" = "Floral resource survey only",
                      "Neither" = "Gut Metabarcoding only",
                      "poln only" = "Pollen metabarcoding only")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust=0.5),
        legend.position.inside = c(1, 1),
        legend.justification = c(1, 1)) +
  labs(x = "Plant Genus", y = "Positive Detections in Gut Samples", fill = "Detection Method Overlap") +
  ggtitle(fig.poster.title) 

fig.poster




# ------------

save.image(file = here("Data/05_output.RData"))

