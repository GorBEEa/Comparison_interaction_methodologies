#Analyzing interaction methodologies and floral diversity data together 


library(here)
library(tidyverse)
library(tidyselect)
library(easystats)
library(visreg)
library(vegan)
library(easystats)
library(mvabund)
library(kableExtra)
library(ggvenn)
library(ComplexUpset)



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

taxa.all.methodologies <- list(
  "Gut-content\nMetabarcoding\n131 genera" = genus.hits.23$genus,
  "Interactions\n27 genera" = gut.detected.int.genus$genus, #this works to give the correct data, but it's sketchy. There is probably a better way
  "Flower Count\n117 genera" = flower.count.genera$flower_genus,
  "Pollen\nMetabarcoding\n122 genera" = poln.genus.hits.2023$genus)


fig.venn <- ggvenn(taxa.all.methodologies,
               show_percentage = FALSE,
               fill_color = c("forestgreen","lightblue","slategrey","goldenrod1"),
               stroke_size = 0.5,
               set_name_size = 5,
               text_size = 8) + 
  coord_cartesian(clip = "off") +
  theme(plot.title = element_text(face="bold", vjust = 2, hjust = 0.01))

ggsave(here("results/venn.figure.png"),fig.venn, width=10, height=9.35, units="in", dpi=300)




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


fc.line <- mean.lines %>% filter(LineType == "Flower Count")

png(here("docs/manuscript_figures/interaction.diversity.periods.png"), width =2000, height = 2000, res = 350) 

fig.methods.w.fc <- ggplot(int.gen.x.periods, aes(period, n.genera, fill = method)) + 
  geom_col(position = "Dodge", alpha = 0.8) + 
  theme_classic() + 
  xlab("Sampling Period") +
  ylab("Number of Genera") +
  scale_x_continuous(breaks = 1:6, labels = 1:6) + 
  scale_fill_manual(values = method.colors, labels = c(
    "n.genera.int" = "Interactions transects",
    "n.genera.pmb" = "Pollen metabarcoding",
    "n.genera.gmb" = "Gut-content metabarcoding")) +
  labs(fill = NULL, color = NULL, linetype = NULL) +
  theme(legend.position = "bottom",
        legend.direction = "vertical", 
        axis.ticks.x = element_blank()) + 
  geom_line(
    data = fc.line,
    aes(x = period, y = mean.genera, linetype = "Flower Count"),
    color = "slategrey",
    linewidth = 1,
    inherit.aes = FALSE) +
  scale_linetype_manual(values = c("Flower Count" = "dotdash")) +
  ggtitle("B.")

fig.methods.w.fc

dev.off()



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


#simplify factors and data for nMDS 
site.all <- as.factor(clean4stats.bp23.all.binary$site)
period.all <- as.factor(clean4stats.bp23.all.binary$period)
methodology <- as.factor(clean4stats.bp23.all.binary$method)
#length(factor) #to count/check factor lengths (should all be the same and same as row # in clean4stats.bp23.all.binary and all.plants)
all.plants <- clean4stats.bp23.all.binary %>% 
  select(!c(site, period, method))


#nMDS visualization of data

#prepare nMDS data with vegan

dist.all.plants <- vegdist(all.plants, method = "raup", binary = TRUE) #calc distance between communities for later stat analysis
set.seed(123) #this should make it so that the nmds results are always the same despite permutations
all.plant.mds <- metaMDS(all.plants, distance = "raup", trace = FALSE) 


#build nMDS visualization

nmds_points <- as.data.frame(all.plant.mds$points)
nmds_points <- nmds_points %>% 
  mutate(methodology = methodology)  %>% 
  slice(-8) #if you want to remove the outlier for whatever reason
method.colors2 <- c("count" ="slategrey",
                    "interaction" = "lightblue",
                    "gut.metabarcoding" = "forestgreen",
                    "pollen.metabarcoding" = "goldenrod1")
method_labels <- c("count" = "Flower count",
                   "interaction" = "Visitation observations",
                   "gut.metabarcoding" = "Gut-content metabarcoding",
                   "pollen.metabarcoding" = "Pollen metabarcoding")


polygon_data <- nmds_points %>%
  group_by(methodology) %>%
  slice(chull(MDS1, MDS2))

#NMDS.title <- expression(paste("Composition of interaction plant community by methodology"))

NMDS.method.comparisons <- ggplot(nmds_points, aes(x = MDS1, y = MDS2,
                        color = methodology,
                        shape = methodology)) +
  geom_polygon(data = polygon_data, 
               aes(fill = methodology, color = NULL), 
               alpha = 0.2, 
               show.legend = FALSE) +
  geom_point(size = 3) + 
  scale_color_manual(values = method.colors2,
                     labels = c(
                       "count" = "Flower count",
                       "interaction" = "Visitation observations",
                       "gut.metabarcoding" = "Gut-content metabarcoding",
                       "pollen.metabarcoding" = "Pollen metabarcoding")
  ) + 
  scale_shape_manual(values = c(
    "count" = 16,               
    "gut.metabarcoding" = 17,   
    "interaction" = 15,         
    "pollen.metabarcoding" = 3),
    labels = method_labels) + 
  scale_fill_manual(values = method.colors2) +
  theme_classic() +  
  labs(x = "NMDS1",
       y = "NMDS2",
       color = NULL, #wrtie "Methodology" if you want the legend to specify
       shape = NULL) + #same as above
  theme(
    legend.position = "bottom",        
    legend.direction = "vertical" 
  )+
  ggtitle("C.")



#statistical analysis using PERMANOVA
#Are the patterns observed withing NMDS real?

permanova.all.data <- adonis2(all.plants ~ methodology, permutations = 9999, method = "raup", pairwise = TRUE)
rownames(permanova.all.data) <- c("Methodology","Residual","Total")

permanova.all.clean <- permanova.all.data %>% 
  mutate(
    `Pr(>F)` = ifelse(`Pr(>F)` < 0.001, "<0.001", signif(`Pr(>F)`, 3))  # adjust column name if needed!
  )

permanova.kbl <- permanova.all.clean %>% 
  kbl(col.names = c("DF", "Sum of sqs", "R\u00B2", "F", "p")) %>% 
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


pairwise.disp.anova <- TukeyHSD(metodology.disp)





# try upset plot instead of ggvenn --------------------------------------------------------
taxa_cols <- clean4stats.bp23.all.binary %>%
  select(-period, -site, -method) %>%
  colnames()


bp23_all_binary_4upset <- clean4stats.bp23.all.binary %>%
  group_by(method) %>%
  summarise(across(all_of(taxa_cols), ~ as.integer(any(. == 1))),
            .groups = "drop")

upset_bp23_all <- bp23_all_binary_4upset %>%
  pivot_longer(
    cols = -method,
    names_to = "genus",
    values_to = "present") %>%
  pivot_wider(
    names_from = method,
    values_from = present,
    values_fill = 0)

upset_bp23_all <- as.data.frame(upset_bp23_all)
rownames(upset_bp23_all) <- upset_bp23_all$genus
upset_bp23_all <- upset_bp23_all[, names(method.colors2)]
upset_bp23_all$genus <- NULL
colnames(upset_bp23_all) <- c(
  "Flower count",
  "Visitation observations",
  "Gut-content metabarcoding",
  "Pollen metabarcoding"
)


upset_fig <- UpSetR::upset(
  upset_bp23_all,
  sets = colnames(upset_bp23_all),
  order.by = "degree",
  keep.order = TRUE,
  empty.intersections = "on",
  
  # colors
  sets.bar.color = unname(method.colors2),
  matrix.color = "black",
  main.bar.color = adjustcolor("grey30", alpha.f = 0.4),
  
  # axis labels
  mainbar.y.label = "Overlapping taxa",
  sets.x.label = "Total taxa",
  
  show.numbers = "yes",   # make sure it is "yes" as string in older versions
  text.scale = c(2, 1.6, 1.6, 1.6, 1.4, 1.4)
)





#Figure: metabarcoding results and co-occurence in other methods -----------------------------
#figure used in EcoFlor poster
#should this use the detections by samples and not by days? I think so... here we want resolution and we don't need it to match for comparison

#maybe I have to do the average detections between MB methods and 
#detects.by.genus <- as.data.frame(colSums(bp23.genomic.binary[16:ncol(bp23.genomic.binary)])) %>% #THR COLUMNS SELECTED HERE ARE IMPORTANT FOR THE RESULTS YOU SEE. Make sure that they include all taxa
 # rownames_to_column(var = "genus") %>% 
  #rename(n.sample.detections = "colSums(bp23.genomic.binary[16:ncol(bp23.genomic.binary)])")

#detects.comparison <- right_join(detects.by.genus,observed.mb.genus, by = "genus") %>% 
 # mutate(detected.fc.int = int.detected + flower.count.detected)
#detects.comparison <- detects.comparison[order(detects.comparison$n.sample.detections, decreasing = TRUE) , ] %>% 
 # mutate(color_group = case_when(
  #  detected.fc.int == 2 ~ "Both",
   # detected.fc.int == 1 & int.detected == 1 ~ "Int Only",
    #detected.fc.int == 1 & flower.count.detected == 1 ~ "Flower Only",
    #detected.fc.int == 0 ~ "Neither",
    #detected.fc.int == 0 & pmb.detected == 1 ~ "poln Only",
  #))
#this system depends on the fact that any interaction observed will have included a plant species already documented in the flower count. Which should always be the case.


#select top occurrences
#top.detects.comparison <- detects.comparison[1:30,] #30 is a detailed but not overwhelming number for visualization
#contaminant/misidentified species should have been removed in metabarcoding_data after importing said data

#plot
#fig.poster.title <- expression(paste("Top plant genera detected in", italic(" B. pascuorum "), "genetic sampling 2023"))
#fig.poster <- ggplot(top.detects.comparison, aes(x = reorder(genus, -n.sample.detections)
 #                                                , y = n.sample.detections, fill = color_group)) +
 # geom_col(alpha = 0.7) +
  #scale_fill_manual(values = c("Both" = "skyblue4",
   #                            "Int Only" = "lightblue",
    #                           "Flower Only" = "skyblue2",
     #                          "Neither" = "grey80",
      #                         "poln only" = "forestgreen"),
       #             labels = c(
        #              "Both" = "Interactions + floral resource survey",
         #             "Int Only" = "Interactions only",
          #            "Flower Only" = "Floral resource survey only",
           #           "Neither" = "Gut Metabarcoding only",
            #          "poln only" = "Pollen metabarcoding only")) +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1),
 #       plot.title = element_text(hjust=0.5),
  #      legend.position.inside = c(1, 1),
   #     legend.justification = c(1, 1)) +
#  labs(x = "Plant Genus", y = "Positive Detections in Gut Samples", fill = "Detection Method Overlap") +
#  ggtitle(fig.poster.title) 

# fig.poster




# ------------

save.image(file = here("Data/05_output.RData"))

