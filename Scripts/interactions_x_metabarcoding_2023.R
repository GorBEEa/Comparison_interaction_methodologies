#Analyzing interaction and metabarcoding data together 
#Interaction_Data.R and metabarcoding_data.R should already be sourced

library(vegan)
library(mvabund)
library(easystats)
library(kableExtra)



detected.int.genus <- df.int.genus %>% distinct(genus) #Clean list of genera from BP interactions & interaction count
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




#Inverse analysis of above - which genera observed in metabarcoding were observed in interactions -----
observed.mb.genus <- genus.hits.23
observed.mb.genus$int.detected <- as.integer(observed.mb.genus$genus %in% df.int.genus$genus)
observed.mb.genus %>% filter(int.detected == 1)
#All 27 genera observed by interactions were detected by metabarcoding



#and which detected by metabarcoding were observed in flower counts
observed.mb.genus$flower.count.detected <- as.integer(observed.mb.genus$genus %in% flower.count.genera$flower_genus)
observed.mb.genus %>% filter(flower.count.detected == 1)
#Of the 160 genera detected by metabarcoding, only 63 were even observed in the transects


cp.flower.count.genera <- flower.count.genera
cp.flower.count.genera$in.mb <- as.integer(flower.count.genera$flower_genus %in% genus.hits.23$genus)
in.fc.not.mb <- cp.flower.count.genera %>% filter(in.mb == 0)

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


#Another basic analysis of diveristy by method (over space and time) ------
#linear models relating diversity (n.genera) to method*site*period





#ok these analyses are interesting for context at least
#could later go deeper and look at diversity by period across sites



#PERMANOVA time --------
#first an NMDS visual

#simplify factors/data involved
#can do for data with read counts (bp23.genomic.analys) or presence absence (bp23.genomic.binary)
#just change these three lines

site <- as.factor(bp23.genomic.binary$site)
period <- as.factor(bp23.genomic.binary$period)
gut.flowers <- bp23.genomic.binary %>% select(Abelmoschus:last_col())


#NMDS visualization
dist.gut.flowers <- vegdist(gut.flowers, method = "jaccard") #calc distance between communities for later stat analysis
gut.flower.mds <- metaMDS(gut.flowers, distance = "jaccard")
#plot(gut.flower.mds$points, col = site, pch = 16)
plot(gut.flower.mds$points, col = period, pch = 16)


#permanova test (play with ~ variables to understand more)
permanova.gut.flowers <- adonis2(gut.flowers ~ period*site, permutations = 9999, method = "jaccard", by = "terms")
summary(permanova.gut.flowers)
permanova.gut.flowers


# Alternative analysis: many glm -----
#extracting effect of site or period for each species using multiple glm
#this might not be possible with our data and my computer
gut.flowers.spp <- mvabund(
  bp23.genomic.analys %>% 
    select(Abelmoschus:last_col())
  )
#mglm.gut.flowers <- manyglm(gut.flowers.spp ~ bp23.genomic.analys$period, family = "")
#anova(mglm.gut.flowers, p.uni="adjusted") #this takes a lot of computing power
#should show deviation and probable significance of effect for each species



#statistical analysis of methodologies ------

#bring together binary presence absence data from interactions and metabarcoding into one table

bp23.all.binary <- full_join(bp23.int4stats.wide.binary, bp23.genomic.binary4stats) %>% 
  full_join(.,bp23.fc4stats.wide.binary) 
#bp23.all.binary <- replace(is.na(bp23.all.binary), 0) #this isn't working well. You have to create bp23.all.binary and then recreate with this part

#simplify factors/data involved
#can do for data with read counts (bp23.genomic.analys) or presence absence (bp23.genomic.binary)
#just change these three lines

site <- as.factor(bp23.all.binary$site)
period <- as.factor(bp23.all.binary$period)
methodology <- as.factor(bp23.all.binary$method)
all.flowers <- bp23.all.binary %>% 
  select(!c(site, period, method))  
all.flowers <- all.flowers %>% replace(is.na(all.flowers), 0)

#NMDS visualiztion
dist.all.flowers <- vegdist(all.flowers, method = "bray") #calc distance between communities for later stat analysis
all.flower.mds <- metaMDS(all.flowers, distance = "bray")
#plot(gut.flower.mds$points, col = site, pch = 16)
plot(all.flower.mds$points, col = method.colors[as.numeric(methodology)], pch = 16)
legend("topleft", legend = levels(methodology), col = method.colors, pch = 16, title = "Methodology")

#in ggplot2
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


#statistical analysis
permanova.all.data <- adonis2(all.flowers ~ site*period*methodology, permutations = 9999, method = "bray", by = "terms")

permanova.all.data %>% kbl(caption =
                             "this is my caption!" 
                           ) %>% 
  kable_minimal(full_width = F, html_font = "Cambria")








#Poster figure ------
detects.by.genus <- as.data.frame(colSums(bp23.genomic.binary[12:171])) %>% 
  rownames_to_column(var = "genus")

detects.comparison <- right_join(detects.by.genus,observed.mb.genus, by = "genus") 
detects.comparison <- detects.comparison[-161,] %>% 
  rename(n.sample.detections = "colSums(bp23.genomic.binary[12:171])") %>% 
  mutate(detected.fc.int = int.detected + flower.count.detected)
detects.comparison <- detects.comparison[order(detects.comparison$n.sample.detections, decreasing = TRUE) , ] %>% 
  mutate(color_group = case_when(
    detected.fc.int == 2 ~ "Both",
    detected.fc.int == 1 & int.detected == 1 ~ "Int Only",
    detected.fc.int == 1 & flower.count.detected == 1 ~ "Flower Only",
    detected.fc.int == 0 ~ "Neither"
  ))

#select top occurrences
top.detects.comparison <- detects.comparison[1:31 ,]
top.detects.comparison <- top.detects.comparison %>% filter(genus != "Symphytum")

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
                      "Both" = "Interactions + diversity survey",
                      "Int Only" = "Interactions only",
                      "Flower Only" = "Diversity survey only",
                      "Neither" = "Metabarcoding only")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust=0.5),
        legend.position = c(1, 1),
        legend.justification = c(1, 1)) +
  labs(x = "Plant Genus", y = "Positive Detections in Gut Samples", fill = "Detection Type") +
  ggtitle(fig.poster.title) 

fig.poster
