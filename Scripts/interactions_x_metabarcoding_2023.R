#Analyzing interaction and metabarcoding data together 
#Interaction_Data.R and metabarcoding_data.R should already be sourced

library(vegan)
library(mvabund)


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





#Diversity by periods ----- 

compare.gen.by.periods <- right_join(int.genus.by.period, bp23.genomic.periods, by = "period") %>% 
  right_join(., flower.genus.by.period, by = "period")
compare.gen.by.periods <- compare.gen.by.periods %>% 
  select(c(period, n.genera.x, n.genera.y, n.flower.count.genera)) %>% 
  rename(n_genera_metabarcoding = n.genera.y) %>% 
  rename(n_genera_interactions = n.genera.x) %>% 
  rename(n_genera_flower_count = n.flower.count.genera)
  

long.gen.by.periods <- compare.gen.by.periods %>% 
  pivot_longer(!period) %>% 
  rename(method = name) %>% 
  rename(n.genera = value)

ggplot(long.gen.by.periods, aes(period, n.genera, fill = method)) +
  geom_col(position = "Dodge") 




#Diversity by site -----

compare.gen.by.sites <- right_join(int.genus.by.site, bp23.genomic.sites, by = "site") %>% 
  right_join(., flower.genus.by.site, by = "site")
compare.gen.by.sites <- compare.gen.by.sites %>% 
  rename(mb_detections = n.genera.y) %>% 
  rename(int_detections = n.genera.x) %>% 
  select(c(site, int_detections, mb_detections, n.flower.count.genera))

long.gen.by.sites <- compare.gen.by.sites %>% 
  pivot_longer(!site) %>% 
  rename(method = name) %>% 
  rename(n.genera = value)

ggplot(long.gen.by.sites, aes(site, n.genera, fill = method)) +
  geom_col(position = "Dodge")

#ok these analyses are interesting for context at least



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
permanova.gut.flowers <- adonis2(gut.flowers ~ period*site, permutations = 9999, method = "jaccard")
summary(permanova.gut.flowers)
permanova.gut.flowers



# Alternative analysis: many glm -----
#extracting effect of site or period for each species using multiple glm
#this might not be possible with our data and my computer
gut.flowers.spp <- mvabund(
  bp23.genomic.analys %>% 
    select(Abelmoschus:last_col())
  )
mglm.gut.flowers <- manyglm(gut.flowers.spp ~ bp23.genomic.analys$period, family = "")
#anova(mglm.gut.flowers, p.uni="adjusted") #this takes a lot of computing power
#should show deviation and probable significance of effect for each species

