
#bring together binary presence absence data from interactions and metabarcoding into one table
int3.binary <- full_join(bp23.int4stats.wide.binary, bp23.genomic.binary4stats.xday) %>% 
  full_join(.,poln.genomic.binary.2023.xday.4stats)
int3.binary[is.na(int3.binary)] <- 0 

clean4stats.int3.binary <- int3.binary[rowSums(int3.binary[, 4:ncol(int3.binary)], na.rm = TRUE) > 0, ] #keeps only the rows that have greater than 0 sums in binary presence absence
clean4stats.int3.binary <- clean4stats.int3.binary %>% 
  select(1:3, # keep metadata columns unchanged
         where(~ is.numeric(.) && sum(., na.rm = TRUE) > 0)) #remove 0 sum columns


#simplify factors and data for nMDS 
site.int3 <- as.factor(clean4stats.int3.binary$site)
period.int3 <- as.factor(clean4stats.int3.binary$period)
methodology.int3 <- as.factor(clean4stats.int3.binary$method)
#length(factor) #to count/check factor lengths (should all be the same and same as row # in int3.binary and int3.plants)
int3.plants <- clean4stats.int3.binary %>% 
  select(!c(site, period, method))



#NMDS visualization of data ----

#prepare NMDS data with vegan

dist.int3.plants <- vegdist(int3.plants, method = "raup", binary = TRUE) #calc distance between communities for later stat analysis
set.seed(123) #this should make it so that the nmds results are always the same despite permutations
int3.plant.mds <- metaMDS(int3.plants, distance = "raup") 

#make figure
int3_nmds_points <- as.data.frame(int3.plant.mds$points)
int3_nmds_points <- int3_nmds_points %>% 
  mutate(methodology.int3 = methodology.int3) # %>% 
#slice(-8) #if you want to remove the outlier for whatever reason
method.colors3 <- c("interaction" = "lightblue",
                    "gut.metabarcoding" = "forestgreen",
                    "pollen.metabarcoding" = "goldenrod1")


int3_polygon_data <- int3_nmds_points %>%
  group_by(methodology.int3) %>%
  slice(chull(MDS1, MDS2))

int3.NMDS.title <- expression(paste("NMDS visualization of network composition by interaction methodology.int3"))

NMDS.int3 <- ggplot(int3_nmds_points, aes(x = MDS1, y = MDS2, color = methodology.int3)) +
  geom_polygon(data = int3_polygon_data, 
               aes(fill = methodology.int3, color = NULL), 
               alpha = 0.2, 
               show.legend = FALSE) +
  geom_point(size = 3) + 
  scale_color_manual(values = method.colors3,
                     labels = c(
                       "interaction" = "Interaction observations",
                       "gut.metabarcoding" = "Gut metabarcoding",
                       "pollen.metabarcoding" = "Pollen metabarcoding")
  ) +
  scale_fill_manual(values = method.colors3) +
  theme_classic() +  
  labs(
    title = int3.NMDS.title,
    x = "NMDS1",
    y = "NMDS2",
    color = "methodology"
  ) +
  theme(plot.title = element_text(hjust=0.5),
        legend.position.inside = c(0.13, 0.85)
  )

NMDS.int3

#statistical analysis using PERMANOVA
#Are the patterns observed withing NMDS real?

permanova.int3 <- adonis2(int3.plants ~ methodology.int3, permutations = 9999, method = "raup", pairwise = TRUE)

permanova.kbl <- permanova.int3 %>% 
  kbl(caption = "PERMANOVA analysis of interaction methodology's effect on observed plant community") %>% 
  kable_minimal(full_width = F, html_font = "Cambria")

#second check: are the groups really different in terms of how they are independently dispersed?
int3.metodology.disp <- betadisper(dist.int3.plants, clean4stats.int3.binary$method)
disp.int3.anova <- anova(int3.metodology.disp)







save.image(file = here("Data/05.4_output.RData"))


