#this script is the saved code for a parallel analysis tothe principal methodologies comaprison
#here I isolated the sampling days where all methodologies had data to create a perfectly balanced comparison
#did not continue with this analysis because thre are not enough 2023 data

#this script can be run after the base 05 script
#look for where this analysis "starts" in parallel to the main analysis in the base 05 script by searching:
#"Here you can begin with the script here("Scripts/trials/05.3_community_comparison_full_2023.R")"


#Join data only by shared sampling days
n_methods <- n_distinct(bp23.all.binary$method)
complete_pairs <- bp23.all.binary %>%
  distinct(period, site, method) %>%
  group_by(period, site) %>%
  tally() %>%
  filter(n == n_methods) %>%
  select(period, site)

bp23.full.days.binary <- bp23.all.binary %>% 
  inner_join(complete_pairs, by = c("period", "site"))

#vegan outputs sometimes do not like a few of the "samples" that have no species detections at all
#make a new version of all of the binary data that is "cleaned" of these lines
#but first see what they are/what they mean

clean4stats.bp23.full.days.binary <- bp23.full.days.binary %>% 
  replace(is.na(bp23.full.days.binary), 0) %>% 
  filter(rowSums(pick(4:ncol(bp23.full.days.binary))) != 0)


#simplify factors and data for nMDS
site.full <- as.factor(clean4stats.bp23.full.days.binary$site)
period.full <- as.factor(clean4stats.bp23.full.days.binary$period)
methodology.full <- as.factor(clean4stats.bp23.full.days.binary$method)
full.plants <- clean4stats.bp23.full.days.binary %>% select(!c(site, period, method))


#NMDS visualization of data ----

#prepare NMDS data with vegan
dist.full.plants <- vegdist(full.plants, method = "raup", binary = TRUE) #calc distance between communities for later stat analysis
set.seed(123) #this should make it so that the nmds results are always the same despite permutations
full.plant.mds <- metaMDS(full.plants, distance = "raup") 

#create NMDS figure
full.nmds_points <- as.data.frame(full.plant.mds$points)
full.nmds_points$methodology <- methodology.full
full.polygon_data <- full.nmds_points %>%
  group_by(methodology) %>%
  slice(chull(MDS1, MDS2))
full.NMDS.title <- expression(paste("NMDS visualization of network composition by methodology for days with samples from all methodologies"))
full.NMDS.method.comparisons <- ggplot(full.nmds_points, aes(x = MDS1, y = MDS2, color = methodology)) +
  geom_polygon(data = full.polygon_data, 
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
  scale_fill_manual(values = method.colors2) +
  theme_classic() +  
  labs(
    title = full.NMDS.title,
    x = "NMDS1",
    y = "NMDS2",
    color = "Methodology"
  ) +
  theme(plot.title = element_text(hjust=0.5),
        legend.position.inside = c(0.13, 0.85)
  )
