#Analyzing interaction and metabarcoding data together 
#Interaction_Data.R and metabarcoding_data.R should already be sourced


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

fig.zz




