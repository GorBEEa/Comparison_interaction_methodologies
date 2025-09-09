#Script for looking at individual level differences in taxa detected by 2 metabarcoding methodologies


#library(here)
#library(tidyverse)
#library(tidyr)
#library(tidyselect)

#Data prep ----

#Bring pollen metabarcoding data in and format for this analysis
poln.2023.indv <- poln.2023.genomic.specs #copy the existing pollen mb data and clean it up for this analysis
poln.2023.indv$ID <- sub('^(Y\\d{8}P).*', '\\1', poln.2023.genomic.specs$sample) #get clean IDs
poln.2023.indv <- poln.2023.indv %>% relocate(ID) %>% 
  select(!c(sample, year, period, site, specimen, color_p, color_s, type, plate, quant_reading, is.neg)) %>% 
  mutate(type = rep("pollen", 25))%>% 
  relocate(type, .after = ID)

#Bring gut metabarcoding data in and format for this analysis
gut.2023.indv <- bp23.genomic.analys #copy the existing gut mb data and clean it up for this analysis
gut.2023.indv <- gut.2023.indv %>% 
  select(!c(year, specimen, color_p, color_s, type, plate, quant_reading, is.neg)) %>% 
  mutate(type = rep("gut", 126)) %>% 
  relocate(type, .after = sample)

#Need to be able to match gut/pollen samples by specimen
IDS <- as.data.frame(read.csv2(here("Data/pollen_gut_id_match.csv"), sep = ",")) #create a df for connecting samples from both methodologies
IDS <- IDS %>% 
  rename(sample = Bombus_sample) %>% 
  rename(ID = sample_ID)

#Isolate data for specimens with both sample types by reducing gut samples in analysis
gut.w.poln.2023 <- left_join(IDS,gut.2023.indv, by = "sample")






#Analysis ----

#Check which taxa were identified in each sample by methodology
#tool for doing this:
get_nonzero_cols <- function(df, id_col = "ID") {
  setNames(
    lapply(df[[id_col]], function(this_id) {
      ix <- which(df[[id_col]] == this_id)
      if (length(ix) == 0) return(character(0))
      data_cols <- setdiff(colnames(df), c(id_col, "sample", "type","period", "site"))
      valid_cols <- data_cols[!is.na(df[ix, data_cols]) & df[ix, data_cols] != 0]
      valid_cols
    }),
    df[[id_col]]
  )
}

#results, with names of genera detected
taxa_results_poln <- get_nonzero_cols(poln.2023.indv) #for pollen
taxa_results_gut <- get_nonzero_cols(gut.w.poln.2023) #for guts
gut.2023.indv <- gut.2023.indv %>% rename (ID = sample) #reformat quickly - make sure everything is run IN ORDER
taxa_results_all_guts <- get_nonzero_cols(gut.2023.indv) #for all gut data, disregarding pollen

common_ids <- intersect(names(taxa_results_poln), names(taxa_results_gut))
common_results <- lapply(common_ids, function(id) {
  intersect(taxa_results_poln[[id]], taxa_results_gut[[id]])
})
names(common_results) <- common_ids


#same results but in number of taxa detected
n_results_poln <-sapply(taxa_results_poln, length)
n_results_gut <- sapply(taxa_results_gut, length)
n_results_all_guts <- sapply(taxa_results_all_guts, length)

#organize the last results into nice dfs
n_by_sample_poln <- data.frame(
  ID = names(n_results_poln),
  n.taxa.p = as.numeric(n_results_poln))

n_by_sample_gut <- data.frame(
  ID = names(n_results_gut),
  n.taxa.g = as.numeric(n_results_gut))
n_by_sample_gut <- left_join(n_by_sample_gut, IDS, by = "ID") %>%
  relocate(sample)

n_by_sample_all_guts <- data.frame(
  ID = names(n_results_all_guts),
  n.taxa.g = as.integer(n_results_all_guts))
n_by_sample_all_guts <- n_by_sample_all_guts %>% 
  mutate(period = as.integer(str_sub(ID, 6, 7)),
         site   = as.integer(str_sub(ID, 8, 9)))

#summary of these data:
individual.period.data <- n_by_sample_all_guts %>% group_by(period) %>% summarise(mean(n.taxa.g), sd(n.taxa.g))
ggplot(individual.period.data, aes(x = factor(period), y = `mean(n.taxa.g)`)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = `mean(n.taxa.g)` - `sd(n.taxa.g)`, ymax = `mean(n.taxa.g)` + `sd(n.taxa.g)`), 
                width = 0.2, size = 1) +
  labs(x = "Period", y = "Mean taxa per sample", title = "Mean taxa per individual gut sample and standard deviation by period (2023)") +
  theme_minimal()





#Create a df that shows the number of shared taxa between the two methodologies by sample
all_ids <- intersect(names(results_poln), names(results_gut))
sharing <- data.frame(
  ID = all_ids,
  shared_count = sapply(all_ids, function(id) {
    sum(as.integer(results_gut[[id]] %in% results_poln[[id]]))
  })
)

#make a df summarizing the comparison between mb methodologies at the individual level
total.taxa.specimen <- left_join(n_by_sample_gut, n_by_sample_poln, by = "ID")
total.taxa.specimen <- total.taxa.specimen %>% 
  mutate(total.per.specimen = n.taxa.g + n.taxa.p) %>% 
  left_join(sharing, by = "ID") %>% 
  mutate(portion.shared = shared_count/total.per.specimen) %>% 
  mutate(period = as.integer(str_sub(sample, 6, 7)),
    site   = as.integer(str_sub(sample, 8, 9)))

