#Script for looking at differences in taxa detected by metabarcoding methodologies
#analysis at individual specimen level

#library(here)
#library(tidyverse)
#library(tidyr)
#library(tidyselect)

#Data prep ---------------------------------------------------------------------------------------------------

#Bring pollen metabarcoding data in and format for this analysis
poln.2023.indv <- poln.2023.genomic.specs #copy the existing pollen mb data and clean it up for this analysis
poln.2023.indv$ID <- sub('^(Y\\d{8}P).*', '\\1', poln.2023.genomic.specs$sample) #get clean IDs
poln.2023.indv <- poln.2023.indv %>% relocate(ID) %>% 
  select(!c(sample, year, period, site, specimen, color_p, color_s, type, plate, quant_reading, is.neg)) %>% 
  mutate(type = rep("pollen", 25))%>% 
  relocate(type, .after = ID)
poln.2023.indv <- poln.2023.indv %>% filter(!ID %in% c("Y23050403P","Y23061405P")) #rm 2 samples with no gut pair

#Bring gut metabarcoding data in and format for this analysis
gut.2023.indv <- bp23.genomic.analys #copy the existing gut mb data and clean it up for this analysis
gut.2023.indv <- gut.2023.indv %>% 
  select(!c(year, specimen, color_p, color_s, type, plate, quant_reading, is.neg)) %>% 
  mutate(type = rep("gut", 122)) %>% 
  relocate(type, .after = sample)

#Need to be able to match gut/pollen samples by specimen
IDS <- as.data.frame(read.csv2(here("Data/pollen_gut_id_match.csv"), sep = ",")) #create a df for connecting samples from both methodologies
IDS <- IDS %>% 
  rename(sample = Bombus_sample) %>% 
  rename(ID = sample_ID)
#band-aid patch for removing two samples with 0 gut ASVs
IDS <- IDS %>% filter(!sample %in% c("GBP23050403M","GBP23061405M"))

#Isolate data for specimens with both sample types by reducing gut samples in analysis
gut.w.poln.2023 <- left_join(IDS,gut.2023.indv, by = "sample")






#Data analysis -----------------------------------------------------------------------------------------------

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


#Create a df that shows the number of shared taxa between the two methodologies by sample
all_ids <- intersect(names(n_results_poln), names(n_results_gut))
sharing <- data.frame(
  ID = all_ids,
  shared_count = sapply(all_ids, function(id) {
    sum(as.integer(taxa_results_gut[[id]] %in% taxa_results_poln[[id]]))
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






#statistical analyses ------------------------------------------------------------------------------------- 

#prep
cp.gut.2023.indv <- gut.2023.indv #make a new copy of gut data
cp.gut.2023.indv <- cp.gut.2023.indv %>% #rename for the next step
 # rename(ID = sample) %>% 
  select(!c(period,site))

#create a dataset of all metabarcoding data for individual samples
mb.2023.indv <- full_join(cp.gut.2023.indv,poln.2023.indv)
mb.2023.indv[is.na(mb.2023.indv)] <- 0 #Remove NAs
mb.2023.indv <- mb.2023.indv %>% 
  rename(method = type)

#try with only specimens with shared samples
mb2x.2023.indv <- full_join(gut.w.poln.2023,poln.2023.indv)
mb2x.2023.indv[is.na(mb2x.2023.indv)] <- 0 #Remove NAs




#clean out zero sum rows and columns 
clean4stats.mb.2023.indv <- mb.2023.indv %>% 
  filter(rowSums(across(3:last_col())) > 0) %>% 
  select(1:2, # keep metadata columns unchanged
         where(~ is.numeric(.) && sum(., na.rm = TRUE) > 0)) #remove 0 sum columns

#same for only specimens with shared samples
clean4stats.mb2x.2023.indv <- mb2x.2023.indv %>% 
  filter(rowSums(across(6:last_col())) > 0) %>% 
  select(1:5, # keep metadata columns unchanged
         where(~ is.numeric(.) && sum(., na.rm = TRUE) > 0)) #remove 0 sum columns





#create elements for statistical analysis
mb.2023.indv.methodology <- as.factor(clean4stats.mb.2023.indv$method)
#length(factor) #to count/check factor lengths (should all be the same and same as row # in clean4stats.bp23.all.binary and all.plants)
mb.2023.indv.plants <- clean4stats.mb.2023.indv %>% 
  select(!c(ID,method))

#same for only specimens with shared samples
mb2x.2023.indv.methodology <- as.factor(clean4stats.mb2x.2023.indv$type)
mb2x.2023.indv.plants <- clean4stats.mb2x.2023.indv %>% 
  select(!c(ID,sample,type,period,site))
sample_blocks <- as.factor(clean4stats.mb2x.2023.indv$ID)





#Test for only specimens with shared samples, using specimen as a block to force methodological comparison by specimen
permanova.2023.indv.x.sample <- adonis2(mb2x.2023.indv.plants ~ mb2x.2023.indv.methodology,
  permutations = 999, 
  binary = TRUE,
  method = "raup",
  strata = sample_blocks)

#chatgpt thinks I should do this: adonis2(mb2x.2023.indv.plants ~ mb2x.2023.indv.methodology + ID, permutations = 999,binary = TRUE, method = "raup",by = "margin")
#it answers a different question: “After accounting for how different specimens are overall, does method still explain a meaningful fraction of remaining multivariate variance?”
#current code asks: “Are communities from method A and B consistently different within specimens, regardless of how different specimens are from each other?”


indv.permanova.kbl <- permanova.2023.indv.x.sample %>% 
  kbl(caption = "Specimen level comparison of plant community between metabarcoding methodologies") %>% 
  kable_minimal(full_width = F, html_font = "Cambria")


dist.mb2x.2023.indv.plants <- vegdist(mb2x.2023.indv.plants, method = "raup", binary = TRUE)
indiv.disp <- betadisper(dist.mb2x.2023.indv.plants, clean4stats.mb2x.2023.indv$type)
permutest(indiv.disp)


#Per MKD recommendation - visualize this with a paired nMDS

set.seed(123)

nmds_mb2x <- metaMDS(
  dist.mb2x.2023.indv.plants,
  k = 2,
  trymax = 100,
  autotransform = FALSE,
  trace = FALSE)

nmds_mb2x$stress 
#kinda high, be very clear about this

nmds_mb2x_points <- as.data.frame(nmds_mb2x$points)
nmds_mb2x_points <- nmds_mb2x_points %>%
  mutate(methodology = mb2x.2023.indv.methodology)

fig_nmds_mb2x <- ggplot(nmds_mb2x_points, aes(x = MDS1, y = MDS2, color = methodology)) +
  geom_point(size = 3)


scores_df$ID     <- clean4stats.mb2x.2023.indv$ID
scores_df$method <- clean4stats.mb2x.2023.indv$type
scores_df <- scores_df %>% filter(!ID %in% c("Y23050403P","Y23061405P"))#removal of unpaired samples didn't propagate to here


save.image(file = here("Data/05.2_output.RData"))

