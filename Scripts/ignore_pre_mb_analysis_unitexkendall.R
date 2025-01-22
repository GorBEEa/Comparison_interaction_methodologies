## Not useful analysis of arbitrary number outputs from dada2 with interaction data
#maybe some of the code will be useful for something else eventually though








#script Interaction_Data.R needs to be run first
#source("C:/Users/christian.gostout/R/Comparison_interaction_methodologies/Scripts/Interaction_Data.R")


#Upload UNITE metabarcoding data 
#barcoding data downloaded from cluster

#upload metabarcoding data and reformat into friendly df
metab.genus.hits <- read_csv(here("Data/UNITE_genus_hits.csv"))
df.metab <- data.frame(metab.genus.hits)
colnames(metab.genus.hits) <- c("tax","count")
metab.genus.hits$genus <- sapply(strsplit(as.character(metab.genus.hits$tax), "__"), `[`, 2)
metab.genus.hits <- metab.genus.hits[,-1]

#next step is to visualize this. The data are in distinct format from the obs data 
# ggplot(data = df.metab.genus, aes(x = genus, y = count)) + geom_col() 
#+ theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12))
# that's fine, but better to consolidate to fewer genera again

top.metab.genus <- subset(metab.genus.hits, count > 10) #filter out the genera with top read counts

#make new figure with fewer selected data
title.top.metab.genus <- expression(paste("Read counts of top floral genera identified from ", italic("B. pascuorum"), " gut DNA (2023)"))
fig.a <- ggplot(data = top.metab.genus, aes(x = genus, y = count)) + geom_col() +
  labs(x = "Floral genus", y = "Read count", title = title.top.metab.genus) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12), plot.title = element_text(hjust=0.5))


##Bring data from interactions and metabarcoding together
#dataframes at least need to be in the same format (genus column, integer column)
smry.int.genus <- table(df.int.genus)
df.int.genus.smry <- data.frame(genus = names(smry.int.genus), interactions = as.integer(smry.int.genus))
df.int.genus.smry <- arrange(df.int.genus.smry,interactions)

#merge datasets by genus name
combo.by.genus <- merge(df.int.genus.smry, metab.genus.hits, by = 'genus', all = TRUE)
combo.by.genus[is.na(combo.by.genus)] <- 0 #NAs to 0 if you want
ktau.genus <- cor.test(combo.by.genus$count, combo.by.genus$interactions, method = "kendall")

#Ok so they are not significantly correlated in terms of rank


