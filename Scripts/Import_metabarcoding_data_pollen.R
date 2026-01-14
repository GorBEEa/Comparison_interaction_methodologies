## Script for importing taxonomic assignation data from dada2
# data must be downloaded from cluster
# they can be found in dada2_plant_**ANALISIS_ID**/data/dada2/03_taxonomy/**ID**/**ID**_tax_assignation.rds"
# download to project Data/dada2_outputs

library(here)
library(phyloseq)


#import data output from dada2 to R
p.taxa.table <- readRDS(here("Data/dada2_outputs/2023_24_pollen_tax_assignation.rds"))

p.taxa.df <-as.data.frame(p.taxa.table)

#summarize data by taxonomic order of interest (here I use Genus)
p.taxa.summary <- table(p.taxa.df$Genus, p.taxa.df$Species) #create a summary table of taxa
p.taxa.summary.df <- as.data.frame(p.taxa.summary) # turn the table into a df
colnames(p.taxa.summary.df) <- c("Genus","Species","Count") #rename the columns appropriately
p.taxa.summary.df <- p.taxa.summary.df[order(-p.taxa.summary.df$Count),] #organize the taxa in descending order of occurrence frequency

#if you want a condensed version:
#taxa.hits <- head(bell.taxa.summary.df, 50) # view the top 50 taxa identified across samples


#Just genus bc that appears to be the highest resolution tax level I have

p.genus.summary <- table(p.taxa.df$Genus)
p.genus.summary.df <- as.data.frame(p.genus.summary)
colnames(p.genus.summary.df) <- c("Genus","Count")
p.genus.summary.df <- p.genus.summary.df[order(-p.genus.summary.df$Count),]

#if you want a condensed version:
#p.genus.hits <- head(p.genus.summary.df, 50)




