## Script for importing taxonomic assignation data from dada2
# data must be downloaded from cluster, which has R bioconductor
# they can be found in dada2_plant_**ANALISIS_ID**/data/dada2/03_taxonomy/**ID**/**ID**_tax_assignation.rds"
# download to project Data/dada2_outputs

library(here)
library(phyloseq) #needs to be done in bioconductor


#import data output from dada2 to R
taxa.table <- readRDS(here("Data/dada2_outputs/2023_plantv2_bell_tax_assignation.rds"))

taxa.df <-as.data.frame(taxa.table)

#summarize data by taxonomic order of interest (here I use Genus)
taxa.summary <- table(taxa.df$Genus, taxa.df$Species) #create a summary table of taxa
taxa.summary.df <- as.data.frame(taxa.summary) # turn the table into a df
colnames(taxa.summary.df) <- c("Genus","Species","Count") #rename the columns appropriately
taxa.summary.df <- taxa.summary.df[order(-taxa.summary.df$Count),] #organize the taxa in descending order of occurrence frequency

#if you want a condensed version:
#taxa.hits <- head(bell.taxa.summary.df, 50) # view the top 50 taxa identified across samples


#Just genus bc that appears to be the highest resolution tax level I have

genus.summary <- table(taxa.df$Genus)
genus.summary.df <- as.data.frame(genus.summary)
colnames(genus.summary.df) <- c("Genus","Count")
genus.summary.df <- genus.summary.df[order(-genus.summary.df$Count),]

#if you want a condensed version:
#genus.hits <- head(genus.summary.df, 50)




