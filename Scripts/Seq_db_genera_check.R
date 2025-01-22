##Script for obtaining two clean lists of genera from both methodologies for completeness comparison


#import interaction taxa
int_taxa_list <- read.csv("Data/int_taxa_list.csv")
int_taxa_df <-as.data.frame(int_taxa_list)
colnames(int_taxa_df) <- c("taxa")


#import metabarcoding taxa
bell_taxa_hits <- read.csv("Data/bell_taxa_hits.csv")
bell_taxa_hits <- bell_taxa_hits[,-1] #get rid of useless column
bell_taxa_hits <- filter(bell_taxa_hits, bell_taxa_hits[,3] >= 1) #filter out 0 detection hits 
bell_taxa_df <- as.data.frame(bell_taxa_hits[,-3]) #get rid of counts column
bell_taxa_df$taxa <- paste(bell_taxa_df$Genus, bell_taxa_df$Species)
bell_taxa <- as.data.frame(bell_taxa_df[,-c(1,2)])
colnames(bell_taxa) <-c("taxa")

#clean plant species names
source("C:/Users/christian.gostout/R/Comparison_interaction_methodologies/Scripts/transectos250_plantas_disp_check.R") #cleans species names
source("C:/Users/christian.gostout/R/Comparison_interaction_methodologies/Scripts/transect250_genera_check.R")
int_taxa_df <- data.d2 #cleaned output df
#write.table(int_taxa_df, "Data/output/gorbeia_interaction_plants.txt")

#check how many times each species from interactions appears in the results from metabarcoding
cp_int_taxa <- int_taxa_df
cp_int_taxa$in_bell <- as.integer(int_taxa_df$Planta %in% bell_taxa$taxa)


#for confirming specifics: 
#bell_taxa %>% filter(grepl('Ranunculus', taxa))

##Try with entire Bell db list
seq_db_vec <- read_lines("Data/bell_db_taxa.txt")
seq_db_df <- data.frame(taxa = seq_db_vec)
colnames(seq_db_df) <- c("taxa")
cp_int_taxa$in_seq_db <- as.integer(int_taxa_df$Planta %in% seq_db_df$taxa)
colnames(cp_int_taxa) <- c("Plant", "Occurrence_in_mb_results","occurrence_in_ref_database")
#write.csv(cp_int_taxa, "Data/output/ref_seq_database_check.csv")
view(cp_int_taxa)


#Any missing taxa: 1)check if it makes sense, 2)check if it is in bell fasta

