# Comparison_interaction_methodologies
Here we compare 3 different methodologies used to infer interaction frequencies between plants and pollinators: transect observations, gut DNA contents, and pollen ball DNA contents. Code and data also support analyses of changes in genetic detections of interactions by varying factors such as site, period, and specimen size/gender.

# Data
We have two primary data sources used in this analysis: Interaction transect data (including B. pascuorum interactions and floral resource surveys) and B. pascuorum specimen data, including individual specifics (period, site, dimensions, sex) and sequencing data for ITS2 reads, processed using DADA2 (see repository for DADA2) and imported into R using phyloseq.


# Scripts
Data are primarily processed in scripts according to their origin (e.g. Interaction_Data.R, metabarcoding_data.R, etc.), and then recalled in other scripts intended for primary analysis (e.g. interactions_x_metabarcoding.R) or side analyses and cleaning (e.g. sed_db_genera_check.R).

