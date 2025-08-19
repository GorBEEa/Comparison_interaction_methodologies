# Comparison_interaction_methodologies
Here we compare three different methodologies (and use a fourth for reference) used to infer plant interaction networks for the bumblebee, _Bombus pascuorum_: transect interaction observations, gut DNA content, and corbicular pollen ball DNA content. Floral diversity survey data are also integrated into the analysis. Code and data are also prepared to support analyses of changes in genetic detections of interactions by varying factors such as site, period, and bee specimen size/gender.

# Data
We have two primary data sources used in this analysis: Interaction transect data (including B. pascuorum interactions and floral resource surveys) and B. pascuorum specimen data, including individual specifics (period, site, dimensions, sex) and sequencing data for ITS2 reads, processed using DADA2 (see repository for DADA2) and imported into R using phyloseq.


# Scripts
Data are generally prepared and processed in scripts according to their methodological origin (e.g. 01_interaction_data_2023.R, 03_gut_metabarcoding_data_2023.R, etc.), and then recalled in other scripts intended for comparative analyses (e.g. 05_methodology_comaprison_2023.R) or side analyses and cleaning (e.g. sed_db_genera_check.R). 


