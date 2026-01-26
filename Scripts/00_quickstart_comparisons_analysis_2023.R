#Quick start script, sourcing data processing scripts from all methodologies 
#Allows you to go straight to interactions_x_metabarcoding_2023.R and do analyses
library(here)


#Load interaction data -----
source(here("Scripts/01_interaction_data_2023.R"), echo = FALSE)

#Load flower count data ------
source(here("Scripts/02_flower_count_data_2023.R"), echo = FALSE)

#Load gut metabarcoding data ------
source(here("Scripts/03_gut_metabarcoding_data_2023.R"), echo = FALSE)
#maybe there will be some errors and slow things
#there is code that is not essential to continuing with further analysis

#Load pollen metabarcoding data ------
source(here("Scripts/04_pollen_metabarcoding_data_2023_2024.R"), echo = FALSE)
#maybe there will be some errors and slow things
#there is code that is not essential to continuing with further analysis

save.image(file = here("Data/methodologies_data.RData"))
