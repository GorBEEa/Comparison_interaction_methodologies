#Quick start script, sourcing data processing scripts from all methodologies 
#Allows you to go straight to interactions_x_metabarcoding_2023.R and do analyses

#Load interaction data -----
source(
  "C:/Users/christian.gostout/R/Comparison_interaction_methodologies/Scripts/
  Interaction_Data.R",
  echo = FALSE)

#Load flower count data ------
source(
  "C:/Users/christian.gostout/R/Comparison_interaction_methodologies/Scripts/
  flower_count_data.R"
  , echo = FALSE)

#Load gut metabarcoding data ------
source(
  "C:/Users/christian.gostout/R/Comparison_interaction_methodologies/Scripts/
  metabarcoding_data.R"
  , echo = FALSE)
#maybe there will be some errors and slow things
#there is code that is not essential to continuing with further analysis


#Load pollen metabarcoding data ------
source(
  "C:/Users/christian.gostout/R/Comparison_interaction_methodologies/Scripts/
  pollen_metabarcoding_data_2023_2024.R"
  , echo = FALSE)
#maybe there will be some errors and slow things
#there is code that is not essential to continuing with further analysis

