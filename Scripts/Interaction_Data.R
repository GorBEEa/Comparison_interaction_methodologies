##new script
setwd("C:/Users/christian.gostout/R/Comparison_interaction_methodologies")

#If you want to clear environment ->...
#rm(list = ls()) 

#things u need
library(readr)
library(tidyverse)
library(tidyr)
library(tidyselect)

#load interaction transect data
Interaction_transect <- read_csv("Data/Interaction_transect.csv") #load
df.int.trans <- data.frame(Interaction_transect) #make df
as_tibble(df.int.trans) #make tibble

#extract Bombus pascuorum interactions
bpasc.int <- filter(df.int.trans, Polinizador == "Bombus pascuorum")


