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
df.bpasc <- data.frame(flor.int = bpasc.int$Planta) #make df that is jsut flowers from pascuorum interactions

#clean plant species names
source("C:/Users/christian.gostout/R/Comparison_interaction_methodologies/Scripts/transectos250_plantas_disponibilid.R") #cleans species names
source("C:/Users/christian.gostout/R/Comparison_interaction_methodologies/Scripts/transect250_plantastotal (1).R")
df.bpasc.int <- data.d #cleaned output df


## Time to look at what species interactions were observed
#prep figure features
title.bpasc.int <- expression(paste("Total ", italic("B. pascuorum"), " interactions with flower species 2023 season")) #new plot, give it a title
cap.bpasc.int <- expression(paste(bold("Figure X. "), "From April to June, 2023, we surveyed plant-pollinator interactions of 16 transcets in Gorbea Natural Park. Across the field survey season, the bumblebee species,", italic(" B. pascuorum,"), " interacted with X distinct flower species.The most visited flower species was ", italic("Prunella vulgaris.") ))

#make figure
fig.x <- ggplot(data = df.bpasc.int, aes(Planta)) + geom_bar() + 
  labs(x = "Floral species", y = "Interaction count 2023 Season", title = title.bpasc.int, caption = cap.bpasc.int) + 
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),plot.title = element_text(hjust=0.5))

#after several hours I still have no idea how to make a caption wrap acceptably to the figure size
#obviously that figure is overwhelming




## Focus on flowers with more interactions - make a dataframe only with species with multiple interactions

flor_summary <-table(df.bpasc.int$Planta) #get sums of interactions for 2023 by species
topflor <- which(flor_summary > 1) #filter out single interaction species
topflor_df <- data.frame(Species = names(topflor), interactions = as.integer(topflor)) #turn into a nice clean dataframe

#make figure.y
title.topflor.int <- expression(paste("Top floral species for ", italic("B. pascuorum"), " interactions (2023) ")) #new plot, give it a title

fig.y <- ggplot(topflor_df, aes(x = Species, y = interactions)) + geom_bar(stat = "identity") +
  labs(x = "Floral species", y = "Interaction count 2023 Season", title = title.topflor.int) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),plot.title = element_text(hjust=0.5))



## As of now, using UNITE for gut DNA metabarcoding, it looks like genus is the best I can get for taxonomic specificity
#redo all of this just for Genus level

df.int.genus <- data.frame(Genus = sapply(strsplit(as.character(df.bpasc.int$Planta), " "), `[`, 1))

#Make new figure, Figure.Z

title.int.genus <- expression(paste("Total ", italic("B. pascuorum"), " flower interactions by plant genus 2023 season")) #new plot, give it a title
fig.z <- ggplot(data = df.int.genus, aes(Genus)) + geom_bar() + 
  labs(x = "Floral genus", y = "Interaction count 2023 Season", title = title.int.genus) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),plot.title = element_text(hjust=0.5))
