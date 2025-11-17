#Here we construct the "combined network" for Bombus pascuorum
#Using data from 3 methodologies with bipartite
#essentially the script for building Fig. ___

library(here)
library(tidyverse)
library(tidyr)
library(bipartite)
library(treemap)
library(viridis)

#load data needed for analysis: 3 lists of detections
# add flower count data for emphasis?
load(file = here("Data/05_output.RData"))



method.networks <- clean4stats.bp23.all.binary[,-c(1:2)] %>%
  filter(method != "count") %>%
  group_by(method) %>%
  summarise(across(Lathyrus:last_col(), ~ sum(.)))
method.networks.binary <- method.networks
method.networks.binary[ , -1] <- (method.networks[ , -1] != 0) * 1


#prep data for bipartite
method.networks.matrix <- as.matrix(method.networks.binary[ , -1])
rownames(method.networks.matrix) <- method.networks.binary[[1]]
method.colors3 <- c("interaction" = "lightblue",
                    "gut.metabarcoding" = "forestgreen",
                    "pollen.metabarcoding" = "goldenrod1")

interaction.colors <- method.colors3[rownames(method.networks.matrix)]

interaction.colors.vec <- rep(interaction.colors, times = rowSums(method.networks.matrix))

link.list <- which(method.networks.matrix == 1, arr.ind = TRUE)
link.colors.by.edge <- method.colors3[rownames(method.networks.matrix)[link.list[, "row"]]]


plotweb(method.networks.matrix,
        col.high = method.colors3[rownames(method.networks.matrix)],
        col.low = "grey66",
        bor.col.high = "black",
        bor.col.low = "black",
        method = "normal",
        low.spacing = 0,
        x.lim = c(0,1.1),
        low.plot = TRUE) #draws methodology boxes or not

        