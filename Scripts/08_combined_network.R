#Here we construct the "combined network" for Bombus pascuorum
#Using data from 3 methodologies with bipartite
#essentially the script for building Fig. ___

library(here)
library(tidyverse)
library(tidyr)
library(bipartite)
library(treemap)
library(viridis)
library(circlize)

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
method.networks.matrix <- sortweb(method.networks.matrix, sort.order = "ca")

#Setup colormap
lower.col <- rep("black", nrow(method.networks.matrix))
names(lower.col) <- rownames(method.networks.matrix)
lower.col["interaction"] <- "lightblue"
lower.col["gut.metabarcoding"] <- "forestgreen"
lower.col ["pollen.metabarcoding"] <- "goldenrod1"

plotweb(method.networks.matrix,
        horizontal = TRUE,
        curved_links = FALSE,
        lower_labels = FALSE,
        spacing = 0.001,
        box_size = c(0.0001,0.0001),
        text_size = 0.6,
        higher_italic = TRUE,
        lower_color = lower.col,
        link_color = "lower",
        y_lim = c(0,10))

#export to inkscape and edit



#Try.. uhhh... with individual samples?

indv.method.networks <- clean4stats.bp23.all.binary[,-c(1:2)] %>%
  filter(method != "count")
indv.method.networks.mtx <- as.matrix(indv.method.networks[ , -1])
rownames(indv.method.networks.mtx) <- indv.method.networks[[1]]


lower.col2 <- rep("black", nrow(indv.method.networks.mtx))
names(lower.col2) <- rownames(indv.method.networks.mtx)
lower.col2["interaction"] <- "lightblue"
lower.col2["gut.metabarcoding"] <- "forestgreen"
lower.col2["pollen.metabarcoding"] <- "goldenrod1"

plotweb(indv.method.networks.mtx,
        horizontal = TRUE,
        lower_labels = FALSE,
        lower_color = lower.col,
        link_color = "lower",
        y_lim = c(0,100)
        )        
#yikes


#try not binary

#prep data for bipartite
method.networks.matrix2 <- as.matrix(method.networks[ , -1])
rownames(method.networks.matrix2) <- method.networks[[1]]
#method.networks.matrix2 <- sortweb(method.networks.matrix2, sort.order = "ca")


plotweb(method.networks.matrix2,
        horizontal = TRUE,
        curved_links = TRUE,
        lower_labels = FALSE,
        spacing = 0.001,
        box_size = c(0.01,0.0001),
        text_size = 0.6,
        higher_italic = TRUE,
        lower_color = lower.col,
        link_color = "lower",
        y_lim = c(0,200)
        )
#the consumption only links disappear?


ints <- df.int.genus %>% distinct(genus)
genus.hits.23 <- unlist(genus.hits.23)
poln.genus.hits.2023 <- unlist(poln.genus.hits.2023)
ints <- unlist(ints)

all.taxa <- sort(unique(c(genus.hits.23, poln.genus.hits.2023, ints)))

membership <- data.frame(
  genus = all.taxa,
  in.gmb   = all.taxa %in% genus.hits.23,
  in.pmb   = all.taxa %in% poln.genus.hits.2023,
  in.ints   = all.taxa %in% ints
)

membership.stack <- c(membership$genus[membership$in.gmb & membership$in.pmb & !membership$in.ints],
                      membership$genus[membership$in.gmb & !membership$in.pmb & !membership$in.ints],
                      membership$genus[membership$in.gmb & membership$in.ints & !membership$in.pmb],
                      membership$genus[membership$in.gmb & membership$in.ints & membership$in.pmb],
                      membership$genus[membership$in.pmb & !membership$in.ints & !membership$in.gmb]
                      )

order <- data.frame(
  genus = membership.stack,
  order = 1:length(membership.stack)
)


membership.orderd <- membership %>%
  left_join(order, by = "genus") %>%
  arrange(order) %>%
  select(-order)

orderd.genera <- membership.orderd$genus


circos.clear()
circos.par(gap.after = c(rep(1, length(all.taxa) - 1), 5))

circos.par(cell.padding = c(0, 0, 0, 0))
circos.initialize(
  factors = membership.orderd$genus,
  xlim = cbind(rep(0, nrow(membership.orderd)), rep(1, nrow(membership.orderd)))
)

#track for plant names
circos.track(
  ylim = c(0, 1),
  track.height = 0.12,
  bg.border = NA,
  panel.fun = function(x, y) {
    tax <- get.cell.meta.data("sector.index")
    xlim <- get.cell.meta.data("xlim")
    ylim <- get.cell.meta.data("ylim")
    circos.text(
      x = mean(xlim),
      y = ylim[2],
      labels = tax,
      facing = "clockwise",
      niceFacing = TRUE,
      cex = 0.5,
      adj = c(0.5, 0),
      font = 4
    )
  }
)

# Track for gut mb
circos.track(
  ylim = c(0, 1),
  panel.fun = function(x, y) {
    tax <- get.cell.meta.data("sector.index")
    xlim <- get.cell.meta.data("xlim")
    if (membership.orderd[membership.orderd$genus == tax, "in.gmb"]) {
      circos.rect(xlim[1], 0, xlim[2], 1, col = "forestgreen", border = NA)
    } 
  },
  track.height = 0.1, bg.border = NA
)



# Track for pollen MB
circos.track(
  ylim = c(0, 1),
  panel.fun = function(x, y) {
    tax <- get.cell.meta.data("sector.index")
    xlim <- get.cell.meta.data("xlim")
    if (membership.orderd[membership.orderd$genus == tax, "in.pmb"]) {
      circos.rect(xlim[1], 0, xlim[2], 1, col = "goldenrod1", border = NA)
    } 
  },
  track.height = 0.1, bg.border = NA
)


# Track for interactions
circos.track(
  ylim = c(0, 1),
  panel.fun = function(x, y) {
    tax <- get.cell.meta.data("sector.index")
    xlim <- get.cell.meta.data("xlim")
    if (membership.orderd[membership.orderd$genus == tax, "in.ints"]) {
      circos.rect(xlim[1], 0, xlim[2], 1, col = "lightblue", border = NA)
    } 
  },
  track.height = 0.1, bg.border = NA
)

