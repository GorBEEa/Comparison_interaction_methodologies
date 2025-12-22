#Interaction network metrics analyzed using 2023 data from field transects
#Here we calculate and visualize:
#d' specialization of Bombus pascuorum and importantity of plants in the BP interaction network

#packages ---------------------------------------------------------------------
#library(here)
#library(tidyverse)
#library(tidyr)
library(ggplot2)
library(bipartite)
library(treemap)
library(viridis)
library(viridisLite)
library(grid)
library(treemapify)
library(shadowtext)


#prepare data -----------------------------------------------------------------

#the data in this script all come from clean4stats.bp23.all.binary, created in 05

int.networks.x.methodology <- clean4stats.bp23.all.binary %>% 
  group_by(method,period) %>% 
  filter(method != "count") %>% 
  summarise(across(Lathyrus:last_col(), ~ sum(.))) %>% 
  mutate(sample = paste(method,"period", period)) %>% 
  relocate(sample)

#create some vectors for later use
dp.methods <- int.networks.x.methodology$method
methods_reorder <- c("interaction", "gut.metabarcoding", "pollen.metabarcoding")
dp.methods <- dp.methods[order(match(dp.methods, methods_reorder))]
dp.periods <- int.networks.x.methodology$period

#create principal interaction dataset for analysis
int.networks.x.methodology <- as.data.frame(int.networks.x.methodology)
rownames(int.networks.x.methodology) <- int.networks.x.methodology[,1]
int.networks.x.methodology <- int.networks.x.methodology[,-c(1:3)]

#make interaction matrices for calculating d' over 6 periods
int.web <- as.matrix(int.networks.x.methodology[7:12, 1:209])
gmb.web <- as.matrix(int.networks.x.methodology[1:6, 2:209])
pmb.web <- as.matrix(int.networks.x.methodology[13:18, 2:209])

#calculate d'
dprime.int <- dfun(int.web)
dprime.gmb <- dfun(gmb.web)
dprime.pmb <- dfun(pmb.web)



#compare d calculations across methodologies ----------------------------------

#make vectors for d' across periods for individual methodologies
specvec.int <- as.vector(dprime.int$dprime)
specvec.gmb <- as.vector(dprime.gmb$dprime)
specvec.pmb <- as.vector(dprime.pmb$dprime)
specvec.all <- c(specvec.int,specvec.gmb,specvec.pmb)


#join data and metadata into one df
specialization <- as.data.frame(bind_cols(dp.periods, dp.methods, specvec.all))
names(specialization)[1] <- paste("period")
names(specialization)[2] <- paste("method")
names(specialization)[3] <- paste("dprime")
specialization$method <- factor(specialization$method,
                                levels = c("gut.metabarcoding","pollen.metabarcoding", "interaction"))

#standard project colors for plotting
int.method.colors <- c("interaction" = "lightblue",
                   "pollen.metabarcoding" = "goldenrod1",
                   "gut.metabarcoding" = "forestgreen") 


#Figure comparing d' evolution over season by methodology
fig.dprime.title <- expression(paste("Specialization of", italic(" B. pascuorum "), "over time as indicated by interaction methodology"))


fig.dprime <- ggplot(specialization, aes(x = period, y = dprime, group = method,
                                         color = method, linetype = method, shape = method)) + 
  geom_line(size = 1.2) +
  geom_point(size = 3, alpha = 0.9) + 
  xlab("Sampling Period") +
  ylab("d' specialization") +
  scale_x_continuous(breaks = 1:6, labels = 1:6) + 
  scale_color_manual(
    values = int.method.colors,
    labels = c(
      "interaction" = "Interactions Transects",
      "pollen.metabarcoding" = "Pollen Metabarcoding",
      "gut.metabarcoding" = "Gut Content Metabarcoding")) +
  scale_shape_manual(
    values = c(
      "gut.metabarcoding" = 17,   
      "interaction" = 15,         
      "pollen.metabarcoding" = 3),
    labels = c(
      "interaction" = "Interactions Transects",
      "pollen.metabarcoding" = "Pollen Metabarcoding",
      "gut.metabarcoding" = "Gut Content Metabarcoding")) +
  scale_linetype_manual(
    values = c(
      "gut.metabarcoding" = "solid",   
      "interaction" = "dashed",         
      "pollen.metabarcoding" = "dotted"
    ),
    labels = c(
      "interaction" = "Interactions Transects",
      "pollen.metabarcoding" = "Pollen Metabarcoding",
      "gut.metabarcoding" = "Gut Content Metabarcoding" )) +
  labs(color = "Methodology", shape = "Methodology", linetype = "Methodology") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") + 
  annotate("text", x = Inf, y = 0.9, label = "perfect specialist", 
           hjust = 1.1, vjust = -0.5, color = "black", size = 4) +
  ggtitle(fig.dprime.title) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.8)
  )


#calculate importance for plant taxa in interaction networks ------------

#set up analysis

#Isolate all interactions for all plants for the 3 methodologies
interaction.importance <- clean4stats.bp23.all.binary %>% 
  group_by(method) %>% 
  filter(method != "count") %>% 
  summarise(across(Lathyrus:last_col(), ~ sum(.)))

#calculate total interaction number for each methodology
m.int <- sum(interaction.importance[2, 2:209]) #number of interactions from interactions
m.gmb <- sum(interaction.importance[1, 2:209]) #gut 
m.pmb <- sum(interaction.importance[3, 2:209]) #pollen

#calculate importance value for each plant genera for each methodology
gmb.importance <-interaction.importance[1,2:209]/m.gmb
int.importance <- interaction.importance[2,2:209]/m.int
pmb.importance <- interaction.importance[3,2:209]/m.pmb



#Importance visualization setup ----------------------------------------------------

#prep gut metabarcoding data
gmb.importance_vec <- as.numeric(gmb.importance)
names(gmb.importance_vec) <- colnames(gmb.importance)
gmb_nonzero <- gmb.importance_vec[gmb.importance_vec != 0]
gmb_sorted_nonzero <- sort(gmb_nonzero, decreasing = TRUE)

gmb_df <- data.frame(
  genus = names(gmb_sorted_nonzero),
  Importance = as.numeric(gmb_sorted_nonzero),
  row.names = NULL,
  stringsAsFactors = FALSE
)

gmb.most.important <- gmb_df[1:27,1:2]
gmb.most.important$Importance <- round(gmb.most.important$Importance, 3)
gmb.most.important$label <- paste0(gmb.most.important$genus, "\n", round(gmb.most.important$Importance, 4))


#prep interaction importance data
int.importance_vec <- as.numeric(int.importance)
names(int.importance_vec) <- colnames(int.importance)
int_nonzero <- int.importance_vec[int.importance_vec != 0]
int_sorted_nonzero <- sort(int_nonzero, decreasing = TRUE)


int_df <- data.frame(
  genus = names(int_sorted_nonzero),
  Importance = as.numeric(int_sorted_nonzero),
  row.names = NULL,
  stringsAsFactors = FALSE
)

int.most.important <- int_df[1:27,1:2]
int.most.important$Importance <- round(int.most.important$Importance, 3)
int.most.important$label <- paste0(int.most.important$genus, "\n", round(int.most.important$Importance, 4))


#prep pollen metabarcoding importance data
pmb.importance_vec <- as.numeric(pmb.importance)
names(pmb.importance_vec) <- colnames(pmb.importance)
pmb_nonzero <- pmb.importance_vec[pmb.importance_vec != 0]
pmb_sorted_nonzero <- sort(pmb_nonzero, decreasing = TRUE)

pmb_df <- data.frame(
  genus = names(pmb_sorted_nonzero),
  Importance = as.numeric(pmb_sorted_nonzero),
  row.names = NULL,
  stringsAsFactors = FALSE
)

pmb.most.important <- pmb_df[1:27,1:2]
pmb.most.important$Importance <- round(pmb.most.important$Importance, 3)
pmb.most.important$label <- paste0(pmb.most.important$genus, "\n", round(pmb.most.important$Importance, 4))


#establish boundaries for a healthy relationship (with your data)
global_min <- min(
  gmb.most.important$Importance,
  int.most.important$Importance,
  pmb.most.important$Importance)

global_max <- max(
  gmb.most.important$Importance,
  int.most.important$Importance,
  pmb.most.important$Importance)


#some color settings that work with these boundaries in ggplot
my_palette <- viridisLite::viridis(
  n = 60,
  begin = 0.53,   # low end of the viridis range
  end   = 1.0    # high end
)



#analyze importance results for interactions -------------------------------

fig.int.importance <- ggplot(int.most.important,
       aes(area = Importance, fill = Importance, label = label)) +
  geom_treemap() +
  geom_treemap_text(
    colour   = "grey30",
    fontface = "italic",
    place    = "centre",
    reflow   = TRUE,
    size     = 6) +
  scale_fill_gradientn(
    colours = my_palette,
    limits  = c(global_min, global_max)) +
  ggtitle("A. Importance of plant taxa in interaction network")


#visualize "importance" results for gut contents ------------------------

fig.gmb.importance <- ggplot(gmb.most.important,
                             aes(area = Importance, fill = Importance, label = label)) +
  geom_treemap() +
  geom_treemap_text(
    colour   = "white",
    fontface = "italic",
    place    = "centre",
    reflow   = TRUE,
    size     = 6) +
  scale_fill_gradientn(
    colours = my_palette,
    limits  = c(global_min, global_max)) +
  ggtitle("B. Gut Content Metabarcoding")


#analyze importance results for pollen metabarcoding ----------------------------
fig.pmb.importance <- ggplot(pmb.most.important,
       aes(area = Importance, fill = Importance, label = label)) +
  geom_treemap() +
  geom_treemap_text(
    colour   = "white",
    fontface = "italic",
    place    = "centre",
    reflow   = TRUE,
    size     = 6) +
  scale_fill_gradientn(
    colours = my_palette,
    limits  = c(global_min, global_max)) +
  ggtitle("C. Corbicular Pollen Metabarcoding")


#the end ---------------------------

save.image(file = here("Data/07_output.RData"))

