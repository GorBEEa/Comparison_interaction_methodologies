#Interaction network metrics analyzed using 2023 data from field transects
#Here we calculate and visualize:
#d' specialization of Bombus pascuorum and centrality of plants in the BP interaction network

#packages ---------------------------------------------------------------------
#library(here)
#library(tidyverse)
#library(tidyr)
library(bipartite)
library(treemap)
library(viridis)


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
int.web <- as.matrix(int.networks.x.methodology[7:12, 1:210])
gmb.web <- as.matrix(int.networks.x.methodology[1:6, 2:210])
pmb.web <- as.matrix(int.networks.x.methodology[13:18, 2:210])

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

fig.dprime <- ggplot(specialization, aes(period, dprime, fill = method)) + 
  geom_col(position = "Dodge", alpha = 0.8) + 
  xlab("Sampling Period") +
  ylab("d' specialization") +
  scale_x_continuous(breaks = 1:6, labels = 1:6) + 
  scale_fill_manual(values = int.method.colors, labels = c(
    "interaction" = "Interactions Transects",
    "pollen.metabarcoding" = "Pollen Metabarcoding",
    "gut.metabarcoding" = "Gut Content Metabarcoding")) +
  labs(fill = "Methodology",
       color = NULL ,
       linetype = NULL) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") + 
  annotate("text", x = Inf, y = 0.9, label = "perfect specialist", 
           hjust = 1.1, vjust = -0.5, color = "black", size = 4) +
  ggtitle(fig.dprime.title) +
  theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust = 0.7))

fig.dprime



#calculate network centrality for plant taxa within interaction networks as characterized by methodologies ------------

#Isolate all interactions for all plants for the 3 methodologies
centr.interactions <- clean4stats.bp23.all.binary %>% 
  group_by(method) %>% 
  filter(method != "count") %>% 
  summarise(across(Lathyrus:last_col(), ~ sum(.)))

#calculate total interactions for each methodology
m.int <- sum(centr.interactions[2, 2:210]) #number of interactions from interactions
m.gmb <- sum(centr.interactions[1, 2:210]) #gut 
m.pmb <- sum(centr.interactions[3, 2:210]) #pollen

#calculate centrality for each plant genera for each methodology
gmb.centralities <-centr.interactions[1,2:210]/m.gmb
int.centralities <- centr.interactions[2,2:210]/m.int
pmb.centralities <- centr.interactions[3,2:210]/m.pmb


#some color settings before making 3 plots
start_col <- "#440154FF"
end_col <- "#FDE725FF"
custom_viridis <- viridis(100)


#analyze centrality results for gut contents
gmb.centralities_vec <- as.numeric(gmb.centralities)
names(gmb.centralities_vec) <- colnames(gmb.centralities)
gmb_nonzero <- gmb.centralities_vec[gmb.centralities_vec != 0]
gmb_sorted_nonzero <- sort(gmb_nonzero, decreasing = TRUE)

gmb_df <- data.frame(
  genus = names(gmb_sorted_nonzero),
  Importance = as.numeric(gmb_sorted_nonzero),
  row.names = NULL,
  stringsAsFactors = FALSE
)

gmb.most.central <- gmb_df[1:27,1:2]
gmb.most.central$label <- paste0(gmb.most.central$genus, "\n", round(gmb.most.central$Importance, 4))

fig.centrality.gmb <- treemap(gmb.most.central,
        index = "label",      
        vSize = "Importance",
        vColor = "Importance",
        type = "value",
        palette = custom_viridis,
        range = c(0.0136, 0.133),
        title = expression(paste("Importance of plant taxa within the", italic(" B. pascuorum "), 
                                 "interaction network revealed by gut content metabarcoding")))

fig.centrality.gmb


#analyze centrality results for interactions
int.centralities_vec <- as.numeric(int.centralities)
names(int.centralities_vec) <- colnames(int.centralities)
int_nonzero <- int.centralities_vec[int.centralities_vec != 0]
int_sorted_nonzero <- sort(int_nonzero, decreasing = TRUE)


int_df <- data.frame(
  genus = names(int_sorted_nonzero),
  Importance = as.numeric(int_sorted_nonzero),
  row.names = NULL,
  stringsAsFactors = FALSE
)

int.most.central <- int_df[1:27,1:2]
int.most.central$label <- paste0(int.most.central$genus, "\n", round(int.most.central$Importance, 4))

fig.centrality.int <- treemap(int.most.central,
        index = "label",      
        vSize = "Importance",
        vColor = "Importance",
        type = "value",
        palette = custom_viridis,
        range = c(0.0136, 0.133),
        title = expression(paste("Importance of plant taxa within the", italic(" B. pascuorum "), 
                                 "interaction network revealed by interaction transects")))

fig.centrality.int


#analyze centrality results for pollen metabarcoding
pmb.centralities_vec <- as.numeric(pmb.centralities)
names(pmb.centralities_vec) <- colnames(pmb.centralities)
pmb_nonzero <- pmb.centralities_vec[pmb.centralities_vec != 0]
pmb_sorted_nonzero <- sort(pmb_nonzero, decreasing = TRUE)

pmb_df <- data.frame(
  genus = names(pmb_sorted_nonzero),
  Importance = as.numeric(pmb_sorted_nonzero),
  row.names = NULL,
  stringsAsFactors = FALSE
)

pmb.most.central <- pmb_df[1:27,1:2]
pmb.most.central$label <- paste0(pmb.most.central$genus, "\n", round(pmb.most.central$Importance, 4))

fig.centrality.pmb <- treemap(pmb.most.central,
        index = "label",      
        vSize = "Importance",
        vColor = "Importance",
        type = "value",
        palette = custom_viridis,
        range = c(0.0136, 0.133),
        title = expression(paste("Importance of plant taxa within the", italic(" B. pascuorum "), 
                                 "interaction network revealed by corbicular pollen metabarcoding")))

fig.centrality.pmb




save.image(file = here("Data/07_output.RData"))
