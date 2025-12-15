#Script to analyze groups of plant types relevant to pollinators detected by metabarcoding
#chatGPT used to identify which of metabarcoding taxa were anemophilpous, which from groups:
#grasses, woody plants, other herbaceous
#results organized here

#packages -------------------------------------------------------------
#library(tidyverse)
#library(tidyr)
#library(tidyselect)
#library(ggplot2)
library(ggrepel)
library(patchwork)

#Breakdown of functional groups in gut content metabarcoding results ----------------------------------

#I fed chat gpt the list of plant genera detected in metabarcoding results and asked it to classify each under one of 4 categories:
taxa_breakdown <- c(90,9,19,3) #in order as listed below
taxa_groups <- c("Entomophilous (floral)","Poaceae/grasses","Trees/Woody Plants","Anemophilous herbaceous")
plot_labels <- c("Entomophilous (n = 90)","Poaceae/grasses (n = 9)","Trees/Woody Plants (n = 19)","Anemophilous herbaceous (n = 3)")
taxa.df <- data.frame(taxa_breakdown,taxa_groups,plot_labels)


#build a df with info for plotting
taxa.df <- taxa.df %>% 
  mutate(group = taxa_groups) %>% 
  mutate(n.taxa = taxa_breakdown) %>% 
  mutate(plot_labs = plot_labels) %>% 
  select(c(group,n.taxa,plot_labs)) %>%
  arrange(desc(group)) %>%  # sort in the order you want slices to appear
  mutate(
    group = factor(group, levels = unique(group)),  # fix factor levels order
    fraction = n.taxa / sum(n.taxa),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, -1)),
    labelPosition = (ymax + ymin) / 2,
    label = paste0(group, "\n", n.taxa))

tax_colors <- c("cyan4", "wheat", "forestgreen", "darkolivegreen3")

fig.gmb.fun.groups <- ggplot(taxa.df, aes(ymax = ymax, ymin = ymin, xmax = 5, xmin = 4.8, fill = group)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y") +
  xlim(c(2, 7)) +  # extend x limits further to give enough room for outside labels
  theme_void() +
  scale_fill_manual(values = tax_colors) +
  geom_text_repel(
    aes(x = 5.1, y = labelPosition - 0.02, label = plot_labs),
    family = "sans",
    size = 4,
    nudge_x = 0.2,
    segment.color = "white",
    direction = "y",
    hjust = 0,
    box.padding = 0.5,
    point.padding = 0.3) +
  ggtitle("A. Gut content metabarcoding") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.3, vjust = -3, face = "bold", size = 12, margin = margin(b = -25))) 


#Breakdown of functional groups in pollen metabarcoding results ----------------------------------

#I fed chat gpt the list of plant genera detected in metabarcoding results and asked it to classify each under one of 4 categories:
pmb.taxa_breakdown <- c(79,15,21,7) #in order as listed below
pmb.plot_labels <- c("Entomophilous (n = 79)","Poaceae/grasses (n = 15)","Trees/Woody Plants (n = 21)","Anemophilous herbaceous (n = 7)")
pmb.taxa.df <- data.frame(pmb.taxa_breakdown,taxa_groups,pmb.plot_labels)


pmb.taxa.df <- pmb.taxa.df %>% 
  mutate(group = taxa_groups) %>% 
  mutate(n.taxa = pmb.taxa_breakdown) %>% 
  mutate(plot_labs = pmb.plot_labels) %>% 
  select(c(group,n.taxa,plot_labs)) %>%
  arrange(desc(group)) %>%  # sort in the order you want slices to appear
  mutate(
    group = factor(group, levels = unique(group)),  # fix factor levels order
    fraction = n.taxa / sum(n.taxa),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, -1)),
    labelPosition = (ymax + ymin) / 2,
    label = paste0(group, "\n", n.taxa))


fig.pmb.fun.groups <- ggplot(pmb.taxa.df, aes(ymax = ymax, ymin = ymin, xmax = 5, xmin = 4.8, fill = group)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y") +
  xlim(c(2, 7)) +  # extend x limits further to give enough room for outside labels
  theme_void() +
  scale_fill_manual(values = tax_colors) +
  geom_text_repel(
    aes(x = 5.1, y = labelPosition - 0.02, label = plot_labs),
    family = "sans",
    size = 4,
    nudge_x = 0.2,
    segment.color = "white",
    direction = "y",
    hjust = 0,
    vjust = 2.5,
    box.padding = 0.5,
    point.padding = 0.3) +
  ggtitle("B. Corbicular pollen metabarcoding") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5, vjust = -3, face = "bold", size = 12, margin = margin(b = -25))) 


#Figure for supplemental content
fig.gmb.fun.groups + fig.pmb.fun.groups


#analysis of which taxa were observed by metabarcoding by period ----------------------------------
#this is an investigation of which taxa are responsible for the difference in number of taxa observed across methodologies and periods 


gmb.taxa.gap <- clean4stats.bp23.all.binary %>% 
  filter(method == "gut.metabarcoding")

pmb.taxa.gap <- clean4stats.bp23.all.binary %>% 
  filter(method == "pollen.metabarcoding")

smry.gmb.taxa.site <- gmb.taxa.gap %>%
  select(!period) %>%
  group_by(site) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop")

smry.gmb.taxa.period <- gmb.taxa.gap %>%
  select(!site) %>%
  group_by(period) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop")

smry.pmb.taxa.site <- pmb.taxa.gap %>%
  select(!period) %>%
  group_by(site) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop")

smry.pmb.taxa.period <- pmb.taxa.gap %>%
  select(!site) %>%
  group_by(period) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop")

#function for extracting deetected taxa names
get_nonzero_cols <- function(df, id_col = "ID") {
  setNames(
    lapply(df[[id_col]], function(this_id) {
      ix <- which(df[[id_col]] == this_id)
      if (length(ix) == 0) return(character(0))
      data_cols <- setdiff(colnames(df), c(id_col, "sample", "type","period", "site"))
      valid_cols <- data_cols[!is.na(df[ix, data_cols]) & df[ix, data_cols] != 0]
      valid_cols
    }),
    df[[id_col]]
  )
}

#results
gmb.taxa.sites <- get_nonzero_cols(smry.gmb.taxa.site, id_col = "site")
gmb.taxa.periods <- get_nonzero_cols(smry.gmb.taxa.period, id_col = "period")

pmb.taxa.sites <- get_nonzero_cols(smry.pmb.taxa.site, id_col = "site")
pmb.taxa.periods <- get_nonzero_cols(smry.pmb.taxa.period, id_col = "period")


#ask AI which of the resulting names are from anemophilous taxa - 

#Luisja suggested analysis representing functional groups in a boxplot within the bar

#guts
#p1 woody: (4) Betula, Fagus, Platanus, Quercus
#p1 grasses: (0) Grasses.
#p1 other an: (2) Plantago, Urtica.
#p1 ent: (43)
#6/50 #are anemophilous

#p2 woody: (3) Betula, Fagus, Quercus
#p2 grasses: (3) Holcus, Carex, Anthoxanthum.
#p2 other: (2) Plantago, Urtica.
#p2 ent: (45)
#7/59

#p3: (4) Betula, Fagus, Quercus, Juglans
#p3: (3) Aegilops, Arrhenatherum, Brachypodium.
#p3: (4) Plantago, Anthoxanthum, Sisymbrium, Pleuropterus.
#p3 ent: (39)
#11/55

#p4: (0) 
#p4: (3) Dactylis, Festuca, Holcus.
#p4: (4) Plantago, Urtica, Arenaria, Sisymbrium.
#p4 ent: (51)
#7/62

#p5: (1) Fagus
#p5: (0) Grasses.
#p5: (1) Plantago.
#p5 ent: (36)
#2/42

#p6: (3) Alnus, Juglans, Quercus
#p6: (9) Agrostis, Aegilops, Arrhenatherum, Brachypodium, Dactylis, Festuca, Holcus, Lolium, Poa.
#p6: (5) Plantago, Parietaria, Stellaria, Urtica, Raphanus.
#p6 ent: (68)
#17/93

proportion.anemophilous <- c(0.12, 0.12, 0.2, 0.11, 0.05, 0.18) #results of dividing the proportions above

gmb.p1 <- c(4,0,2,44)
gmb.p2 <- c(3,3,2,51)
gmb.p3 <- c(4,3,4,44)
gmb.p4 <- c(0,3,4,55)
gmb.p5 <- c(1,0,1,40)
gmb.p6 <- c(3,9,5,76)

taxa_groups2 <- c("Woody","Poaceae","Anemophilous_other","Entomophilous")
gmb.groups.x.period <- rbind(gmb.p1, gmb.p2, gmb.p3, gmb.p4, gmb.p5, gmb.p6)
colnames(gmb.groups.x.period) <- taxa_groups2
gmb.groups.x.period <- data.frame(period = rownames(gmb.groups.x.period), gmb.groups.x.period, row.names = NULL)
gmb.groups.x.period$all_anemophilous <-  rowSums(gmb.groups.x.period[,c(2,3,4)])
gmb.groups.x.period$total <-  rowSums(gmb.groups.x.period[,c(2,3,4,5)])
gmb.groups.x.period$prcnt_ane <-  gmb.groups.x.period$all_anemophilous/gmb.groups.x.period$total


gmb.groups.x.period_long <- pivot_longer(gmb.groups.x.period, cols = c(Woody, Poaceae, Anemophilous_other, Entomophilous), 
                        names_to = "group", values_to = "value")


gmb.groups.x.period_long$group <- factor(gmb.groups.x.period_long$group, 
                                         levels = c("Woody", "Poaceae", "Anemophilous_other","Entomophilous"))


custom_colors <- c(
  "Entomophilous" = "forestgreen",  
  "Woody" = "cyan4",              
  "Poaceae" = "wheat",              
  "Anemophilous_other" = "darkolivegreen3"    
)

gmb.plant.groups.x.period <- ggplot(gmb.groups.x.period_long, aes(x = period, y = value, fill = group)) +
  geom_bar(stat = "identity", alpha = 0.6, width = 0.3) +
  scale_fill_manual(values = custom_colors,
                    labels = c(
                      "Entomophilous" = "Entomophilous",
                      "Woody" = "Trees & Woody Plants",
                      "Poaceae" = "Grasses",
                      "Anemophilous_other" = "Anemophilous Herbaceous")) +
  labs(x = "Period", y = "Count", title = "Plant groups detected by metabarcoding of gut contents across sampling periods", fill = "Group") +
  xlab("Sampling Period") +
  ylab("Detected Plant Genera") +
  scale_x_discrete(labels= c("1","2","3","4","5","6")) +
  theme_minimal()

gmb.plant.groups.x.period

