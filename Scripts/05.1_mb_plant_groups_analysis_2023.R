#Script to analyze groups of plant types relevant to pollinators detected by metabarcoding
#Perplexity used to identify which of metabarcoding taxa were anemophilpous, which from groups:
#grasses, woody plants, other herbaceous
#results organized here

#library(tidyverse)
#library(tidyr)
#library(tidyselect)
#library(ggplot2)
library(ggrepel)

#list fed to perplexity of all mb taxa:
#full_join(genus.hits.23, poln.genus.hits.2023, by = 'genus')

#returned
#9/172 #woody plants 5%
#15/172 #grasses or poaceae 9%
#7/172 #other herbaceous 4%
#leaves 141 entomophilous

taxa_breakdown <- c(141,15,9,7)
taxa_groups <- c("Entomophilous (floral)","Poaceae","Woody Plants","Anemophilous herbaceous")
plot_labels <- c("Entomophilous (n = 141)","Poaceae (n = 15)","Woody Plants (n = 9)","Anemophilous herbaceous (n = 7)")

taxa.df <- data.frame(taxa_breakdown,taxa_groups,plot_labels)

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
    label = paste0(group, "\n", n.taxa)
  )

tax_colors <- c("chocolate1", "lightskyblue1", "wheat3", "darkolivegreen3")

ggplot(taxa.df, aes(ymax = ymax, ymin = ymin, xmax = 5, xmin = 4.8, fill = group)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y") +
  xlim(c(2, 7)) +  # extend x limits further to give enough room for outside labels
  theme_void() +
  scale_fill_manual(values = tax_colors) +
  geom_text_repel(
    aes(x = 5.1, y = labelPosition, label = plot_labs),
    family = "sans",
    size = 4,
    nudge_x = 0.02,
    segment.color = "white",
    direction = "y",
    hjust = 0,
    box.padding = 0.5,
    point.padding = 0.3) +
  ggtitle("Pollinator interaction plant groups detected by metabarcoding methodologies (2023)") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.3, face = "bold", size = 12, margin = margin(b = -25))) 
 


#analysis of which taxa were observed by metabarcoding by period ----------------------------------
#this is an investigation of which taxa are responsible for the difference in number of taxa observed across methodologies and periods 


gmb.taxa.gap <- clean4stats.bp23.all.binary %>% 
  filter(method == "gut.metabarcoding")

pmb.taxa.gap <- clean4stats.bp23.all.binary %>% 
  filter(method == "pollen.metabarcoding")

smry.gmb.taxa.gap <- gmb.taxa.gap %>%
  select(!period) %>%
  group_by(site) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop")

smry.pmb.taxa.gap <- pmb.taxa.gap %>%
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
gmb.taxa.periods <- get_nonzero_cols(smry.gmb.taxa.gap, id_col = "site")
pmb.taxa.periods <- get_nonzero_cols(smry.pmb.taxa.gap, id_col = "period")



#ask AI which of the resulting names are from anemophilous taxa - 

#Luisja suggested analysis representing functional groups in a boxplot within the bar

#guts
#p1 woody: (5) Betula, Fagus, Platanus, Quercus, Salix
#p1 grasses: (0) Grasses.
#p1 other an: (2) Plantago, Urtica.
#p1 ent: (43)

#p2 woody: (9) Betula, Fagus, Eucalyptus, Prunus, Pyrus, Quercus, Robinia, Sambucus, Salix.
#p2 grasses: (3) Holcus, Carex, Anthoxanthum.
#p2 other: (2) Plantago, Urtica.
#p2 ent: (45)

#p3: (8) Betula, Fagus, Quercus, Ilex, Juglans, Prunus, Eucalyptus, Crataegus
#p3: (3) Aegilops, Arrhenatherum, Brachypodium.
#p3: (4) Plantago, Anthoxanthum, Sisymbrium, Pleuropterus.
#p3 ent: (39)

#p4: (4) Actinidia, Crataegus, Rosa, Sambucus,
#p4: (3) Dactylis, Festuca, Holcus.
#p4: (4) Plantago, Urtica, Arenaria, Sisymbrium.
#p4 ent: (51)

#p5: (4) Castanea, Citrus, Eucalyptus, Fagus
#p5: (0) Grasses.
#p5: (1) Plantago.
#p5 ent: (37)

#p6: (11) Alnus, Castanea, Crataegus, Citrus, Eucalyptus, Ilex, Juglans, Prunus, Quercus, Robinia, Sambucus
#p6: (9) Agrostis, Aegilops, Arrhenatherum, Brachypodium, Dactylis, Festuca, Holcus, Lolium, Poa.
#p6: (5) Plantago, Parietaria, Stellaria, Urtica, Raphanus.
#p6 ent: (50)

periods <- c(1,2,3,4,5,6)
methods <- c("Interactions Transects", "Flower Count", "Pollen Metabarcoding", "Gut Content Metabarcoding")
type.methods.period <- 

