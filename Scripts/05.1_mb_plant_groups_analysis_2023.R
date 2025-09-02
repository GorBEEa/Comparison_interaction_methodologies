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
 
