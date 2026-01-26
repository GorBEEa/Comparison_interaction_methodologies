#Simplified versions of diversity detected by different methodologies over
#periods
#Figures used to visually step up to Fig. 2 of manuscript in conference presentation

#to proceed:
#load(here("Data/05_output.RData"))


#Flower Count by period ------------------------------------------

fig.fc.x.periods <- ggplot(mean.fc.taxa.periods, aes(period, mean.genera)) + 
  geom_col(alpha = 0.8,  width = 0.3, fill = "slategrey") + 
  theme_minimal() + 
  xlab("Sampling Period") +
  ylab("Number of Genera") +
  scale_x_continuous(breaks = 1:6, labels = 1:6) + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank()) +
  ggtitle("Flowering Herbaceous Plant Diversity in Transects")
 




#Interaction methodologies by period -------------------------------
int.gen.x.periods <- long.gen.by.periods %>% filter(method != "n.genera.fc")
  
fig.int.methods.x.periods <- ggplot(int.gen.x.periods, aes(period, n.genera, fill = method)) + 
  geom_col(position = "Dodge", alpha = 0.8) + 
  theme_minimal() + 
  xlab("Sampling Period") +
  ylab("Number of Genera") +
  scale_x_continuous(breaks = 1:6, labels = 1:6) + 
  scale_fill_manual(values = method.colors, labels = c(
    "n.genera.int" = "Interactions Transects",
    "n.genera.pmb" = "Pollen Metabarcoding",
    "n.genera.gmb" = "Gut Content Metabarcoding")) +
  labs(fill = "Methodology", color = NULL , linetype = NULL) +
  theme(plot.title = element_text(hjust = 0.6),
        axis.ticks.x = element_blank()) +
  ggtitle("Interaction diversity by methodology")
 




#Manuscript Figure 2: period breakdown of diversity by all surveys ---------------

fc.line <- mean.lines %>% filter(LineType == "Flower Count")

png(here("docs/manuscript_figures/interaction.diversity.periods.png"), width =2000, height = 1000, res = 300) 

fig.methods.w.fc <- ggplot(int.gen.x.periods, aes(period, n.genera, fill = method)) + 
  geom_col(position = "Dodge", alpha = 0.8) + 
  theme_minimal() + 
  xlab("Sampling Period") +
  ylab("Number of Genera") +
  scale_x_continuous(breaks = 1:6, labels = 1:6) + 
  scale_fill_manual(values = method.colors, labels = c(
    "n.genera.int" = "Interactions transects",
    "n.genera.pmb" = "Pollen metabarcoding",
    "n.genera.gmb" = "Gut-content metabarcoding")) +
  labs(fill = "Methodology", color = NULL , linetype = NULL) +
  theme(plot.title = element_text(hjust = 0.6),
        axis.ticks.x = element_blank()) + 
  geom_line(
    data = fc.line,
    aes(x = period, y = mean.genera, linetype = "Flower Count"),
    color = "slategrey",
    linewidth = 1,
    inherit.aes = FALSE) +
  scale_linetype_manual(values = c("Flower Count" = "dotdash"))

fig.methods.w.fc

dev.off()






#Just gut metabarcoding and flower count by period -------------------

gmb.fc.periods <- long.gen.by.periods %>% filter(method %in% c("n.genera.fc", "n.genera.gmb"))

fig.gmb.fc <- ggplot(gmb.fc.periods, aes(period, n.genera, fill = method)) + 
  geom_col(position = "Dodge", alpha = 0.8, width = 0.5) + 
  theme_minimal() + 
  xlab("Sampling Period") +
  ylab("Number of Genera") +
  scale_x_continuous(breaks = 1:6, labels = 1:6) + 
  scale_fill_manual(values = method.colors, labels = c(
    "n.genera.fc" = "Flower Count",
    "n.genera.gmb" = "Gut Content Metabarcoding")) +
  labs(fill = "Methodology", color = NULL , linetype = NULL) +
  theme(plot.title = element_text(hjust = 0.6),
        axis.ticks.x = element_blank()) +
  ggtitle("Interaction diversity by methodology") 


















