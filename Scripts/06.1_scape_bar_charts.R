fig.fc.x.periods <- ggplot(mean.fc.taxa.periods, aes(period, mean.genera)) + 
  geom_col(alpha = 0.8,  width = 0.3, fill = "slategrey") + 
  theme_minimal() + 
  xlab("Sampling Period") +
  ylab("Number of Genera") +
  scale_x_continuous(breaks = 1:6, labels = 1:6) + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank()) +
  ggtitle("Flowering Herbaceous Plant Diversity in Transects")
 
fig.fc.x.periods






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
 
fig.int.methods.x.periods






fc.line <- mean.lines %>% filter(LineType == "Flower Count")

fig.methods.w.fc <- ggplot(int.gen.x.periods, aes(period, n.genera, fill = method)) + 
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
  ggtitle("Interaction diversity by methodology") + 
  geom_line(
    data = fc.line,
    aes(x = period, y = mean.genera, linetype = "Flower Count"),
    color = "slategrey",
    linewidth = 1,
    inherit.aes = FALSE) +
  scale_linetype_manual(values = c("Flower Count" = "dotdash"))
  

fig.methods.w.fc




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

fig.gmb.fc


















