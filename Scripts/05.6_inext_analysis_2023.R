#Sampling completeness, rarefaction, extrapolation

library(here)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(iNEXT)


load(here("Data/05_output.RData"))


#bring together binary presence absence data from interactions and metabarcoding into one table
int23.table4.inext <- bp23.int4stats.wide.binary %>%
  mutate(sample_id = paste("p", period, "s", site)) %>%
  select(sample_id, everything(), -period, -site, -method) %>%
  column_to_rownames("sample_id") %>%
  as.matrix()
int23.table4.inext <- t(int23.table4.inext)
int23.inext.list <- list(interactions23 = int23.table4.inext)

  
gut23.table4.inext <- bp23.genomic.binary %>% 
  select(!c(period, site, year, specimen, color_p, color_s, type, plate, quant_reading, is.neg, genera.by.indiv)) %>% 
  column_to_rownames("sample") %>%
  as.matrix()
gut23.table4.inext <- t(gut23.table4.inext)
gut23.inext.list <- list(gut23_mb = gut23.table4.inext)



poln23.table4.inext <- poln.genomic.binary.2023 %>% 
  select(!c(period, site, year, color_p, color_s, type, conc, is.neg, genera.by.indiv)) %>%
  column_to_rownames("sample") %>%
  as.matrix()
poln23.table4.inext <- t(poln23.table4.inext)
poln23.inext.list <- list(poln23_mb = poln23.table4.inext)


#run analyses -------------------------------------------

int23.inext <- iNEXT(int23.inext.list,
           q = 0,
           datatype = "incidence_raw")

gut23.inext <- iNEXT(gut23.inext.list,
                     q = 0,
                     datatype = "incidence_raw")

poln23.inext <- iNEXT(poln23.inext.list,
                     q = 0,
                     datatype = "incidence_raw")


# combined analysis -------------------------------------
inext.all.list <- list(
  interactions23 = int23.table4.inext,
  gut23_mb = gut23.table4.inext,
  poln23_mb = poln23.table4.inext
)

inext.all <- iNEXT(
  inext.all.list,
  q = 0,
  datatype = "incidence_raw"
)

#look at results 

inext.all$DataInfo

#Figure generation

method.colors.inext <- c("interactions23" = "lightblue",
                         "poln23_mb" = "goldenrod1",
                         "gut23_mb" = "forestgreen")
method_labels_inext <- c("interactions23" = "Visitation observations",
                         "gut23_mb" = "Gut-content metabarcoding",
                         "poln23_mb" = "Corbicular pollen metabarcoding")



sampling_units_fig <- ggiNEXT(inext.all, type = 1) + #How does observed/estimated richness change as the number of informative sampling units increases?
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 50)) +
  ggtitle("A.") + 
  ylab("Number of plant genera") +
  scale_color_manual(
    name = "Sampling method",
    values = method.colors.inext,
    breaks = names(method.colors.inext),
    labels = method_labels_inext
  ) +
  scale_shape_manual(
    name = "Sampling method",
    values = c(
      "gut23_mb" = 17,
      "interactions23" = 15,
      "poln23_mb" = 3
    ),
    breaks = names(method.colors.inext),
    labels = method_labels_inext
  ) +
  scale_fill_manual(
    values = method.colors.inext,
    guide = "none") +
  theme_minimal() +
  theme(legend.position = "none")
  
  
sampling_completeness_fig <- ggiNEXT(inext.all, type = 3) + #How does richness change as estimated sampling completeness increases?
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 50)) +
  ggtitle("B.") + 
  ylab(NULL) +
  scale_color_manual(
    name = "Sampling method",
    values = method.colors.inext,
    breaks = names(method.colors.inext),
    labels = method_labels_inext
  ) +
  scale_shape_manual(
    name = "Sampling method",
    values = c(
      "gut23_mb" = 17,
      "interactions23" = 15,
      "poln23_mb" = 3
    ),
    breaks = names(method.colors.inext),
    labels = method_labels_inext
  ) +
  scale_fill_manual(
    values = method.colors.inext,
    guide = "none")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.y = element_blank())

legend_combined_plot <- cowplot::get_legend(
  sampling_units_fig +
    theme(legend.position = "top"))

#figure for all results, legend is not coming out nicely
(sampling_units_fig + sampling_completeness_fig) /
  legend_combined_plot +
  plot_layout(heights = c(1, 0.25))

#unpack results and look at specific rarefaction/extrapolation scenarios
target_SC <- c(0.74, 0.76, 0.78, 0.80, 0.82, 0.84, 0.86)

coverage_data <- inext.all$iNextEst$coverage_based %>%
  filter(Order.q == 0) %>%
  select(
    Assemblage,
    SC,
    qD,
    qD.LCL,
    qD.UCL
  ) %>%
  filter(!is.na(SC), !is.na(qD))

coverage_table <- coverage_data %>%
  group_by(Assemblage) %>%
  arrange(SC) %>%
  reframe(
    target_SC = target_SC,
    qD = approx(
      x = SC,
      y = qD,
      xout = target_SC,
      rule = 1
    )$y,
    qD.LCL = approx(
      x = SC,
      y = qD.LCL,
      xout = target_SC,
      rule = 1
    )$y,
    qD.UCL = approx(
      x = SC,
      y = qD.UCL,
      xout = target_SC,
      rule = 1
    )$y
  ) %>%
  ungroup()

coverage_table_wide <- coverage_table %>%
  pivot_wider(
    names_from = Assemblage,
    values_from = c(qD, qD.LCL, qD.UCL)
  ) %>%
  arrange(target_SC)



#unpack results and look at specific rarefaction/extrapolation scenarios
target_t <- c(25, 34, 126)

size_data <- inext.all$iNextEst$size_based %>%
  filter(Order.q == 0) %>%
  select(
    Assemblage,
    t,
    qD,
    qD.LCL,
    qD.UCL
  ) %>%
  filter(!is.na(t), !is.na(qD))

size_table <- size_data %>%
  group_by(Assemblage) %>%
  arrange(t) %>%
  reframe(
    target_t = target_t,
    qD = approx(
      x = t,
      y = qD,
      xout = target_t,
      rule = 1
    )$y,
    qD.LCL = approx(
      x = t,
      y = qD.LCL,
      xout = target_t,
      rule = 1
    )$y,
    qD.UCL = approx(
      x = t,
      y = qD.UCL,
      xout = target_t,
      rule = 1
    )$y
  ) %>%
  ungroup()

size_table_wide <- size_table %>%
  pivot_wider(
    names_from = Assemblage,
    values_from = c(qD, qD.LCL, qD.UCL)
  ) %>%
  arrange(target_t)

