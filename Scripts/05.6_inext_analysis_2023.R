#Sampling completeness, rarefaction, extrapolation

#library(here)
#library(tidyverse)
#library(ggplot2)
library(patchwork)
library(iNEXT)


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

sampling_units_fig <- ggiNEXT(inext.all,
                              type = 1) #How does observed/estimated richness change as the number of informative sampling units increases?

sampling_completeness_fig <- ggiNEXT(inext.all,
                                     type = 3) #How does richness change as estimated sampling completeness increases?

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

