#Script for looking at individual level differences in taxa detected by 2 metabarcoding methodologies


#library(here)
#library(tidyverse)
#library(tidyr)
#library(tidyselect)

poln.2023.indv <- poln.2023.genomic.specs #copy the existing pollen mb data and clean it up for this analysis
poln.2023.indv$ID <- sub('^(Y\\d{8}P).*', '\\1', poln.2023.genomic.specs$sample) #get clean IDs
poln.2023.indv <- poln.2023.indv %>% relocate(ID) %>% 
  select(!c(sample, year, period, site, specimen, color_p, color_s, type, plate, quant_reading, is.neg)) %>% 
  mutate(type = rep("pollen", 25))%>% 
  relocate(type, .after = ID)

gut.2023.indv <- bp23.genomic.analys #copy the existing gut mb data and clean it up for this analysis
gut.2023.indv <- gut.2023.indv %>% 
  select(!c(year, period, site, specimen, color_p, color_s, type, plate,
            quant_reading, is.neg, intertegular_dist_mm, abdomen_length_mm, total_length_mm)) %>% 
  mutate(type = rep("gut", 126)) %>% 
  relocate(type, .after = sample)


IDS <- as.data.frame(read.csv2(here("Data/pollen_gut_id_match.csv"), sep = ",")) #create a df for connecting samples from both methodologies
IDS <- IDS %>% 
  rename(sample = Bombus_sample) %>% 
  rename(ID = sample_ID)

gut.w.poln.2023 <- left_join(IDS,gut.2023.indv, by = "sample")

#Use these lines of code to check by ID
poln.2023.indv$ID #list of IDs
colnames(poln.2023.indv)[poln.2023.indv$ID == "Y23060301P" & poln.2023.indv != 0] #taxa in pollen mb for individual x
colnames(gut.w.poln.2023)[gut.w.poln.2023$ID == "Y23060301P" & gut.w.poln.2023 != 0] #taxa in gut mb for individual x
