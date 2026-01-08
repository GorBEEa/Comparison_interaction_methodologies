#Plant check
#Which taxa detected in metabarcoding are NOT present in Gorbeia
#as far as we know

#Generate a list of plant genera present in Gorbeia (from Macizo de Gorbeia blog)
gorbeia_plants <- readLines(here("Data/all_gorbeia_plants.txt"))
xtract_genus <- sub(" .*", "", gorbeia_plants)
gorbeia_genera <- tibble(genus = xtract_genus) 
gorbeia_genera <- unique(gorbeia_genera)

#Which gut metabarcoding genera are also present in Gorbeia?
gut_mb_genera <- genus.hits.23 #make a copy
gut_mb_genera$in.gorbeia <- as.integer(gut_mb_genera$genus %in% gorbeia_genera$genus) #presence absence comparison
sus_gut_mb_genera <- gut_mb_genera %>% filter(in.gorbeia == 0)

#Which pollen metabarcoding genera are also present in Gorbeia?
poln_mb_genera <- poln.genus.hits.2023
poln_mb_genera$in.gorbeia <- as.integer(poln_mb_genera$genus %in% gorbeia_genera$genus) #presence absence comparison
sus_poln_mb_genera <- poln_mb_genera %>% filter(in.gorbeia == 0)

#Consolidate list of red flag metabarcoding detected taxa 
its2_sus_genera <- full_join(sus_gut_mb_genera,sus_poln_mb_genera, by = "genus")
its2_sus_genera <- its2_sus_genera$genus
write.csv(its2_sus_genera, here("Data/its2_sus_genera.csv"))

#pass csv to Xabi, Brais, etc. plant boys to confirm species to keep or eliminate

load(file = here("Data/known.misIDs.RData")) #Not sure when or where this was created, but it is good to have. Here you can add on to it

known.misIDs <- append(known.misIDs, "Spondias")

save(known.misIDs, file = here("Data/known.misIDs.RData"))
