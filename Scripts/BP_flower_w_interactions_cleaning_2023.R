###################
#Working with Transect Plants Interactions
##################

#this script is called in Interaction_Data.R and this copy is for use with that script and analysis


data.f <- bp.interactions.2023
#View(data.f)
#colnames(data.f)
#unique(data.f[c("Planta")])

library(tidyverse)
library(stringr)
data.f %>%
  pull(Planta) %>%
  unique %>%
  sort

#abbreviation clean ups
data.f$Planta <- str_replace(data.f$Planta, "sp", "sp.")
data.f$Planta <- str_replace(data.f$Planta, "sp..", "sp.")
data.f$Planta <- str_replace(data.f$Planta, "sp. ", "sp.")
data.f$Planta <- str_replace(data.f$Planta, "cf", "cf.")
data.f$Planta <- str_replace(data.f$Planta, "c.f.", "cf.")
data.f$Planta <- str_replace(data.f$Planta, "cf. ", "cf.")
data.f$Planta <- str_replace(data.f$Planta, "cf..", "cf.")
data.f$Planta <- str_replace(data.f$Planta, "cf. ", "cf.")
data.f$Planta <- str_replace(data.f$Planta, "sp.cf.", "sp. cf.")


#Corrections going alphabetical order
####################################

#temp name
data.f$Planta <- ifelse(data.f$Planta %in% c('\"5pet amari\"'), 'Hypericum humifusum', data.f$Planta)
#Poza uses this code and similars several times -> Hypericum humifusum confirmed

#ask jon
# He uses this name several times, he doesn't remember which species he was refering to
# It was not Lamium galeobdolon
data.f$Planta <- ifelse(data.f$Planta %in% c('\"Ajuga\" amarilla', 'Ajuga sp.amarilla'), 'SI (tipo Ajuga amarilla)', data.f$Planta)

#confirmed species
data.f$Planta <- ifelse(data.f$Planta %in% c('\"Hutsinsia alta\"', 'Arabis aplina'), 'Arabis alpina', data.f$Planta)

#p5s4 same species
data.f$Planta <- ifelse(data.f$Planta %in% c('Acinos alpinus'), 'Satureja alpina ssp. pyrenaea', data.f$Planta)

#spelling, confirmed cf
data.f$Planta <- ifelse(data.f$Planta %in% c('Ajuga reptans cf.'), 'Ajuga reptans', data.f$Planta)

#spelling
data.f$Planta <- ifelse(data.f$Planta %in% c('Anthillys vulneraria'), 'Anthyllis vulneraria', data.f$Planta)

#look at 41310 specimen andrena for Asteraceae sp s12p4 ?
#View(data.f[data.f$Planta == 'Asteraceae sp.',])
data.f$Planta[data.f$Planta == 'Asteraceae sp.' & data.f$Sitio == '16' & data.f$Periodo == '4'] <- 'Pilosella lactucella'
data.f$Planta[data.f$Planta == 'Asteraceae sp.' & data.f$Sitio == '13' & data.f$Periodo == '4'] <- 'Taraxacum officinale'

#spelling, confirmed cf
data.f$Planta <- ifelse(data.f$Planta %in% c('Betonica officinalis cf.'), 'Betonica officinalis', data.f$Planta)

#p4s12 Campanula?
##View(data.f[data.f$Planta == 'Campana arenaria',])
## Acording to poza could be Convallaria majalis
data.f$Planta <- ifelse(data.f$Planta %in% c('Campana arenaria'), 'Convallaria majalis', data.f$Planta)

#p6s2 Campanula sp. Ask Jon
##View(data.f[data.f$Planta == 'Campanilla morada',])
data.f$Planta <- ifelse(data.f$Planta %in% c('Campanilla morada'), 'Campanula sp.', data.f$Planta)

#spelling s5p6
data.f$Planta <- ifelse(data.f$Planta %in% c('Campanula rotundifolia cf.'), 'Campanula scheuchzeri', data.f$Planta)
data.f$Planta <- ifelse(data.f$Planta %in% c('Campanula hisp.nica'), 'Campanula hispanica', data.f$Planta)

#View(data.f[data.f$Planta == 'Campanula sp.',])
#P5 Site 13 Confirmation needed, Photo in Drive, Campanula glomerata
data.f$Planta[data.f$Planta == 'Campanula sp.' & data.f$Sitio == '13' & data.f$Periodo == '5'] <- 'Campanula glomerata'
#p6s10, Photo in Drive, Campanula glomerata
data.f$Planta[data.f$Planta == 'Campanula sp.' & data.f$Sitio == '10' & data.f$Periodo == '6'] <- 'Campanula glomerata'

#spelling
data.f$Planta <- ifelse(data.f$Planta %in% c('Capsella bursa'), 'Capsella bursa-pastoris', data.f$Planta)

#Sitio 11, Periodo 4 Cirsium sp.
data.f$Planta <- ifelse(data.f$Planta %in% c('Carduus sp. cf.'), 'Cirsium sp.', data.f$Planta)

#Written this way on Vegeticion list, Carthamus is a basionym
data.f$Planta <- ifelse(data.f$Planta %in% c('Carthamus mitissimus'), 'Carduncellus mitissimus', data.f$Planta)

#confirmed
data.f$Planta[data.f$Planta == 'Cerastium sp.' & data.f$Sitio == '13' & data.f$Periodo == '5'] <- 'Cerastium fontanum'

#name
data.f$Planta <- ifelse(data.f$Planta %in% c('Cirsium eriophorum'), 'Cirsium eriophorum ssp. richterianum', data.f$Planta)

#p5s14 Cirsium not id in flower count, but given C. vulgare in transect.. not correct
data.f$Planta[data.f$Planta == 'Cirsium vulgare' & data.f$Sitio == '14' & data.f$Periodo == '5'] <- 'Cirsium sp.'
#Photos in Drive under p6 s10, ID needs second confirmation, Sitio 10 sp 2 Cirsium eriophorum and sp 1 Cirsium vulgare
#Brais confirms:
#View(data.f[data.f$Planta == 'Cirsium sp.1',])
#View(data.f[data.f$Planta == 'Cirsium sp.2',])
data.f$Planta[data.f$Planta == 'Cirsium sp.1'] <- 'Cirsium vulgare'
data.f$Planta[data.f$Planta == 'Cirsium sp.2'] <- 'Cirsium eriophorum ssp. richterianum'
#Cirsium sp.
data.f$Planta[data.f$Planta == 'Cirsium sp.' & data.f$Sitio == '14'] <- 'Cirsium eriophorum ssp. richterianum'

#Cardo -> Cirsium sp.
data.f$Planta <- ifelse(data.f$Planta %in% c('Cordo sp.'), 'Cirsium sp.', data.f$Planta)

#spelling
data.f$Planta <- ifelse(data.f$Planta %in% c('Colchicum montanm'), 'Colchicum montanum', data.f$Planta)

#p4s6
#View(data.f[data.f$Planta == 'Cruciata sp. cf.',])
data.f$Planta <- ifelse(data.f$Planta %in% c('Cruciata sp. cf.'), 'Cruciata laevipes', data.f$Planta)

#p4s8 can we check to confirm 
#View(data.f[data.f$Planta == 'Erica tetralix cf.',])
# Erica tetralix in flower count
data.f$Planta <- ifelse(data.f$Planta %in% c('Erica tetralix cf.'), 'Erica tetralix', data.f$Planta)

#spacing
data.f$Planta <- ifelse(data.f$Planta %in% c('Erica vagans '), 'Erica vagans', data.f$Planta)

#View(data.f[data.f$Planta == 'Erodium sp.',])
#p2s10 in flower count
#p2s12 in flower count and one interaction
#No confirmed sp.
data.f$Planta <- ifelse(data.f$Planta %in% c('Erodium sp.'), 'Erodium sp.', data.f$Planta)

#I believe this is all the same species
#View(data.f[data.f$Planta == 'Eryngium bourgatii',])
#s14 Poza escribe Eryngium cf., creo que se refiere a Cirsium sp. por las fotos que tiene del p6
data.f$Planta[data.f$Planta == 'Eryngium bourgatii' & data.f$Sitio == '14'] <- 'Cirsium sp.'
#En s14 p6 en el conteo anota 3 "Eryngium" supuestamente distintos
#En dos de ellos observa interacciones
#View(data.f[data.f$Planta == 'Eryngium sp.',])
#View(data.f[data.f$Planta == 'Eryngium sp.2',])
data.f$Planta <- ifelse(data.f$Planta %in% c('Eryngium sp.', 'Eryngium sp.2'), 'Cirsium sp.', data.f$Planta)
## Quiza Cirsium vulgare y/o Cirsium eriophorum ? Además, una de las fotos de Poza PlantNet la ID como Carduus defloratus...
#Preguntar opinión Brais ?

#View(data.f[data.f$Planta == 'Euphorbia sp.',])
#Euphorbia dulcis in flower count
data.f$Planta <- ifelse(data.f$Planta %in% c('Euphorbia sp.'), 'Euphorbia dulcis', data.f$Planta)
#spelling
data.f$Planta <- ifelse(data.f$Planta %in% c('Euphorbia dulce'), 'Euphorbia dulcis', data.f$Planta)

#confirmed spellings for Chamaemelum nobile
data.f$Planta <- ifelse(data.f$Planta %in% c('Fake manzania', 'Manzalnilla', 'Manzania sp.', 'Manzanilla', 'Manzanilla sp.', 'Manzanita falso', 'Manzanita sp.'), 'Chamaemelum nobile', data.f$Planta)

#s8s9 maybe H. rad, Ask Jon, confirmed
#View(data.f[data.f$Planta == 'Falso Taraxacum sp.',])
data.f$Planta <- ifelse(data.f$Planta %in% c('Falso Taraxacum sp.'), 'Hypochaeris radicata', data.f$Planta)

#spelling, but check to narrow down to species level?
#Galium sp. photo in S11 in google drive (P4)
data.f$Planta <- ifelse(data.f$Planta %in% c('Gallium sp.'), 'Galium sp.', data.f$Planta)
#could not be concluded
data.f$Planta <- ifelse(data.f$Planta %in% c('Galium sp.2'), 'Galium sp.', data.f$Planta)
data.f$Planta[data.f$Planta == 'Galium sp.' & data.f$Sitio == '5' & data.f$Periodo == '4'] <- 'Galium saxatile'
data.f$Planta[data.f$Planta == 'Galium sp.' & data.f$Sitio == '11' & data.f$Periodo == '4'] <- 'Galium saxatile'
data.f$Planta[data.f$Planta == 'Galium sp.' & data.f$Sitio == '13' & data.f$Periodo == '4'] <- 'Galium pinetorum'
data.f$Planta[data.f$Planta == 'Galium sp.' & data.f$Sitio == '3' & data.f$Periodo == '5'] <- 'Galium pinetorum'
data.f$Planta[data.f$Planta == 'Galium sp.' & data.f$Sitio == '12' & data.f$Periodo == '5'] <- 'Galium pinetorum'
data.f$Planta[data.f$Planta == 'Galium sp.' & data.f$Sitio == '14' & data.f$Periodo == '5'] <- 'Galium aparine'

#Galium, but revisit paperwork abundance vs transect: Galium pinetorum, saxatile, Galium verum cf.?
#Galium sp. in S11 has photos in Drive (Periodo 4)
#View(data.f[data.f$Planta == 'Galium sp.',])
#View(data.f[data.f$Planta == 'Galium sp.2',])

#add subpecies, listed throughout paperwork
data.f$Planta <- ifelse(data.f$Planta %in% c('Genista hisp.nica', 'Genista hispanica'), 'Genista hispanica ssp. occidentalis', data.f$Planta)

#View(data.f[data.f$Planta == 'Globularia sp.',])
#Periodo 4, Sitio 11, Photo in drive -> Jasione laevis 
#Globularia sp. is found in Sitio 1, no interactions
data.f$Planta <- ifelse(data.f$Planta %in% c('Globularia sp.'), 'Jasione laevis', data.f$Planta)

#spelling
data.f$Planta <- ifelse(data.f$Planta %in% c('Helianthemum sp.', 'Helianthemum nummularim'), 'Helianthemum nummularium', data.f$Planta)
data.f$Planta <- ifelse(data.f$Planta %in% c('Hippocrepis nomosa'), 'Hippocrepis comosa', data.f$Planta)
data.f$Planta <- ifelse(data.f$Planta %in% c('Hutchinisia alpina', 'Hutchsinsia alpina'), 'Hutchinsia alpina', data.f$Planta)

#View(data.f[data.f$Planta == 'Jacobaea sp.',])
#p6s3 it looks like Jacobaea aquatica (a.k.a. Senecio aquaticus) but id unconfirmed, Photo in Drive
data.f$Planta <- ifelse(data.f$Planta %in% c('Jacobaea sp.'), 'Jacobaea sp. cf.', data.f$Planta)

#Sitio 11 is Jasione laevis according to biomass records
#View(data.f[data.f$Planta == 'Jasione sp.',])
data.f$Planta[data.f$Planta == 'Jasione sp.' & data.f$Sitio == '11'] <- 'Jasione laevis'
#View(data.f[data.f$Planta == 'Jasione montana',])
data.f$Planta[data.f$Planta == 'Jasione montana' & data.f$Sitio == '5'] <- 'Jasione laevis'

#confirmed
data.f$Planta <- ifelse(data.f$Planta %in% c('Lamium purpureum cf.'), 'Lamium purpureum', data.f$Planta)

#View(data.f[data.f$Planta == 'Lamiaceae sp.1',])
#Comment "Lamiaceae amarilla" -> Lamium galeobdolon
data.f$Planta <- ifelse(data.f$Planta %in% c('Lamiaceae sp.1'), 'Lamium galeobdolon', data.f$Planta)

#spelling
data.f$Planta <- ifelse(data.f$Planta %in% c('Lathyrus linofolius'), 'Lathyrus linifolius', data.f$Planta)

#could it be narrowed down to one species -> yes
data.f$Planta <- ifelse(data.f$Planta %in% c('Medicago sp.'), 'Medicago lupulina', data.f$Planta)

#name change (Synonymy)
data.f$Planta <- ifelse(data.f$Planta %in% c('Merendera montana'), 'Colchicum montanum', data.f$Planta)

#spacing, but can we narrow it to species
data.f$Planta <- ifelse(data.f$Planta %in% c('Myositis sp.', 'Myosotis sp.'), 'Myosotis lamottiana', data.f$Planta)

#spelling
data.f$Planta <- ifelse(data.f$Planta %in% c('Narcissus pseudonarcissus subs. palidiflorus'), 'Narcissus pseudonarcissus ssp. pallidiflorus', data.f$Planta)
data.f$Planta <- ifelse(data.f$Planta %in% c('Origanum vulgaris'), 'Origanum vulgare', data.f$Planta)

#spelling
data.f$Planta <- ifelse(data.f$Planta %in% c('Potentiila erecta'), 'Potentilla erecta', data.f$Planta)
data.f$Planta <- ifelse(data.f$Planta %in% c('Potentilla sterillis'), 'Potentilla sterilis', data.f$Planta)

#to do later, see if can narrow down to species level at certain sites ?
#R. repens in S15p4 may be other species?
data.f$Planta <- ifelse(data.f$Planta %in% c('Ranuculus sp.', 'Ranunculus sp. '), 'Ranunculus sp.', data.f$Planta)
#Come back to site 10 period 5 to determine Ranunculus repens or tuberous?
data.f$Planta <- ifelse(data.f$Planta %in% c('Ranuculus repens', 'Ranunculus tuberous'), 'Ranunculus sp.', data.f$Planta)
#confirmed
data.f$Planta[data.f$Planta == 'Ranunculus sp.' & data.f$Sitio == '4' & data.f$Periodo == '4'] <- 'Ranunculus tuberosus'
data.f$Planta[data.f$Planta == 'Ranunculus sp.' & data.f$Sitio == '3' & data.f$Periodo == '5'] <- 'Ranunculus tuberosus'
data.f$Planta[data.f$Planta == 'Ranunculus sp.' & data.f$Sitio == '16' & data.f$Periodo == '5'] <- 'Ranunculus repens'
data.f$Planta[data.f$Planta == 'Ranunculus repens' & data.f$Sitio == '10' & data.f$Periodo == '6'] <- 'Ranunculus tuberosus'
#check with data?
data.f$Planta[data.f$Planta == 'Ranunculus repens' & data.f$Sitio == '3' & data.f$Periodo == '6'] <- 'Ranunculus sp.'

#R. ficaria p1s14.. paperworks says sp but others during the period were labelled ficaria? Ask Jon
#data.f$Planta <- ifelse(data.f$Planta %in% c('Ranuculus ficaria'), 'Ranunculus sp.', data.f$Planta)
#Jon doesm't remember, better as sp.

#space
data.f$Planta <- ifelse(data.f$Planta %in% c('Ranunculus repens '), 'Ranunculus repens', data.f$Planta)

#p3s5 ask Brais? 
data.f$Planta[data.f$Planta == 'Ranunculus repens' & data.f$Sitio == '5' & data.f$Periodo == '3'] <- 'Ranunculus sp.'

#spelling
data.f$Planta <- ifelse(data.f$Planta %in% c('Ranunculus tuberous'), 'Ranunculus tuberosus', data.f$Planta)

#can we confirm id, sitio 7, photo in drive under period 4, site 7. it was a solo individual by water stream, also Xabier said it was observed in sitio 9
# Yes
data.f$Planta <- ifelse(data.f$Planta %in% c('Ranunculus flammula cf.'), 'Ranunculus flammula', data.f$Planta)

#all just one species
data.f$Planta <- ifelse(data.f$Planta %in% c('Rubus sp.'), 'Rubus ulmifolius', data.f$Planta)

#View(data.f[data.f$Planta == 'Saxifraga sp.',])
#p3s14 could be S. hirsuta based on flower count, but not sure ?
#data.f$Planta <- ifelse(data.f$Planta %in% c('Saxifraga sp.'), 'Saxifraga sp.', data.f$Planta)

#confirmed
data.f$Planta <- ifelse(data.f$Planta %in% c('Scorzonera sp.'), 'Scorzonera humilis', data.f$Planta)

#s14p6 check to see if it can be narrowed down to species
data.f$Planta <- ifelse(data.f$Planta %in% c('Scrophularia sp.'), 'Scrophularia alpestris', data.f$Planta) #alpestris
data.f$Planta <- ifelse(data.f$Planta %in% c('Scrophularia sp.2'), 'SI (Scrophularia cf. blanca)', data.f$Planta) #blanca maybe Teucrium scorodonia
data.f$Planta <- ifelse(data.f$Planta %in% c('Scrophularia sp.3'), 'Scrophularia alpestris', data.f$Planta) #alpestris

#View(data.f[data.f$Planta == 'Sedum sp.',])
#can it be narrowed down to species?
#p5 s8 ID flower count
data.f$Planta[data.f$Planta == 'Sedum sp.' & data.f$Sitio == '8' & data.f$Periodo == '5'] <- 'Sedum anglicum'
#p5 s13, pictuer Jenn, Sedum anglicum
data.f$Planta[data.f$Planta == 'Sedum sp.' & data.f$Sitio == '13' & data.f$Periodo == '5'] <- 'Sedum anglicum'
#p5 s14, comment says Sedum sp.3 -> Sedum album in flower count
data.f$Planta[data.f$Planta == 'Sedum sp.' & data.f$Sitio == '14' & data.f$Periodo == '5'] <- 'Sedum album'

#spelling
data.f$Planta <- ifelse(data.f$Planta %in% c('Taraxacum gr. officinale'), 'Taraxacum officinale', data.f$Planta)

#View(data.f[data.f$Planta == 'Taraxacum sp.',])
#s1 p2 and p3 Taraxacum officinale confirmed with flower count
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '1' & data.f$Periodo == '2'] <- 'Taraxacum officinale'
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '1' & data.f$Periodo == '3'] <- 'Taraxacum officinale'
#s2 p1, p2 and p3 Taraxacum officinale confirmed with flower count
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '2' & data.f$Periodo == '1'] <- 'Taraxacum officinale'
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '2' & data.f$Periodo == '2'] <- 'Taraxacum officinale'
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '2' & data.f$Periodo == '3'] <- 'Taraxacum officinale'
#s3 p1 Taraxacum officinale confirmed with flower count
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '3' & data.f$Periodo == '1'] <- 'Taraxacum officinale'
#s5 p1, p2 and p3 Taraxacum officinale confirmed with flower count
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '5' & data.f$Periodo == '1'] <- 'Taraxacum officinale'
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '5' & data.f$Periodo == '2'] <- 'Taraxacum officinale'
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '5' & data.f$Periodo == '3'] <- 'Taraxacum officinale'
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '5' & data.f$Periodo == '6'] <- 'Taraxacum officinale'
#s7 p1, p2, p3, p4 and p5 Taraxacum officinale confirmed with flower count
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '7' & data.f$Periodo == '1'] <- 'Taraxacum officinale'
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '7' & data.f$Periodo == '2'] <- 'Taraxacum officinale'
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '7' & data.f$Periodo == '3'] <- 'Taraxacum officinale'
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '7' & data.f$Periodo == '4'] <- 'Taraxacum officinale'
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '7' & data.f$Periodo == '5'] <- 'Taraxacum officinale'
#s8 p4 Hypochaeris radicata confirmed with flower count
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '8' & data.f$Periodo == '4'] <- 'Hypochaeris radicata'
#s9 p2 Taraxacum officinale confirmed with flower count
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '9' & data.f$Periodo == '2'] <- 'Taraxacum officinale'
#s10 p1, p2, p3, p4 and p6 Taraxacum officinale confirmed with flower count
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '10'] <- 'Taraxacum officinale'
#s11 p5 Hypochaeris radicata confirmed with flower count
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '11' & data.f$Periodo == '5'] <- 'Hypochaeris radicata'
#s12 p1. p3 and p4 Taraxacum officinale confirmed with flower count
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '12'] <- 'Taraxacum officinale'
#13 p1, p2, p4 and p5  Taraxacum officinale confirmed with flower count
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '13'] <- 'Taraxacum officinale'
#14 p1, p2, p4 and p5  Taraxacum officinale confirmed with flower count
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '14'] <- 'Taraxacum officinale'
#15 p1, p2, p4 and p5 Taraxacum officinale confirmed with flower count
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '15'] <- 'Taraxacum officinale'
#16 p1, p2 and p5 Taraxacum officinale confirmed with flower count
data.f$Planta[data.f$Planta == 'Taraxacum sp.' & data.f$Sitio == '16'] <- 'Taraxacum officinale'

#View(data.f[data.f$Planta == 'Taraxacum sp. cf.',])
#s8 p4 Hypochaeris radicata confirmed with flower count
data.f$Planta <- ifelse(data.f$Planta %in% c('Taraxacum sp. cf.'), 'Hypochaeris radicata', data.f$Planta)

#spelling
data.f$Planta <- ifelse(data.f$Planta %in% c('Teurium pyrenaicum'), 'Teucrium pyrenaicum', data.f$Planta)

#confirmed species
data.f$Planta <- ifelse(data.f$Planta %in% c('Teucrium sp.2'), 'Teucrium scorodonia', data.f$Planta)

#species correction
data.f$Planta <- ifelse(data.f$Planta %in% c('Thesium sp.', 'Thesium pyrenaicum'), 'Thesium pyrenaicum ssp. pyrenaicum', data.f$Planta)

#All Thymus were the same species
data.f$Planta <- ifelse(data.f$Planta %in% c('Thymus polytrichus', 'Thymus praecox ssp.politrichus', 'Thymus sp.2', 'Thymus sp.'), 'Thymus praecox ssp. polytrichus', data.f$Planta)

#spacing
data.f$Planta <- ifelse(data.f$Planta %in% c('Trifolium repens '), 'Trifolium repens', data.f$Planta)

#one species
data.f$Planta <- ifelse(data.f$Planta %in% c('Ulex sp.', 'Ulix gallii'), 'Ulex gallii', data.f$Planta)

#just one species -> Vaccinium myrtillus
data.f$Planta <- ifelse(data.f$Planta %in% c('Vaccinium sp.'), 'Vaccinium myrtillus', data.f$Planta)

#View(data.f[data.f$Planta == 'Veronica sp.',])
#s11 p4 Veronica officinalis in flower count
data.f$Planta[data.f$Planta == 'Veronica sp.' & data.f$Sitio == '11' & data.f$Periodo == '4'] <- 'Veronica officinalis'
#S14 p3 no confirmed ID
#s7 p3 no confirmed ID

#View(data.f[data.f$Planta == 'Veronica sp.1',])
#From flower count Veronica sp.1 -> V. arvensis
data.f$Planta[data.f$Planta == 'Veronica sp.1' & data.f$Sitio== '14' & data.f$Periodo== '3'] <- 'Veronica arvensis'

#View(data.f[data.f$Planta == 'Vicia sp.',])
#Only in p3 S14, Poza counts 5 "Vicia sp. 1" (later identified as Vicia sepium) and 24 "Vicia pyrenaica"
#Interactions with Vicia pyrenaica are well identified
#Vicia sp. probably Vicia sepium
data.f$Planta[data.f$Planta == 'Vicia sp.' & data.f$Sitio == '14' & data.f$Periodo == '3'] <- 'Vicia sepium'

#re#View changes
data.f %>%
  pull(Planta) %>%
  unique %>%
  sort

#save data
bp.interactions.clean <- data.f
#write.csv(here("Data/BP_interaction_data_clean.csv"))