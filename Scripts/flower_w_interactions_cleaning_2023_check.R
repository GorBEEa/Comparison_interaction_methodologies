###################
#Working with Transect Plants Interactions
##################

#cleaning for sequence db check, step 3

data.d2
#View(data.d2)
#colnames(data.d2)
#unique(data.d2[c("Planta")])

library(tidyverse)
library(stringr)

data.d2 %>%
  pull(Planta) %>%
  unique %>%
  sort

#abbreviation clean ups
data.d2$Planta <- str_replace(data.d2$Planta, "sp", "sp.")
data.d2$Planta <- str_replace(data.d2$Planta, "sp..", "sp.")
data.d2$Planta <- str_replace(data.d2$Planta, "sp. ", "sp.")
data.d2$Planta <- str_replace(data.d2$Planta, "cf", "cf.")
data.d2$Planta <- str_replace(data.d2$Planta, "c.f.", "cf.")
data.d2$Planta <- str_replace(data.d2$Planta, "cf. ", "cf.")
data.d2$Planta <- str_replace(data.d2$Planta, "cf..", "cf.")
data.d2$Planta <- str_replace(data.d2$Planta, "cf. ", "cf.")
data.d2$Planta <- str_replace(data.d2$Planta, "sp.cf.", "sp. cf.")


#Corrections going alphabetical order
####################################

#temp name
data.d2$Planta <- ifelse(data.d2$Planta %in% c('\"5pet amari\"'), 'Hypericum humifusum', data.d2$Planta)
#Poza uses this code and similars several times -> Hypericum humifusum confirmed

#ask jon
# He uses this name several times, he doesn't remember which species he was refering to
# It was not Lamium galeobdolon
data.d2$Planta <- ifelse(data.d2$Planta %in% c('\"Ajuga\" amarilla', 'Ajuga sp.amarilla'), 'SI (tipo Ajuga amarilla)', data.d2$Planta)

#confirmed species
data.d2$Planta <- ifelse(data.d2$Planta %in% c('\"Hutsinsia alta\"', 'Arabis aplina'), 'Arabis alpina', data.d2$Planta)

#p5s4 same species
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Acinos alpinus'), 'Satureja alpina ssp. pyrenaea', data.d2$Planta)

#spelling, confirmed cf
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Ajuga reptans cf.'), 'Ajuga reptans', data.d2$Planta)

#spelling
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Anthillys vulneraria'), 'Anthyllis vulneraria', data.d2$Planta)

#look at 41310 specimen andrena for Asteraceae sp s12p4 ?
#View(data.d2[data.d2$Planta == 'Asteraceae sp.',])
data.d2$Planta[data.d2$Planta == 'Asteraceae sp.' & data.d2$Sitio == '16' & data.d2$Periodo == '4'] <- 'Pilosella lactucella'
data.d2$Planta[data.d2$Planta == 'Asteraceae sp.' & data.d2$Sitio == '13' & data.d2$Periodo == '4'] <- 'Taraxacum officinale'

#spelling, confirmed cf
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Betonica officinalis cf.'), 'Betonica officinalis', data.d2$Planta)

#p4s12 Campanula?
##View(data.d2[data.d2$Planta == 'Campana arenaria',])
## Acording to poza could be Convallaria majalis
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Campana arenaria'), 'Convallaria majalis', data.d2$Planta)

#p6s2 Campanula sp. Ask Jon
##View(data.d2[data.d2$Planta == 'Campanilla morada',])
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Campanilla morada'), 'Campanula sp.', data.d2$Planta)

#spelling s5p6
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Campanula rotundifolia cf.'), 'Campanula scheuchzeri', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Campanula hisp.nica'), 'Campanula hispanica', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Campanula sp.',])
#P5 Site 13 Confirmation needed, Photo in Drive, Campanula glomerata
data.d2$Planta[data.d2$Planta == 'Campanula sp.' & data.d2$Sitio == '13' & data.d2$Periodo == '5'] <- 'Campanula glomerata'
#p6s10, Photo in Drive, Campanula glomerata
data.d2$Planta[data.d2$Planta == 'Campanula sp.' & data.d2$Sitio == '10' & data.d2$Periodo == '6'] <- 'Campanula glomerata'

#spelling
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Capsella bursa'), 'Capsella bursa-pastoris', data.d2$Planta)

#Sitio 11, Periodo 4 Cirsium sp.
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Carduus sp. cf.'), 'Cirsium sp.', data.d2$Planta)

#Written this way on Vegeticion list, Carthamus is a basionym
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Carthamus mitissimus'), 'Carduncellus mitissimus', data.d2$Planta)

#confirmed
data.d2$Planta[data.d2$Planta == 'Cerastium sp.' & data.d2$Sitio == '13' & data.d2$Periodo == '5'] <- 'Cerastium fontanum'

#name
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Cirsium eriophorum'), 'Cirsium eriophorum ssp. richterianum', data.d2$Planta)

#p5s14 Cirsium not id in flower count, but given C. vulgare in transect.. not correct
data.d2$Planta[data.d2$Planta == 'Cirsium vulgare' & data.d2$Sitio == '14' & data.d2$Periodo == '5'] <- 'Cirsium sp.'
#Photos in Drive under p6 s10, ID needs second confirmation, Sitio 10 sp 2 Cirsium eriophorum and sp 1 Cirsium vulgare
#Brais confirms:
#View(data.d2[data.d2$Planta == 'Cirsium sp.1',])
#View(data.d2[data.d2$Planta == 'Cirsium sp.2',])
data.d2$Planta[data.d2$Planta == 'Cirsium sp.1'] <- 'Cirsium vulgare'
data.d2$Planta[data.d2$Planta == 'Cirsium sp.2'] <- 'Cirsium eriophorum ssp. richterianum'
#Cirsium sp.
data.d2$Planta[data.d2$Planta == 'Cirsium sp.' & data.d2$Sitio == '14'] <- 'Cirsium eriophorum ssp. richterianum'

#Cardo -> Cirsium sp.
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Cordo sp.'), 'Cirsium sp.', data.d2$Planta)

#spelling
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Colchicum montanm'), 'Colchicum montanum', data.d2$Planta)

#p4s6
#View(data.d2[data.d2$Planta == 'Cruciata sp. cf.',])
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Cruciata sp. cf.'), 'Cruciata laevipes', data.d2$Planta)

#p4s8 can we check to confirm 
#View(data.d2[data.d2$Planta == 'Erica tetralix cf.',])
# Erica tetralix in flower count
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Erica tetralix cf.'), 'Erica tetralix', data.d2$Planta)

#spacing
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Erica vagans '), 'Erica vagans', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Erodium sp.',])
#p2s10 in flower count
#p2s12 in flower count and one interaction
#No confirmed sp.
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Erodium sp.'), 'Erodium sp.', data.d2$Planta)

#I believe this is all the same species
#View(data.d2[data.d2$Planta == 'Eryngium bourgatii',])
#s14 Poza escribe Eryngium cf., creo que se refiere a Cirsium sp. por las fotos que tiene del p6
data.d2$Planta[data.d2$Planta == 'Eryngium bourgatii' & data.d2$Sitio == '14'] <- 'Cirsium sp.'
#En s14 p6 en el conteo anota 3 "Eryngium" supuestamente distintos
#En dos de ellos observa interacciones
#View(data.d2[data.d2$Planta == 'Eryngium sp.',])
#View(data.d2[data.d2$Planta == 'Eryngium sp.2',])
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Eryngium sp.', 'Eryngium sp.2'), 'Cirsium sp.', data.d2$Planta)
## Quiza Cirsium vulgare y/o Cirsium eriophorum ? Además, una de las fotos de Poza PlantNet la ID como Carduus defloratus...
#Preguntar opinión Brais ?

#View(data.d2[data.d2$Planta == 'Euphorbia sp.',])
#Euphorbia dulcis in flower count
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Euphorbia sp.'), 'Euphorbia dulcis', data.d2$Planta)
#spelling
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Euphorbia dulce'), 'Euphorbia dulcis', data.d2$Planta)

#confirmed spellings for Chamaemelum nobile
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Fake manzania', 'Manzalnilla', 'Manzania sp.', 'Manzanilla', 'Manzanilla sp.', 'Manzanita falso', 'Manzanita sp.'), 'Chamaemelum nobile', data.d2$Planta)

#s8s9 maybe H. rad, Ask Jon, confirmed
#View(data.d2[data.d2$Planta == 'Falso Taraxacum sp.',])
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Falso Taraxacum sp.'), 'Hypochaeris radicata', data.d2$Planta)

#spelling, but check to narrow down to species level?
#Galium sp. photo in S11 in google drive (P4)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Gallium sp.'), 'Galium sp.', data.d2$Planta)
#could not be concluded
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Galium sp.2'), 'Galium sp.', data.d2$Planta)
data.d2$Planta[data.d2$Planta == 'Galium sp.' & data.d2$Sitio == '5' & data.d2$Periodo == '4'] <- 'Galium saxatile'
data.d2$Planta[data.d2$Planta == 'Galium sp.' & data.d2$Sitio == '11' & data.d2$Periodo == '4'] <- 'Galium saxatile'
data.d2$Planta[data.d2$Planta == 'Galium sp.' & data.d2$Sitio == '13' & data.d2$Periodo == '4'] <- 'Galium pinetorum'
data.d2$Planta[data.d2$Planta == 'Galium sp.' & data.d2$Sitio == '3' & data.d2$Periodo == '5'] <- 'Galium pinetorum'
data.d2$Planta[data.d2$Planta == 'Galium sp.' & data.d2$Sitio == '12' & data.d2$Periodo == '5'] <- 'Galium pinetorum'
data.d2$Planta[data.d2$Planta == 'Galium sp.' & data.d2$Sitio == '14' & data.d2$Periodo == '5'] <- 'Galium aparine'

#Galium, but revisit paperwork abundance vs transect: Galium pinetorum, saxatile, Galium verum cf.?
#Galium sp. in S11 has photos in Drive (Periodo 4)
#View(data.d2[data.d2$Planta == 'Galium sp.',])
#View(data.d2[data.d2$Planta == 'Galium sp.2',])

#add subpecies, listed throughout paperwork
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Genista hisp.nica', 'Genista hispanica'), 'Genista hispanica ssp. occidentalis', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Globularia sp.',])
#Periodo 4, Sitio 11, Photo in drive -> Jasione laevis 
#Globularia sp. is found in Sitio 1, no interactions
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Globularia sp.'), 'Jasione laevis', data.d2$Planta)

#spelling
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Helianthemum sp.', 'Helianthemum nummularim'), 'Helianthemum nummularium', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Hippocrepis nomosa'), 'Hippocrepis comosa', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Hutchinisia alpina', 'Hutchsinsia alpina'), 'Hutchinsia alpina', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Jacobaea sp.',])
#p6s3 it looks like Jacobaea aquatica (a.k.a. Senecio aquaticus) but id unconfirmed, Photo in Drive
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Jacobaea sp.'), 'Jacobaea sp. cf.', data.d2$Planta)

#Sitio 11 is Jasione laevis according to biomass records
#View(data.d2[data.d2$Planta == 'Jasione sp.',])
data.d2$Planta[data.d2$Planta == 'Jasione sp.' & data.d2$Sitio == '11'] <- 'Jasione laevis'
#View(data.d2[data.d2$Planta == 'Jasione montana',])
data.d2$Planta[data.d2$Planta == 'Jasione montana' & data.d2$Sitio == '5'] <- 'Jasione laevis'

#confirmed
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Lamium purpureum cf.'), 'Lamium purpureum', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Lamiaceae sp.1',])
#Comment "Lamiaceae amarilla" -> Lamium galeobdolon
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Lamiaceae sp.1'), 'Lamium galeobdolon', data.d2$Planta)

#spelling
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Lathyrus linofolius'), 'Lathyrus linifolius', data.d2$Planta)

#could it be narrowed down to one species -> yes
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Medicago sp.'), 'Medicago lupulina', data.d2$Planta)

#name change (Synonymy)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Merendera montana'), 'Colchicum montanum', data.d2$Planta)

#spacing, but can we narrow it to species
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Myositis sp.', 'Myosotis sp.'), 'Myosotis lamottiana', data.d2$Planta)

#spelling
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Narcissus pseudonarcissus subs. palidiflorus'), 'Narcissus pseudonarcissus ssp. pallidiflorus', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Origanum vulgaris'), 'Origanum vulgare', data.d2$Planta)

#spelling
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Potentiila erecta'), 'Potentilla erecta', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Potentilla sterillis'), 'Potentilla sterilis', data.d2$Planta)

#to do later, see if can narrow down to species level at certain sites ?
#R. repens in S15p4 may be other species?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Ranuculus sp.', 'Ranunculus sp. '), 'Ranunculus sp.', data.d2$Planta)
#Come back to site 10 period 5 to determine Ranunculus repens or tuberous?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Ranuculus repens', 'Ranunculus tuberous'), 'Ranunculus sp.', data.d2$Planta)
#confirmed
data.d2$Planta[data.d2$Planta == 'Ranunculus sp.' & data.d2$Sitio == '4' & data.d2$Periodo == '4'] <- 'Ranunculus tuberosus'
data.d2$Planta[data.d2$Planta == 'Ranunculus sp.' & data.d2$Sitio == '3' & data.d2$Periodo == '5'] <- 'Ranunculus tuberosus'
data.d2$Planta[data.d2$Planta == 'Ranunculus sp.' & data.d2$Sitio == '16' & data.d2$Periodo == '5'] <- 'Ranunculus repens'
data.d2$Planta[data.d2$Planta == 'Ranunculus repens' & data.d2$Sitio == '10' & data.d2$Periodo == '6'] <- 'Ranunculus tuberosus'
#check with data?
data.d2$Planta[data.d2$Planta == 'Ranunculus repens' & data.d2$Sitio == '3' & data.d2$Periodo == '6'] <- 'Ranunculus sp.'

#R. ficaria p1s14.. paperworks says sp but others during the period were labelled ficaria? Ask Jon
#data.d2$Planta <- ifelse(data.d2$Planta %in% c('Ranuculus ficaria'), 'Ranunculus sp.', data.d2$Planta)
#Jon doesm't remember, better as sp.

#space
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Ranunculus repens '), 'Ranunculus repens', data.d2$Planta)

#p3s5 ask Brais? 
data.d2$Planta[data.d2$Planta == 'Ranunculus repens' & data.d2$Sitio == '5' & data.d2$Periodo == '3'] <- 'Ranunculus sp.'

#spelling
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Ranunculus tuberous'), 'Ranunculus tuberosus', data.d2$Planta)

#can we confirm id, sitio 7, photo in drive under period 4, site 7. it was a solo individual by water stream, also Xabier said it was observed in sitio 9
# Yes
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Ranunculus flammula cf.'), 'Ranunculus flammula', data.d2$Planta)

#all just one species
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Rubus sp.'), 'Rubus ulmifolius', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Saxifraga sp.',])
#p3s14 could be S. hirsuta based on flower count, but not sure ?
#data.d2$Planta <- ifelse(data.d2$Planta %in% c('Saxifraga sp.'), 'Saxifraga sp.', data.d2$Planta)

#confirmed
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Scorzonera sp.'), 'Scorzonera humilis', data.d2$Planta)

#s14p6 check to see if it can be narrowed down to species
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Scrophularia sp.'), 'Scrophularia alpestris', data.d2$Planta) #alpestris
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Scrophularia sp.2'), 'SI (Scrophularia cf. blanca)', data.d2$Planta) #blanca maybe Teucrium scorodonia
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Scrophularia sp.3'), 'Scrophularia alpestris', data.d2$Planta) #alpestris

#View(data.d2[data.d2$Planta == 'Sedum sp.',])
#can it be narrowed down to species?
#p5 s8 ID flower count
data.d2$Planta[data.d2$Planta == 'Sedum sp.' & data.d2$Sitio == '8' & data.d2$Periodo == '5'] <- 'Sedum anglicum'
#p5 s13, pictuer Jenn, Sedum anglicum
data.d2$Planta[data.d2$Planta == 'Sedum sp.' & data.d2$Sitio == '13' & data.d2$Periodo == '5'] <- 'Sedum anglicum'
#p5 s14, comment says Sedum sp.3 -> Sedum album in flower count
data.d2$Planta[data.d2$Planta == 'Sedum sp.' & data.d2$Sitio == '14' & data.d2$Periodo == '5'] <- 'Sedum album'

#spelling
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Taraxacum gr. officinale'), 'Taraxacum officinale', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Taraxacum sp.',])
#s1 p2 and p3 Taraxacum officinale confirmed with flower count
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '1' & data.d2$Periodo == '2'] <- 'Taraxacum officinale'
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '1' & data.d2$Periodo == '3'] <- 'Taraxacum officinale'
#s2 p1, p2 and p3 Taraxacum officinale confirmed with flower count
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '2' & data.d2$Periodo == '1'] <- 'Taraxacum officinale'
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '2' & data.d2$Periodo == '2'] <- 'Taraxacum officinale'
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '2' & data.d2$Periodo == '3'] <- 'Taraxacum officinale'
#s3 p1 Taraxacum officinale confirmed with flower count
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '3' & data.d2$Periodo == '1'] <- 'Taraxacum officinale'
#s5 p1, p2 and p3 Taraxacum officinale confirmed with flower count
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '5' & data.d2$Periodo == '1'] <- 'Taraxacum officinale'
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '5' & data.d2$Periodo == '2'] <- 'Taraxacum officinale'
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '5' & data.d2$Periodo == '3'] <- 'Taraxacum officinale'
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '5' & data.d2$Periodo == '6'] <- 'Taraxacum officinale'
#s7 p1, p2, p3, p4 and p5 Taraxacum officinale confirmed with flower count
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '7' & data.d2$Periodo == '1'] <- 'Taraxacum officinale'
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '7' & data.d2$Periodo == '2'] <- 'Taraxacum officinale'
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '7' & data.d2$Periodo == '3'] <- 'Taraxacum officinale'
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '7' & data.d2$Periodo == '4'] <- 'Taraxacum officinale'
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '7' & data.d2$Periodo == '5'] <- 'Taraxacum officinale'
#s8 p4 Hypochaeris radicata confirmed with flower count
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '8' & data.d2$Periodo == '4'] <- 'Hypochaeris radicata'
#s9 p2 Taraxacum officinale confirmed with flower count
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '9' & data.d2$Periodo == '2'] <- 'Taraxacum officinale'
#s10 p1, p2, p3, p4 and p6 Taraxacum officinale confirmed with flower count
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '10'] <- 'Taraxacum officinale'
#s11 p5 Hypochaeris radicata confirmed with flower count
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '11' & data.d2$Periodo == '5'] <- 'Hypochaeris radicata'
#s12 p1. p3 and p4 Taraxacum officinale confirmed with flower count
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '12'] <- 'Taraxacum officinale'
#13 p1, p2, p4 and p5  Taraxacum officinale confirmed with flower count
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '13'] <- 'Taraxacum officinale'
#14 p1, p2, p4 and p5  Taraxacum officinale confirmed with flower count
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '14'] <- 'Taraxacum officinale'
#15 p1, p2, p4 and p5 Taraxacum officinale confirmed with flower count
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '15'] <- 'Taraxacum officinale'
#16 p1, p2 and p5 Taraxacum officinale confirmed with flower count
data.d2$Planta[data.d2$Planta == 'Taraxacum sp.' & data.d2$Sitio == '16'] <- 'Taraxacum officinale'

#View(data.d2[data.d2$Planta == 'Taraxacum sp. cf.',])
#s8 p4 Hypochaeris radicata confirmed with flower count
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Taraxacum sp. cf.'), 'Hypochaeris radicata', data.d2$Planta)

#spelling
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Teurium pyrenaicum'), 'Teucrium pyrenaicum', data.d2$Planta)

#confirmed species
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Teucrium sp.2'), 'Teucrium scorodonia', data.d2$Planta)

#species correction
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Thesium sp.', 'Thesium pyrenaicum'), 'Thesium pyrenaicum ssp. pyrenaicum', data.d2$Planta)

#All Thymus were the same species
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Thymus polytrichus', 'Thymus praecox ssp.politrichus', 'Thymus sp.2', 'Thymus sp.'), 'Thymus praecox ssp. polytrichus', data.d2$Planta)

#spacing
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Trifolium repens '), 'Trifolium repens', data.d2$Planta)

#one species
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Ulex sp.', 'Ulix gallii'), 'Ulex gallii', data.d2$Planta)

#just one species -> Vaccinium myrtillus
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Vaccinium sp.'), 'Vaccinium myrtillus', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Veronica sp.',])
#s11 p4 Veronica officinalis in flower count
data.d2$Planta[data.d2$Planta == 'Veronica sp.' & data.d2$Sitio == '11' & data.d2$Periodo == '4'] <- 'Veronica officinalis'
#S14 p3 no confirmed ID
#s7 p3 no confirmed ID

#View(data.d2[data.d2$Planta == 'Veronica sp.1',])
#From flower count Veronica sp.1 -> V. arvensis
data.d2$Planta[data.d2$Planta == 'Veronica sp.1' & data.d2$Sitio== '14' & data.d2$Periodo== '3'] <- 'Veronica arvensis'

#View(data.d2[data.d2$Planta == 'Vicia sp.',])
#Only in p3 S14, Poza counts 5 "Vicia sp. 1" (later identified as Vicia sepium) and 24 "Vicia pyrenaica"
#Interactions with Vicia pyrenaica are well identified
#Vicia sp. probably Vicia sepium
data.d2$Planta[data.d2$Planta == 'Vicia sp.' & data.d2$Sitio == '14' & data.d2$Periodo == '3'] <- 'Vicia sepium'

#re#View changes
data.d2 %>%
  pull(Planta) %>%
  unique %>%
  sort

#write.csv(here("Data/BP_interaction_data_clean.csv"))