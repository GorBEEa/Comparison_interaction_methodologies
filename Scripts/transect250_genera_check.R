#source after running t250 plantas dispo.... you should have a table called data.d22

#library(tidyverse)
#library(stringr)
data.d2 %>%
  pull(Planta) %>%
  unique %>%
  sort

data.d2$Planta <- str_replace(data.d2$Planta, "sp", "sp.")
data.d2$Planta <- str_replace(data.d2$Planta, "sp..", "sp.")
data.d2$Planta <- str_replace(data.d2$Planta, "sp. ", "sp.")
data.d2$Planta <- str_replace(data.d2$Planta, "cf", "cf.")
data.d2$Planta <- str_replace(data.d2$Planta, "c.f.", "cf.")
data.d2$Planta <- str_replace(data.d2$Planta, "cf. ", "cf.")
data.d2$Planta <- str_replace(data.d2$Planta, "cf..", "cf.")
data.d2$Planta <- str_replace(data.d2$Planta, "cf. ", "cf.")
data.d2$Planta <- str_replace(data.d2$Planta, "sp.cf.", "sp. cf.")

#Spelling errors and confirmed ID's - easy fix (alphabetically)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Achemilla alpigena'), 'Alchemilla alpigena', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Ajuga sp.'), 'Ajuga reptans', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Apriacea sp.'), 'Apiaceae sp.', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Armaria serpyllifolia'), 'Arenaria serpyllifolia', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Aphanes arvensis'), 'Alchemilla arvensis', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Betonica officinalis cf.'), 'Betonica officinalis', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Campunula glomerata cf.', 'Campanula glomerata cf.'), 'Campanula glomerata', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Campanula hisp.nica'), 'Campanula hispanica', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Capsella bursa', 'Capsella bursa-pastores'), 'Capsella bursa-pastoris', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Cardamine fleuosa'), 'Cardamine flexuosa', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Carduncellus mittimus', 'Carthamus mitissimus'), 'Carduncellus mitissimus', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Cerasium sp.'), 'Cerastium sp.', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Fake manzanita', 'Manzanilla falso', 'Manzanilla sp.'), 'Chamaemelum nobile', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Cirsium eriophorum ssp.richteria', 'Cirsium eriophorum'), 'Cirsium eriophorum ssp. richteria', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Cragaetus sp.'), 'Crataegus monogyna', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Daboeecia cantabrica'), 'Daboecia cantabrica', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Egriloliurium sp.'), 'Epilobium sp.', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Erica arborea cf.'), 'Erica arborea', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Erica cinerea '), 'Erica cinerea', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Erica tetralix cf.'), 'Erica tetralix', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Ericea sp.'), 'Erica sp.', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Erisimum sp.'), 'Erysimum sp.', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Erysimum garbenum', 'Erysimum gorbea cf.'), 'Erysimum gorbeanum', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Euphorbia dulce'), 'Euphorbia dulcis', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Galium pinetocum'), 'Galium pinetorum', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Gallium sp.'), 'Galium sp.', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Genista hisp.nica occidentalis'), 'Genista hispanica ssp. occidentalis', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Genista hisp.nica'), 'Genista hispanica', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Geranium pyrenaica'), 'Geranium pyrenaicum', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Globulana sp.'), 'Globularia sp.', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Helianthemum sp.'), 'Helianthemum nummularium', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Heracleum sp.ondyllum'), 'Heracleum sphondylium', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Hesp.rocyon hederaceus cf.'), 'Wahlenbergia hederacea', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Hippocreppis', 'Hippocrepis comosa'), 'Hippocrepis comosa', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Hutchinisia alpina', 'Hutchinsia alpina ', 'Hutchinsia sp.'), 'Hutchinsia alpina', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Hypochaeris radicata ', 'Hypochaeris radicata cf.'), 'Hypochaeris radicata', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Laimium galeobdolon', 'Lamium sp.amarillo'), 'Lamium galeobdolon', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Lamiaceae purpureum', 'Lamium sp.(purpureum cf.)'), 'Lamium purpureum', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Linaria cathart'), 'Linum catharticum', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Meconopsis cantrica'), 'Meconopsis cambrica', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Medicago sp.'), 'Medicago lupulina', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Myosotis sp.', 'Myosotis lammotiana cf.', 'Myosotis lammotiana'), 'Myosotis lamottiana', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Oxalis sp.'), 'Oxalis acetosella', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Pedicularis'), 'Pedicularis sylvatica', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Plantago sp.(media+lanceolata)'), 'Plantago sp.', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Polygala vulgaris ', 'Polygala rosa'), 'Polygala vulgaris', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Potentilla recta', 'Potentiila erecta'), 'Potentilla erecta', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Potentilla sterills', 'Potentilla sterillis'), 'Potentilla sterilis', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Prunelle vulgaris'), 'Prunella vulgaris', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Prunus sp.nosa'), 'Prunus spinosa', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Ranunculus reprens'), 'Ranunculus repens', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Satureja alpina subsp.pyrenaea', 'Acinos alpinus'), 'Satureja alpina ssp. pyrenaea', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Scorzonera humillis', 'Scorzonera humilis cf.'), 'Scorzonera humilis', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Sedum ablum'), 'Sedum album', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Sedum sp. cf.'), 'Sedum dasyphyllum', data.d2$Planta) #Foto p5s15
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Sherardia sp.'), 'Sherardia arvensis', data.d2$Planta) #Foto p5s15
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Stellavia media', 'Stellaria media ','Stellaria media cf.'), 'Stellaria media', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Taraxacum gr. officinale', 'Taraxacum ', 'Taraxacum sp.'), 'Taraxacum officinale', data.d2$Planta) ##Puede que en algun momento se colaran H. radicata dentro de las cuentas ?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Teucrium scourdronia', 'Teucrium scorodonia (Scrophulariacea amarilla, preguntar a xabi)'), 'Teucrium scorodonia', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Teucrium sp.'), 'Teucrium pyrenaicum', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Thesium humifusum cf.', 'Thesium pyrenaicum subsp.pyrenaicum', 'Thesium sp.'), 'Thesium pyrenaicum ssp. pyrenaicum', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Thymus praecox ssp.politrichus', 'Thymus polytrichus ssp.praecox', 'Thymus sp.', 'Thymus polytrichus', 'Thymus praecrox'), 'Thymus praecox ssp. polytrichus', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Trifolium platino'), 'Trifolium pratense', data.d2$Planta) #typo error
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Ulex galli', 'Ulex gallii cf.', 'Ulex sp.', 'Ulex europaeus'), 'Ulex gallii', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Vaccinium sp.'), 'Vaccinium myrtillus', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Veronica cp.'), 'Veronica sp.', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Veronica enana'), 'Veronica arvensis', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Vicia pyrenica'), 'Vicia pyrenaica', data.d2$Planta)


#review changes
data.d2 %>%
  pull(Planta) %>%
  unique %>%
  sort



#cf label format ?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Ranunculus flammula cf.'), 'Ranunculus cf. flammula', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Ranunculus repends cf.'), 'Ranunculus cf. repens', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Sedum sp. cf.lbum', 'Sedum album cf.'), 'Sedum cf. album', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Sedum dasyphyllum cf.'), 'Sedum cf. dasyphyllum', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Veronica officinalis cf.'), 'Veronica cf. officinalis', data.d2$Planta)





##############################################
#### Checking flower IDs alphabetically ######

#View(data.d2[data.d2$Planta == '\"Ajuga\" amarilla flor',])
#View(data.d2[data.d2$Planta == 'Ajuga sp.amarilla',])
#En p3 s14 y s16, quiza Lamium amarillo ? Preguntar Poza
data.d2$Planta <- ifelse(data.d2$Planta %in% c('\"Ajuga\" amarilla flor', 'Ajuga sp.amarilla'), 'SI (tipo Ajuga amarilla)', data.d2$Planta)

#View(data.d2[data.d2$Planta == '\"Floc lota hederacea\"',])
#p5s9, ni idea, preguntar Poza, decia "Flor lila.. hederacea"
#Wahlenbergia hederacea
data.d2$Planta <- ifelse(data.d2$Planta %in% c('\"Floc lota hederacea\"'), 'Wahlenbergia hederacea', data.d2$Planta)

#View(data.d2[data.d2$Planta == '\"Florencio amarillo\"',])
#p3s14, preguntar Poza ?, Probablemente Meconopsis cambrica
data.d2$Planta <- ifelse(data.d2$Planta %in% c('\"Florencio amarillo\"'), 'Meconopsis cambrica', data.d2$Planta)

#View(data.d2[data.d2$Planta == '\"ravia\"',])
#p2 s13, ni idea, preguntar Jenn ?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('"ravia"'), 'SI (ravia)', data.d2$Planta)

#View(data.d2[data.d2$Planta == '\"Spetalos amarillos\"',])
#p5 s9, ni idea, papel dice '5 petalos amarillos', preguntar Poza ?
#View(data.d2[data.d2$Planta == 'Amarilla s petalos',])
#p5s16, ni idea, papel dice 'Amarilla 5 petalos'  preguntar Poza
data.d2$Planta <- ifelse(data.d2$Planta %in% c('\"Spetalos amarillos\"', 'Amarilla s petalos'), 'SI (5 petalos amarillos)', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Achemilla alpigena',])
#In p4s1 Brais.. wrote A. alpigena but VegetacionSitios says Alchemilla vetteri and Alchemilla catalaunica
##Las Alchemilas son dificiles de determinar, comentar a Brais ?

#View(data.d2[data.d2$Planta == 'Apiaceae sp.',])
#p4s1 Brais Apiaceae sp. ?
#p6s4 ID pliego Seseli libanotis ssp. pyrenaicum
data.d2$Planta[data.d2$Planta == 'Apiaceae sp.' & data.d2$Sitio== '4' & data.d2$Periodo== '6'] <- 'Seseli libanotis ssp. pyrenaicum'

#View(data.d2[data.d2$Planta == 'Arenaria alpina',])
#En P6 s14, Arenaria alpina no existe, querían decir Arenaria montana ?
#Preguntar Poza
data.d2$Planta[data.d2$Planta == 'Arenaria alpina'] <- 'Arenaria montana'

#View(data.d2[data.d2$Planta == 'Asteraceae senecio ',])
#En p6 s2, Preguntar Poza ?, quiza Jacobaea aquatica (o Senecio aquaticus)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Asteraceae senecio '), 'Asteraceae senecio', data.d2$Planta)
#View(data.d2[data.d2$Planta == 'Jacobaea sp. cf.',])
#p6s3, foto en drive, plantnet propone Jacobaea aquatica (Senecio aquaticus) ?
#Será lo mismo que Poza llama 'Asteracea senecio' en p6s2 ??


#View(data.d2[data.d2$Planta == 'Biscutella n.',])
#p3 s10, Biscutella laevigata identificada en otros sittios
#Preguntar Brais ? Si no sp.
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Biscutella n.'), 'Biscutella sp.', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Campana arenaria',])
#p4s12, preguntar Poza ?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Campana arenaria'), 'SI (Campana arenaria)', data.d2$Planta)


#View(data.d2[data.d2$Planta == 'Campana sp.',])
#p2s14, preguntar Poza ?

#View(data.d2[data.d2$Planta == 'Campanilla morada ',])
#p6s2, Poza ?, check if right, note on paperwork campana morada
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Campanilla morada '), 'Campanula sp.', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Campanula rotundifolia cf.',])
#p6 s5, Brais identifica Campanula scheuchzeri en s5
#No hay constancia de C. rotundifolia en nuestros datos, probablemente Campanula scheuchzeri
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Campanula rotundifolia cf.'), 'Campanula scheuchzeri', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Campanula scheuchzeri cf.',])
#p6s10,Comentario dice foto en drive
#Foto en drive de Campanula glomerata
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Campanula scheuchzeri cf.'), 'Campanula glomerata', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Campanula sp.?',])
#En p3s14, Preguntar poza, comentario dice 'amarilla'
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Campanula sp.?'), 'SI (tipo Campanula amarilla)', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Cardamine sp.',])
#En p4s9, Poza comentario "blanca pequeña", Brais identifica pliego como Cardamine flexuosa en s9
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Cardamine sp.'), 'Cardamine flexuosa', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Cardamine sp.amarilla',])
##p3s14, Poza refiriendose a Brasicaceae amarilla ?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Cardamine sp.amarilla'), 'Cardamine cf. amarilla', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Carduus sp. cf.',])
#p4s11
#View(data.d2[data.d2$Planta == 'Cordo sp.',])
#p6s12
##En ambos casos probablemente algun tipo de Cirsium sp. ?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Cordo sp.', 'Carduus sp. cf.'), 'Cirsium sp.', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Chrysanthus erngusta',])
#p6s1 - preguntar Brais, dice que Rhinanthus angustifolius
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Chrysanthus erngusta'), 'Rhinanthus angustifolius', data.d2$Planta)

# View(data.d2[data.d2$Planta == 'Cirsium sp.1',])
# View(data.d2[data.d2$Planta == 'Cirsium sp.2',])
#p5s14 Poza separa dos Cirsium, blanco y rosa
# No se si son carácteres diagnosticos, quiza variacion dentro de la especie
#Preguntar ?
#p6s10 Jenn separa Cirsium sp2 de Cirsium vulgare cf.
#data.d2$Planta <- ifelse(data.d2$Planta %in% c('Cirsium vulgare cf.'), 'Cirsium cf. vulgare', data.d2$Planta)
##Hay fotos en Drive, preguntar por ID ?

#View(data.d2[data.d2$Planta == 'Cruciata sp. cf.',])
##p4s6, en toda la temporada solo se identificó una Cruciata, Cruciata laevipes ?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Cruciata sp. cf.'), 'Cruciata laevipes', data.d2$Planta)

# View(data.d2[data.d2$Planta == 'Eryngium sp.',])
# View(data.d2[data.d2$Planta == 'Eryngium sp.1',])
# View(data.d2[data.d2$Planta == 'Eryngium sp.2',])
##p6s14 Preguntar a Poza ? Eryngium bourgatii es la unica especie identificada
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Eryngium sp.', 'Eryngium sp.2', 'Eryngium sp.1'), 'Eryngium bourgatii', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Estrella amarilla',])
##p5s6, preguntar Poza ?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Estrella amarilla'), 'SI (estrella amarilla)', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Estrellita sp.hoja',])
#p3s16, preguntar Poza ?
#data.d2$Planta <- ifelse(data.d2$Planta %in% c('Estrellita sp.hoja'), 'UNKNOWN', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'falso taraxacum',])
#p4s8 y p4s9, preguntar Poza ?, probablemente Hypochaeris radicata
data.d2$Planta <- ifelse(data.d2$Planta %in% c('falso taraxacum'), 'Hypochaeris radicata', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Flor amarilla ',])
#p4s9, preguntar Poza ?, comentario 'hojas opuestas'
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Flor amarilla '), 'SI (Flor amarilla)', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Flor rosa',])
#p5s16, preguntar Poza ?, comentario '6 petalos sale desde suelo'
#Probablemente Colchicum montanum
#View(data.d2[data.d2$Planta == 'Planta sp.',])
#p6s14, comentario 'flor rosa sale desde suelo, 5 petalos' (probably this species but ask Poza ?)
#Colchicum montanum = Merendera
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Flor rosa','Planta sp.', 'Merendera montana', 'Morcerdera sp.'), 'Colchicum montanum', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Galium saxatila cf.',])
#p4s10, probablemente G. saxatile o pinetorum ?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Galium saxatila cf.'), 'Galium cf. saxatile', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Galium sp.2',])
#p3s10 and p4s8, s9, s12, could be Galium saxatile or Galium pinetorum ?
#View(data.d2[data.d2$Planta == 'Gallium sp.2',])
#p4s3, could be Galium saxatile or Galium pinetorum
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Galium sp.2', 'Gallium sp.2'), 'Galium sp.', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Galium verum cf.',])
#p5s15, ID correcta ?, Galium amarillo, preguntar Brais
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Galium verum cf.'), 'Galium cf. verum', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Geranium molle cf.',])
##p2s14, Brais comenta para s14 en VegetacionSitios, probablemente Geranium lucidum ?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Geranium molle cf.'), 'Geranium lucidum', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Geranium sp.1',])
#p3s14, preguntar a Poza ?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Geranium sp.1'), 'Geranium sp.', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Geranium sp.s',])
#p3s13, en los papeles se ve que es Geranium pyrenaicum
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Geranium sp.s'), 'Geranium pyrenaicum', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Globularia sp.',])
#p4s11, Jenn identifico las Jasione como Globularia sp., en la biomasa s11 la llamamos Jasione laevis ?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Globularia sp.'), 'Jasione laevis', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Hepatica sp.',])
## Probablemente Hepatica nobilis ?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Hepatica sp.'), 'Hepatica nobilis', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'hortiga',])
#En p2s14, preguntar a Jon ?, quiza se refiera a un Lamium o cuenta las ortigas??
#data.d2$Planta <- ifelse(data.d2$Planta %in% c('hortiga'), 'Lamium sp.', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Jasione montana',])
#View(data.d2[data.d2$Planta == 'Jasione montana cf.',])
#View(data.d2[data.d2$Planta == 'Jasione sp.',])
#View(data.d2[data.d2$Planta == 'Jasoine sp.',])
##Las dejamos como Jasione
##Probablemente todas Jasione laevis ? Preguntar a Brais
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Jasoine sp.', 'Jasione montana cf.', 'Jasione montana'), 'Jasione sp.', data.d2$Planta)
data.d2$Planta[data.d2$Planta == 'Jasione sp.' & data.d2$Sitio== '11'] <- 'Jasione laevis'


#View(data.d2[data.d2$Planta == 'Lamiaceae sp.1',])
#En p6s14, comentario 'amarilla' maybe Lamium galeobdolon ?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Lamiaceae sp.1'), 'Lamium galeobdolon', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Orphiaceae?',])
#p3s14 comentario "flor apagada rojiza/granate de hoja grande"
##Quiza una orquídea ? Preguntar Poza
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Orphiaceae?'), 'Orchidaceae sp.', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Mystery S1',])
#p1s13, coment says Cerastium sp. possible ?
#data.d2$Planta <- ifelse(data.d2$Planta %in% c('Mystery S1'), 'Cerastium sp.', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Pink sp.1',])
#p2s13, ? Jenn doesn't remember
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Pink sp.1'), 'SI (Pink sp.1)', data.d2$Planta)

data.d2$Planta[data.d2$Planta == 'Plantago lanceolata' & data.d2$Sitio== '2' & data.d2$Periodo=='5'] <- 'Plantago sp.'

#View(data.d2[data.d2$Planta == 'Pompon mari sp. cf.',])
#p4s2, ? Preguntar Poza
#data.d2$Planta <- ifelse(data.d2$Planta %in% c('Pompon mari sp. cf.'), 'UNKNOWN', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Primula elatior cf.',])
#p3s1, preguntar ?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Primula elatior cf.'), 'Primula cf. elatior', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Ranunculus sp.',])
##Ranunculus repens en algunos casos puede dar dudas, puede que lo usasemos como cajon de sastre
#s14 p1 y p2 podrían ser Ranunculus ficaria, preguntar Poza ? 
#p3s5, ID dudosa de Ranunculus repens mejor sp.
#p4s2, Poza escribe R. tuberosus, en la primera interaccion luego solo Ranunculus (al contar solo Ranunculus) ?
data.d2$Planta[data.d2$Planta == 'Ranunculus repens' & data.d2$Sitio== '5' & data.d2$Periodo=='3'] <- 'Ranunculus sp.'
#p5s10 R. tuberosus ID not clear igual mejor R sp. ?
#p6s3 R. repenes ID not clear igual mejor R sp. ?
data.d2$Planta[data.d2$Planta == 'Ranunculus sp.' & data.d2$Sitio== '9' & data.d2$Periodo=='6'] <- 'Ranunculus cf. flammula'


#View(data.d2[data.d2$Planta == 'Ranunculus sp.2',])
#p4s9, comentario Xabi 'hojas largas', se refiere a Ranunculus cf. flammula
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Ranunculus sp.2'), 'Ranunculus cf. flammula', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Rosaceae sp. cf.',])
#p3s15, preguntar a Brais ?
#data.d2$Planta <- ifelse(data.d2$Planta %in% c('Rosaceae sp. cf.'), 'Rosaceae cf.', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Rubus sp.',])
#View(data.d2[data.d2$Planta == 'Rupus sp.',])
#View(data.d2[data.d2$Planta == 'Rubus ulmifolius',])
##Sería todo la misma sp Rubus ulmifolius ? o mejor dejar como sp.
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Rubus sp.', 'Rupus sp.'), 'Rubus ulmifolius', data.d2$Planta)

#View(data.d2[data.d2$Planta == 's15',])
##En p3s9, El papel decía 'SI 5', no sp ?. preguntar a Brais por sus notas
data.d2$Planta <- ifelse(data.d2$Planta %in% c('s15'), 'SI 5', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Scrophularia sp.',])
#View(data.d2[data.d2$Planta == 'Scrophularia sp.2',])
#En p5s14 Scrophularia sp., luego Brais la identifica como Scrophularia alpestris
#En p6s14 Scrophularia sp.2, Poza comenta 'clásica', haciendo referencia a esa
#En p6s14 Scrophularia sp., Poza comenta 'blanca, cf.' ? #Quiza Teucrium scorodonia
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Scrophularia sp.2'), 'Scrophularia alpestris', data.d2$Planta)
data.d2$Planta[data.d2$Planta == 'Scrophularia sp.' & data.d2$Periodo == '5'] <- 'Scrophularia alpestris'
data.d2$Planta[data.d2$Planta == 'Scrophularia sp.' & data.d2$Periodo == '6'] <- 'Scrophularia cf. (blanca)' #Quiza Teucrium scorodonia

#View(data.d2[data.d2$Planta == 'Sedum sp.3',])
#p5s14, Poza cuenta 3 sedum distintos, ID dasyphyllum y anglicum, el que queda album
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Sedum sp.3'), 'Sedum album', data.d2$Planta)

#Brais brain fart
data.d2$Planta[data.d2$Planta == 'Trifolium repens' & data.d2$Sitio== '3' & data.d2$Periodo== '5'] <- 'Trifolium pratense'

#View(data.d2[data.d2$Planta == 'Unknown White Sp 1',])
#p1s10, ask Jenn ?, Cerastium sp possible
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Unknown White Sp 1'), 'SI (white sp.4)', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'Veronica sp.1',])
#View(data.d2[data.d2$Planta == 'Veronica sp.2',])
#p3s12, Veronica sp.1 probablemente officinalis o chamaedrys
#p3s14, Veronica sp.1 probablemente arvensis, cuenta 5200
#p3s14, Veronica sp.2 probablemente officinalis o chamaedrys
#Preguntar Poza ?

#View(data.d2[data.d2$Planta == 'Vicia serpyllifolia',])
#View(data.d2[data.d2$Planta == 'Vicia sativa',])
#View(data.d2[data.d2$Planta == 'Vicia sp.1',])
#En p5s14 Poza llama una (Vicia serpidofolia), alguien la cambia a V. sativa en el excel
#En p6s14, la llama Vicia serpyllifolia,
#Ese nombre no existe, quiza Vicia sepium ?
#En p3s14 y En p4s2, Vicia sp.1 (no es V. Pyrenaica, también contada)
#Quiza también Vicia sepium ?
#Brais tiene Vicia sepium identificada en s2
#Preguntar Poza
#En p4s4 Vicia sativa bien identificada 

#View(data.d2[data.d2$Planta == 'White S1',])
#In p1s15, ?, 'Hutchinsia?' says paperwork note 
data.d2$Planta <- ifelse(data.d2$Planta %in% c('White S1'), 'SI (White sp.1)', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'White S2',])
#In p1s15, no clue ?, ask Jenn 
data.d2$Planta <- ifelse(data.d2$Planta %in% c('White S2'), 'SI (White sp.2)', data.d2$Planta)

#View(data.d2[data.d2$Planta == 'White sp.1',])
#In p2s1, Jenn thinks Sherardia arvensis maybe ? recorded in p3s1
data.d2$Planta <- ifelse(data.d2$Planta %in% c('White sp.1'), 'SI (White sp.3)', data.d2$Planta)


#review changes
data.d2 %>%
  pull(Planta) %>%
  unique %>%
  sort







