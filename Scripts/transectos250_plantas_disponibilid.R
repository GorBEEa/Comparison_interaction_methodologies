##########################
#Working with abundance plants
###########################

data.d <- df.bpasc
View(data.d)
colnames(data.d)
names(data.d)[1] <- paste("Planta")
unique(data.d[c("Planta")])

#library(tidyverse)
library(stringr)
data.d %>%
  pull(Planta) %>%
  unique %>%
  sort

data.d$Planta <- str_replace(data.d$Planta, "sp", "sp.")
data.d$Planta <- str_replace(data.d$Planta, "sp..", "sp.")
data.d$Planta <- str_replace(data.d$Planta, "sp. ", "sp.")

data.d$Planta <- str_replace(data.d$Planta, "cf", "cf.")
data.d$Planta <- str_replace(data.d$Planta, "c.f.", "cf.")
data.d$Planta <- str_replace(data.d$Planta, "cf. ", "cf.")
data.d$Planta <- str_replace(data.d$Planta, "cf..", "cf.")
data.d$Planta <- str_replace(data.d$Planta, "cf. ", "cf.")

data.d$Planta <- str_replace(data.d$Planta, "sp.cf.", "sp. cf.")

data.d %>%
  pull(Planta) %>%
  unique %>%
  sort

#Jenn wrote this, checked notes. confirmed.
data.d$Planta <- ifelse(data.d$Planta %in% c('Anthemideae sp. cf.'), 'Chamaemelum nobile', data.d$Planta)

#Possibly Anthyllis vulneraria, check with Brais ? S10P2
data.d$Planta <- ifelse(data.d$Planta %in% c('Anthyllis sp.'), 'Anthyllis sp.', data.d$Planta)

#Vegetacion list says "Apiaceae sp. Seseli libanotis subsp. pyrenaicum P6 S4" but this was observed in P4 ...confirmation?
#View(data.d[data.d$Planta == 'Apiaceae sp.',])
data.d$Planta <- ifelse(data.d$Planta %in% c('Apiaceae sp.'), 'Apiaceae sp.', data.d$Planta)

#p2s3 Arum cf. italicum? seems to be main species, looks like what Jenn saw. (no foto)
#View(data.d[data.d$Planta == 'Arum sp.',])
data.d$Planta <- ifelse(data.d$Planta %in% c('Arum sp.'), 'Arum sp.', data.d$Planta)

#p4s4 Brais: ID doesn't match to a particular species?
#View(data.d[data.d$Planta == 'Biscutella nigra',])

#spelling
data.d$Planta <- ifelse(data.d$Planta %in% c('Calthe palustris'), 'Caltha palustris', data.d$Planta)

#p6s2 - maybe Campanula sp?, based on field notes
#View(data.d[data.d$Planta == 'Campana morada',])
data.d$Planta <- ifelse(data.d$Planta %in% c('Campana morada'), 'Campana morada', data.d$Planta)

#confirm species?
#Site 10 confirmed with photo (drive), s3 unknown
#View(data.d[data.d$Planta == 'Campanula glomerata cf.',])
data.d$Planta[data.d$Planta == 'Campanula glomerata cf.' & data.d$Sitio == '10'] <- 'Campanula glomerata'

#confirm species? photo in drive p6s10
#View(data.d[data.d$Planta == 'Campanula scheuchzeri cf.',])

#can we get to species level?
#View(data.d[data.d$Planta == 'Campanula sp.',])

#p5s15 fotos in drive cf ? Cirsium vulgare cf?
data.d$Planta <- ifelse(data.d$Planta %in% c('Cardo esp.nas grandes'), 'Cirsium sp.', data.d$Planta)

#confirmed
data.d$Planta <- ifelse(data.d$Planta %in% c('Chamaenloera radatec'), 'Chamaemelum nobile', data.d$Planta)

#Can we get to species level? Ask Jon #S9, #10 not possible
#View(data.d[data.d$Planta == 'Cirsium sp.',])

#spelling
data.d$Planta <- ifelse(data.d$Planta %in% c('Colchicum montana'), 'Colchicum montanum', data.d$Planta)

#spacing
data.d$Planta <- ifelse(data.d$Planta %in% c('Erinus alpinus '), 'Erinus alpinus', data.d$Planta)

#spacing
data.d$Planta <- ifelse(data.d$Planta %in% c('Erysimum gorbea'), 'Erysimum gorbeanum', data.d$Planta)

#can we confirm cf? Ask Brais
#View(data.d[data.d$Planta == 'Erysimum gorgeaum cf.',])

#p5s6 Lysimachia nemorum maybe? Ask Jon
data.d$Planta <- ifelse(data.d$Planta %in% c('Estrella amarilla'), 'Estrella amarilla', data.d$Planta)

#wasn't able to narrow down to species level? - jenn
data.d$Planta <- ifelse(data.d$Planta %in% c('Euphorbia sp.'), 'Euphorbia sp.', data.d$Planta)

#ask poza, because there was conflict between Achillea id s16? Chamaemelum nobile
data.d$Planta <- ifelse(data.d$Planta %in% c('Manzailla sp.', 'Manzanillla', 'Manzanilla', 'Manzanilla sp.'), 'Manzanilla', data.d$Planta)
#confirmed
data.d$Planta <- ifelse(data.d$Planta %in% c('Fake manzanita', 'Manzanita (falso)'), 'Chamaemelum nobile', data.d$Planta)

#p4s9 could it be H. radicata? Ask Jon
#View(data.d[data.d$Planta == 'Falso taraxacum sp.',])
data.d$Planta <- ifelse(data.d$Planta %in% c('Falso taraxacum sp.'), 'Falso taraxacum sp.', data.d$Planta)

#species level? Ranunculus ficaria? 
#View(data.d[data.d$Planta == 'Ficaria sp.',])

#identification
data.d$Planta <- ifelse(data.d$Planta %in% c('Fragaria sp.'), 'Fragaria vesca', data.d$Planta)

#confirmation?
#View(data.d[data.d$Planta == 'Galium saxatile cf.',])

#spelling, but can we get them to species level? Brais notes could be helpful, not much info in VegetacionSitios for conclusions
#View(data.d[data.d$Planta == 'Gallium sp.',])
#View(data.d[data.d$Planta == 'Galium sp.',])
#View(data.d[data.d$Planta == 'Galium sp.2',])

#spelling, but include spp?
data.d$Planta <- ifelse(data.d$Planta %in% c('Genista hisp.nica'), 'Genista hispanica', data.d$Planta)

#adjust for all sitio 1? Brais question,  because it has been also called g. robertium
#View(data.d[data.d$Planta == 'Geranium lucidum',])

#? Ask Jon
#View(data.d[data.d$Planta == 'Globularia de prado',])

#Jasione sp. for sitio 8
data.d$Planta <- ifelse(data.d$Planta %in% c('Globularia sp.'), 'Jasione sp.', data.d$Planta)

#incomplete on papaerwork? Ask Brais
#View(data.d[data.d$Planta == 'Hot can',])

#sitio 10 id? maybe laevis
#View(data.d[data.d$Planta == 'Jasione montana cf.',])
data.d$Planta <- ifelse(data.d$Planta %in% c('Jasione montana cf.'), 'Jasione sp.', data.d$Planta)
#confirmed for sitio 11, but not 13, 10
#View(data.d[data.d$Planta == 'Jasione sp.',])
data.d$Planta[data.d$Planta == 'Jasione sp.' & data.d$Sitio == '11'] <- 'Jasione laevis'
#sitio 16, confirmed 
#View(data.d[data.d$Planta == 'Jasoine sp.',])
data.d$Planta[data.d$Planta == 'Jasoine sp.' & data.d$Sitio == '16'] <- 'Jasione laevis'

#spelling
data.d$Planta <- ifelse(data.d$Planta %in% c('Lathirus linifolius'), 'Lathyrus linifolius', data.d$Planta)
#confirm species? Ask Jon
#View(data.d[data.d$Planta == 'Lathyrus sp.',])

#correct? Linaria propinqua, Ask Brais
#View(data.d[data.d$Planta == 'Linaria prop. sp.',])
data.d$Planta <- ifelse(data.d$Planta %in% c('Linaria prop. sp.'), 'Linaria prop. sp.', data.d$Planta)

#narrow down to one species?
#View(data.d[data.d$Planta == 'Medicago sp.',])
#View(data.d[data.d$Planta == 'Medicago sp.1',])
#View(data.d[data.d$Planta == 'Medicago sp.2',])

#species id? Narcissus pseudonarcissus maybe, ask Brais
#View(data.d[data.d$Planta == 'Narcissus sp.',])

#Maybe Orchidaceae sp. sheets note from Brais to Jon "alguna pista sobre esto??"
#View(data.d[data.d$Planta == 'Orchidea sp.',])
#narrow down to species?
#View(data.d[data.d$Planta == 'Orchidaceae sp.',])

#same species? Oxalis acetosella. Jenn thinks so.. check with Brais
#View(data.d[data.d$Planta == 'Oxalis sp.',])

#sometimes counted all together, leave as is?
#View(data.d[data.d$Planta == 'Plantago sp.',])

#spelling
data.d$Planta <- ifelse(data.d$Planta %in% c('Potentiila montana', 'Pontentilla montana'), 'Potentilla montana', data.d$Planta)
data.d$Planta <- ifelse(data.d$Planta %in% c('Potentilla sterillis'), 'Potentilla sterilis', data.d$Planta)
data.d$Planta <- ifelse(data.d$Planta %in% c('Potentialla erecta'), 'Potentilla erecta', data.d$Planta)
data.d$Planta <- ifelse(data.d$Planta %in% c('Prunela vulgaris'), 'Prunella vulgaris', data.d$Planta)

#confirmed through photo id (in drive)
#View(data.d[data.d$Planta == 'Prunella sp.',])
data.d$Planta <- ifelse(data.d$Planta %in% c('Prunella sp.'), 'Campanula glomerata', data.d$Planta)

#p3s3 tall species id, - have had trouble to ID - Jenn
#View(data.d[data.d$Planta == 'Purple sp.cies',])
data.d$Planta <- ifelse(data.d$Planta %in% c('Purple sp.cies'), 'Purple species', data.d$Planta)

#can we id any to species?
#16 ask jon
#View(data.d[data.d$Planta == 'Rananculus sp.',])
data.d$Planta <- ifelse(data.d$Planta %in% c('Rananculus sp.'), 'Ranunculus sp.', data.d$Planta)
#multiple sites
#View(data.d[data.d$Planta == 'Ranunculus sp.',])

#one species? Rubus ulmifolius
data.d$Planta <- ifelse(data.d$Planta %in% c('Rubus sp.'), 'Rubus sp.', data.d$Planta)

#nonpolliating plant?
#View(data.d[data.d$Planta == 'Rumes acetosella subsp.angiocarpus',])
#View(data.d[data.d$Planta == 'Rumex acetosella',])

#possible to id to species? sitio 4
#View(data.d[data.d$Planta == 'Saxifraga sp.',])

#possible to id to species?
#View(data.d[data.d$Planta == 'Sedum album cf.',])
#sitio 10, photo in drive but I think it is from the transect, not abundance
data.d$Planta[data.d$Planta == 'Sedum album cf.' & data.d$Sitio == '10'] <- 'Sedum album cf.'

#possible to id to species?
#View(data.d[data.d$Planta == 'Sedum sp.',])

#possible to id to species?
#View(data.d[data.d$Planta == 'Sisymbrium sp.',])

#possible to id to species? and check paperwork if correct
#View(data.d[data.d$Planta == 'Taraxacum',])
#Jenn's sites, comfortable with ID confirmation
data.d$Planta <- ifelse(data.d$Planta %in% c('Taraxacum'), 'Taraxacum officinale', data.d$Planta)
#maybe we can confirm period 1, because of the season. talk later about other periods?
#View(data.d[data.d$Planta == 'Taraxacum sp.',])
#View(data.d[data.d$Planta == 'Taraxacump sp.',])
#View(data.d[data.d$Planta == 'Taraxcum sp.',])

#spacing
data.d$Planta <- ifelse(data.d$Planta %in% c('Teucrium pyrenaicum ssp.pinnatifidum'), 'Teucrium pyrenaicum ssp. pinnatifidum', data.d$Planta)

#subspecies? pinnatifidum
#View(data.d[data.d$Planta == 'Teucrium pyrenaicum', 'Thesium pyrenianca']) 
data.d$Planta <- ifelse(data.d$Planta %in% c('Thesium pyrenianca'), 'Teucrium pyrenaicum', data.d$Planta)

#all same species
data.d$Planta <- ifelse(data.d$Planta %in% c('Thymus praecox ssp.politrichus', 'Thymus polytrichus ssp.praecox', 'thymus sp.'), 'Thymus praecox ssp. polytrichus', data.d$Planta)
data.d$Planta <- ifelse(data.d$Planta %in% c('Thymus polytrichus', 'Thymus praecrox', 'thymus sp.'), 'Thymus praecox ssp. polytrichus', data.d$Planta)

#spelling
data.d$Planta <- ifelse(data.d$Planta %in% c('Trifolium pratense'), 'Trifolium pratanse', data.d$Planta)

#spacing
data.d$Planta <- ifelse(data.d$Planta %in% c('Trifolium reprens', 'Trifolium medium'), 'Trifolium repens', data.d$Planta)

#is it just one species in Gorbea? .... conflit with Ulex Ulex europaeus in flower count p5s6 
data.d$Planta <- ifelse(data.d$Planta %in% c('Ulex sp.'), 'Ulex sp.', data.d$Planta)

#unknown? might be Hutchinsia sp. according to a field note
#View(data.d[data.d$Planta == 'Unknown White Specie',]) 
data.d$Planta <- ifelse(data.d$Planta %in% c('"Hutsinsia alta"'), 'Hutchinsia sp', data.d$Planta)

#same species? Vaccinium myrtillus
#View(data.d[data.d$Planta == 'Vaccinium sp.',]) 

#can we id to species? 
#View(data.d[data.d$Planta == 'Veronica sp.',]) 

#can we id to species? 
#View(data.d[data.d$Planta == 'Vicia sp.',])

#unknown? field work notes says maybe S. holostea
#View(data.d[data.d$Planta == 'White',])

#review changes
data.d %>%
  pull(Planta) %>%
  unique %>%
  sort

