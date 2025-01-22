##########################
#Working with abundance plants
###########################

data.d2 <- int_taxa_df
#View(data.d2)
colnames(data.d2)
names(data.d2)[1] <- paste("Planta")
unique(data.d2[c("Planta")])

#library(tidyverse)
library(stringr)
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

data.d2 %>%
  pull(Planta) %>%
  unique %>%
  sort

#Jenn wrote this, checked notes. confirmed.
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Anthemideae sp. cf.'), 'Chamaemelum nobile', data.d2$Planta)

#Possibly Anthyllis vulneraria, check with Brais ? S10P2
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Anthyllis sp.'), 'Anthyllis sp.', data.d2$Planta)

#Vegetacion list says "Apiaceae sp. Seseli libanotis subsp. pyrenaicum P6 S4" but this was observed in P4 ...confirmation?
#View(data.d2[data.d2$Planta == 'Apiaceae sp.',])
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Apiaceae sp.'), 'Apiaceae sp.', data.d2$Planta)

#p2s3 Arum cf. italicum? seems to be main species, looks like what Jenn saw. (no foto)
#View(data.d2[data.d2$Planta == 'Arum sp.',])
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Arum sp.'), 'Arum sp.', data.d2$Planta)

#p4s4 Brais: ID doesn't match to a particular species?
#View(data.d2[data.d2$Planta == 'Biscutella nigra',])

#spelling
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Calthe palustris'), 'Caltha palustris', data.d2$Planta)

#p6s2 - maybe Campanula sp?, based on field notes
#View(data.d2[data.d2$Planta == 'Campana morada',])
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Campana morada'), 'Campana morada', data.d2$Planta)

#confirm species?
#Site 10 confirmed with photo (drive), s3 unknown
#View(data.d2[data.d2$Planta == 'Campanula glomerata cf.',])
data.d2$Planta[data.d2$Planta == 'Campanula glomerata cf.' & data.d2$Sitio == '10'] <- 'Campanula glomerata'

#confirm species? photo in drive p6s10
#View(data.d2[data.d2$Planta == 'Campanula scheuchzeri cf.',])

#can we get to species level?
#View(data.d2[data.d2$Planta == 'Campanula sp.',])

#p5s15 fotos in drive cf ? Cirsium vulgare cf?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Cardo esp.nas grandes'), 'Cirsium sp.', data.d2$Planta)

#confirmed
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Chamaenloera radatec'), 'Chamaemelum nobile', data.d2$Planta)

#Can we get to species level? Ask Jon #S9, #10 not possible
#View(data.d2[data.d2$Planta == 'Cirsium sp.',])

#spelling
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Colchicum montana'), 'Colchicum montanum', data.d2$Planta)

#spacing
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Erinus alpinus '), 'Erinus alpinus', data.d2$Planta)

#spacing
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Erysimum gorbea'), 'Erysimum gorbeanum', data.d2$Planta)

#can we confirm cf? Ask Brais
#View(data.d2[data.d2$Planta == 'Erysimum gorgeaum cf.',])

#p5s6 Lysimachia nemorum maybe? Ask Jon
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Estrella amarilla'), 'Estrella amarilla', data.d2$Planta)

#wasn't able to narrow down to species level? - jenn
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Euphorbia sp.'), 'Euphorbia sp.', data.d2$Planta)

#ask poza, because there was conflict between Achillea id s16? Chamaemelum nobile
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Manzailla sp.', 'Manzanillla', 'Manzanilla', 'Manzanilla sp.'), 'Manzanilla', data.d2$Planta)
#confirmed
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Fake manzanita', 'Manzanita (falso)'), 'Chamaemelum nobile', data.d2$Planta)

#p4s9 could it be H. radicata? Ask Jon
#View(data.d2[data.d2$Planta == 'Falso taraxacum sp.',])
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Falso taraxacum sp.'), 'Falso taraxacum sp.', data.d2$Planta)

#species level? Ranunculus ficaria? 
#View(data.d2[data.d2$Planta == 'Ficaria sp.',])

#identification
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Fragaria sp.'), 'Fragaria vesca', data.d2$Planta)

#confirmation?
#View(data.d2[data.d2$Planta == 'Galium saxatile cf.',])

#spelling, but can we get them to species level? Brais notes could be helpful, not much info in VegetacionSitios for conclusions
#View(data.d2[data.d2$Planta == 'Gallium sp.',])
#View(data.d2[data.d2$Planta == 'Galium sp.',])
#View(data.d2[data.d2$Planta == 'Galium sp.2',])

#spelling, but include spp?
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Genista hisp.nica'), 'Genista hispanica', data.d2$Planta)

#adjust for all sitio 1? Brais question,  because it has been also called g. robertium
#View(data.d2[data.d2$Planta == 'Geranium lucidum',])

#? Ask Jon
#View(data.d2[data.d2$Planta == 'Globularia de prado',])

#Jasione sp. for sitio 8
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Globularia sp.'), 'Jasione sp.', data.d2$Planta)

#incomplete on papaerwork? Ask Brais
#View(data.d2[data.d2$Planta == 'Hot can',])

#sitio 10 id? maybe laevis
#View(data.d2[data.d2$Planta == 'Jasione montana cf.',])
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Jasione montana cf.'), 'Jasione sp.', data.d2$Planta)
#confirmed for sitio 11, but not 13, 10
#View(data.d2[data.d2$Planta == 'Jasione sp.',])
data.d2$Planta[data.d2$Planta == 'Jasione sp.' & data.d2$Sitio == '11'] <- 'Jasione laevis'
#sitio 16, confirmed 
#View(data.d2[data.d2$Planta == 'Jasoine sp.',])
data.d2$Planta[data.d2$Planta == 'Jasoine sp.' & data.d2$Sitio == '16'] <- 'Jasione laevis'

#spelling
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Lathirus linifolius'), 'Lathyrus linifolius', data.d2$Planta)
#confirm species? Ask Jon
#View(data.d2[data.d2$Planta == 'Lathyrus sp.',])

#correct? Linaria propinqua, Ask Brais
#View(data.d2[data.d2$Planta == 'Linaria prop. sp.',])
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Linaria prop. sp.'), 'Linaria prop. sp.', data.d2$Planta)

#narrow down to one species?
#View(data.d2[data.d2$Planta == 'Medicago sp.',])
#View(data.d2[data.d2$Planta == 'Medicago sp.1',])
#View(data.d2[data.d2$Planta == 'Medicago sp.2',])

#species id? Narcissus pseudonarcissus maybe, ask Brais
#View(data.d2[data.d2$Planta == 'Narcissus sp.',])

#Maybe Orchidaceae sp. sheets note from Brais to Jon "alguna pista sobre esto??"
#View(data.d2[data.d2$Planta == 'Orchidea sp.',])
#narrow down to species?
#View(data.d2[data.d2$Planta == 'Orchidaceae sp.',])

#same species? Oxalis acetosella. Jenn thinks so.. check with Brais
#View(data.d2[data.d2$Planta == 'Oxalis sp.',])

#sometimes counted all together, leave as is?
#View(data.d2[data.d2$Planta == 'Plantago sp.',])

#spelling
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Potentiila montana', 'Pontentilla montana'), 'Potentilla montana', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Potentilla sterillis'), 'Potentilla sterilis', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Potentialla erecta'), 'Potentilla erecta', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Prunela vulgaris'), 'Prunella vulgaris', data.d2$Planta)

#confirmed through photo id (in drive)
#View(data.d2[data.d2$Planta == 'Prunella sp.',])
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Prunella sp.'), 'Campanula glomerata', data.d2$Planta)

#p3s3 tall species id, - have had trouble to ID - Jenn
#View(data.d2[data.d2$Planta == 'Purple sp.cies',])
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Purple sp.cies'), 'Purple species', data.d2$Planta)

#can we id any to species?
#16 ask jon
#View(data.d2[data.d2$Planta == 'Rananculus sp.',])
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Rananculus sp.'), 'Ranunculus sp.', data.d2$Planta)
#multiple sites
#View(data.d2[data.d2$Planta == 'Ranunculus sp.',])

#one species? Rubus ulmifolius
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Rubus sp.'), 'Rubus sp.', data.d2$Planta)

#nonpolliating plant?
#View(data.d2[data.d2$Planta == 'Rumes acetosella subsp.angiocarpus',])
#View(data.d2[data.d2$Planta == 'Rumex acetosella',])

#possible to id to species? sitio 4
#View(data.d2[data.d2$Planta == 'Saxifraga sp.',])

#possible to id to species?
#View(data.d2[data.d2$Planta == 'Sedum album cf.',])
#sitio 10, photo in drive but I think it is from the transect, not abundance
data.d2$Planta[data.d2$Planta == 'Sedum album cf.' & data.d2$Sitio == '10'] <- 'Sedum album cf.'

#possible to id to species?
#View(data.d2[data.d2$Planta == 'Sedum sp.',])

#possible to id to species?
#View(data.d2[data.d2$Planta == 'Sisymbrium sp.',])

#possible to id to species? and check paperwork if correct
#View(data.d2[data.d2$Planta == 'Taraxacum',])
#Jenn's sites, comfortable with ID confirmation
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Taraxacum'), 'Taraxacum officinale', data.d2$Planta)
#maybe we can confirm period 1, because of the season. talk later about other periods?
#View(data.d2[data.d2$Planta == 'Taraxacum sp.',])
#View(data.d2[data.d2$Planta == 'Taraxacump sp.',])
#View(data.d2[data.d2$Planta == 'Taraxcum sp.',])

#spacing
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Teucrium pyrenaicum ssp.pinnatifidum'), 'Teucrium pyrenaicum ssp. pinnatifidum', data.d2$Planta)

#subspecies? pinnatifidum
#View(data.d2[data.d2$Planta == 'Teucrium pyrenaicum', 'Thesium pyrenianca']) 
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Thesium pyrenianca'), 'Teucrium pyrenaicum', data.d2$Planta)

#all same species
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Thymus praecox ssp.politrichus', 'Thymus polytrichus ssp.praecox', 'thymus sp.'), 'Thymus praecox ssp. polytrichus', data.d2$Planta)
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Thymus polytrichus', 'Thymus praecrox', 'thymus sp.'), 'Thymus praecox ssp. polytrichus', data.d2$Planta)

#spelling
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Trifolium pratense'), 'Trifolium pratanse', data.d2$Planta)

#spacing
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Trifolium reprens', 'Trifolium medium'), 'Trifolium repens', data.d2$Planta)

#is it just one species in Gorbea? .... conflit with Ulex Ulex europaeus in flower count p5s6 
data.d2$Planta <- ifelse(data.d2$Planta %in% c('Ulex sp.'), 'Ulex sp.', data.d2$Planta)

#unknown? might be Hutchinsia sp. according to a field note
#View(data.d2[data.d2$Planta == 'Unknown White Specie',]) 
data.d2$Planta <- ifelse(data.d2$Planta %in% c('"Hutsinsia alta"'), 'Hutchinsia sp', data.d2$Planta)

#same species? Vaccinium myrtillus
#View(data.d2[data.d2$Planta == 'Vaccinium sp.',]) 

#can we id to species? 
#View(data.d2[data.d2$Planta == 'Veronica sp.',]) 

#can we id to species? 
#View(data.d2[data.d2$Planta == 'Vicia sp.',])

#unknown? field work notes says maybe S. holostea
#View(data.d2[data.d2$Planta == 'White',])

#review changes
data.d2 %>%
  pull(Planta) %>%
  unique %>%
  sort

