#Interaction network metrics

int.networks.x.methodology <- clean4stats.bp23.all.binary %>% 
  group_by(method) %>% 
  filter(method != "count") %>% 
  summarise(across(Lathyrus:last_col(), ~ sum(.)))

int.networks.x.methodology.binary <- clean4stats.bp23.all.binary %>% 
  group_by(method) %>% 
  filter(method != "count") %>% 
  summarise(across(Lathyrus:last_col(), ~ as.integer(any(. == 1)))

#a = interaction frequency between BP and a plant genus (one cell of the interaction web)

Ai = 1
Aj = 210
            
#m = total number of interactions            
m.int <- sum(int.networks.x.methodology[2, 2:210]) #number of interactions from interactions
m.gmb <- sum(int.networks.x.methodology[1, 2:210]) #gut 
m.pmb <- sum(int.networks.x.methodology[3, 2:210]) #pollen



p.int <- a.int/m.all 
p.gmb <- a.gmb/m.all
p.pmb <- a.pmb/m.all



#This is the number within 210 taxa observed within all methodologies, including flower count


#Example: In interactions, for plant species x (Lathyrus)

aij = as.numeric(int.networks.x.methodology[2, "Lathyrus"])
pij = aij/m.int
p.prime = aij/Ai
qj = Aj/m.int
di = #the sum of pij*ln(pij/qj) for each plant species ... wait, but then  they're all the same...