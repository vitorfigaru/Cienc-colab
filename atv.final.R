#####pacotes

library(tidyverse)
library(rgbif)
library(robis)
library(dplyr)


# baixar ocorrencias
nemo_gbif <- occ_data(scientificName = "Amphiprion ocellaris", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)

# dimensoes
dim(nemo_gbif)


dim(nemo_gbif$data)

# checar campos
nemo_gbif$data %>% names


gbif_issues()


# checar problemas reportados
issues_gbif <- nemo_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% issues_gbif)



nemo_gbif1 <- nemo_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, basisOfRecord, waterBody, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, locality) 


nemo_gbif1 <- nemo_gbif1 %>% 
  distinct() 

# checar niveis dos fatores
lapply(nemo_gbif1, unique)



library(bdc)
library(CoordinateCleaner)

# checar coordenadas válidas
check_pf <- 
  bdc::bdc_coordinates_outOfRange(
    data = nemo_gbif1,
    lat = "decimalLatitude",
    lon = "decimalLongitude")


# checar coordenadas válidas e próximas a capitais 
#(muitas vezes as coordenadas são erroneamente associadas a capitais dos países)

cl <- nemo_gbif1 %>%
  select(acceptedScientificName, decimalLatitude, decimalLongitude) %>%
  rename(decimallongitude = decimalLongitude,
         decimallatitude = decimalLatitude,
         scientificName = acceptedScientificName) %>% 
  as_tibble() %>% 
  mutate(val = cc_val(., value = "flagged"),
         sea = cc_sea(., value = "flagged"),
         capital = cc_cap(., value = "flagged"))


# verificar coordenadas com flags

# capitais (padrão é um raio de 10km)
cl %>% 
  rename(decimalLongitude = decimallongitude,
         decimalLatitude = decimallatitude) %>% 
  bdc::bdc_quickmap(., col_to_map = "capital")  


cl %>% 
  rename(decimalLongitude = decimallongitude,
         decimalLatitude = decimallatitude) %>% 
  bdc::bdc_quickmap(., col_to_map = "sea")  




# investigar niveis suspeitos
nemo_gbif1 %>% 
  distinct(waterBody) %>% 
  pull()


# waterBody
nemo_gbif1 %>%
  group_by(waterBody) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=waterBody)) +
  geom_bar(stat = 'identity') 


# fonte das regioes erradas
nemo_gbif1 %>% 
  filter(waterBody %in% c("Atlantic Ocean", "Carribean",
                          "Royal Caribbean", "Carribean Sea", "Bonaire")) %>% 
  distinct(datasetName)


# 25 ocorrencias
nemo_gbif1 %>% 
  filter(datasetName %in% c("Diveboard - Scuba diving citizen science"))



# filtrar todas do dataset suspeito
nemo_gbif_ok <- nemo_gbif1 %>% 
  filter(!datasetName %in% c("Diveboard - Scuba diving citizen science"))



library(ggmap)
library(maps)
library(mapdata)

world <- map_data('world')

# checar pontos

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = nemo_gbif_ok, aes(x = decimalLongitude, y = decimalLatitude),
             color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Amphiprion ocellaris")))




## OBIS
nemo_obis <- robis::occurrence("Amphiprion ocellaris")


# checar dados
names(nemo_obis)


nemo_obis1 <- nemo_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
                flags, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, locality, habitat) %>% 
  distinct()

# check problemas reportados (flags)
nemo_obis1 %>% 
  distinct(flags)


# check NA em datasetName
nemo_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land",
                       "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         is.na(datasetName)) %>% 
  distinct(waterBody)






# checar niveis
nemo_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land",
                       "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique")) %>% 
  lapply(., unique)


# ok
nemo_obis_ok <- nemo_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land",
                       "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique", NA)) 


# check
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = nemo_obis_ok, aes(x = decimalLongitude,
                                      y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Amphiprion ocellaris")))


# unir GBIF e OBIS

# ver diferencas
setdiff(names(nemo_gbif_ok), names(nemo_obis_ok))

setdiff(names(nemo_obis_ok), names(nemo_gbif_ok))



all_data1 <- bind_rows(nemo_gbif_ok %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      nemo_obis_ok %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Amphiprion ocellaris") %>% 
  dplyr::select(-rn)


# mapear ocorrencias
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data1, aes(x = decimalLongitude,
                                  y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Amphiprion ocellaris")))



###### Dori


# baixar ocorrencias
dori_gbif <- occ_data(scientificName = "Paracanthurus hepatus", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)

# dimensoes
dim(dori_gbif)


dim(dori_gbif$data)

# checar campos
dori_gbif$data %>% names


gbif_issues()



# checar problemas reportados
issues_gbif <- dori_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% issues_gbif)





dori_gbif1 <- dori_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) 


dori_gbif1 <- dori_gbif1 %>% 
  distinct() 

# checar niveis dos fatores
lapply(dori_gbif1, unique)



library(bdc)
library(CoordinateCleaner)

# checar coordenadas válidas
check_pf <- 
  bdc::bdc_coordinates_outOfRange(
    data = dori_gbif1,
    lat = "decimalLatitude",
    lon = "decimalLongitude")


# checar coordenadas válidas e próximas a capitais 
#(muitas vezes as coordenadas são erroneamente associadas a capitais dos países)

cl <- dori_gbif1 %>%
  select(acceptedScientificName, decimalLatitude, decimalLongitude) %>%
  rename(decimallongitude = decimalLongitude,
         decimallatitude = decimalLatitude,
         scientificName = acceptedScientificName) %>% 
  as_tibble() %>% 
  mutate(val = cc_val(., value = "flagged"),
         sea = cc_sea(., value = "flagged"),
         capital = cc_cap(., value = "flagged"))


# verificar coordenadas com flags

# capitais (padrão é um raio de 10km)
cl %>% 
  rename(decimalLongitude = decimallongitude,
         decimalLatitude = decimallatitude) %>% 
  bdc::bdc_quickmap(., col_to_map = "capital")  


cl %>% 
  rename(decimalLongitude = decimallongitude,
         decimalLatitude = decimallatitude) %>% 
  bdc::bdc_quickmap(., col_to_map = "sea")  




# investigar niveis suspeitos
dori_gbif1 %>% 
  distinct(waterBody) %>% 
  pull()


# waterBody
dori_gbif1 %>%
  group_by(waterBody) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=waterBody)) +
  geom_bar(stat = 'identity') 


# fonte das regioes erradas
dori_gbif1 %>% 
  filter(waterBody %in% c("Atlantic Ocean", "Carribean",
                          "Royal Caribbean", "Carribean Sea", "Bonaire")) %>% 
  distinct(datasetName)


# 25 ocorrencias
dori_gbif1 %>% 
  filter(datasetName %in% c("Diveboard - Scuba diving citizen science"))



# filtrar todas do dataset suspeito
dori_gbif_ok <- dori_gbif1 %>% 
  filter(!datasetName %in% c("Diveboard - Scuba diving citizen science"))



library(ggmap)
library(maps)
library(mapdata)

world <- map_data('world')

# checar pontos

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_gbif_ok, aes(x = decimalLongitude, y = decimalLatitude),
             color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Paracanthurus hepatus")))





# checar profundidade
dori_gbif_ok %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 


## OBIS
dori_obis <- robis::occurrence("Paracanthurus hepatus")


# checar dados
names(dori_obis)


dori_obis1 <- dori_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
                flags, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) %>% 
  distinct()

# check problemas reportados (flags)
dori_obis1 %>% 
  distinct(flags)


# check NA em datasetName
dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land",
                       "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         is.na(datasetName)) %>% 
  distinct(waterBody)



# depth ok
dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land",
                       "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique")) %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 


# checar niveis
dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land",
                       "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique")) %>% 
  lapply(., unique)


# ok
dori_obis_ok <- dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land",
                       "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique", NA)) 


# check
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_obis_ok, aes(x = decimalLongitude,
                                      y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Paracanthurus hepatus")))


# unir GBIF e OBIS

# ver diferencas
setdiff(names(dori_gbif_ok), names(dori_obis_ok))

setdiff(names(dori_obis_ok), names(dori_gbif_ok))



all_data <- bind_rows(dori_gbif_ok %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      dori_obis_ok %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, depth) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Paracanthurus hepatus") %>% 
  dplyr::select(-rn)


# mapear ocorrencias
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data, aes(x = decimalLongitude,
                                  y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Paracanthurus hepatus")))




### Nemo e Dori

all_data2 <- bind_rows(all_data1 %>% 
                        mutate(repo = paste0("Nemo", row.names(.))), 
                      all_data %>% 
                        mutate(repo = paste0("Dori", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, depth) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "") %>% 
  dplyr::select(-rn)


# mapear ocorrencias
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data2, aes(x = decimalLongitude,
                                  y = decimalLatitude, color = datasetName)) +
  scale_color_manual(values=c("blue", "red"))+
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression("Procurando ambos"))


