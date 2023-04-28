#####pacotes

library(tidyverse)
library(rgbif)
library(robis)
library(dplyr)



# checar funcoes
?occ_data

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



all_data <- bind_rows(nemo_gbif_ok %>% 
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
  geom_point(data = all_data, aes(x = decimalLongitude,
                                  y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Amphiprion ocellaris")))



write.csv(all_data, "occ_GBIF-OBIS_par_hepa.csv", row.names = FALSE)



# funcao para classificar ocorrencias suspeitas
flag_outlier <- function(df, species){
  
  # funcao para classificar ocorrencias suspeitas
  # baseada no calculo do centroide de todas ocorrencias
  # indica como 'check' as ocorrencias que tem distancias até o centroide
  # acima do 90th quantil (default) das distancias calculadas
  
  dados <- df %>% 
    dplyr::filter(scientificName == species); 
  
  dados2 <- geosphere::distVincentyEllipsoid(
    dados %>%
      summarise(centr_lon = median(decimalLongitude),
                centr_lat = median(decimalLatitude)),
    dados %>% 
      dplyr::select(decimalLongitude, decimalLatitude)
  ) %>% 
    bind_cols(dados) %>% 
    rename(dist_centroid = '...1') %>% 
    mutate(flag = ifelse(dist_centroid < quantile(dist_centroid, probs = 0.9), "OK",
                         ifelse(dist_centroid >= 
                                  quantile(dist_centroid, probs = 0.90) &
                                  dist_centroid < quantile(dist_centroid, probs = 0.95),
                                "check > Q90",
                                ifelse(dist_centroid >=
                                         quantile(dist_centroid, probs = 0.95),
                                       "check > Q95", "OK"))))
  
  # mutate(flag = ifelse(dist_centroid > quantile(dist_centroid,
  #probs = prob), "check", "OK"))
  
  print(dados2)
  
}


# classificar ocorrências
marcados <- nemo_gbif$data %>% 
  data.frame() %>% 
  dplyr::select(scientificName, decimalLongitude, decimalLatitude, datasetName) %>% 
  distinct() %>% 
  flag_outlier(., "Amphiprion ocellaris (Linnaeus, 1766)")



# mapa
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = marcados, 
             aes(x = decimalLongitude, y = decimalLatitude, 
                 color = flag)) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Amphiprion ocellaris")))


## Graficos interativos

library(tidyverse)
library(rgbif)

# ocorrencias
nemo_gbif <- occ_data(scientificName = "Amphiprion ocellaris", 
                        hasCoordinate = TRUE,
                        hasGeospatialIssue = FALSE)
# checar issues
issues_gbif <- nemo_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% issues_gbif)

nemo <- nemo_gbif$data %>%
  dplyr::select(scientificName, decimalLatitude, decimalLongitude) %>% 
  distinct()

library(leaflet)

# conferir no mapa
nemo %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(~decimalLongitude,
             ~decimalLatitude)

library(plotly)
nemo %>% 
  mutate(lat = round(decimalLatitude)) %>% 
  group_by(lat, scientificName) %>%
  summarise(occ = length(scientificName)) %>%
  ggplot(aes(y = occ, x = lat, color = scientificName)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  labs(x = "latitude", y = 'ocorrências')


