###Dados
library(dplyr)
library(ggplot2)
library(tidyr)
library(validate)
library(taxize)
library(vegan)
library(stringr)

iris <- read.csv("DADOS.csv", header = T, sep = ",", dec = ",")


lapply(iris, unique)


iris %>% 
  select(ESP, SEP_L:PET_W) %>% 
  pivot_longer(cols = -ESP, names_to = "variavel", values_to = "valores") %>% 
  ggplot(aes(x = as.numeric(valores), fill = ESP)) +
  geom_histogram() +
  facet_wrap(~ variavel, scales = 'free_x') +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "tamanho (mm)") +
  scale_fill_discrete(
    expression(bold("ESP:")),
    labels = c(expression(italic("Iris setosa")), 
               expression(italic("Iris versicolor")), 
               expression(italic("Iris virginica"))))

rules <- validator(in_range(LATITUDE, min = -90, max = 90),
                   in_range(LATITUDE, min = -180, max = 180),
                   is.character(SITE),
                   is.numeric(DATA),
                   all_complete(iris))

out   <- confront(iris, rules)
summary(out)

plot(out)


iris$ESP <- str_replace(iris$ESP, "IRIS_SETOSA", "Iris setosa")
iris$ESP <- str_replace(iris$ESP, "IRIS_VIRGINICA", "Iris virginica")
iris$ESP <- str_replace(iris$ESP, "IRIS_VERSICOLOR", "Iris versicolor")

# check taxa
species <- iris %>% 
  distinct(ESP) %>% 
  pull() %>% 
  c("Iris murchosa", .) %>% # inserimos uma espécie fictícia para teste
  get_tsn() %>% 
  data.frame() %>% 
  bind_cols(ESP = iris %>% 
              distinct(ESP) %>% 
              pull() %>% 
              c("Iris murchosa", .))



iris_1 <- iris %>% 
  dplyr::mutate(eventID = paste(SITE, DATA, sep = "_"), # create indexing fields 
                occurrenceID = paste(SITE, DATA, AMOSTRA, sep = "_")) %>% 
  left_join(iris %>% 
              select(ESP)) %>% # add species unique identifier
  dplyr::rename(decimalLongitude = LONGITUDE, # rename fields according to DwC 
                decimalLatitude = LATITUDE,
                eventDate = DATA,
                scientificName = ESP,
                scientificNameID = ESP) %>% 
  mutate(geodeticDatum = "WGS84", # and add complimentary fields
         verbatimCoordinateSystem = "decimal degrees",
         georeferenceProtocol = "Random coordinates obtained from Google Earth",
         locality = "Gaspe Peninsula",
         recordedBy = "Edgar Anderson",
         taxonRank = "Species",
         organismQuantityType = "individuals",
         basisOfRecord = "Human observation")



## create eventCore
eventCore <- iris_1 %>% 
  select(eventID, eventDate, decimalLongitude, decimalLatitude, locality,
         geodeticDatum, verbatimCoordinateSystem, georeferenceProtocol) %>% 
  distinct() 


## create occurrence
occurrences <- iris_1 %>% 
  select(eventID, occurrenceID, scientificNameID,
         recordedBy, taxonRank, organismQuantityType, basisOfRecord) %>%
  distinct() 


## create measurementsOrFacts
eMOF <- iris_1 %>% 
  select(eventID, occurrenceID, recordedBy, SEP_L:PET_W) %>%  
  pivot_longer(cols = SEP_L:PET_W,
               names_to = "measurementType",
               values_to = "measurementValue") %>% 
  mutate(measurementUnit = "cm",
         measurementType = plyr::mapvalues(measurementType,
                                           from = c("SEP_L", "SEP_W",
                                                    "PET_W", "PET_L"), 
                                           to = c("sepal length", "sepal width",
                                                  "petal width", "petal length")))



# check if all eventID matches
setdiff(eventCore$eventID, occurrences$eventID)

setdiff(eventCore$eventID, eMOF$eventID)


setdiff(occurrences$eventID, eMOF$eventID)


# check NA values
eMOF %>%
  filter(is.na(eventID))


occurrences %>%
  filter(is.na(eventID))


rm(list = setdiff(ls(), c("eventCore", "occurrences", "eMOF")))

files <- list(eventCore, occurrences, eMOF) 
data_names <- c("DF_eventCore","DF_occ","DF_eMOF")
dir.create("Dwc_Files")


for(i in 1:length(files)) {
  path <- paste0(getwd(), "/", "DwC_Files")
  write.csv(files[[i]], paste0(path, "/", data_names[i], ".csv"))
}


