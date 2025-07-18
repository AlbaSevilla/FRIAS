#Prueba mapas con gbif
library(leaflet)
library(tidyverse)
kenya_occurrence <- read.delim("D:/NextCloud/FRIASpackage_68 - copia/Occurrences/0054538-160910150852091/0054538-160910150852091.csv")

#Preparar el dataset para el mapping
#1-. Renombrar columnas
library(dplyr)
kenya_occurrence <- dplyr::rename(kenya_occurrence, latitude = decimallatitude, longitude = decimallongitude)
View(kenya_occ)

#2-. Filtrar los datos
library(tidyverse)
kenya_occurrence %>% drop_na(class) %>% count(class, sort=TRUE)

#Por alguna razón elimina aqui las aves para dejar menos registros
kenya_occ <- filter(kenya_occurrence, class!="Aves")
kenya_occ <- kenya_occ %>% filter(!is.na(latitude)) %>% filter(!is.na(longitude))



#Crear el mapa con leaflet
kenya_20 <- kenya_occ[1:20,]
library(dplyr)
library(leaflet)
k <- leaflet::leaflet(kenya_20) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~longitude, ~latitude, popup = kenya_20$species)
k


##############################################
library(rgbif)
map_fetch()



map_fetch(srs='EPSG:3031',taxonKey=7190978,style='glacier.point', base_style="gbif-dark")









#########################################################
#########################################################
############ PROBAMOS CON DATOS DESCARGADOS #############
#########################################################
#########################################################
#Prueba mapas con gbif
library(leaflet)
library(tidyverse)
kenya_occurrence <- read.csv("D:/NextCloud/FRIASpackage_68 - copia/Occurrences/GBIF_occurrences/0023909-250711103210423/occurrence.csv")

#Preparar el dataset para el mapping
#1-. Renombrar columnas
library(dplyr)
kenya_occurrence <- dplyr::rename(kenya_occurrence, latitude = decimalLatitude, longitude = decimalLongitude)
View(kenya_occ)

#2-. Filtrar los datos
library(tidyverse)
kenya_occurrence %>% drop_na(class) %>% count(class, sort=TRUE)
kenya_occ <- kenya_occurrence %>% filter(!is.na(latitude)) %>% filter(!is.na(longitude))
names(kenya_occ)


#Crear el mapa con leaflet
kenya_occ_ABRAMISBRAMA <- kenya_occ %>% filter
kenya_20 <- kenya_occ
library(dplyr)
library(leaflet)
k <- leaflet::leaflet(kenya_20) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~longitude, ~latitude, popup = kenya_20$species)
k


##############################################
library(rgbif)
map_fetch()



map_fetch(srs='EPSG:3031',taxonKey=7190978,style='glacier.point', base_style="gbif-dark")
