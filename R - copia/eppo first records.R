# VAMOS A HACER WEB SCRAPPING DE LA BASE DE DATOS ONLINE EPPO-GD PARA OBTENER
# LOS FIRST RECORDS DEL LISTADO DE ESPECIES DE LA MASTERLIST

library(readxl)
Masterlist <- read_excel("Output/Intermediate/Step2_ObtainTaxonNAS_MasterList.xlsx")
names(Masterlist)
listado_especies <- Masterlist$AcceptedNameGBIF
codigos_especies <- Masterlist$ID_GBIF
head(listado_especies,100)

# Generar los links reemplazando espacios por "+"
links_eppo <- paste0("https://gd.eppo.int/search?k=", gsub(" ", "+", listado_especies))

library(httr)
url <- "https://gd.eppo.int/taxon/ABIAL/reporting"

# Realizar la solicitud GET para obtener el contenido HTML
respuesta <- GET(url)

# Obtener el contenido de la respuesta en formato texto
html_contenido <- content(respuesta, "text")

# Mostrar el contenido HTML (puedes guardarlo en un archivo o usarlo para análisis)
cat(html_contenido)

