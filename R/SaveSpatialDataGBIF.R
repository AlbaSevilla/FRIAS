library(rgbif)

#SET UP MY GBIF USERNAME AND PASSWORD
usethis::edit_r_environ()

# remember to set up your GBIF credentials
occ_download(pred("taxonKey", 2436775),format = "SIMPLE_CSV")


#Para saber si ha terminado de descargarse
occ_download_wait('0011507-250711103210423')


#Una descarga en la que
#Eliminamos incongruencias espaciales
#Guardamos registros con coordenadas como tal, sin gaps
#Eliminamos los records vacios
#Eliminamos fosiles
occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"),
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
  pred("taxonKey", 2436775),
  format = "SIMPLE_CSV"
)

occ_download_wait('0011556-250711103210423')




especies <- c("Canis lupus", "Ursus arctos")

for (sp in especies) {
  cat("Procesando:", sp, "\n")

  taxon_key <- name_backbone(name = sp)$usageKey

  download <- occ_download(
    pred("taxonKey", taxon_key),
    pred("hasCoordinate", TRUE),
    pred("hasGeospatialIssue", FALSE),
    format = "SIMPLE_CSV",
    user = "albasevilla",
    pwd = "2001GBIFALBA#a",
    email = "albasevillanavarro@ipe.csic.es"
  )

  occ_download_wait(download)

  folder <- path("Ocurrences/GBIF_downloads", gsub(" ", "_", sp))
  dir_create(folder)

  zipfile <- occ_download_get(download, path = folder, overwrite = TRUE)$path
  unzip(zipfile, exdir = folder)

  cat("CSV para", sp, "descargado en", folder, "\n")
}

