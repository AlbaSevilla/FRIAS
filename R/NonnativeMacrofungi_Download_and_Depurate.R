NonnativeMacrofungi_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://zenodo.org/records/6572849/files/DATA_Non-native_Macrofungi.zip?download=1"
  destfile <- "InputFiles/DATA_Non-native_Macrofungi.zip"
  download.file(url, destfile, mode = "wb")

  #Descomprimir el zip
  directorio_original <- getwd()
  setwd("Inputfiles")
  ufile = list.files(pattern = '.zip$')
  ufile
  unzip("DATA_Non-native_Macrofungi.zip")
  setwd(directorio_original)


  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  dataset_taxon <- read.xlsx(file.path("Inputfiles","Spp_DATASET.xlsx"),sheet=2)
  head(dataset_taxon)
  names(dataset_taxon)
  attach(dataset_taxon)

  dataset_taxon <- dataset_taxon %>%
    filter(taxonRank == "SPECIES")

  #ELIMINAMOS DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  dataset_taxon_final <- noduplicates(dataset_taxon, "acceptedScientificName")

  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))


  #Obtain habitats
  dataset <- dataset_taxon_final
  especies_lista0 <- dataset$acceptedScientificName
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)

  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))

  # Mover la columna 'Species' al inicio
  dataset_freshwater <- dataset_freshwater[, c("species", setdiff(names(dataset_freshwater), "species"))]

  write.xlsx(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_NonNativeMacrofungi.xlsx")
  cat("Archivo descargado correctamente: /Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_NonNativeMacrofungi.xlsx")
}
