InvasiveReptilesAmphibiansFredKraus_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  # Definir la URL y el archivo de destino
  # archivo descargado en la url: https://www.annualreviews.org/content/table/10.1146/annurev-ecolsys-112414-054450.t1?fmt=ahah&fullscreen=true&lang=en#right-ref-t1_fn_a

  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  ruta_inputfiles <- file.path("Inputfiles")
  data_name_reptiles <- list.files(path = ruta_inputfiles, pattern = "Step0_OriginalDatabase_InvasiveReptilesAmphibiansFredKraus.xlsx", full.names=TRUE)
  dat <- read.xlsx(data_name_reptiles) # Aquí estamos indicando que sustraiga los excel de las bases de datos iniciales de la carpeta 'InputFiles'.
  names(dat)[names(dat) == "Location"] <- "Invaded_Country"

  #NO DUPLICADOS
  source(file.path("R","noduplicates.r"))
  amfibios_sin_duplicados <- noduplicates(dat, column_name_species = "Species")

  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  dataset <- amfibios_sin_duplicados
  especies_lista0 <- dataset$Species
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)

  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))


  dataset_freshwater$ScientificName <- dataset_freshwater$Species
  dataset_freshwater <- as.data.frame(dataset_freshwater)
  write_xlsx(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_InvasiveReptilesAmphibiansFredKraus.xlsx")
  cat("Archivo descargado correctamente: /Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_InvasiveReptilesAmphibiansFredKraus.xlsx")

}
