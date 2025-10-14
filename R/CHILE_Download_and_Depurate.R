Chile_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  #nos lo descargamos en la pag




  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  ruta_inputfiles <- file.path("Inputfiles")
  data_name_Chile <- list.files(path = ruta_inputfiles, pattern = "Step0_OriginalDatabase_Chile.csv", full.names = TRUE)
  dat <- read.csv("Inputfiles/Step0_OriginalDatabase_Chile.csv")
  names(dat)

  #ELIMINAMOS DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  Chile_sinduplicados <- noduplicates(dat, "scientific_name")


  ##############################################################################
  ############ obtención habitat ###############################################
  ##############################################################################
  source(file.path("R", "check_habitat.r"))

  #Aplicacion
  dataset <- Chile_sinduplicados
  especies_lista0 <- Chile_sinduplicados$scientific_name
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  Chile_actualizado <- check_habitat(especies_lista, dataset)

  ########################################################
  ############ OBTENCIÓN FRESHWATERS #####################
  ########################################################
  dataset_freshwater <- Chile_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))

  Invaded_country <- "Chile"
  dataset_freshwater$Invaded_country <- Invaded_country

  # Mover la columna 'Species' al inicio
  dataset_freshwater <- dataset_freshwater[, c("Species", setdiff(names(dataset_freshwater), "Species"))]

  write.xlsx (dataset_freshwater, "./InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Chile.xlsx")
  cat("El archivo se descargó correctamente: InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Chile.xlsx")
  #write.csv2(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Chile.csv")
}
