LIBChile_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  #nos lo descargamos en la pag https://bd.institutoLIBChile.org.br/especies


  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  dat <- read_excel("Inputfiles/Step0_OriginalDatabase_LIBChile.xlsx")

  #OBTENEMOS ESPECIE ÚNICA POR FILA
  source(file.path("R","noduplicates.r"))
  LIBChile_sinduplicados <- noduplicates(dat, "scientific_name")


  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  #OBTAIN HABITAT
  dataset <- LIBChile_sinduplicados
  especies_lista0 <- dataset$scientific_name
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)


  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))

  # Mover la columna 'Species' al inicio
  dataset_freshwater <- dataset_freshwater[, c("scientific_name", setdiff(names(dataset_freshwater), "scientific_name"))]

  # AÑADIMOS PAÍS INVADIDO
  Invaded_country <- "Chile"
  dataset_freshwater$Invaded_Country <- Invaded_country

  write.xlsx(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_LIBChile.xlsx")
  cat("Archivo descargado correctamente: /Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_LIBChile.xlsx")
}
