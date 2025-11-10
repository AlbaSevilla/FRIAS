base_nacional_de_invasoras_de_costa_rica_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  #nos lo descargamos en la pag https://cr-invasoras.ucr.ac.cr/especies


  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  ruta_InputFiles <- file.path("InputFiles")
  data_name_CostaRica <- list.files(path = ruta_InputFiles, pattern = "originaldatabase_base_nacional_de_invasoras_de_costa_rica.xlsx", full.names = TRUE)
  dat <- read.xlsx(data_name_CostaRica)

  #ELIMINAMOS DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  CostaRica_sinduplicados <- noduplicates(dat, "scientific_name")


  ##############################################################################
  ############ obtención habitat ###############################################
  ##############################################################################
  source(file.path("R", "check_habitat.r"))

  #Aplicacion
  dataset <- CostaRica_sinduplicados
  especies_lista0 <- CostaRica_sinduplicados$scientific_name
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  CostaRica_actualizado <- check_habitat(especies_lista, dataset)

  ########################################################
  ############ OBTENCIÓN FRESHWATERS #####################
  ########################################################
  dataset_freshwater <- CostaRica_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))

  Invaded_country <- "Costa Rica"
  dataset_freshwater$Invaded_country <- Invaded_country

  # Mover la columna 'Species' al inicio
  dataset_freshwater <- dataset_freshwater[, c("Species", setdiff(names(dataset_freshwater), "Species"))]

  write.xlsx (dataset_freshwater, "./InputFiles/freshwatersubset_base_nacional_de_invasoras_de_costa_rica.xlsx")
  }
