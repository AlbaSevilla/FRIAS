USRIIS_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  #Descargado manualmente en la web https://www.sciencebase.gov/catalog/item/62d59ae5d34e87fffb2dda99

  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  ruta_inputfiles <- file.path("Inputfiles")
  data_name_USRIIS <- list.files(path = ruta_inputfiles, pattern = "Step0_OriginalDatabase_USRIIS.xlsx", full.names = TRUE)
  dat <- read.xlsx(data_name_USRIIS) #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.


  #OBTENER EL FILTRADO QUE QUEREMOS DE ESPECIES
  USRIIS_freshwater <- dat %>%
    filter(taxonRank %in% c("Species")) %>%
    filter(isHybrid %in% c("FALSE"))


  #NO DUPLICADOS
  source(file.path("R","noduplicates.r"))
  USRIIS_sinduplicados <- noduplicates(USRIIS_freshwater, "scientificName")

  #OBTENEMOS HABITAT
  source(file.path("R","check_habitat.r"))
  dataset <- USRIIS_sinduplicados
  nombres_especies <- dataset$scientificName
  dataset_actualizado <- check_habitat(nombres_especies, dataset)
  #Seleccionamos las freshwater
  dataset_freshwater <- dataset_actualizado %>% filter(grepl("FRESHWATER", Habitat))

    write.xlsx(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_USRIIS.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_USRIIS.xlsx", "\n")
  # write.csv2(USRIIS_sinduplicados, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_USRIIS.csv")
  # cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_USRIIS.csv", "\n")
  #
}
