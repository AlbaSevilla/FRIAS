USRIIS_2022_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  #Descargado manualmente en la web https://www.sciencebase.gov/catalog/item/62d59ae5d34e87fffb2dda99

  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  ruta_inputfiles <- file.path("Inputfiles")
  data_name_USRIIS_2022 <- list.files(path = ruta_inputfiles, pattern = "Step0_OriginalDatabase_USRIIS_2022.xlsx", full.names = TRUE)
  dat <- read.xlsx(data_name_USRIIS_2022) #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.


  #OBTENER EL FILTRADO QUE QUEREMOS DE ESPECIES
  USRIIS_2022_freshwater <- dat %>%
    filter(taxonRank %in% c("Species")) %>%
    filter(isHybrid %in% c("FALSE"))


  #NO DUPLICADOS
  source(file.path("R","noduplicates.r"))
  USRIIS_2022_sinduplicados <- noduplicates(USRIIS_2022_freshwater, "scientificName")

  #OBTENEMOS HABITAT
  source(file.path("R","check_habitat.r"))
  dataset <- USRIIS_2022_sinduplicados
  nombres_especies <- dataset$scientificName
  dataset_actualizado <- check_habitat(nombres_especies, dataset)
  #Seleccionamos las freshwater
  dataset_freshwater <- dataset_actualizado %>% filter(grepl("FRESHWATER", Habitat))

    write.xlsx(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_USRIIS_2022.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_USRIIS_2022.xlsx", "\n")
  # write.csv2(USRIIS_2022_sinduplicados, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_USRIIS_2022.csv")
  # cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_USRIIS_2022.csv", "\n")
  #
}
