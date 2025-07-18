GAVIA_Download_and_Depurate <- function(){
  ##########################################################
  ############## DESCARGAR #################################
  ##########################################################
  url <- "https://figshare.com/ndownloader/files/6940181"
  destfile <- "InputFiles/Step0_OriginalDatabase_GAVIA.csv"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")




  ###############################################
  ########## DEPURAR ############################
  ###############################################
  ruta_inputfiles <- file.path("Inputfiles")
  data_name_GAVIA <- list.files(path = ruta_inputfiles, pattern = "Step0_OriginalDatabase_GAVIA.csv", full.names = TRUE)
  dat <- read.csv(data_name_GAVIA, sep=",") #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.
  names(dat)
  columnas_quedar <- c("Binomial", "Order", "Family", "CountryName",
                       "Realm", "Island", "LandType", "IntroducedDateGrouped")
  dat <- dat[,columnas_quedar]

  colnames(dat)[colnames(dat) == "CountryName"] <- "InvadedCountry"
  colnames(dat)[colnames(dat) == "Binomial"] <- "ScientificName"

  #NO DUPLICADOS
  source(file.path("R","noduplicates.r"))
  GAVIA_sinduplicados <- noduplicates(dat, column_name_species="ScientificName")

  # FECHAS ANTIGUAS
  # Asegúrate de que los valores sean numéricos
  GAVIA_sinduplicados2 <- GAVIA_sinduplicados
  GAVIA_sinduplicados2$IntroducedDateGrouped <- as.numeric(GAVIA_sinduplicados2$IntroducedDateGrouped)

  # Calcula la fecha más antigua por especie (ignorando NA)
  source(file.path("R", "OldestDate.r"))
  GAVIA_sinduplicados2 <- OldestDate(GAVIA_sinduplicados2, "IntroducedDateGrouped")

  ####################################################
  ###### OBTENER FRESHWATER ##########################
  ####################################################
  source(file.path("R", "check_habitat.r"))
  dataset <- GAVIA_sinduplicados2
  nombres <- GAVIA_sinduplicados2$ScientificName
  nombres_aceptados <- name_backbone_checklist(nombres)$canonicalName
  dataset_act <- check_habitat(nombres_aceptados, dataset)

  dataset_fresh <- dataset_act %>%
    filter(grepl("FRESHWATER", Habitat))
  dataset_fresh

  write.xlsx(dataset_fresh, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_GAVIA.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_GAVIA.xlsx", "\n")

}
