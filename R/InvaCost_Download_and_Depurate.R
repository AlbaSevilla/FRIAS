InvaCost_Download_and_Depurate <- function(){
  ##############################################
  ####### DECARGAR #############################
  ##############################################
  url <- "https://figshare.com/ndownloader/files/33669518"
  destfile <- "InputFiles/Step0_OriginalDatabase_InvaCost.xlsx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  ###############################################
  ########## DEPURAR ############################
  ###############################################
  ruta_inputfiles <- file.path("Inputfiles")
  data_name_InvaCost <- list.files(path = ruta_inputfiles, pattern = "Step0_OriginalDatabase_InvaCost.xlsx", full.names = TRUE)
  dat <- read.xlsx(data_name_InvaCost) #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.
  names(dat)
  columnas_quedar <- c("Species", "Kingdom", "Order", "Family", "Environment",
                       "Environment_IAS", "Official_country", "Probable_starting_year_adjusted",
                       "Impacted_sector")
  dat <- dat[,columnas_quedar]

  #OBTENER EL FILTRADO QUE QUEREMOS DE ESPECIES, ESTABLISHMENTMEANS Y TAXONRANK
  InvaCost_freshwater <- dat %>%
    filter(Environment %in% c("Semi-aquatic", "Aquatic/Terrestrial", "Aquatic", "Aquatic/Semi-aquatic", "Aquatic/Terrestrial/Semi-aquatic")) %>%
    filter(Environment_IAS %in% c("Semi-aquatic", "Aquatic/Terrestrial", "Aquatic"))

  #NO DUPLICADOS
  source(file.path("R","noduplicates"))
  InvaCost_sinduplicados <- noduplicates(InvaCost_freshwater, "Species")


  ####################################################
  ###### OBTENER FRESHWATER ##########################
  ####################################################
  source(file.path("R", "check_habitat.r"))
  dataset <- InvaCost_sinduplicados
  nombres <- InvaCost_sinduplicados$Species
  nombres_aceptados <- name_backbone_checklist(nombres)$canonicalName
  dataset_act <- check_habitat(nombres_aceptados, dataset)

  dataset_fresh <- dataset_act %>%
    filter(grepl("FRESHWATER", Habitat))

  # FECHAS ANTIGUAS
  # Asegúrate de que los valores sean numéricos
  dataset_fresh2 <- dataset_fresh
  dataset_fresh2$Probable_starting_year_adjusted <- as.numeric(dataset_fresh2$Probable_starting_year_adjusted)

  # Calcula la fecha más antigua por especie (ignorando NA)
  dataset_fresh2$OldestDate <- ave(dataset_fresh2$Probable_starting_year_adjusted,
                                         dataset_fresh2$Species,
                                         FUN = function(x) min(x, na.rm = TRUE))
  dataset_fresh2$OldestDate[is.infinite(dataset_fresh2$OldestDate)] <- NA


  write.xlsx(dataset_fresh2, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_InvaCost.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_InvaCost.xlsx", "\n")

}
