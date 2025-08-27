NonNativeFishesIndiaLohith_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  #el dataset lo ha subido Belinda al drive




  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  ruta_inputfiles <- file.path("Inputfiles")
  data_name_NonNativeFishesIndia <- list.files(path = ruta_inputfiles, pattern = "Step0_OriginalDatabase_NonNativeFishesIndia.xlsx", full.names = TRUE)
  dat <- read.xlsx(data_name_NonNativeFishesIndia) #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.

  # Cambiar los nombres de las columnas, reemplazando ":" y "." por "_"
  names(dat) <- gsub("[:.]", "_", names(dat))
  nrow(dat)

  #NO DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  NonNativeFishesIndia_sinduplicados <- noduplicates(dat, "Species")


  NonNativeFishesIndia_sinduplicados$Habitat <- "Freshwater"
  rownames(NonNativeFishesIndia_sinduplicados) <- NULL
  NonNativeFishesIndia_sinduplicados$Oldest_Record <- NonNativeFishesIndia_sinduplicados$`Time_of_introduction/first_report`


  ########################################################################
  ## COMO NO SABEMOS CUALES SON FRESHWATER, LO SACAMOS GRACIAS A GBIF ####
  ########################################################################
 #source(file.path("R", "check_habitat.r"))

  #Salen todos freshwater por lo que nos ahorramos el código
  names(NonNativeFishesIndia_sinduplicados)

  # Mover la columna 'Species' al inicio
  NonNativeFishesIndia_sinduplicados <- NonNativeFishesIndia_sinduplicados[, c("Species", setdiff(names(NonNativeFishesIndia_sinduplicados), "Species"))]

  names(NonNativeFishesIndia_sinduplicados)
  head(NonNativeFishesIndia_sinduplicados$Native_range__Detailed_area)

  source(file.path("R", "ConservarLocalizaciones.r"))
  NonNativeFishesIndia_sinduplicados$Native_range__Detailed_area <- sapply(NonNativeFishesIndia_sinduplicados$Native_range__Detailed_area, filtrar_mayusculas)
  NonNativeFishesIndia_sinduplicados$Native_range__Detailed_area

  #### GUARDAMOS LOS RESULTADOS
  write.xlsx(NonNativeFishesIndia_sinduplicados, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_NonNativeFishesIndiaLohith.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_NonNativeFishesIndiaLohith.xlsx", "\n")
  #write.csv2(NonNativeFishesIndia_sinduplicados, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_NonNativeFishesIndiaLohith.csv")
  #cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_GRIIS.csv", "\n")
}
