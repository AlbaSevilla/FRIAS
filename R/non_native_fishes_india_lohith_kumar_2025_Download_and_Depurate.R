non_native_fishes_india_lohith_kumar_2025_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  #el dataset lo ha subido Belinda al drive


  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  ruta_inputfiles <- file.path("Inputfiles")
  data_name_NonNativeFishesIndia <- list.files(path = ruta_inputfiles, pattern = "originaldatabase_non_native_fishes_india_lohith_kumar_2025.xlsx", full.names = TRUE)
  dat <- read.xlsx(data_name_NonNativeFishesIndia) #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.

  # Cambiar los nombres de las columnas, reemplazando ":" y "." por "_"
  names(dat) <- gsub("[:.]", "_", names(dat))

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

   #### GUARDAMOS LOS RESULTADOS
  write.xlsx(NonNativeFishesIndia_sinduplicados, "./Inputfiles/freshwatersubset_non_native_fishes_india_lohith_kumar_2025.xlsx")
}
