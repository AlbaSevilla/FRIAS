GIDIAS_Download_and_Depurate <- function() {

  ##########################################################
  ######### PARA DESCARGAR LA BASE DE DATOS GIDIAS ##########
  ##########################################################
  url <- "https://springernature.figshare.com/ndownloader/articles/27908838/versions/1"
  destfile <- "InputFiles/Step0_OriginalDatabase_GIDIAS.zip"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }

  download.file(url, destfile, mode = "wb")
  cat("Archivo descargado correctamente:", destfile, "\n")

  # LO DESCOMPRIMIMOS
  #TARDA UN RATILLO
  unzip("InputFiles/Step0_OriginalDatabase_GIDIAS.zip", exdir = "InputFiles")
  directorio_original <- getwd()

  ##########################################################
  ########## PARA DEPURAR GIDIAS ############################
  ##########################################################
  dat <- read_excel("Inputfiles/GIDIAS_20250417_Excel.xlsx")
  head(dat)

  GIDIAS_freshwater_01 <- dat %>%
    filter(Realm %in% c("Freshwater"))


  #ELIMINAR LOS REGISTROS CON FECHAS MENORES A 0
  GIDIAS_freshwater_02 <- subset(GIDIAS_freshwater_01, Year.of.impact > 0 & !is.na(Year.of.impact))

  #OBTENEMOS LA COLUMNA EICAT_by_MECHANISM
  names(GIDIAS_freshwater_02)
  GIDIAS_freshwater_02 <- GIDIAS_freshwater_02 %>%
    mutate(EICAT_by_Mechanism = paste(magnitude.Nature, mechanism.Nature.clean, sep=" -"))

  #NO DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  GIDIAS_sinduplicados <- noduplicates(GIDIAS_freshwater_02, column_name_species = "IAS.Species.Name")
  GIDIAS_sinduplicados <- as.data.frame(GIDIAS_sinduplicados)
  GIDIAS_sinduplicados <- GIDIAS_sinduplicados %>%
    select(-Reference)
  GIDIAS_sinduplicados <- GIDIAS_sinduplicados %>%
    select(-Text.excerpt)
  write.xlsx(GIDIAS_sinduplicados, "Inputfiles/Step0_OriginalDatabase_GIDIAS.xlsx")

  ########################################################################
  #### PARA OBTENER LA FECHA MÁS ANTIGUA           #######################
  ########################################################################
  GIDIAS_sinduplicados <- read_excel("InputFiles/Step0_OriginalDatabase_GIDIAS.xlsx")

  #OLDEST DATE
  source(file.path("R", "OldestDate.r"))
  GIDIAS_sinduplicados <- OldestDate(GIDIAS_sinduplicados, "Year.of.impact")
  GIDIAS_freshwater_03 <- GIDIAS_sinduplicados

  #HAY FILAS DONDE COUNTRY.LOCATION tiene más de un país en la celda, y por tanto
  #luego a la hora de sacar los codigos ISO3, da error porque solo tiene que haber
  #un pais en la celda, por tanto vamos a separar en un unico país cada fila
  #duplicando el registro pero teniendo diferente country.location

  names(GIDIAS_freshwater_03)[names(GIDIAS_freshwater_03) == "IAS.Taxon"] <- "Group"
  names(GIDIAS_freshwater_03) <- gsub("\\.", "_", names(GIDIAS_freshwater_03))

  #Reemplazamos ; por ,
  GIDIAS_final <- GIDIAS_freshwater_03 %>% filter(direction_Nature == "Negative")
  # Eliminar todo lo que está entre paréntesis
  GIDIAS_final$EICAT_by_Mechanism <- gsub("\\([^)]*\\)", "", GIDIAS_final$EICAT_by_Mechanism)
  # Eliminar espacios dobles innecesarios
  GIDIAS_final$EICAT_by_Mechanism <- gsub("\\s{2,}", " ", GIDIAS_final$EICAT_by_Mechanism)
  # Eliminar espacios antes de comas
  GIDIAS_final$EICAT_by_Mechanism <- gsub("\\s+,", ",", GIDIAS_final$EICAT_by_Mechanism)
  # Quitar espacios iniciales y finales
  GIDIAS_final$EICAT_by_Mechanism <- trimws(GIDIAS_final$EICAT_by_Mechanism)



  write.xlsx(GIDIAS_final, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_GIDIAS.xlsx")
  cat("Archivo descargado correctamente: InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_GIDIAS.xlsx")
  #write.csv2(GIDIAS_final, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_GIDIAS.csv")
}
