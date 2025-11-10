gidias_Download_and_Depurate <- function() {

  ##########################################################
  ######### PARA DESCARGAR LA BASE DE DATOS gidias_2025 ##########
  ##########################################################
  url <- "https://springernature.figshare.com/ndownloader/articles/27908838/versions/1"
  destfile <- "InputFiles/originaldatabase_gidias_2025.zip"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }

  download.file(url, destfile, mode = "wb")
  cat("Archivo descargado correctamente:", destfile, "\n")

  # LO DESCOMPRIMIMOS
  #TARDA UN RATILLO
  unzip("InputFiles/originaldatabase_gidias_2025.zip", exdir = "InputFiles")
  directorio_original <- getwd()

  ##########################################################
  ########## PARA DEPURAR gidias_2025 ############################
  ##########################################################
  dat <- read_excel("InputFiles/GIDIAS_20250417_Excel.xlsx")
  head(dat)

  gidias_freshwater_01 <- dat %>%
    filter(Realm %in% c("Freshwater"))


  #ELIMINAR LOS REGISTROS CON FECHAS MENORES A 0
  gidias_freshwater_02 <- subset(gidias_freshwater_01, Year.of.impact > 0 & !is.na(Year.of.impact))

  #OBTENEMOS LA COLUMNA EICAT_by_MECHANISM
  names(gidias_freshwater_02)
  gidias_freshwater_02 <- gidias_freshwater_02 %>%
    mutate(EICAT_by_Mechanism = paste(magnitude.Nature, mechanism.Nature.clean, sep=" -"))

  #NO DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  gidias_sinduplicados <- noduplicates(gidias_freshwater_02, column_name_species = "IAS.Species.Name")
  gidias_sinduplicados <- as.data.frame(gidias_sinduplicados)
  gidias_sinduplicados <- gidias_sinduplicados %>%
    select(-Reference)
  gidias_sinduplicados <- gidias_sinduplicados %>%
    select(-Text.excerpt)
  write.xlsx(gidias_sinduplicados, "InputFiles/originaldatabase_gidias_2024.xlsx")

  ########################################################################
  #### PARA OBTENER LA FECHA MÁS ANTIGUA           #######################
  ########################################################################
  gidias_sinduplicados <- read_excel("InputFiles/originaldatabase_gidias_2024.xlsx")

  #OLDEST DATE
  source(file.path("R", "OldestDate.r"))
  gidias_sinduplicados <- OldestDate(gidias_sinduplicados, "Year.of.impact")
  gidias_freshwater_03 <- gidias_sinduplicados

  #HAY FILAS DONDE COUNTRY.LOCATION tiene más de un país en la celda, y por tanto
  #luego a la hora de sacar los codigos ISO3, da error porque solo tiene que haber
  #un pais en la celda, por tanto vamos a separar en un unico país cada fila
  #duplicando el registro pero teniendo diferente country.location

  names(gidias_freshwater_03)[names(gidias_freshwater_03) == "IAS.Taxon"] <- "Group"
  names(gidias_freshwater_03) <- gsub("\\.", "_", names(gidias_freshwater_03))

  gidias_final <- gidias_freshwater_03 %>% filter(direction_Nature == "Negative")
  gidias_final$EICAT_by_Mechanism <- gsub("\\([^)]*\\)", "", gidias_final$EICAT_by_Mechanism)
  gidias_final$EICAT_by_Mechanism <- gsub("\\s{2,}", " ", gidias_final$EICAT_by_Mechanism)
  gidias_final$EICAT_by_Mechanism <- gsub("\\s+,", ",", gidias_final$EICAT_by_Mechanism)
  gidias_final$EICAT_by_Mechanism <- trimws(gidias_final$EICAT_by_Mechanism)

  write.xlsx(gidias_final, "InputFiles/freshwatersubset_gidias_2025.xlsx")
}
