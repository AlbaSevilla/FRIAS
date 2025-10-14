Tedesco_et_al_2017_Download_and_Depurate <- function(){
  ##############################################
  ############## DESCARGAR #####################
  ##############################################
  url <- "https://figshare.com/ndownloader/files/8964583"
  destfile <- "InputFiles/OriginalDatabase_Tedesco_et_al_2017.zip"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  # Descomprimir el archivo
  unzip("InputFiles/OriginalDatabase_Tedesco_et_al_2017.zip", exdir = "InputFiles")
  directorio_original <- getwd()

  ##############################################
  ########### DEPURAR ##########################
  ##############################################
  ruta_inputfiles <- file.path("Inputfiles")
  data_name <- list.files(path = ruta_inputfiles, pattern = "Occurrence_Table.csv", full.names = TRUE)
  dat <- read.csv(data_name, sep=";") #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.
  names(dat) <- gsub("\\.", "_", names(dat))
  rownames(dat) <- NULL
  head(dat)
  write.csv2(dat, "Inputfiles/Step0_OriginalDatabase_Tedesco_et_al_2017.csv")


  #FILTRAMOS AL ESTATUS
  dat2 <- dat %>% filter(X3_Native_Exotic_Status == "exotic")
  dat2

  #NO DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  dat_noduplicados <- noduplicates(dat2, "X6_Fishbase_Valid_Species_Name")
  dat_noduplicados[] <- lapply(dat_noduplicados, \(x) if (is.character(x)) gsub("\\.", " ", x) else x)

  #Obtener Habitat
  source(file.path("R", "check_habitat.r"))
  dataset <- dat_noduplicados
  nombres <- dataset$X6_Fishbase_Valid_Species_Name
  acept_name <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(acept_name, dataset)
  dat_act

  #Obtener si es freshwater
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh



  #Conectamos con la tabla Drainage_basins_Table que nos da las regiones
  ### CARGAMOS EL DATASET de la bd
  DatabaseBasin <-dat_fresh
  #Localizaciones en X1_Basin_Name
  head(DatabaseBasin)

  ### CARGAMOS LA TABLA CON LAS LOCALIZACIONES Y LOS CÓDIGOS PARA LA CORRESPONDENCIA
  locations_table <- read.csv2("Inputfiles/Drainage_Basins_Table.csv")
  #Localizaciones en X1.Basin.Name
  #Paises en X2.Country
  head(locations_table)

  library(stringr)  # Por si usas str_detect

  DatabaseBasin$BasinCountries <- mapply(function(basins_str) {
    basins <- unlist(strsplit(basins_str, ","))  # Separar cuencas por coma
    COUNTRIES <- unlist(sapply(basins, function(b) {
      b <- trimws(b)  # Quitar espacios

      # Coincidencia exacta
      country_match <- locations_table$X2.Country[locations_table$X1.Basin.Name == b]

      # Coincidencia por keyword (si quisieras usarla, podrías agregar una columna de keywords)
      if (length(country_match) == 0) {
        country_match <- locations_table$X2.Country[str_detect(locations_table$X1.Basin.Name, b)]
      }

      if (length(country_match) == 0) return(NA)

      # Si hay múltiples países en un solo string
      split_match <- unlist(strsplit(country_match, ";"))
      trimws(split_match)
    }))

    COUNTRIES <- unique(na.omit(COUNTRIES))

    if (length(COUNTRIES) == 0) {
      return(NA)
    } else {
      return(paste(COUNTRIES, collapse = "; "))
    }
  }, DatabaseBasin$X1_Basin_Name)


  write.xlsx(DatabaseBasin, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Tedesco_et_al_2017.xlsx")
}
