GISIN_Download_and_Depurate <- function(){
  ##############################################
  ############## DESCARGAR #####################
  ##############################################
  url <- "https://web.archive.org/web/20160530030833/http://gisin.org/GODMGenerated/GISIN_Downloads/GISIN_SpeciesResourceURLs.zip"
  destfile <- "InputFiles/OriginalDatabase_GISIN.zip"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  # Descomprimir el archivo
  unzip("InputFiles/OriginalDatabase_GISIN.zip", exdir = "InputFiles")
  directorio_original <- getwd()

  ##############################################
  ########### DEPURAR ##########################
  ##############################################
  ruta_inputfiles <- file.path("Inputfiles")
  data_name <- list.files(path = ruta_inputfiles, pattern = "GISIN_SpeciesResourceURLs_2015_2_06.txt", full.names = TRUE)
  dat <- read.csv(data_name, sep="\t") #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.
  names(dat) <- gsub("\\.", "_", names(dat))
  rownames(dat) <- NULL
  head(dat)
  write.csv2(dat, "Inputfiles/Step0_OriginalDatabase_GISIN.csv")

  #NO DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  dat_noduplicados <- noduplicates(dat, "ScientificName")

  #Nos quedamos con las columnas de interés:
  dat_noduplicados2 <- dat_noduplicados %>% select("ScientificName")

  #Obtener Habitat
  source(file.path("R", "check_habitat.r"))
  dataset <- dat_noduplicados2
  nombres <- dataset$ScientificName
  acept_name <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(acept_name, dataset)
  dat_act

  #Obtener si es freshwater
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh

  write.xlsx(dat_fresh, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_GISIN.xlsx")
}
