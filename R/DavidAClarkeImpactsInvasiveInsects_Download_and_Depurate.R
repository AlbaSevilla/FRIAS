DavidAClarkeImpactsInvasiveInsects_Download_and_Depurate <- function(){

  url <- "https://zenodo.org/records/8191061/files/DavidAClarke/Impacts_invasive_insects-v2.1.1.zip?download=1"
  destfile <- "InputFiles/OriginalDatabase_DavidAClarkeImpactsInvasiveInsects.zip"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  # Descomprimir el archivo
  unzip(destfile, exdir = "InputFiles")
  directorio_original <- getwd()



  ###############################################################
  ########## ARCHIVO ############################################
  ###############################################################
  ruta_inputfiles <- file.path("Inputfiles/DavidAClarke-Impacts_invasive_insects-cda7b94/Data")
  data_name <- list.files(path = ruta_inputfiles, pattern = "Assessment_information.xlsx", full.names = TRUE)
  dat <- read.xlsx(data_name, sheet="Assessment_results_input") #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.
  head(dat)
  write.xlsx(dat, "Inputfiles/Step0_OriginalDatabase_DavidAClarkeImpactsInvasiveInsects.xlsx")

  #No duplicados
  source(file.path("R", "noduplicates.r"))
  dat_noduplicados <- noduplicates(dat, "scientificName")
  dat_noduplicados

  #Habitat
  source(file.path("R", "check_habitat.r"))
  dataset <- dat_noduplicados
  especies <- dat_noduplicados$scientificName
  acept_name <- name_backbone_checklist(especies)$canonicalName
  dat_act <- check_habitat(acept_name, dataset)


  #Freshwater?
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))

  write.xlsx(dat_fresh, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_DavidAClarkeImpactsInvasiveInsects.xlsx")
}
