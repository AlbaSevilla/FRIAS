FCDFloraCroaticaDatabase_Download_and_Depurate <- function(){
  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://hirc.botanic.hr/tmp/7bdf66b2-4b44-4b5b-b772-61a62d20ea7d.xlsx.zip"
  destfile <- "InputFiles/Step0_OriginalDatabase_FCDFloraCroaticaDatabase.zip"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  # Descomprimir el archivo
  unzip("InputFiles/Step0_OriginalDatabase_FCDFloraCroaticaDatabase.zip", exdir = "InputFiles")
  directorio_original <- getwd()

  ##############################################
  ########### DEPURAR ##########################
  ##############################################
  dat <- read_excel("Inputfiles/FCD.xlsx", col_names = FALSE)
  names(dat) <- gsub("\\.", "_", names(dat))
  dat
  names(dat) <- c("CodeID", "Species_name")
  write.xlsx(dat, "Inputfiles/Step0_OriginalDatabase_FCDFloraCroaticaDatabase.xlsx")

  #NO DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  dat_noduplicados <- noduplicates(dat, "Species_name")

  #Obtener Habitat
  source(file.path("R", "check_habitat.r"))
  dataset <- dat_noduplicados
  nombres <- dataset$Species_name
  acept_name <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(acept_name, dataset)
  dat_act

  #Obtener si es freshwater
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh

  dat_fresh$Invaded_country <- "Croatia"

  write.xlsx(dat_fresh, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_FCDFloraCroaticaDatabase.xlsx")
}
