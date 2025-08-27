GIATAR_Download_and_Depurate <- function(){
  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://zenodo.org/records/15498757/files/GIATAR.zip?download=1"
  destfile <- "InputFiles/Step0_OriginalDatabase_GIATAR.zip"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  # Descomprimir el archivo
  #TARDA UN RATILLO
  unzip("InputFiles/Step0_OriginalDatabase_GIATAR.zip", exdir = "InputFiles")
  directorio_original <- getwd()

  ##############################################
  ########### DEPURAR ##########################
  ##############################################
  ###################################################################
  ################ TABLA DE RANGOS NATIVOS ##########################
  rangos_nativos <- read.csv("Inputfiles/dataset/native ranges/all_sources_native_ranges.csv")
  names(rangos_nativos)
  head(rangos_nativos)
  rangos_nativos <- rangos_nativos[,-1]
  seleccionar_columnas <- c("genus_species", "bioregion", "DAISIE_region", "usageKey")
  rangos_nativos <- rangos_nativos[,seleccionar_columnas]
  colnames(rangos_nativos) <- c("ScientificName", "Bioregion", "Native_Range", "usageKey")

  #NO DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  rangos_nativos_noduplicados <- noduplicates(rangos_nativos, "ScientificName")

  ####################################################################
  ############### TABLA DE FIRST RECORDS #############################
  first_records <- read.csv("Inputfiles/dataset/occurrences/all_records.csv")
  names(first_records)
  head(first_records)
  seleccionar_columnas2 <- c("usageKey", "location", "year", "ISO3")
  first_records <- first_records[,seleccionar_columnas2]

  #NO DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  first_records_noduplicados <- noduplicates(first_records, "usageKey")
  colnames(first_records_noduplicados) <- c("usageKey", "Invaded_country", "Year", "ISO3")

  ####################################################################
  ##########  JUNTAMOS AMBOS DATASETS ################################
  Dataset_Completo <- merge(first_records_noduplicados, rangos_nativos_noduplicados, by = "usageKey")
  Dataset_Completo <- as.data.frame(Dataset_Completo)
  head(Dataset_Completo)
  Dataset_Completo <- Dataset_Completo %>%
    select(ScientificName, everything())
  Dataset_Completo2 <- Dataset_Completo %>% select(-usageKey)

  write_xlsx(Dataset_Completo2, "Step0_OriginalDatabase_GIATAR.xlsx")

  #Obtener Habitat
  source(file.path("R", "check_habitat.r"))
  dataset <- Dataset_Completo2
  nombres <- dataset$ScientificName
  acept_name <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(acept_name, dataset)
  dat_act

  #Obtener si es freshwater
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh

  #Obtener fecha más antigua
  source(file.path("R", "OldestDate.r"))
  dat_fresh <- OldestDate(dat_fresh,"Year")

  write.xlsx(dat_fresh, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_GIATAR.xlsx")
}
