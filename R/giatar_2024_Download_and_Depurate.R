giatar_2024_Download_and_Depurate <- function(){
  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://zenodo.org/records/17123201/files/GIATAR.zip?download=1"
  destfile <- "InputFiles/originaldatabase_giatar_2024.zip"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  # Descomprimir el archivo
  #TARDA UN RATILLO
  unzip("InputFiles/originaldatabase_giatar_2024.zip", exdir = "InputFiles")
  directorio_original <- getwd()

  ##############################################
  ########### DEPURAR ##########################
  ##############################################
  ###################################################################
  ################ TABLA DE RANGOS NATIVOS ##########################
  rangos_nativos <- read.csv("InputFiles/dataset/native ranges/all_sources_native_ranges.csv")
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
  first_records <- read.csv("InputFiles/dataset/occurrences/all_records.csv")
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

  write_xlsx(Dataset_Completo2, "InputFiles/originaldatabase_giatar_2024.xlsx")

  #Obtener Habitat
  source(file.path("R", "check_habitat.r"))
  dataset <- Dataset_Completo2
  especies_lista0 <- dataset$ScientificName
  resultados <- character(length(especies_lista0))
  for (i in seq_along(especies_lista0)) {
    especie <- especies_lista0[i]
    tryCatch({
      res <- name_backbone_checklist(especie)
      resultados[i] <- res$canonicalName
    }, error = function(e) {
      message("Species not found ", especie, ": ", e$message)
      resultados[i] <- NA
    })

    Sys.sleep(0.2)
  }
  especies_lista <- resultados
  dat_act <- check_habitat(especies_lista, dataset)


  #freshwater
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh
  source(file.path("R", "OldestDate.r"))
  dat_fresh <- OldestDate(dat_fresh,"Year")

  write.xlsx(dat_fresh, "InputFiles/freshwatersubset_giatar_2024.xlsx")
}
