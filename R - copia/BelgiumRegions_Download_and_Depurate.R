BelgiumRegions_Download_and_Depurate <- function(){
  ####################################################
  ########          DESCARGA         #################
  ####################################################
  url <- "https://zenodo.org/records/10897501/files/ranking_emerging_status_hierarchical_strategy_Wallonia.tsv?download=1"
  destfile <- "InputFiles/ranking_emerging_status_points_strategy_Wallonia.tsv"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  url <- "https://zenodo.org/records/10897501/files/ranking_emerging_status_hierarchical_strategy_Flanders.tsv?download=1"
  destfile <- "InputFiles/ranking_emerging_status_points_strategy_Flanders.tsv"
  download.file(url, destfile, mode = "wb")

  url <- "https://zenodo.org/records/10897501/files/ranking_emerging_status_hierarchical_strategy_Brussels.tsv?download=1"
  destfile <- "InputFiles/ranking_emerging_status_points_strategy_Brussels.tsv"
  download.file(url, destfile, mode = "wb")

  url <- "https://zenodo.org/records/10897501/files/ranking_emerging_status_hierarchical_strategy_Belgium.tsv?download=1"
  destfile <- "InputFiles/ranking_emerging_status_points_strategy_Belgium.tsv"
  download.file(url, destfile, mode = "wb")



  ##############################
  ########## WALLONIA ##########
  ##############################
  data <- read.csv("InputFiles/ranking_emerging_status_points_strategy_Wallonia.tsv", fill=TRUE, header=TRUE, sep="\t")
  region <- "Wallonia"
  data$region <- region
  dataset <- data %>%
    select("canonicalName", "kingdom", "region")

  ##############################
  ########## FLANDERS ##########
  ##############################
  data2 <- read.csv("InputFiles/ranking_emerging_status_points_strategy_Flanders.tsv", fill=TRUE, header=TRUE, sep="\t")
  region <- "Flanders"
  data2$region <- region
  dataset2 <- data2 %>%
    select("canonicalName", "kingdom", "region")


  ##############################
  ########## BRUSSELS ##########
  ##############################
  data3 <- read.csv("InputFiles/ranking_emerging_status_points_strategy_Brussels.tsv", fill=TRUE, header=TRUE, sep="\t")
  region <- "Brussels"
  data3$region <- region
  dataset3 <- data3 %>%
    select("canonicalName", "kingdom", "region")

  ##############################
  ########## BELGIUM ###########
  ##############################
  data4 <- read.csv("InputFiles/ranking_emerging_status_points_strategy_Belgium.tsv", fill=TRUE, header=TRUE, sep="\t")
  region4 <- "Brussels"
  data4$region <- region
  dataset4 <- data4 %>%
    select("canonicalName", "kingdom", "region")


  #############################################
  ######## UNIFICAMOS LOS 4 DATASETS ##########
  #############################################
  DATASET_final <- rbind(dataset, dataset2, dataset3, dataset4)


  ##############
  #Eliminamos duplicados
  ##############
  DATASET_sinduplicados <- DATASET_final %>%
    dplyr::group_by(canonicalName) %>%
    dplyr::summarise(across(everything(), ~ {
      texto <- as.character(.)
      vals <- unlist(str_split(texto, "[,;|]+|\\s{2,}"))
      vals <- str_trim(vals)
      vals <- vals[vals != ""]
      vals <- unique(vals)
      paste(vals, collapse = ", ")
    }), .groups = "drop")

  #Se crea DATASET_sinduplicados
  write.xlsx(DATASET_sinduplicados, "./InputFiles/Step0_OriginalDatabase_BelgiumRegions.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabase_BelgiumRegions.xlsx", "\n")

  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  #Obtain habitats
  dataset <- DATASET_sinduplicados
  especies_lista0 <- dataset$canonicalName
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)

  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat)) %>%
    select(-"Species")


  write.xlsx(dataset_freshwater, "./InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_BelgiumRegions.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_BelgiumRegions.xlsx", "\n")

}

