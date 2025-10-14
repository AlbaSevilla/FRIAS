CanoBarbacidLifeInvasaqua_Download_and_Depurate <- function() {
  #Descarga
  #BD PROPORCIONADA POR BELINDA

  ##############################################
  ########## DEPURACIÓN ########################
  ##############################################
  datos_blacklist <- read.xlsx(file.path("Inputfiles","Step0_OriginalDatabase_CanoBarbacid.xlsx"),sheet=3)
  datos_alertlist <- read.xlsx(file.path("Inputfiles","Step0_OriginalDatabase_CanoBarbacid.xlsx"),sheet=4)

  ######################
  ##### BLACKLIST ######
  ######################
  data_subset_blacklist <- datos_blacklist

  #Its a database of invasive species in Spain so:
  Invaded_Country <- "Spain"
  data_subset_blacklist$Invaded_Country <- Invaded_Country

  #Select alien that are established in Spain:
  data_subset_blacklist2 <- data_subset_blacklist %>%
    filter(Status %in% c("Established"))

  #Change abbreviations into full names
  native_range_map <- c("Eur" = "Europe", "Afr" = "Africa", "As" = "Asia-temperate",
                        "At" = "Asia-tropical", "Aus" = "Australasia", "Pac" = "Pacific",
                        "NAm" = "N America", "SAm" = "S America", "Ant" = "Antarctica")

  pathways_map <- c("1" = "Release", "2" = "Escape", "3" = "Contaminant",
                    "4" = "Stowaway", "5" = "Corridor", "6" = "Unaided", "7" = "Unknown")


 data_subset_blacklist3 <- data_subset_blacklist2 %>%
    mutate(
      Native.Range = str_replace_all(Native.Range, native_range_map),
      Pathways = str_replace_all(Pathways, pathways_map)
    )

  #Change columns names
  names(data_subset_blacklist3)[names(data_subset_blacklist3) == "Black.list.-.Scientific.name"] <- "ScientificName"
  names(data_subset_blacklist3)[names(data_subset_blacklist3) == "Native.Range"] <- "Native_Range"
  data_subset_blacklist4 <- data_subset_blacklist3


  #NO DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  blacklist_sinduplicados <- noduplicates(data_subset_blacklist4, "ScientificName")


  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  # Preparar dataset
  dataset <- blacklist_sinduplicados
  especies_lista0 <- dataset$ScientificName
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)


  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater_blacklist <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))


  write.xlsx (dataset_freshwater_blacklist, "./InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_blacklistLIFEINVASAQUA.xlsx")
  cat("El archivo se descargó correctamente: InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_blacklistLIFEINVASAQUA.xlsx")





  ######################
  ##### AlertList ######
  ######################
  data_subset_alertlist <- datos_alertlist

  #Its a database of invasive species in Spain so:
  Invaded_Country <- "Spain"
  data_subset_alertlist$Invaded_Country <- Invaded_Country

  #Select alien that are established in Spain:
  data_subset_alertlist2 <- data_subset_alertlist

  #Change abbreviations into full names
  native_range_map <- c("Eur" = "Europe", "Afr" = "Africa", "As" = "Asia-temperate",
                        "At" = "Asia-tropical", "Aus" = "Australasia", "Pac" = "Pacific",
                        "NAm" = "N America", "SAm" = "S America", "Ant" = "Antarctica")

  pathways_map <- c("1" = "Release", "2" = "Escape", "3" = "Contaminant",
                    "4" = "Stowaway", "5" = "Corridor", "6" = "Unaided", "7" = "Unknown")


  data_subset_alertlist3 <- data_subset_alertlist2 %>%
    mutate(
      Native.Range = str_replace_all(Native.Range, native_range_map),
      Pathways = str_replace_all(Pathways, pathways_map)
    )

  #Change columns names
  names(data_subset_alertlist3)[names(data_subset_alertlist3) == "Alert.list.-.Scientific.name"] <- "ScientificName"
  names(data_subset_alertlist3)[names(data_subset_alertlist3) == "Native.Range"] <- "Native_Range"
  data_subset_alertlist4 <- data_subset_alertlist3


  #NO DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  alertlist_sinduplicados <- noduplicates(data_subset_alertlist4, "ScientificName")


  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  # Preparar dataset
  dataset <- alertlist_sinduplicados
  especies_lista0 <- dataset$ScientificName
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)


  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater_alertList <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))


  write.xlsx (dataset_freshwater_alertList, "./InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_alertlistLIFEINVASAQUA.xlsx")
  cat("El archivo se descargó correctamente: InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_alertlistLIFEINVASAQUA.xlsx")
}

