oliva_paterna_life_invasaqua_Download_and_Depurate <- function() {
  #Depurate
  data_blacklist <- read.xlsx(file.path("InputFiles","originaldatabase_oliva_paterna_life_invasaqua.xlsx"),sheet=3)
  data_alertlist <- read.xlsx(file.path("InputFiles","originaldatabase_oliva_paterna_life_invasaqua.xlsx"),sheet=4)
  data_subset_blacklist <- data_blacklist
  Invaded_Country <- "Spain"
  data_subset_blacklist$Invaded_Country <- Invaded_Country
  data_subset_blacklist2 <- data_subset_blacklist %>%
    filter(Status %in% c("Established"))
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
  names(data_subset_blacklist3)[names(data_subset_blacklist3) == "Black.list.-.Scientific.name"] <- "ScientificName"
  names(data_subset_blacklist3)[names(data_subset_blacklist3) == "Native.Range"] <- "Native_Range"
  data_subset_blacklist4 <- data_subset_blacklist3
  blacklist_noduplicates <- noduplicates(data_subset_blacklist4, "ScientificName")
  dataset <- blacklist_noduplicates
  species_list0 <- dataset$ScientificName
  species_list <- name_backbone_checklist(species_list0)$canonicalName
  dataset_updated <- check_habitat(species_list, dataset)
  dataset_freshwater_blacklist <- dataset_updated %>%
    filter(grepl("FRESHWATER", Habitat))
  data_subset_alertlist <- data_alertlist
  Invaded_Country <- "Spain"
  data_subset_alertlist$Invaded_Country <- Invaded_Country
  data_subset_alertlist2 <- data_subset_alertlist
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
  names(data_subset_alertlist3)[names(data_subset_alertlist3) == "Alert.list.-.Scientific.name"] <- "ScientificName"
  names(data_subset_alertlist3)[names(data_subset_alertlist3) == "Native.Range"] <- "Native_Range"
  data_subset_alertlist4 <- data_subset_alertlist3
  alertlist_noduplicates <- noduplicates(data_subset_alertlist4, "ScientificName")
  dataset <- alertlist_noduplicates
  species_list0 <- dataset$ScientificName
  species_list <- name_backbone_checklist(species_list0)$canonicalName
  dataset_updated <- check_habitat(species_list, dataset)
  dataset_freshwater_alertList <- dataset_updated %>%
    filter(grepl("FRESHWATER", Habitat))
  names(dataset_freshwater_alertList)[names(dataset_freshwater_alertList) == "Taxa"] <- "Taxa.included"
  FINAL_dataset <- rbind(dataset_freshwater_blacklist, dataset_freshwater_alertList)

  #Save
  write.xlsx(FINAL_dataset, "./InputFiles/freshwatersubset_oliva_paterna_life_invasaqua.xlsx")
}

