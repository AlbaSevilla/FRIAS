seebens_et_al_2023_Download_and_Depurate <- function() {
  #Download
  url <- "https://zenodo.org/records/10039630/files/GlobalAlienSpeciesFirstRecordDatabase_v3.1_freedata.xlsx?download=1"
  destfile <- "InputFiles/originaldatabase_seebens_et_al_2023.xlsx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dataset <- read.xlsx(file.path("InputFiles","originaldatabase_seebens_et_al_2023.xlsx"), sheet=2)
  dataset <- dataset[dataset$FirstRecord >= 0, ]
  dataset <- dataset %>% filter(PresentStatus %in% c("alien", "Established invasive"))
  dataset <- dataset %>%
    mutate(
      Habitat_freshwater = as.numeric(Habitat_freshwater),
      Habitat_terrestrial = as.numeric(Habitat_terrestrial),
      Habitat_marine = as.numeric(Habitat_marine)
    ) %>%
    filter(Habitat_freshwater == 1 |
             (Habitat_freshwater == 0 & Habitat_terrestrial == 0 & Habitat_marine == 0) |
             (is.na(Habitat_freshwater) & is.na(Habitat_terrestrial) & is.na(Habitat_marine)) |
             (Habitat_freshwater == 0 & is.na(Habitat_terrestrial) & is.na(Habitat_marine)) |
             (is.na(Habitat_freshwater) & Habitat_terrestrial == 0 & is.na(Habitat_marine)) |
             (is.na(Habitat_freshwater) & is.na(Habitat_terrestrial) & Habitat_marine == 0) |
             (is.na(Habitat_freshwater) & Habitat_terrestrial == 0 & Habitat_marine == 0) |
             (Habitat_freshwater == 0 & is.na(Habitat_terrestrial) & Habitat_marine == 0) |
             (Habitat_freshwater == 0 & Habitat_terrestrial == 0 & is.na(Habitat_marine))
    )
  dataset[] <- lapply(dataset, as.character)
  dataset <- noduplicates(dataset, "TaxonName")
  dataset <- OldestDate(dataset, "FirstRecord")
  colnames(dataset)[colnames(dataset) == "LifeForm"] <- "Group"
  species_names <- dataset$TaxonName
  accepted_names <- name_backbone_checklist(species_names)$canonicalName
  dataset <- check_habitat(accepted_names, dataset)
  dataset_freshwater <- dataset %>%
    filter(grepl("FRESHWATER", Habitat))

  write.xlsx(dataset_freshwater, "./InputFiles/freshwatersubset_seebens_et_al_2023.xlsx")
}
