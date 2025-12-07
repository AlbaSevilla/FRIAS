usriis_2022_Download_and_Depurate <- function() {
  #Depurate
  dat <- read.xlsx("InputFiles/originaldatabase_usriis_2022.xlsx") #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.
  usriis_2022_freshwater <- dat %>%
    filter(taxonRank %in% c("Species")) %>%
    filter(isHybrid %in% c("FALSE"))
  dat <- dat %>%
    mutate(occurrenceID = sub("^[^-]+-([^-]+)-.*$", "\\1", occurrenceID))
  usriis_2022_noduplicates <- noduplicates(usriis_2022_freshwater, "scientificName")
  dataset <- usriis_2022_noduplicates
  species_names <- dataset$scientificName
  dataset_updated <- check_habitat(species_names, dataset)
  dataset_freshwater <- dataset_updated %>% filter(grepl("FRESHWATER", Habitat))
  dataset_freshwater$InvadedCountry <- "United States"

  #Save
  write.xlsx(dataset_freshwater, "./Inputfiles/freshwatersubset_usriis_2022.xlsx")
}
