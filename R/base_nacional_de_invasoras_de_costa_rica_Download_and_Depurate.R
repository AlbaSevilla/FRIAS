base_nacional_de_invasoras_de_costa_rica_Download_and_Depurate <- function() {
  #Depurate
  dat <- read.xlsx("InputFiles/originaldatabase_base_nacional_de_invasoras_de_costa_rica.xlsx")
  CostaRica_noduplicates <- noduplicates(dat, "scientific_name")
  dataset <- CostaRica_noduplicates
  species_list0 <- CostaRica_noduplicates$scientific_name
  species_list <- name_backbone_checklist(species_list0)$canonicalName
  CostaRica_new <- check_habitat(species_list, dataset)
  dataset_freshwater <- CostaRica_new %>%
    filter(grepl("FRESHWATER", Habitat))
  Invaded_country <- "Costa Rica"
  dataset_freshwater$Invaded_country <- Invaded_country
  dataset_freshwater <- dataset_freshwater[, c("Species", setdiff(names(dataset_freshwater), "Species"))]

  #Save
  write.xlsx (dataset_freshwater, "./InputFiles/freshwatersubset_base_nacional_de_invasoras_de_costa_rica.xlsx")
  }
