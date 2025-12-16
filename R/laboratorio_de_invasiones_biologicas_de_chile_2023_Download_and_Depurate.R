laboratorio_de_invasiones_biologicas_de_chile_2023_Download_and_Depurate <- function() {
  #Depurate
  dat <- read_excel("Inputfiles/originaldatabase_laboratorio_de_invasiones_biologicas_de_chile.xlsx")
  laboratorio_de_invasiones_biologicas_de_chile_2023_noduplicates <- noduplicates(dat, "scientific_name")
  dataset <- laboratorio_de_invasiones_biologicas_de_chile_2023_noduplicates
  species_list0 <- dataset$scientific_name
  species_list <- name_backbone_checklist(species_list0)$canonicalName
  dataset_updated <- check_habitat(species_list, dataset)
  dataset_freshwater <- dataset_updated %>%
    filter(grepl("FRESHWATER", Habitat))
  dataset_freshwater <- dataset_freshwater[, c("scientific_name", setdiff(names(dataset_freshwater), "scientific_name"))]
  Invaded_country <- "Chile"
  dataset_freshwater$Invaded_Country <- Invaded_country

  #Save
  write.xlsx(dataset_freshwater, "./Inputfiles/freshwatersubset_laboratorio_de_invasiones_biologicas_de_chile_2023.xlsx")
}
