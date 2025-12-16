horus_Download_and_Depurate <- function() {
  #Depurate
  data <- read.csv("InputFiles/originaldatabase_horus.csv")
  horus_unique <- noduplicates(data, "scientific_name")
  dataset <- horus_unique
  species_list0 <- dataset$scientific_name
  species_list <- name_backbone_checklist(species_list0)$canonicalName
  updated_dataset <- check_habitat(species_list, dataset)
  freshwater_dataset <- updated_dataset %>%
    filter(grepl("FRESHWATER", Habitat))
  freshwater_dataset <- freshwater_dataset[, c("scientific_name", setdiff(names(freshwater_dataset), "scientific_name"))]
  freshwater_dataset$Invaded_Country <- "Brazil"

  #Save
  write.xlsx(freshwater_dataset, "./Inputfiles/freshwatersubset_horus.xlsx")
}
