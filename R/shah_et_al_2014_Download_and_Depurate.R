shah_et_al_2014_Download_and_Depurate <- function(){
  #Depurate
  dataset <- read_excel("Inputfiles/originaldatabase_shah_et_al_2014.xlsx")
  species_names <- dataset$Especies
  accepted_names <- name_backbone_checklist(species_names)$canonicalName
  dataset <- check_habitat(accepted_names, dataset)
  dataset <- dataset %>% filter(grepl("FRESHWATER", Habitat))
  dataset$Invaded_Country <- "India"

  #Save
  write.xlsx(dataset, "Inputfiles/freshwatersubset_shah_et_al_2014.xlsx")
}
