carboneras_et_al_2018_Download_and_Depurate <- function(){
  #Depurate
  dataset_Carboneras <- read.xlsx(file.path("InputFiles","originaldatabase_carboneras_et_al_2018.xlsx"),sheet = "EU IAS prioritised list")
  dataset_Carboneras <- noduplicates(dataset_Carboneras, "Species.name")
  dataset <- dataset_Carboneras
  specieslist0 <- dataset$Species.name
  specieslist <- name_backbone_checklist(specieslist0)$canonicalName
  updated_dataset <- check_habitat(specieslist, dataset)
  updated_dataset2 <- updated_dataset %>%
    select(-Species)
  dataset_freshwater <- updated_dataset2 %>%
    filter(grepl("FRESHWATER", Habitat))
  dataset_freshwater$InvadedRange <- "Europe"

  #Save
  write.xlsx(dataset_freshwater, "./InputFiles/freshwatersubset_carboneras_et_al_2018.xlsx")
}
