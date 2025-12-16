inbuy_uruguay_2021_Download_and_Depurate <- function(){
  #Depurate
  dataset <- read_excel("InputFiles/originaldatabase_inbuy_uruguay_2021.xlsx")
  dataset <- dataset %>% select(-author)
  dataset <- noduplicates(dataset, "scientific_name")
  dataset$Invaded_Country <- "Uruguay"
  species_names <- dataset$scientific_name
  species_names_gbif <- name_backbone_checklist(species_names)$canonicalName
  dataset <- check_habitat(species_names_gbif, dataset)
  dataset <- dataset %>% filter(grepl("FRESHWATER", Habitat))

  #Save
  write.xlsx(dataset, "InputFiles/freshwatersubset_inbuy_uruguay_2021.xlsx")
}
