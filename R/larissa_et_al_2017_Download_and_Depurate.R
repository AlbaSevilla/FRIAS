larissa_et_al_2017_Download_and_Depurate <- function(){
  #Download
  url <- "https://static-content.springer.com/esm/art%3A10.1007%2Fs10750-017-3446-2/MediaObjects/10750_2017_3446_MOESM2_ESM.xlsx"
  destfile <- "InputFiles/originaldatabase_larissa_et_al_2017.xlsx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dat <- read_excel("InputFiles/originaldatabase_larissa_et_al_2017.xlsx", sheet="Introduced Species")
  dataset <- as.data.frame(dat)
  species_names <- dataset$Species
  acep_names <- name_backbone_checklist(species_names)$canonicalName
  dataset_updated <- check_habitat(acep_names, dataset)
  dataset_fresh <- dataset_updated %>% filter(grepl("FRESHWATER", Habitat))
  dataset_fresh$Invaded_country <- "Brazil"
  dataset_fresh <- dataset_fresh %>% select(Species, everything())

  #Save
  write.xlsx(dataset_fresh, "InputFiles/freshwatersubset_larissa_et_al_2017.xlsx")
}
