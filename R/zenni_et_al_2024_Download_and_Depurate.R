zenni_et_al_2024_Download_and_Depurate <- function(){
  #Download
  url <- "https://static-content.springer.com/esm/art%3A10.1007%2Fs10530-024-03302-9/MediaObjects/10530_2024_3302_MOESM3_ESM.xlsx"
  destfile <- "InputFiles/originaldatabase_zenni_et_al_2024.xlsx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  datos <- read.xlsx(file.path("InputFiles","originaldatabase_zenni_et_al_2024.xlsx"),sheet=3)
  data_subset_zenni_et_al_20242 <- datos %>%
    filter(Habitat %in% c("Terrestrial, Freshwater", "Freshwater")) %>%
    filter(Origin %in% c("Non-native in Brazil", "Hybrid with no native range"))
  invaded_country <- "Brazil"
  data_subset_zenni_et_al_20242$Invaded_Country <- invaded_country
  zenni_et_al_2024_noduplicates <- noduplicates(data_subset_zenni_et_al_20242, "Species.name")
  names <- zenni_et_al_2024_noduplicates$Species.name
  zenni_et_al_2024_noduplicates$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName

  #Save
  write.xlsx (zenni_et_al_2024_noduplicates, "./InputFiles/freshwatersubset_zenni_et_al_2024.xlsx")
}
