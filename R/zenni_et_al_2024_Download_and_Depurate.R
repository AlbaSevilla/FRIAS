zenni_et_al_2024_Download_and_Depurate <- function(){
  ####################################################################################
  ################### PARA DESCARGAR LA BASE DE DATOS ################################
  ####################################################################################
  url <- "https://static-content.springer.com/esm/art%3A10.1007%2Fs10530-024-03302-9/MediaObjects/10530_2024_3302_MOESM3_ESM.xlsx"
  destfile <- "InputFiles/originaldatabase_zenni_et_al_2024.xlsx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  ####################################################################################
  #################### PARA DEPURAR LA BASE DE DATOS #################################
  ####################################################################################
  datos <- read.xlsx(file.path("Inputfiles","originaldatabase_zenni_et_al_2024.xlsx"),sheet=3)

  data_subset_zenni_et_al_20242 <- datos %>%
    filter(Habitat %in% c("Terrestrial, Freshwater", "Freshwater")) %>%
    filter(Origin %in% c("Non-native in Brazil", "Hybrid with no native range"))

  invaded_country <- "Brazil"
  data_subset_zenni_et_al_20242$Invaded_Country <- invaded_country

  #Eliminar duplicados
  source(file.path("R","noduplicates.r"))
  zenni_et_al_2024_sinduplicados <- noduplicates(data_subset_zenni_et_al_20242, "Species.name")

  write.xlsx (zenni_et_al_2024_sinduplicados, "./InputFiles/freshwatersubset_zenni_et_al_2024.xlsx")
}
