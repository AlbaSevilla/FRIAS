larissa_et_al_2017_Download_and_Depurate <- function(){
  #####################################
  ########### DESCARGAR ###############
  #####################################
  url <- "https://static-content.springer.com/esm/art%3A10.1007%2Fs10750-017-3446-2/MediaObjects/10750_2017_3446_MOESM2_ESM.xlsx"
  destfile <- "InputFiles/originaldatabase_larissa_et_al_2017.xlsx"
  download.file(url, destfile, mode = "wb")

  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  dat <- read_excel("Inputfiles/originaldatabase_larissa_et_al_2017.xlsx", sheet="Introduced Species")
  head(dat)
  dat <- as.data.frame(dat)

  #Habitat
  source(file.path("R", "check_habitat.r"))
  dataset <- dat
  nombres <- dataset$Species
  nom_acep <- name_backbone_checklist(nombres)$canonicalName
  habitat_dat <- check_habitat(nom_acep, dataset)

  #Freshwater
  freshwater_species <- habitat_dat %>% filter(grepl("FRESHWATER", Habitat))
  freshwater_species$Invaded_country <- "Brazil"
  freshwater_species <- freshwater_species %>%
    select(Species, everything())

  write.xlsx(freshwater_species, "Inputfiles/freshwatersubset_larissa_et_al_2017.xlsx")
}
