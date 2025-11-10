dahruddin_et_al_2017_Download_and_Depurate <- function(){
  ########### DESCARGAR ####################
  ##########################################
  #A mano:
  #url <- "https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2F1755-0998.12528&file=men12528-sup-0002-TableS1.xlsx"

  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  dat <- read_excel("Inputfiles/originaldatabase_dahruddin_et_al_2017.xlsx")

  #No duplicados
  source(file.path("R", "noduplicates.r"))
  dat_noduplicates <- noduplicates(dat, "Species")

  #Habitat
  source(file.path("R", "check_habitat.r"))
  dataset <- dat_noduplicates
  nombres <- dataset$Species
  nom_acep <- name_backbone_checklist(nombres)$canonicalName
  habitat_dat <- check_habitat(nom_acep, dataset)

  #Freshwater
  freshwater_species <- habitat_dat %>% dplyr::filter(grepl("FRESHWATER", Habitat))
  freshwater_species$Invaded_country <- "Indonesia"
  freshwater_species <- freshwater_species %>% filter(Status == "Introduced")
  write.xlsx(freshwater_species, "Inputfiles/freshwatersubset_dahruddin_et_al_2017.xlsx")
}
