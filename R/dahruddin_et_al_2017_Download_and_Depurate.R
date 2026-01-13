dahruddin_et_al_2017_Download_and_Depurate <- function(){
  #Depurate
  dat <- read_excel("Inputfiles/originaldatabase_dahruddin_et_al_2017.xlsx")
  dat_noduplicates <- noduplicates(dat, "Species")
  dataset <- dat_noduplicates
  names <- dataset$Species
  accepted_name <- name_backbone_checklist(names)$canonicalName
  habitat_dat <- check_habitat(accepted_name, dataset)
  freshwater_species <- habitat_dat %>% dplyr::filter(grepl("FRESHWATER", Habitat))
  freshwater_species$Invaded_country <- "Indonesia"
  freshwater_species <- freshwater_species %>% filter(Status == "Introduced")

  #Save
  write.xlsx(freshwater_species, "Inputfiles/freshwatersubset_dahruddin_et_al_2017.xlsx")
}
