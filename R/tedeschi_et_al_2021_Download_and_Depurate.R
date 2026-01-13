tedeschi_et_al_2021_Download_and_Depurate <- function() {
 #Depurate
  dat <- read.xlsx(file.path("Inputfiles","originaldatabase_tedeschi_et_al_2021.xlsx"),sheet=2)
  dat <- dat %>%
    filter(Present_Status == "alien")
  dat_noduplicates <- noduplicates(dat, column_name_species = "Species")
  dataset <- dat_noduplicates
  species_list0 <- dataset$Species
  species_list <- name_backbone_checklist(species_list0)$canonicalName
  updated_dataset <- check_habitat(species_list, dataset)
  dataset_freshwater <- updated_dataset %>%
    filter(grepl("FRESHWATER", Habitat))

  #Save
  write.xlsx(dataset_freshwater, "./Inputfiles/freshwatersubset_tedeschi_et_al_2021.xlsx")
}
