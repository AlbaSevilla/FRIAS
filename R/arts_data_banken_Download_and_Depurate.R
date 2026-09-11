arts_data_banken_Download_and_Depurate <- function(){
  #Depurate:
  dat <- read_excel("InputFiles/originaldatabase_arts_data_banken.xlsx")
  dat_noduplicates <- noduplicates(dat, "Nombre_Cientifico")
  dat_noduplicates$invaded_country <- "Norway"
  write.xlsx(dat_noduplicates, "InputFiles/originaldatabase_arts_data_banken.xlsx")
  dataset <- dat_noduplicates
  names <- dataset$Nombre_Cientifico
  accepted_names <- name_backbone_checklist(names)$canonicalName
  final_dataset <- check_habitat(accepted_names, dataset)
  dat_fresh <- final_dataset %>% filter(grepl("FRESHWATER", Habitat))

  #save
  write.xlsx(dat_fresh, "InputFiles/freshwatersubset_arts_data_banken.xlsx")
}
