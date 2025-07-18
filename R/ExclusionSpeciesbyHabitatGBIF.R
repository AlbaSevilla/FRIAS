ExclusionSpeciesbyHabitatGBIF <- function(){
  Masterlist <- read_excel("OutputFiles/Intermediate/Step11_ObtainFuctionalGroup_Masterlist.xlsx")
  names(Masterlist)[names(Masterlist) == "Habitat"] <- "Habitat_Old"

  source(file.path("R", "check_habitat.r"))
  dataset <- Masterlist
  especies <- dataset$AcceptedNameGBIF
  dat_hab <- check_habitat(especies, dataset)

  #Nos quedamos con las freshwater
  dat_fresh <- dat_hab %>% filter(grepl("FRESHWATER", Habitat))
  write.xlsx(dat_fresh, "OutputFiles/Intermediate/Step12_SelectedFreshwaterGBIF_Masterlist.xlsx")

  #Las no seleccionadas, porque no clasifica GBIF como Freshwater:
  dat_nofresh <- dat_hab %>% filter(!grepl("FRESHWATER", Habitat))
  write.xlsx(dat_nofresh, "OutputFiles/Check/NotSelectedFreshwaterGBIF_Masterlist.xlsx")
}
