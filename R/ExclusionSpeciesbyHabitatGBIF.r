ExclusionSpeciesbyHabitatGBIF <- function(){
  # Open database
  step3_masterlist <- read.csv("OutputFiles/Intermediate/step3_correctedacceptednameGBIF_masterlist.csv") %>%
    mutate(Habitat_DB = Habitat) %>%
    mutate(CorrectedAcceptedNameGBIF = AcceptedNameGBIF)
  #Obtain habitat from GBIF
  source(file.path("R", "check_habitat.r"))
  names <- step3_masterlist$CorrectedAcceptedNameGBIF
  check_habitat_dataset <- check_habitat(names, step3_masterlist)
  names(check_habitat_dataset)
  #FinalDataset
  table(check_habitat_dataset$Habitat)
  freshwater_database0 <- check_habitat_dataset %>%
    filter(grepl("FRESHWATER|NA", Habitat, ignore.case = TRUE))
  #Missmatches
  freshwater_database_missmatches <- check_habitat_dataset %>%
    filter(!grepl("FRESHWATER|NA", Habitat, ignore.case = TRUE))
  write.csv(freshwater_database_missmatches, "OutputFiles/Check/Discarted_species.csv")
  #Final
  freshwater_database <- freshwater_database0 %>%
    mutate(Habitat_DB = if_else(is.na(Habitat_DB), "FRESHWATER", Habitat_DB)) %>% #If Habitat_DB is empty, is written 'freshwater'
    mutate(Habitat = if_else(is.na(Habitat), Habitat_DB, Habitat), # If Habitat is empty, is completed with information of Habitat_DB
           Habitat = toupper(Habitat))
  freshwater_database <- freshwater_database %>%
    mutate(AcceptedNameGBIF = CorrectedAcceptedNameGBIF) %>%
    select(-CorrectedAcceptedNameGBIF)%>%
    select(
      OriginalNameDB,AcceptedNameGBIF,ID_GBIF,Kingdom,Phylum,Class,Order,Family,FunctionalGroup,
      Group,establishmentMeans,Pathway,ReportYear,EarliestReport,NativeCountry,InvadedCountry,
      InvadedCountryISO3,AffectedNativeSpecies,EICATImpact,Mechanisms,EICAT_by_Mechanism,Habitat,Source_Data)
  #Colapse
  source(file.path("R", "noduplicates.r"))
  freshwater_database_final <- noduplicates(freshwater_database, "AcceptedNameGBIF")
  #Save
  write.xlsx(freshwater_database_final, "OutputFiles/Intermediate/step4_selectedfreshwatergbif_masterlist.xlsx", sep=";") #masterlist donde el habitat de gbif y de la bbdd SI coinciden
  write.csv(freshwater_database_final, "OutputFiles/Intermediate/step4_selectedfreshwatergbif_masterlist.csv")
}
