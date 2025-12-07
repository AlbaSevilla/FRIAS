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

  #Missmatches
  freshwater_database_missmatches <- check_habitat_dataset %>%
    filter(!is.na(Habitat) & !grepl("FRESHWATER", Habitat, ignore.case = TRUE))
  write.csv(freshwater_database_missmatches, "OutputFiles/Check/Discarted_NotFreshwater_Species.csv")
  write.xlsx(freshwater_database_missmatches, "OutputFiles/Check/Discarted_NotFreshwater_Species.xlsx")

  #Final
  check_habitat_dataset2 <- check_habitat_dataset %>%
    filter(grepl("FRESHWATER", Habitat))
  check_habitat_dataset2$Habitat_DB[is.na(check_habitat_dataset2$Habitat_DB)] <- "FRESHWATER"
  check_habitat_dataset2 <- check_habitat_dataset2 %>%
    rename(Habitat_GBIF = Habitat) %>%
    mutate(
      Habitat = paste(Habitat_GBIF, Habitat_DB, sep = ", "),
      Habitat = toupper(Habitat)
    ) %>%
    select(-Habitat_GBIF,-Habitat_DB) %>%
    separate_rows(Habitat, sep=",") %>%
    mutate(Habitat = trimws(Habitat))

  # Vector de palabras clave en mayúsculas
  freshwater_keywords <- c("FRESHWATER", "RIVER", "STREAM", "CREEK", "LAKE", "POND", "POOL",
                           "WETLAND", "BOG", "FEN", "MARSH", "SPRING", "RIPARIAN", "SHALLOW WATER")

  # Filtrar filas que contengan cualquiera de estas palabras
  freshwater_habitats <- check_habitat_dataset2 %>%
    filter(grepl(paste(freshwater_keywords, collapse = "|"), Habitat)) %>%
    filter(
      !(Habitat %in% c("NA","MARINE", "TERRESTRIAL", "MARINE, TERRESTRIAL", "TERRESTRIAL, MARINE"))
    )


  freshwater_database <- freshwater_habitats %>%
    mutate(AcceptedNameGBIF = CorrectedAcceptedNameGBIF) %>%
    select(-CorrectedAcceptedNameGBIF)%>%
    select(
      OriginalNameDB,AcceptedNameGBIF,ID_GBIF,Kingdom,Phylum,Class,Order,Family,FunctionalGroup,
      Group,establishmentMeans,pathway,ReportYear,EarliestReport,NativeRange,RecipientRange,
      RecipientRangeISO3,AffectedTaxa,EICATImpact,Mechanisms,EICAT_by_Mechanism,Habitat,Source_Data)

  #Colapse
  source(file.path("R", "noduplicates.r"))
  freshwater_database_final <- noduplicates(freshwater_database, "AcceptedNameGBIF")

  #Save
  write.xlsx(freshwater_database_final, "OutputFiles/Intermediate/step4_selectedfreshwatergbif_masterlist.xlsx", sep=";") #masterlist donde el habitat de gbif y de la bbdd SI coinciden
  write.csv(freshwater_database_final, "OutputFiles/Intermediate/step4_selectedfreshwatergbif_masterlist.csv")
}
