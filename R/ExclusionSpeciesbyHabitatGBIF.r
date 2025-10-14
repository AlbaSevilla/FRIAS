ExclusionSpeciesbyHabitatGBIF <- function(){
  #Load necessary functions
  source(file.path("R", "colapse_through_AcceptedNameGBIF.r"))
  source(file.path("R", "noduplicates.r"))
  source(file.path("R", "check_habitat.r"))

  #Preparing files
  ruta_intermediate <- "OutputFiles/Intermediate"
  if (!dir.exists(ruta_intermediate)) dir.create(ruta_intermediate, recursive = TRUE)
  data_files <- list.files(path = ruta_intermediate, pattern = "^Step1_Prepared.*\\.csv$", full.names = TRUE)
  db_names <- sub("^.*Step1_Prepared_(.*)\\.csv$", "\\1", data_files)

  #Merge files
  cat("Merging databases  \n")
  for (i in seq_along(db_names)) {
    cat("Processing database : ", db_names[i], "\n")
    dat <- read.csv(data_files[i], sep = ";", stringsAsFactors = FALSE)
    dat$habitat <- ifelse(
      is.na(dat$Habitat),
      NA,
      paste0(dat$Habitat, " (", db_names[i], ")")
    )
    if (i == 1) {
      alldat <- dat
    } else {
      alldat <- merge(alldat, dat, by = "OriginalNameDB", all = TRUE)
      while (any(grepl("\\.y$", names(alldat)))) {
        dupl_base <- sub("\\.y$", "", grep("\\.y$", names(alldat), value = TRUE))
        for (col in dupl_base) {
          col_x <- paste0(col, ".x")
          col_y <- paste0(col, ".y")
          if (col == "eventDate") {
            alldat[[col]] <- apply(alldat[, c(col_x, col_y)], 1, function(x) {
              x <- as.numeric(x)
              if (all(is.na(x))) NA else min(x, na.rm = TRUE)
            })
          } else {
            alldat[[col]] <- paste(alldat[[col_x]], alldat[[col_y]], sep = "; ")
          }
          alldat[[col]] <- gsub("NA; ", "", alldat[[col]])
          alldat[[col]] <- gsub("; NA", "", alldat[[col]])
          alldat[[col]] <- trimws(alldat[[col]])
        }
        alldat <- alldat[ , !grepl("\\.x$|\\.y$", names(alldat))]
        numero_especies <- nrow(alldat)
      }
    }
  }
  #Correct some mistakes
  alldat$Group <- gsub("NA", "", alldat$Group)
  originalnames <- alldat$OriginalNameDB
  alldat$CorrectedAcceptedNameGBIF <- name_backbone_checklist(originalnames)$canonicalName
  alldat$AcceptedNameGBIF <- alldat$CorrectedAcceptedNameGBIF
  alldat$ID_GBIF <- sapply(strsplit(as.character(alldat$ID_GBIF), ";"), `[`, 1)
  alldat[] <- lapply(alldat, function(x) gsub(",", ";", x))

  #Merge and no duplicates
  alldat_final <- colapse_through_AcceptedNameGBIF(alldat)
  alldat_final <- noduplicates(alldat_final, "OriginalNameDB")
  names(alldat_final)

  #Final dataset with checked habitat
  alldat_final <- alldat_final %>%
    rename(Habitat_FRIAS = habitat) %>%
    {.[ ] <- lapply(., function(x) {
      if (is.factor(x)) as.character(x)
      else if (is.list(x)) sapply(x, toString)
      else x
    }); .
    }

  write.xlsx(alldat_final, file.path("OutputFiles", "Check", "Habitat_MasterList.xlsx"))
  write.csv(alldat_final, file.path("OutputFiles", "Check", "Habitat_MasterList.csv"), row.names = FALSE)


  ##########################################################################################################
  #Separated check habitat files
  MasterList2 <- read_excel("OutputFiles/Check/Habitat_MasterList.xlsx") %>%
    filter(
      AcceptedNameGBIF == "NA" |
        str_count(AcceptedNameGBIF, "\\S+") > 1
    )

  species_names <- MasterList2$OriginalNameDB
  accepted_name <- name_backbone_checklist(species_names)$canonicalName
  MasterList_HabitatGBIFandDatabases <- check_habitat(accepted_name, MasterList2)

  #MASTERLIST WITH HABITAT BY GBIF AND ORIGINAL HABITAT
  MasterList_HabitatGBIFandDatabases <- MasterList_HabitatGBIFandDatabases %>%
    mutate(Habitat_GBIF = Habitat) %>%
    mutate(Habitat_DDBB = Habitat_FRIAS) %>%
    select(-Habitat,-Species,-Habitat_FRIAS)
  write_xlsx(MasterList_HabitatGBIFandDatabases, "OutputFiles/Check/Check_Habitat_Masterlist.xlsx") #MATERLIST CON EL HABITAT POR GBIF Y EL HABITAT ORIGINAL

  #MASTERLIST WHERE THE HABITAT BY GBIF AND THE ORIGINAL HABITAT DO NOT MATCH
  MasterList_Inconsistencies <- MasterList_HabitatGBIFandDatabases %>%
    filter(!grepl("FRESHWATER", Habitat_GBIF))
  write_xlsx(MasterList_Inconsistencies, "OutputFiles/Check/Incoherences_Habitat_Masterlist.xlsx") #masterlist where the GBIF habitat and the database habitat do NOT match

  MasterList_Coincidences <- MasterList_HabitatGBIFandDatabases %>%
    filter(grepl("FRESHWATER", Habitat_GBIF) | is.na(Habitat_GBIF))
  write_xlsx(MasterList_Coincidences, "OutputFiles/Check/Coincidences_Habitat_Masterlist.xlsx") #masterlist where the GBIF habitat and the database habitat do NOT match


  # STICK WITH THE FRESHWATER ON THE CHECKLIST
  Step3_Masterlist <- read_excel("OutputFiles/Intermediate/Step3_CorrectedAcceptedNameGBIF_Masterlist.xlsx") %>%
    mutate(Habitat_DB = Habitat) %>%
    mutate(CorrectedAcceptedNameGBIF = AcceptedNameGBIF)

  names <- Step3_Masterlist$CorrectedAcceptedNameGBIF
  check_habitat_dataset <- check_habitat(names, Step3_Masterlist)
  names(check_habitat_dataset)
  names(Step3_Masterlist)

  #FinalDataset
  freshwater_database <- check_habitat_dataset %>%
    mutate(Habitat_DB = if_else(is.na(Habitat_DB), "freshwater", Habitat_DB)) %>%
    mutate(Habitat = if_else(is.na(Habitat), Habitat_DB, Habitat),
           Habitat = toupper(Habitat)) %>%
    filter(grepl("FRESHWATER", Habitat)) %>%
    select(-Species, -Habitat) %>%
    mutate(Habitat = Habitat_DB) %>%
    select(-Habitat_DB) %>%
    select(-AcceptedNameGBIF) %>%
    mutate(AcceptedNameGBIF = CorrectedAcceptedNameGBIF) %>%
    select(-CorrectedAcceptedNameGBIF)%>%
    select(
      OriginalNameDB,AcceptedNameGBIF,ID_GBIF,Kingdom,Phylum,Class,Order,Family,FunctionalGroup,
      Group,EstablishmentMeans,Pathways,FirstRecords,OldestDate,NativeCountry,InvadedCountry,
      InvadedCountryISO3,AffectedNativeSpecies,EICATImpact,Mechanisms,EICAT_by_Mechanism,Habitat,Source_Data)

  write_xlsx(freshwater_database, "OutputFiles/Intermediate/Step4_SelectedFreshwaterGBIF_Masterlist.xlsx") #masterlist donde el habitat de gbif y de la bbdd SI coinciden
}
