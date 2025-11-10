FinalMasterlist <- function(){
  MasterList <- read.csv("OutputFiles/Intermediate/step13_standardizedestablishmentmeans_masterlist.csv")
  MasterList$AcceptedNameGBIF
  names(MasterList)

  #--------------------------------------------------
  # Make all column names with first Upper case     |
  #--------------------------------------------------
  colnames(MasterList) <- paste0(
    toupper(substr(colnames(MasterList), 1, 1)),   # primera letra a mayúscula
    substr(colnames(MasterList), 2, nchar(colnames(MasterList)))  # resto igual
  )

  MasterList <- MasterList %>%
    mutate(across(
      .cols = -Source_Data,
      .fns = ~ str_replace_all(., "\\b[Nn][Aa]\\b", "") %>%      # elimina 'NA' o 'na' como palabra entera
        str_replace_all(",\\s*,", ",") %>%                       # limpia comas duplicadas
        str_replace_all("^,\\s*|\\s*,$", "") %>%                 # elimina comas al inicio o final
        str_squish()                                             # elimina espacios extra
    ))
  names(MasterList)
  ############ IMPACTS MASTERLIST ############################
  MasterList_impacts <- MasterList %>%
    mutate(EICATImpact = str_split(EICATImpact, ",")) %>%
    unnest(EICATImpact) %>%
    mutate(EICATImpact = str_trim(tolower(EICATImpact))) %>%
    mutate(Mechanisms = str_split(Mechanisms, ", ")) %>%
    unnest(Mechanisms) %>%
    mutate(Mechanisms = str_trim(tolower(Mechanisms)))

  Standardization_EICATImpact <- read_excel("TablesToStandardize/Table S2.xlsx", sheet = "EICAT Impact Equivalences", col_names = TRUE)
  Standardization_EICATImpact <- Standardization_EICATImpact[c(1:8),]
  colnames(Standardization_EICATImpact) <- Standardization_EICATImpact[1,]
  MasterList_impacts$EICATImpact
  AbbreviationToNumbers <- setNames(Standardization_EICATImpact$Number, Standardization_EICATImpact$Abbreviation)
  MasterList_impacts$EICATImpact <- AbbreviationToNumbers[MasterList_impacts$EICATImpact]
names(MasterList_impacts)
  names(MasterList_impacts)[tolower(names(MasterList_impacts)) == "degreeofestablishment"] <- "degreeofEstablishment"
  names(MasterList_impacts)[tolower(names(MasterList_impacts)) == "establishmentmeans"] <- "establishmentMeans"

  MasterList_impacts2 <- MasterList_impacts[,c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF",
                                               "Kingdom","Phylum","Class","Order", "Family", "FunctionalGroup", "Group",
                                               "NativeCountry","NativeCountryISO3","NativeBioregions",
                                               "InvadedCountry","InvadedCountryISO3","InvadedBioregions",
                                               "establishmentMeans","degreeofEstablishment","Pathways","EICATImpact","Mechanisms","EICAT_by_Mechanism",
                                               "AffectedNativeSpecies","Habitat","Source_Data")]
  MasterList_impacts2[] <- lapply(MasterList_impacts2, function(x) gsub(",", ";", x))

  write.xlsx(MasterList_impacts2,
             file.path("FinalFiles", paste0("FRIAS_impacts_masterlist.xlsx")),
             sep = ";",
             row.names = FALSE,
             col.names = TRUE)
  write.csv(MasterList_impacts2, "FinalFiles/FRIAS_impacts_masterlist.csv")

  ####### MASTERLIST DE LISTA DE NOMBRES Y columna de MAXIMO/MINIMO IMPACTO #####
  #GENERAMOS LA MASTERLIST SIN IMPACTOS
  columnas_specieslist <- c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF",
                              "Kingdom","Phylum","Class","Order", "Family", "FunctionalGroup", "Group",
                              "NativeCountry","NativeCountryISO3","NativeBioregions",
                              "InvadedCountry","InvadedCountryISO3","InvadedBioregions",
                              "establishmentMeans","degreeofEstablishment","Pathways","EICATImpact","Mechanisms","EICAT_by_Mechanism",
                              "AffectedNativeSpecies","Habitat","Source_Data")
  MasterList_specieslist <- MasterList %>%
    mutate(EICATImpact = str_split(EICATImpact, ",")) %>%
    unnest(EICATImpact) %>%
    mutate(EICATImpact = str_trim(tolower(EICATImpact))) %>%
    mutate(Mechanisms = str_split(Mechanisms, ";")) %>%
    unnest(Mechanisms) %>%
    mutate(Mechanisms = str_trim(tolower(Mechanisms)))

  Standardization_EICATImpact <- read_excel("TablesToStandardize/Table S2.xlsx", sheet = "EICAT Impact Equivalences", col_names = TRUE)
  Standardization_EICATImpact <- Standardization_EICATImpact[c(1:8),]
  colnames(Standardization_EICATImpact) <- Standardization_EICATImpact[1,]
  AbbreviationToNumbers <- setNames(Standardization_EICATImpact$Number, tolower(Standardization_EICATImpact$Abbreviation))
  NumbersToAbbreviation <- setNames(tolower(Standardization_EICATImpact$Abbreviation), Standardization_EICATImpact$Number)

  MasterList_specieslist <- MasterList_specieslist %>%
    mutate(EICATImpactNum = AbbreviationToNumbers[EICATImpact])

  impact_min <- MasterList_specieslist %>%
    group_by(OriginalNameDB) %>%
    filter(!is.na(EICATImpactNum)) %>%
    slice_min(order_by = EICATImpactNum, n = 1, with_ties = FALSE) %>%
    mutate(Min_EICAT_by_Mechanism = paste0(
      toupper(NumbersToAbbreviation[as.character(EICATImpactNum)]), " - ", Mechanisms
    )) %>%
    select(OriginalNameDB, Min_EICAT_by_Mechanism)

  impact_max <- MasterList_specieslist %>%
    group_by(OriginalNameDB) %>%
    filter(!is.na(EICATImpactNum)) %>%
    slice_max(order_by = EICATImpactNum, n = 1, with_ties = FALSE) %>%
    mutate(Max_EICAT_by_Mechanism = paste0(
      toupper(NumbersToAbbreviation[as.character(EICATImpactNum)]), " - ", Mechanisms
    )) %>%
    select(OriginalNameDB, Max_EICAT_by_Mechanism)

  MasterList <- MasterList %>%
    left_join(impact_min, by = "OriginalNameDB") %>%
    left_join(impact_max, by = "OriginalNameDB")

  names(MasterList)
  names(MasterList)[tolower(names(MasterList)) == "degreeofestablishment"] <- "degreeofEstablishment"
  names(MasterList)[tolower(names(MasterList)) == "establishmentmeans"] <- "establishmentMeans"

  ordered_columns <- c(
    "OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF",
    "Kingdom", "Phylum", "Class", "Order", "Family",
    "FunctionalGroup", "Group",
    "establishmentMeans", "degreeofEstablishment","Pathways",
    "NativeCountry", "NativeCountryISO3", "NativeBioregions",
    "InvadedCountry", "InvadedCountryISO3", "InvadedBioregions",
    "EICATImpact", "Mechanisms", "Min_EICAT_by_Mechanism", "Max_EICAT_by_Mechanism",
    "AffectedNativeSpecies", "Habitat",
    "ReportYear", "EarliestReport",
    "Source_Data"
  )
  impact_maxmin <- MasterList[, ordered_columns]


  source(file.path("R", "noduplicates.r"))
  MasterList_specieslist <- noduplicates(impact_maxmin, "AcceptedNameGBIF")


  MasterList_specieslist <- MasterList_specieslist %>%
    mutate(across(
      .cols = -Source_Data,
      .fns = ~ str_replace_all(., "\\b[Nn][Aa]\\b", "") %>%      # elimina 'NA' o 'na' como palabra entera
        str_replace_all(",\\s*,", ",") %>%                       # limpia comas duplicadas
        str_replace_all("^,\\s*|\\s*,$", "") %>%                 # elimina comas al inicio o final
        str_squish()                                             # elimina espacios extra
    ))

  MasterList_specieslist[] <- lapply(MasterList_specieslist, function(x) gsub(",", ";", x))

  write.xlsx(MasterList_specieslist,
             file.path("FinalFiles", paste0("FRIAS_masterlist.xlsx")),
             sep = ",",
             row.names = FALSE,
             col.names = TRUE)
  write.csv(MasterList_specieslist,"FinalFiles/FRIAS_masterlist.csv")

 species_number <- nrow(MasterList_specieslist)
 cat("🐟 FRIAS Masterlist contains :",  species_number, "Alien Freshwater Species 🦀.", "\n")
}

