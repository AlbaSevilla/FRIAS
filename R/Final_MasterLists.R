Final_MasterLists <- function(){
  MasterList <- read.xlsx("OutputFiles/Intermediate/Step13_StandardizedEstablishmentMeans_Masterlist.xlsx")
  MasterList$AcceptedNameGBIF
  #--------------------------------------------------------------------------------------------
  #ADD OTHER REFERENCES IN DATA SOURCE COLUMN THAT WE HAVEN´T USED BUT OFFERS INFORMATION     |
  #--------------------------------------------------------------------------------------------
  OtherReferences <- read_excel("TablesToStandardize/Harmonization_Databases.xlsx", sheet="MoreReferences")
  colnames(OtherReferences) <- OtherReferences[1, ]
  OtherReferences <- OtherReferences[-1,]
  species_OtherReferences <- OtherReferences %>%
    separate_rows(Species, sep=";")
  species_OtherReferences <- species_OtherReferences[,c("Column_SourceData","Species")]
  species_OtherReferences$AcceptedNameGBIF <- name_backbone_checklist(species_OtherReferences$Species)$canonicalName

  MasterList <- MasterList %>%
    left_join(
      species_OtherReferences %>%
        select(AcceptedNameGBIF, Column_SourceData) %>%
        distinct(),
      by = "AcceptedNameGBIF"
    ) %>%
    mutate(
      Data_Source = ifelse(
        !is.na(Column_SourceData),
        paste0(Source_Data, ", ", Column_SourceData),
        Source_Data
      )
    ) %>%
    select(-Column_SourceData)

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

  ############ IMPACTS MASTERLIST ############################
  if (!file.exists("OutputFiles")){
    dir.create(file.path("OutputFiles/FinalMasterLists"), recursive=TRUE)
  }

  MasterList_impacts <- MasterList %>%
    mutate(EICATImpact = str_split(EICATImpact, ",")) %>%
    unnest(EICATImpact) %>%
    mutate(EICATImpact = str_trim(tolower(EICATImpact))) %>%
    mutate(Mechanisms = str_split(Mechanisms, ", ")) %>%
    unnest(Mechanisms) %>%
    mutate(Mechanisms = str_trim(tolower(Mechanisms)))

  Standardization_EICATImpact <- read_excel("TablesToStandardize/Standardization_EICATImpact.xlsx", sheet=2)
  MasterList_impacts$EICATImpact
  AbbreviationToNumbers <- setNames(Standardization_EICATImpact$Number, Standardization_EICATImpact$Abbreviation)
  MasterList_impacts$EICATImpact <- AbbreviationToNumbers[MasterList_impacts$EICATImpact]

  MasterList_impacts2 <- MasterList_impacts[,c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF",
                                              "Kingdom","Phylum","Class","Order", "Family", "FunctionalGroup", "Group",
                                              "NativeCountry","NativeCountryISO3","NativeBioregions",
                                              "InvadedCountry","InvadedCountryISO3","InvadedBioregions",
                                              "EstablishmentMeans","Pathways","EICATImpact","Mechanisms","EICAT_by_Mechanism",
                                              "AffectedNativeSpecies","Habitat","Source_Data")]
  write.xlsx(MasterList_impacts2,
             file.path("FinalFiles", paste0("FRIAS_AllImpactsList_MasterList.xlsx")),
             sep = ";",
             row.names = FALSE,
             col.names = TRUE)


  ####### MASTERLIST DE LISTA DE NOMBRES Y columna de MAXIMO/MINIMO IMPACTO #####
  #GENERAMOS LA MASTERLIST SIN IMPACTOS
  columnas_specieslist <- c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF",
                              "Kingdom","Phylum","Class","Order", "Family", "FunctionalGroup", "Group",
                              "NativeCountry","NativeCountryISO3","NativeBioregions",
                              "InvadedCountry","InvadedCountryISO3","InvadedBioregions",
                              "EstablishmentMeans","Pathways","EICATImpact","Mechanisms","EICAT_by_Mechanism",
                              "AffectedNativeSpecies","Habitat","Source_Data")
  MasterList_specieslist <- MasterList %>%
    mutate(EICATImpact = str_split(EICATImpact, ",")) %>%
    unnest(EICATImpact) %>%
    mutate(EICATImpact = str_trim(tolower(EICATImpact))) %>%
    mutate(Mechanisms = str_split(Mechanisms, ";")) %>%
    unnest(Mechanisms) %>%
    mutate(Mechanisms = str_trim(tolower(Mechanisms)))

  Standardization_EICATImpact <- read_excel("TablesToStandardize/Standardization_EICATImpact.xlsx", sheet = 2)
  AbbreviationToNumbers <- setNames(Standardization_EICATImpact$Number, tolower(Standardization_EICATImpact$Abbreviation))
  NumbersToAbbreviation <- setNames(tolower(Standardization_EICATImpact$Abbreviation), Standardization_EICATImpact$Number)

  MasterList_specieslist <- MasterList_specieslist %>%
    mutate(EICATImpactNum = AbbreviationToNumbers[EICATImpact])

  impact_min <- MasterList_specieslist %>%
    group_by(OriginalNameDB) %>%
    filter(!is.na(EICATImpactNum)) %>%
    slice_min(order_by = EICATImpactNum, n = 1, with_ties = FALSE) %>%
    mutate(MIN_EICATIMPACTBYMECHANISM = paste0(
      toupper(NumbersToAbbreviation[as.character(EICATImpactNum)]), " - ", Mechanisms
    )) %>%
    select(OriginalNameDB, MIN_EICATIMPACTBYMECHANISM)

  impact_max <- MasterList_specieslist %>%
    group_by(OriginalNameDB) %>%
    filter(!is.na(EICATImpactNum)) %>%
    slice_max(order_by = EICATImpactNum, n = 1, with_ties = FALSE) %>%
    mutate(MAX_EICATIMPACTBYMECHANISM = paste0(
      toupper(NumbersToAbbreviation[as.character(EICATImpactNum)]), " - ", Mechanisms
    )) %>%
    select(OriginalNameDB, MAX_EICATIMPACTBYMECHANISM)

  MasterList <- MasterList %>%
    left_join(impact_min, by = "OriginalNameDB") %>%
    left_join(impact_max, by = "OriginalNameDB")

  ordered_columns <- c(
    "OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF",
    "Kingdom", "Phylum", "Class", "Order", "Family",
    "FunctionalGroup", "Group",
    "EstablishmentMeans", "Pathways",
    "NativeCountry", "NativeCountryISO3", "NativeBioregions",
    "InvadedCountry", "InvadedCountryISO3", "InvadedBioregions",
    "EICATImpact", "Mechanisms", "MIN_EICATIMPACTBYMECHANISM", "MAX_EICATIMPACTBYMECHANISM",
    "AffectedNativeSpecies", "Habitat",
    "FirstRecords", "OldestDate",
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
  write.xlsx(MasterList_specieslist,
             file.path("FinalFiles", paste0("FRIAS_SpeciesList_MasterList.xlsx")),
             sep = ";",
             row.names = FALSE,
             col.names = TRUE)

 species_number <- nrow(MasterList_specieslist)
 cat("🐟 FRIAS MasterList contains :",  species_number, "Alien Freshwater Species 🦀.", "\n")
}

