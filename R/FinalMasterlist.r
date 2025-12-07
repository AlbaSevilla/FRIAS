FinalMasterlist <- function(){
  MasterList <- read.csv("OutputFiles/Intermediate/step14_obtainfunctionalgroup_masterlist.csv")

  colnames(MasterList) <- paste0(
    toupper(substr(colnames(MasterList), 1, 1)),   #first letter to capital
    substr(colnames(MasterList), 2, nchar(colnames(MasterList)))
  )

  MasterList <- MasterList %>%
    mutate(across(
      .cols = -Source_Data,
      .fns = ~ str_replace_all(., "\\b[Nn][Aa]\\b", "") %>%
        str_replace_all(",\\s*,", ",") %>%
        str_replace_all("^,\\s*|\\s*,$", "") %>%
        str_squish()
    ))
  names(MasterList)

  MasterList <- MasterList %>%
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

  MasterList <- MasterList %>%
    mutate(EICATImpactNum = AbbreviationToNumbers[EICATImpact])

  impact_max <- MasterList %>%
    group_by(OriginalNameDB) %>%
    filter(!is.na(EICATImpactNum)) %>%
    slice_max(order_by = EICATImpactNum, n = 1, with_ties = FALSE) %>%
    mutate(Max_EICAT_by_Mechanism = paste0(
      toupper(NumbersToAbbreviation[as.character(EICATImpactNum)]), " - ", Mechanisms
    )) %>%
    select(OriginalNameDB, Max_EICAT_by_Mechanism)

  MasterList <- MasterList %>%
    left_join(impact_max, by = "OriginalNameDB")

  names(MasterList)
  names(MasterList)[tolower(names(MasterList)) == "degreeofestablishment"] <- "degreeofEstablishment"
  names(MasterList)[tolower(names(MasterList)) == "establishmentmeans"] <- "establishmentMeans"

  namesordered <- c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF",
                    "Kingdom","Phylum","Class","Order","Family",
                    "Group","FunctionalGroup",
                    "NativeRange","NativeRangeISO3","NativeBioregions",
                    "RecipientRange","RecipientRangeISO3","RecipientBioregions",
                    "establishmentMeans","degreeofEstablishment",
                    "Pathway","EICATImpact", "Mechanisms","Max_EICAT_by_Mechanism",
                    "AffectedTaxa",
                    "ReportYear","EarliestReport",
                    "Habitat","Source_Data")
  MasterList<- MasterList[,namesordered]
  MasterList <- noduplicates(MasterList, "AcceptedNameGBIF")

  MasterList <- MasterList %>%
    mutate(across(
      .cols = -Source_Data,
      .fns = ~ str_replace_all(., "\\b[Nn][Aa]\\b", "") %>%
        str_replace_all(",\\s*,", ",") %>%
        str_replace_all("^,\\s*|\\s*,$", "") %>%
        str_squish()
    ))

  MasterList[] <- lapply(MasterList, function(x) gsub(",", ";", x))
  MasterList <- MasterList[order(rowSums(is.na(MasterList))), ]

  write.xlsx(MasterList,
             file.path("FinalFiles", paste0("FRIAS_masterlist.xlsx")),
             sep = ",",
             row.names = FALSE,
             col.names = TRUE)
  write.csv(MasterList,"FinalFiles/FRIAS_masterlist.csv")

 species_number <- nrow(MasterList)
 cat("🐟 FRIAS Masterlist contains :",  species_number, "Alien Freshwater Species 🦀.", "\n")
}

