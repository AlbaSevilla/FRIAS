FinalMasterlist <- function(){
  MasterList <- read.xlsx("OutputFiles/Intermediate/step15_obtainfunctionalgroup_masterlist.xlsx")

  #Create Max impact field
  MasterList <- MasterList %>%
    mutate(EICATImpact = str_split(EICATImpact, ",")) %>%
    unnest(EICATImpact) %>%
    mutate(EICATImpact = str_trim(tolower(EICATImpact))) %>%
    mutate(Mechanisms = str_split(Mechanisms, ";")) %>%
    unnest(Mechanisms) %>%
    mutate(Mechanisms = str_trim(tolower(Mechanisms)))
  MasterList <- MasterList %>%
    mutate(
      EICAT_by_Mechanism = str_split(EICAT_by_Mechanism, "[;,]")   # separar por ; o ,
    ) %>%
    unnest(EICAT_by_Mechanism) %>%
    mutate(
      EICAT_by_Mechanism = str_trim(EICAT_by_Mechanism),
      EICAT_by_Mechanism = ifelse(EICAT_by_Mechanism == "" | str_detect(EICAT_by_Mechanism, "^[-–—]"), NA, EICAT_by_Mechanism)
    )

  MasterList <- noduplicates(MasterList, "AcceptedNameGBIF")

  Standardization_EICATImpact <- read_excel("TablesToStandardize/Table S2.xlsx", sheet = "9.1 EICAT Impact", col_names = TRUE)
  Standardization_EICATImpact <- Standardization_EICATImpact[c(1:11),c(5:6)]
  colnames(Standardization_EICATImpact) <- Standardization_EICATImpact[1,]
  Standardization_EICATImpact <- Standardization_EICATImpact[-1,]
  Standardization_EICATImpact <- Standardization_EICATImpact %>%
    filter(!is.na(Abbreviation))
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
  names(MasterList)[tolower(names(MasterList)) == "establishmentmeans"] <- "establishmentMeans"

  #Corrections in earliest report
  MasterList <- MasterList %>%
    mutate(EarliestReport = sapply(EarliestReport, function(x) {
      if (is.na(x) || x == "") return(x)
      str_trim(str_split(x, "\\s*[,;]\\s*", simplify = TRUE)[1])
    }))

  #Capitalize capital letters in each cell
  capitalize_phrase <- function(x) {
    if (!is.character(x)) return(x)
    sapply(x, function(cell) {
      if (is.na(cell) || cell == "") return(cell)
      parts <- str_split(cell, ",", simplify = FALSE)[[1]]
      parts <- sapply(parts, function(p) {
        p <- str_trim(p)
        p <- str_to_lower(p)
        paste0(str_to_upper(str_sub(p, 1, 1)), str_sub(p, 2))
      })
      paste(parts, collapse = ", ")
    })
  }
  MasterList <- MasterList %>%
    mutate(across(
      .cols = !c(Source_Data, NativeRangeISO3, RecipientRangeISO3),
      .fns = capitalize_phrase
    ))

  #Remove duplicates across cells
  remove_duplicates_all <- function(x) {
    if (!is.character(x)) return(x)
    sapply(x, function(cell) {
      if (is.na(cell) || cell == "") return(cell)
      parts <- str_split(cell, ",", simplify = FALSE)[[1]]
      parts <- str_trim(parts)
      parts <- parts[!duplicated(parts)]
      paste(parts, collapse = ", ")
    })
  }
  MasterList <- MasterList %>%
    mutate(across(
      .cols = !c(OriginalNameDB),
      .fns = remove_duplicates_all
    ))


  #
  table1 <- read.xlsx("TablesToStandardize/Table S1.xlsx", sheet="Databases") %>%
    select(FRIAS_name, Reference) %>%
    mutate(FRIAS_name = gsub(" ", "_", FRIAS_name))
  MasterList <- MasterList %>%
    separate_rows(Source_Data, sep=";|,") %>%
    mutate(Source_Data = trimws((Source_Data)))
  head(MasterList$Source_Data)
  merged <- merge(
    MasterList,
    table1,
    by.x = "Source_Data",
    by.y = "FRIAS_name",
    all.x = TRUE
  )
  merged_grouped <- merged %>%
    dplyr::group_by(AcceptedNameGBIF) %>%
    dplyr::summarise(
      across(
        everything(),        # todas las columnas
        ~ paste(unique(.), collapse = "; "),  # combina valores únicos con ";"
        .names = "{.col}"
      ),
      .groups = "drop"
    )

  #
  namesordered <- c("AcceptedNameGBIF","OriginalNameDB","ID_GBIF",
                    "Kingdom","Phylum","Class","Order","Family",
                    "Group","FunctionalGroup",
                    "NativeRange","NativeRangeISO3","NativeBioregions",
                    "RecipientRange","RecipientRangeISO3","RecipientBioregions",
                    "establishmentMeans",
                    "pathway","EICATImpact", "Mechanisms","Max_EICAT_by_Mechanism",
                    "AffectedTaxa",
                    "ReportYear","EarliestReport",
                    "Habitat","Source_Data", "Reference")
  MasterList3<- merged_grouped[,namesordered]

  MasterList3 <- MasterList3 %>%
    mutate(across(
      .cols = -Source_Data,
      .fns = ~ str_replace_all(., "\\b[Nn][Aa]\\b", "") %>%
        str_replace_all(",\\s*,", ",") %>%
        str_replace_all("^,\\s*|\\s*,$", "") %>%
        str_squish()
    ))

  MasterList4 <- MasterList3 %>%
    mutate(
      across(
        -Reference,
        ~ gsub(",", ";", .)
      ),
      across(
        -Reference,
        ~ na_if(., "")
      )
    )

  MasterList5 <- MasterList4[order(rowSums(is.na(MasterList4))), ]

  write.xlsx(MasterList5,
             file.path("FinalFiles", paste0("FRIAS_masterlist.xlsx")),
             sep = ",",
             row.names = FALSE,
             col.names = TRUE)
  write.csv(MasterList5,"FinalFiles/FRIAS_masterlist.csv")

 species_number <- nrow(MasterList5)
 cat("🐟 FRIAS Masterlist contains :",  species_number, "Alien Freshwater Species 🦀.", "\n")
}

