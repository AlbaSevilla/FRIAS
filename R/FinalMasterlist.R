FinalMasterlist <- function(){
  MasterList <- read.xlsx("OutputFiles/Intermediate/step15_obtainfunctionalgroup_masterlist.xlsx")

  #Create Max impact field
  MasterList <- MasterList %>%
    mutate(
      EICAT_by_Mechanism = str_split(EICAT_by_Mechanism, "[;,]")
    ) %>%
    unnest(EICAT_by_Mechanism) %>%
    mutate(
      EICAT_by_Mechanism = str_trim(tolower(EICAT_by_Mechanism)),
      EICAT_by_Mechanism = ifelse(
        str_detect(EICAT_by_Mechanism, "^[a-z]{1,5}\\s*[-–—]"),
        EICAT_by_Mechanism,
        NA
      )
    )
  MasterList <- MasterList %>%
    separate(
      EICAT_by_Mechanism,
      into = c("Mechanism_code", "Mechanism_text"),
      sep = "\\s*[-–—]\\s*",
      extra = "merge",
      fill = "right"
    )
  MasterList <- MasterList %>%
    separate_rows(Mechanism_text, sep = "/") %>%
    mutate(Mechanism_text = str_trim(Mechanism_text))
  equivs <- read_excel(
    "TablesToStandardize/Table S2.xlsx",
    sheet = "9.2 EICAT Mechanisms",
    col_names = TRUE
  ) %>%
    mutate(
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategoriesMechanisms = trimws(StandardizedCategoriesMechanisms)
    ) %>%
    separate_rows(OriginalCategories, sep = ";") %>%
    mutate(OriginalCategories = trimws(OriginalCategories))
  MasterList <- MasterList %>%
    left_join(
      equivs,
      by = c("Mechanism_text" = "OriginalCategories")
    ) %>%
    mutate(
      Mechanism_text = ifelse(
        !is.na(StandardizedCategoriesMechanisms),
        StandardizedCategoriesMechanisms,
        Mechanism_text
      )
    ) %>%
    select(-StandardizedCategoriesMechanisms)
  MasterList <- MasterList %>%
    mutate(
      EICAT_by_Mechanism = paste0(Mechanism_code, " - ", Mechanism_text)
    ) %>%
    select(-Mechanism_code, -Mechanism_text)


  table(MasterList$EICAT_by_Mechanism)

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
  MasterList <- MasterList %>%
    mutate(
      EICAT_code = str_extract(EICAT_by_Mechanism, "^[a-z]{2}"),
      EICAT_priority = case_when(
        EICAT_code == "mv" ~ 6,
        EICAT_code == "mr" ~ 5,
        EICAT_code == "mo" ~ 4,
        EICAT_code == "mn" ~ 3,
        EICAT_code == "mc" ~ 2,
        EICAT_code == "dd" ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    group_by(AcceptedNameGBIF) %>%
    mutate(
      max_priority = max(EICAT_priority, na.rm = TRUE)
    ) %>%
    mutate(
      Max_EICAT_by_Mechanism = if(all(is.na(EICAT_priority))) {
        NA_character_
      } else {
        # todas las filas con la prioridad máxima, concatenadas con "; "
        paste(EICAT_by_Mechanism[EICAT_priority == max_priority], collapse = "; ")
      }
    ) %>%
    ungroup() %>%
    select(-EICAT_code, -EICAT_priority, -max_priority)

  MasterList <- noduplicates(MasterList, "AcceptedNameGBIF")

  names(MasterList)[tolower(names(MasterList)) == "establishmentmeans"] <- "establishmentMeans"

  #Select correct fields after process:
  names_fields <- c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF",
                    "Kingdom","Phylum","Class","Order","Family",
                    "Group","FunctionalGroup",
                    "NativeRange","NativeRangeISO3","NativeBioregions",
                    "RecipientRange","RecipientRangeISO3","RecipientBioregions",
                    "establishmentMeans","pathway",
                    "EICATImpact","Mechanisms","Max_EICAT_by_Mechanism",
                    "AffectedTaxa",
                    "ReportYear","EarliestReport",
                    "Habitat",
                    "Source_Data")

  MasterList <- MasterList[,names_fields]

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
  namesordered <- c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF",
                    "Kingdom","Phylum","Class","Order","Family",
                    "Group","FunctionalGroup",
                    "NativeRange","NativeRangeISO3","NativeBioregions",
                    "RecipientRange","RecipientRangeISO3","RecipientBioregions",
                    "establishmentMeans","pathway",
                    "EICATImpact","Mechanisms","Max_EICAT_by_Mechanism",
                    "AffectedTaxa",
                    "ReportYear","EarliestReport",
                    "Habitat",
                    "Source_Data","Reference")
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
      ),
      ID_GBIF = str_split(ID_GBIF, ";") %>% sapply(`[`, 1) %>% trimws()
    )

  #MasterList5 <- MasterList4[order(rowSums(is.na(MasterList4))), ]
  MasterList5 <- MasterList4[order(MasterList4$AcceptedNameGBIF), ]

  write.xlsx(MasterList5,
             file.path("FinalFiles", paste0("FRIAS_masterlist.xlsx")),
             sep = ",",
             row.names = FALSE,
             col.names = TRUE)
  write.csv(MasterList5,"FinalFiles/FRIAS_masterlist.csv")

 species_number <- nrow(MasterList5)
 cat("🐟 FRIAS Masterlist contains :",  species_number, "Alien Freshwater Species 🦀.", "\n")
}

