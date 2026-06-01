FinalMasterlist <- function(){
  MasterList <- read.xlsx("OutputFiles/Intermediate/step15_obtainfunctionalgroup_masterlist.xlsx") %>%
    separate_rows(Impact, sep=";|,") %>%
    mutate(Impact = trimws(Impact))
  MasterList$Group <- gsub("Sar", "SAR", MasterList$Group)

  #########################################
  ######  Related with impact #############
  #########################################
  x <- as.character(MasterList$Impact)
  x[x == "dd"] <- "data deficient"
  x[x == "major"] <- "4"
  x[x == "massive"] <- "5"
  x[x == "mc"] <- "1"
  x[x == "mn"] <- "2"
  x[x == "mo"] <- "3"
  x[x == "mr"] <- "4"
  x[x == "mv"] <- "5"
  MasterList$Impact <- x
  #Create Max impact field
  MasterList <- MasterList %>%
    mutate(
      Impact_by_Mechanism = str_split(Impact_by_Mechanism, "[;,]")
    ) %>%
    unnest(Impact_by_Mechanism) %>%
    mutate(
      Impact_by_Mechanism = str_trim(tolower(Impact_by_Mechanism)),
      Impact_by_Mechanism = ifelse(
        str_detect(Impact_by_Mechanism, "^[a-z]{1,5}\\s*[-–—]"),
        Impact_by_Mechanism,
        NA
      )
    )
  MasterList <- MasterList %>%
    separate(
      Impact_by_Mechanism,
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
      Impact_by_Mechanism = paste0(Mechanism_code, " - ", Mechanism_text)
    ) %>%
    select(-Mechanism_code, -Mechanism_text)
  table(MasterList$Impact_by_Mechanism)
  Standardization_Impact <- read_excel("TablesToStandardize/Table S2.xlsx", sheet = "9.1 Impact", col_names = TRUE)
  Standardization_Impact <- Standardization_Impact[c(1:11),c(5:6)]
  colnames(Standardization_Impact) <- Standardization_Impact[1,]
  Standardization_Impact <- Standardization_Impact[-1,]
  Standardization_Impact <- Standardization_Impact %>%
    filter(!is.na(Abbreviation))
  AbbreviationToNumbers <- setNames(Standardization_Impact$Number, tolower(Standardization_Impact$Abbreviation))
  NumbersToAbbreviation <- setNames(tolower(Standardization_Impact$Abbreviation), Standardization_Impact$Number)
  MasterList <- MasterList %>%
    mutate(ImpactNum = AbbreviationToNumbers[Impact])
  MasterList <- MasterList %>%
    mutate(
      impact_code = str_extract(Impact_by_Mechanism, "^[a-z]{2}"),
      impact_priority = case_when(
        impact_code == "mv" ~ 6,
        impact_code == "mr" ~ 5,
        impact_code == "mo" ~ 4,
        impact_code == "mn" ~ 3,
        impact_code == "mc" ~ 2,
        impact_code == "dd" ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    group_by(AcceptedNameGBIF) %>%
    mutate(
      max_priority = max(impact_priority, na.rm = TRUE)
    ) %>%
    mutate(
      Max_Impact_by_Mechanism = if(all(is.na(impact_priority))) {
        NA_character_
      } else {
        # todas las filas con la prioridad máxima, concatenadas con "; "
        paste(Impact_by_Mechanism[impact_priority == max_priority], collapse = "; ")
      }
    ) %>%
    ungroup() %>%
    select(-impact_code, -impact_priority, -max_priority)

  MasterList$Max_Impact_by_Mechanism <- gsub("dd","DD",MasterList$Max_Impact_by_Mechanism)
  MasterList$Max_Impact_by_Mechanism <- gsub("mc","1",MasterList$Max_Impact_by_Mechanism)
  MasterList$Max_Impact_by_Mechanism <- gsub("mn","2",MasterList$Max_Impact_by_Mechanism)
  MasterList$Max_Impact_by_Mechanism <- gsub("mo","3",MasterList$Max_Impact_by_Mechanism)
  MasterList$Max_Impact_by_Mechanism <- gsub("mr","4",MasterList$Max_Impact_by_Mechanism)
  MasterList$Max_Impact_by_Mechanism <- gsub("mv","5",MasterList$Max_Impact_by_Mechanism)

  MasterList <- noduplicates(MasterList, "AcceptedNameGBIF")

  names(MasterList)[tolower(names(MasterList)) == "establishmentmeans"] <- "establishmentMeans"

  #Select correct fields after process:
  names_fields <- c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF",
                    "Kingdom","Phylum","Class","Order","Family",
                    "Group","FunctionalGroup",
                    "NativeRange","NativeRangeISO3","NativeBioregions",
                    "RecipientRange","RecipientRangeISO3","RecipientBioregions",
                    "establishmentMeans","pathway",
                    "Impact","Mechanisms","Max_Impact_by_Mechanism",
                    "AffectedTaxa",
                    "ReportYear","EarliestReport",
                    "Habitat",
                    "Source_Data")

  MasterList <- MasterList[,names_fields]

  #####################################################
  ######   Corrections in earliest report  ############
  #####################################################
  MasterList <- MasterList %>%
    mutate(EarliestReport = sapply(EarliestReport, function(x) {
      if (is.na(x) || x == "") return(x)
      str_trim(str_split(x, "\\s*[,;]\\s*", simplify = TRUE)[1])
    }))

  #####################################################
  ####  Capitalize capital letters in each cell  ######
  #####################################################
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

  ################################################
  #####   Remove duplicates across cells   #######
  ################################################
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


  ############################################
  ###### Corrections in Reference field ######
  ############################################
  table1 <- read.xlsx("TablesToStandardize/Table S1.xlsx", sheet = "Databases") %>%
    select(FRIAS_name, Reference) %>%
    mutate(FRIAS_name = gsub(" ", "_", FRIAS_name))

  MasterList <- MasterList %>%
    mutate(
      Source_Data = str_replace_all(Source_Data, "(https?://[^;,]+),([^;,]+)", "\\1<<COMMA>>\\2")
    ) %>%
    separate_rows(Source_Data, sep = ";|,") %>%
    mutate(
      Source_Data = str_replace_all(Source_Data, "<<COMMA>>", ","),
      Source_Data = trimws(Source_Data)
    )

  merged <- merge(
    MasterList,
    table1,
    by.x = "Source_Data",
    by.y = "FRIAS_name",
    all.x = TRUE
  )

  cols_keep <- c("Source_Data", "Reference", "AcceptedNameGBIF")

  merged_grouped <- merged %>%
    dplyr::group_by(AcceptedNameGBIF) %>%
    dplyr::summarise(
      Source_Data = paste(unique(Source_Data), collapse = "; "),
      Reference   = paste(unique(Reference), collapse = "; "),
      dplyr::across(
        dplyr::setdiff(names(merged), cols_keep),
        ~ paste(unique(.), collapse = "; ")
      ),
      .groups = "drop"
    )

  namesordered <- c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF",
                    "Kingdom","Phylum","Class","Order","Family",
                    "Group","FunctionalGroup",
                    "NativeRange","NativeRangeISO3","NativeBioregions",
                    "RecipientRange","RecipientRangeISO3","RecipientBioregions",
                    "establishmentMeans","pathway",
                    "Impact","Mechanisms","Max_Impact_by_Mechanism",
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

  MasterList5 <- MasterList4[order(MasterList4$AcceptedNameGBIF), ]

  ##############################################################
  #######    Again, obtain accepted name by gbif   #############
  ##############################################################
  names <- as.character(MasterList5$AcceptedNameGBIF)
  species_gbif <- character(length(names))
  for (i in seq_along(names)) {
    species_query <- names[i]
    if (!is.na(species_query) && species_query != "") {
      message("Checking: ", species_query, " (", i, "/", length(names), ")")
      tryCatch({
        res <- name_backbone_checklist(species_query)
        if (!is.null(res$species) && !is.na(res$species)) {
          species_gbif[i] <- res$species
        } else {
          species_gbif[i] <- species_query
        }
      }, error = function(e) {
        species_gbif[i] <- species_query
      })
    Sys.sleep(0.01)
    } else {
    species_gbif[i] <- species_query
    }
  }
  MasterList5$AcceptedNameGBIF <- species_gbif

  MasterList5 <- MasterList5 %>%
    filter(!is.na(ID_GBIF))

  noduplicates <- function(data, column_name_species) {
    if (!(column_name_species %in% colnames(data))) {
      stop(paste("Column with species name", column_name_species, "doesn't exist on dataset"))
    }

    columns <- setdiff(names(data), column_name_species)

    data %>%
      dplyr::group_by(across(all_of(column_name_species))) %>%
      dplyr::summarise(
        across(all_of(columns), ~ {
          text <- as.character(.)
          vals <- unlist(str_split(text, "[,;|]+|\\s{2,}"))
          vals <- str_trim(vals)
          vals <- vals[vals != ""]
          vals <- unique(vals)
          paste(vals, collapse = ", ")
        }),
        .groups = "drop"
      )
  }
  MasterList6 <- noduplicates(MasterList5, "AcceptedNameGBIF")

  ref_table <- MasterList6 %>%
    select(ID_GBIF, Reference) %>%
    distinct(ID_GBIF, .keep_all = TRUE)

  MasterList7 <- MasterList6 %>%
    left_join(ref_table, by = "ID_GBIF")

  MasterList7$Reference.x <- NULL
  names(MasterList7)[names(MasterList7) == "Reference.y"] <- "Reference"

  ##########################################
  ####### Order by alphabet ################
  ##########################################
  MasterList8 <- MasterList7 %>%
    mutate(
      across(
        -c(OriginalNameDB, AcceptedNameGBIF, ID_GBIF, Reference),
        ~ sapply(., function(celda) {

          if (is.na(celda) || celda == "") return(celda)

          celda %>%
            str_split("[,;]\\s*") %>%
            unlist() %>%
            .[order(tolower(.))] %>%   # <- orden correcto sin ignore.case
            paste(collapse = ", ")
        })
      )
    )

  #Eliminate:
  #“[unassigned]”, “Not assigned”, “Undefined” y “Unresolved" y “Incertae sedis” in Family and Order.
  MasterList8$Order <- gsub("\\[unassigned\\]", "", MasterList8$Order)
  MasterList8$Family <- gsub("Not assigned","", MasterList8$Family)
  MasterList8$Family <- gsub("Undefined","", MasterList8$Family)
  MasterList8$Family <- gsub("Unresolved","", MasterList8$Family)
  MasterList8$Family <- gsub("Incertae sedis","", MasterList8$Family)

  MasterList8$Order <- gsub("\\[unassigned\\]", "", MasterList8$Order)
  MasterList8$Order <- gsub("Not assigned","", MasterList8$Order)
  MasterList8$Order <- gsub("Undefined","", MasterList8$Order)
  MasterList8$Order <- gsub("Unresolved","", MasterList8$Order)
  MasterList8$Order <- gsub("Incertae sedis","", MasterList8$Order)

  #Replace by 'NA':
  #In Order "Eupercaria", "Eupercaria incertae sedis", "Eupercaria/misc", "Carangaria” and "Carangaria/misc".
  MasterList8$Order <- gsub("Eupercaria","", MasterList8$Order)
  MasterList8$Order <- gsub("Eupercaria incertae sedis","", MasterList8$Order)
  MasterList8$Order <- gsub("Eupercaria/misc","", MasterList8$Order)
  MasterList8$Order <- gsub("Carangaria","", MasterList8$Order)
  MasterList8$Order <- gsub("Carangaria/misc","", MasterList8$Order)


  #If there's >= 2 values in a cell and one of them is 'NA', eliminate that string
  MasterList8 <- MasterList8 %>%
    mutate(
      across(
        -c(OriginalNameDB, AcceptedNameGBIF, ID_GBIF, Source_Data, Reference),
        ~ sapply(., function(celda) {

          if (is.na(celda) || celda == "") return(celda)

          celda %>%
            str_split("[,;]\\s*") %>%
            unlist() %>%
            .[. != "NA"] %>%   # <- elimina el string "NA"
            .[. != ""] %>%     # <- por si hay vacíos
            sort() %>%
            paste(collapse = ", ")
        })
      )
    )


  ##############################################
  ##### FINAL RESULT ###########################
  ##############################################
  write.xlsx(MasterList8,
             file.path("FinalFiles", paste0("(Table S3) FRIAS_masterlist.xlsx")),
             sep = ",",
             row.names = FALSE,
             col.names = TRUE)
  write.csv(MasterList8,"FinalFiles/(Table S3) FRIAS_masterlist.csv")

 species_number <- nrow(MasterList8)
 cat("🐟 FRIAS Masterlist contains :",  species_number, "Alien Freshwater Species 🦀.", "\n")
}

