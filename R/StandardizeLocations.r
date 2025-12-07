StandardizeLocations <- function() {
  MasterList <- read.xlsx("OutputFiles/Intermediate/step8_additionalNativeRecipientRangeWORRMS_masterlist.xlsx")

  ###############################
  ####### RECIPIENT RANGE #######
  ###############################
  #Clean cells text
  clean_text_column <- function(column) {
    column %>%
      gsub("[()]", "", .) %>%
      gsub(",", ";", .) %>%
      gsub(";\\s*", ";", .) %>%
      gsub("xml:space=\\\"preserve\\\">", "", .) %>%
      gsub("(^NA$|^NA;|;NA)", "", .) %>%
      trimws()
  }
  MasterList <- MasterList %>%
    mutate(
      RecipientRange = clean_text_column(RecipientRange),
      NativeRange = clean_text_column(NativeRange),
      NativeRange = gsub(",", ";", NativeRange),
      Mechanisms = gsub("; ", ";", Mechanisms),
      CopyRecipientRange = clean_text_column(RecipientRange)
    ) %>%
    select(-InvadedRange)


  #Replace "" and "NA" to NULL
  cols <- setdiff(names(MasterList), "Source_Data")
  MasterList[cols] <- lapply(MasterList[cols], function(x) {
    x[x == "" | x == "NA"] <- NA
    x
  })

  #Load locations_table
  locations_table <- read.xlsx("TablesToStandardize/Table S2.xlsx", sheet="7. Countries") %>%
    mutate(keywords = gsub("[()]", "", keywords))%>%
    mutate(
      keywords = map(keywords, ~ str_trim(tolower(.x))),
      Location = str_trim(tolower(Location))
    ) %>%
    unnest(keywords)

  #Matching functions
  match_by_keywords <- function(name, loc_table) {
    name_clean <- tolower(str_trim(name))
    matched_row <- loc_table[!is.na(loc_table$keywords) &
                               grepl(paste0("(^|;)\\s*", name_clean, "\\s*(;|$)"),
                                     tolower(loc_table$keywords)), ]
    if (nrow(matched_row) > 0) return(matched_row$Location[1])
    return(NA_character_)
  }

  match_by_iso2 <- function(iso2_code, loc_table) {
    iso2_clean <- toupper(str_trim(iso2_code))
    matched_row <- loc_table[!is.na(loc_table$ISO2) & toupper(loc_table$ISO2) == iso2_clean, ]
    if (nrow(matched_row) > 0) return(matched_row$Location[1])
    return(NA_character_)
  }

  replace_with_location <- function(name, loc_table) {
    name_clean <- str_trim(name)
    loc <- match_by_keywords(name_clean, loc_table)
    if (!is.na(loc)) return(loc)
    loc <- match_by_iso2(name_clean, loc_table)
    if (!is.na(loc)) return(loc)
    match_location <- loc_table$Location[tolower(loc_table$Location) == tolower(name_clean)]
    if (length(match_location) > 0) return(match_location[1])
    return(NA_character_)
  }

  #REmove duplicated iso2 codes
  iso2_transformed_Range <- function(a, b) {
    unique_items <- unique(trimws(c(unlist(strsplit(a, ";")), unlist(strsplit(b, ";")))))
    paste(na.omit(unique_items[unique_items != ""]), collapse = ";")
  }

  #Standardize Recipient Range column
  MasterList$RecipientRange <- pbmapply(iso2_transformed_Range,
                                        MasterList$RecipientRange,
                                        MasterList$RecipientRange)
  MasterList <- MasterList %>%
    mutate(
      RecipientRange_cleaned = pbapply::pblapply(RecipientRange, function(cell) {
        if (is.na(cell) || cell == "") return(list(matched = NA_character_, not_matched = NA_character_))
        items <- unlist(strsplit(cell, "[,;]"))
        items <- str_trim(items)
        items <- items[items != ""]
        replaced <- sapply(items, function(name) replace_with_location(name, locations_table))
        not_matched <- items[is.na(replaced)]
        replaced <- na.omit(unique(replaced))
        list(
          matched = if (length(replaced) == 0) NA_character_ else paste(replaced, collapse = "; "),
          not_matched = if (length(not_matched) == 0) NA_character_ else paste(not_matched, collapse = "; ")
        )
      })
    ) %>%
    mutate(
      RecipientRange = sapply(RecipientRange_cleaned, `[[`, "matched"),       # Solo los que hicieron match
      RecipientRange_no_match = sapply(RecipientRange_cleaned, `[[`, "not_matched") # Lo que no hizo match
    ) %>%
    select(-RecipientRange_cleaned)

  #SAve not matches cases of Recipient Range
  no_match_RecipientRange <- MasterList %>%
    filter(!is.na(RecipientRange_no_match)) %>%
    select(Source_Data, RecipientRange_no_match)
  write.xlsx(no_match_RecipientRange, "OutputFiles/Check/RecipientRange_no_match.xlsx")
  MasterList <- MasterList %>% select(-RecipientRange_no_match)

  #Obtain ISO3 of Recipient Ranges
  get_ISO3 <- function(countries_str) {
    countries <- unlist(strsplit(countries_str, ";"))
    ISO3 <- unlist(sapply(trimws(countries), function(c) {
      iso_match <- locations_table$ISO3[locations_table$Location == c]
      if (length(iso_match) == 0)
        iso_match <- locations_table$ISO3[str_detect(locations_table$keywords, c)]
      if (length(iso_match) == 0) return(NA)
      unique(trimws(unlist(strsplit(iso_match, ";"))))
    }))
    ISO3 <- unique(na.omit(ISO3))
    if (length(ISO3) == 0) return(NA)
    paste(ISO3, collapse = "; ")
  }
  MasterList$RecipientRangeISO3 <- sapply(MasterList$RecipientRange, get_ISO3)

  #Obtain Recipient bioregions
  regions_table <- read.xlsx("TablesToStandardize/Table S2.xlsx", sheet="8. Regions") %>%
    mutate(keywords = str_split(keywords, ";")) %>%
    unnest(keywords) %>%
    mutate(keywords = str_trim(tolower(keywords)))
  find_region <- function(iso3_str, keywords, locations) {
    if (is.na(iso3_str) || iso3_str == "") return(NA)
    matches <- which(keywords %in% str_trim(tolower(unlist(strsplit(iso3_str, ";")))))
    if (length(matches) == 0) return(NA)
    paste(unique(locations[matches]), collapse = "; ")
  }
  MasterList$RecipientBioregions1 <- map_chr(
    tolower(MasterList$RecipientRangeISO3),
    ~find_region(.x, regions_table$keywords, regions_table$Location)
  )
  find_region_CopyRecipientRange <- function(CopyRecipientRange, keywords, locations) {
    if (is.na(CopyRecipientRange) || CopyRecipientRange == "") return(NA)
    matches <- which(keywords %in% str_trim(tolower(unlist(strsplit(CopyRecipientRange, ";")))))
    if (length(matches) == 0) return(NA)
    paste(unique(locations[matches]), collapse = "; ")
  }
  MasterList$RecipientBioregions2 <- map_chr(
    tolower(MasterList$CopyRecipientRange),
    ~find_region(.x, regions_table$keywords, regions_table$Location)
  )
  MasterList <- MasterList %>%
    mutate(RecipientBioregions = paste(RecipientBioregions1, RecipientBioregions2, sep="; ")) %>%
    select(-RecipientBioregions1, -RecipientBioregions2)

  ###############################
  ####### Native  RANGE   #######
  ###############################
  locations_table <- locations_table %>%
    mutate(
      keywords = str_split(keywords, ";"),
      keywords = map(keywords, ~ str_trim(tolower(.x))),
      Location = str_trim(tolower(Location))
    ) %>%
    unnest(keywords)
  standardize_na <- function(df) {
    df[] <- lapply(df, function(col) {
      if (is.character(col)) {
        col[col == "" | toupper(col) == "NA"] <- NA
      }
      return(col)
    })
    return(df)
  }
  MasterList <- standardize_na(MasterList)

  get_native_Range <- function(native_str) {
    if (is.na(native_str) || native_str == "") {
      return(list(matched = NA_character_, not_matched = NA_character_))
    }
    native_items <- str_trim(tolower(unlist(strsplit(native_str, ";"))))
    matched_locations <- c()
    not_matched <- c()
    for (item in native_items) {
      if (item == "") next
      exact_match <- locations_table$Location[locations_table$Location == item]
      keyword_exact <- locations_table$Location[locations_table$keywords == item]
      keyword_partial <- locations_table$Location[str_detect(locations_table$keywords, fixed(item))]
      matches <- unique(c(exact_match, keyword_exact, keyword_partial))
      if (length(matches) == 0) {
        not_matched <- c(not_matched, item)
      } else {
        matched_locations <- c(matched_locations, matches)
      }
    }

    matched_locations <- unique(na.omit(matched_locations))
    not_matched <- unique(na.omit(not_matched))

    return(list(
      matched = if (length(matched_locations) == 0) NA_character_ else paste(matched_locations, collapse = "; "),
      not_matched = if (length(not_matched) == 0) NA_character_ else paste(not_matched, collapse = "; ")
    ))
  }

  #Standardize Native Regions
  MasterList <- MasterList %>%
    mutate(
      NativeRange_split = str_split(NativeRange, ";")
    ) %>%
    unnest(NativeRange_split) %>%
    mutate(NativeRange_split = tolower(str_trim(NativeRange_split)))

  native_results <- lapply(MasterList$NativeRange_split, get_native_Range)

  #SAve matched and not matched native ranges
  MasterList$NativeRange <- sapply(native_results, `[[`, "matched")
  MasterList$NativeRange_no_match <- sapply(native_results, `[[`, "not_matched")
  no_match_NativeRange <- MasterList %>%
    filter(!is.na(NativeRange_no_match)) %>%
    select(Source_Data, NativeRange_no_match)
  write.xlsx(no_match_NativeRange, "OutputFiles/Check/NativeRange_no_match.xlsx")

  #Obtain ISO3 and Bioregions of Native Range
  MasterList$NativeRangeISO3 <- sapply(MasterList$NativeRange, get_ISO3)
  MasterList$NativeBioregions <- map_chr(
    tolower(MasterList$NativeRange),
    ~find_region(.x, regions_table$keywords, regions_table$Location)
  )



  MasterList <- MasterList %>%
    mutate(RecipientRangeISO3 = if_else(
      str_detect(RecipientRangeISO3, "^[a-zA-Z]{6}$"),
      str_replace(RecipientRangeISO3, "^(.{3})(.{3})$", "\\1,\\2"),
      RecipientRangeISO3
    ))
  MasterList <- MasterList %>%
    rename(Phylum=phylum)

  final_columns <- c(
    "OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF",
    "Kingdom", "Order", "Family", "Phylum", "Class", "establishmentMeans", "pathway",
    "FunctionalGroup", "Group",
    "NativeRange","NativeRangeISO3", "NativeBioregions",
    "ReportYear", "EarliestReport", "RecipientRange", "RecipientRangeISO3",
    "AffectedTaxa",
    "EICATImpact", "Mechanisms", "EICAT_by_Mechanism", "Habitat",
    "RecipientBioregions", "Source_Data"
  )
  MasterList <- MasterList[,final_columns]
  MasterList$RecipientRange <- as.character(MasterList$RecipientRange)
  MasterList <- MasterList %>%
    rename(RecipientRange = RecipientRange) %>%
    rename(RecipientRangeISO3 = RecipientRangeISO3) %>%
    rename(NativeRange = NativeRange) %>%
    rename(NativeRangeISO3 = NativeRangeISO3)

  # 7. Remove Duplicates
  source(file.path("R", "noduplicates.r"))
  MASTERLIST_noduplicates <- noduplicates(MasterList, "AcceptedNameGBIF")

  names(MASTERLIST_noduplicates)
  final_columns <- c(
    "OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF",
    "Kingdom", "Order", "Family", "Phylum", "Class", "establishmentMeans", "pathway",
    "FunctionalGroup", "Group",
    "NativeRange","NativeRangeISO3", "NativeBioregions",
    "ReportYear", "EarliestReport", "RecipientRange", "RecipientRangeISO3",
    "AffectedTaxa",
    "EICATImpact", "Mechanisms", "EICAT_by_Mechanism", "Habitat",
    "RecipientBioregions", "Source_Data"
  )

  MASTERLIST_noduplicates <- MASTERLIST_noduplicates[, final_columns]

  # 8. Export results
  write_csv(MASTERLIST_noduplicates,"OutputFiles/Intermediate/step9_standardlocationnames_masterlist.csv")
  write_xlsx(MASTERLIST_noduplicates,"OutputFiles/Intermediate/step9_standardlocationnames_masterlist.xlsx")
}
