StandardizeLocations <- function() {
  # ─────────────────────────────────────────────────────────────
  # 1. Loading and cleaning Masterlist
  # ─────────────────────────────────────────────────────────────
  MasterList <- read_excel("OutputFiles/Intermediate/Step8_FillGapsinLocations_MasterList.xlsx")

  clean_text_column <- function(column) {
    column %>%
      gsub("[()]", "", .) %>%
      gsub(",", ";", .) %>%
      gsub(";\\s*", ";", .) %>%
      trimws()
  }

  MasterList <- MasterList %>%
    mutate(
      InvadedRange = clean_text_column(InvadedRange),
      NativeRange = gsub(",", "", NativeRange),
      Mechanisms = gsub("; ", ";", Mechanisms)
    )

  # ─────────────────────────────────────────────────────────────
  # 2. Loading correspondence tables and transform ISO2 into countries
  # ─────────────────────────────────────────────────────────────
  locations_table <- read.xlsx("TablesToStandardize/Standardization_Locations.xlsx") %>%
    mutate(keywords = gsub("[()]", "", keywords))

  iso_to_country <- setNames(locations_table$Location, locations_table$ISO2)

  translate_iso_list <- function(iso_string) {
    codes <- unlist(strsplit(iso_string, ";"))
    countries <- iso_to_country[codes]
    countries <- countries[!is.na(countries)]
    paste(countries, collapse = ";")
  }

  MasterList <- MasterList %>%
    mutate(
      InvadedRange_full = sapply(InvadedRange, translate_iso_list),
      InvadedRange = sapply(strsplit(InvadedRange, ";\\s*"), function(x) {
        paste(x[!str_detect(x, "^[A-Z]{2}$")], collapse = ";")
      })
    )

  iso2_transformed_country <- function(a, b) {
    unique_items <- unique(trimws(c(unlist(strsplit(a, ";")), unlist(strsplit(b, ";")))))
    paste(na.omit(unique_items[unique_items != ""]), collapse = ";")
  }

  MasterList$InvadedRange <- mapply(iso2_transformed_country, MasterList$InvadedRange, MasterList$InvadedRange_full)
  MasterList <- MasterList %>% select(-InvadedRange_full)

  # ─────────────────────────────────────────────────────────────
  # 3. Obtain Countries of invaded locations
  # ─────────────────────────────────────────────────────────────
  #Merge of Locations Correspondence Table and Locations Table from RNaturalEarth data
  locations_table <- locations_table %>%
    mutate(
      keywords = map(keywords, ~ str_trim(tolower(.x))),
      Location = str_trim(tolower(Location))
    ) %>%
    unnest(keywords)

  locations_table_naturalearth <- read_excel("TablesToStandardize/RNaturalEarthData_withCountries_byType.xlsx") %>%
    select(-iso2.y, -iso3.y) %>% #aqui eliminamos las columnas que no queremos
    mutate(keywords = paste (name.x, name.y, sep=",")) %>% #fusionamos columnas
    select(-name.x, -name.y) %>%
    mutate(
      Location = str_trim(tolower(sovereignt)),
      ISO3 = iso3.x,
      ISO2 = iso2.x) %>%
    select(-iso2.x, -iso3.x, -sovereignt) %>%
    mutate(
      ISO2 = str_split(ISO2, ",") %>% sapply(\(x) trimws(x[1])),
      ISO3 = str_split(ISO3, ",") %>% sapply(\(x) trimws(x[1]))
    ) %>%
    # eliminamos filas donde iso2 o iso3 contengan números
    filter(
      !str_detect(ISO2, "\\d"),
      !str_detect(ISO3, "\\d")
    )

  locations_table <- merge(locations_table, locations_table_naturalearth, by = "Location", all = TRUE)
  col_x <- grep("\\.x$", names(locations_table), value = TRUE)
  col_y <- gsub("\\.x$", ".y", col_x)
  col_base <- gsub("\\.x$", "", col_x)
  for (i in seq_along(col_base)) {
    x_col <- col_x[i]
    y_col <- col_y[i]
    base_col <- col_base[i]
    locations_table[[base_col]] <- ifelse(!is.na(locations_table[[x_col]]), locations_table[[x_col]], locations_table[[y_col]])
  }

  locations_table <- locations_table[ , !(names(locations_table) %in% c(col_x, col_y))]

  locations_table <- locations_table %>%
    arrange(Location) %>%
    filter(
      !grepl("[0-9]", ISO3),   # elimina si ISO3 contiene dígitos
      !grepl("[0-9]", ISO2)    # elimina si ISO2 contiene dígitos
    )


  locations_table <- locations_table %>%
    mutate(
      keywords = str_split(keywords, ";"),
      keywords = map(keywords, ~ str_trim(tolower(.x))),
      Location = str_trim(tolower(Location))
    ) %>%
    unnest(keywords)

  # Función auxiliar: busca en keywords
  match_by_keywords <- function(name, loc_table) {
    name_clean <- tolower(str_trim(name))
    matched_row <- loc_table[!is.na(loc_table$keywords) & grepl(paste0("(^|;)\\s*", name_clean, "\\s*(;|$)"), tolower(loc_table$keywords)), ]
    if (nrow(matched_row) > 0) return(matched_row$Location[1])
    return(NA_character_)
  }

  # Función principal para convertir un país a su Location
  replace_with_location <- function(name, loc_table) {
    name_clean <- tolower(str_trim(name))

    # Primero busca en keywords
    loc <- match_by_keywords(name_clean, loc_table)
    if (!is.na(loc)) return(loc)

    # Si no, busca en Location directamente
    match_location <- loc_table$Location[tolower(loc_table$Location) == name_clean]
    if (length(match_location) > 0) return(match_location[1])

    # Si no encuentra nada
    return(NA_character_)
  }

  MasterList <- MasterList %>%
      mutate(InvadedRange = lapply(InvadedRange, function(x) {
        if (is.na(x) || x == "") return(NA_character_)

        items <- unlist(strsplit(x, ";"))
        items <- str_trim(tolower(items))
        items <- items[items != ""]

        replaced <- sapply(items, function(name) replace_with_location(name, locations_table))
        replaced <- na.omit(unique(replaced))

        if (length(replaced) == 0) return(NA_character_)
        paste(replaced, collapse = "; ")
      }))

  # ─────────────────────────────────────────────────────────────
  # 3. Obtain ISO3 from invaded countries
  # ─────────────────────────────────────────────────────────────
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

  MasterList$InvadedRangeISO3 <- sapply(MasterList$InvadedRange, get_ISO3)

  # ─────────────────────────────────────────────────────────────
  # 4. Obtain invaded bioregions
  # ─────────────────────────────────────────────────────────────
  regions_table <- read.xlsx("TablesToStandardize/Standardization_Regions.xlsx") %>%
    mutate(keywords = str_split(keywords, ";")) %>%
    unnest(keywords) %>%
    mutate(keywords = str_trim(tolower(keywords)))

  find_region <- function(iso3_str, keywords, locations) {
    if (is.na(iso3_str) || iso3_str == "") return(NA)
    matches <- which(keywords %in% str_trim(tolower(unlist(strsplit(iso3_str, ";")))))
    if (length(matches) == 0) return(NA)
    paste(unique(locations[matches]), collapse = "; ")
  }

  MasterList$InvadedBioregions <- map_chr(
    tolower(MasterList$InvadedRangeISO3),
    ~find_region(.x, regions_table$keywords, regions_table$Location)
  )

  # ─────────────────────────────────────────────────────────────
  # 5. Obtener países y bioregiones nativas
  # ─────────────────────────────────────────────────────────────
  # Limpieza y expansión de la tabla de localizaciones
  locations_table <- locations_table %>%
    mutate(
      keywords = str_split(keywords, ";"),                 # Separar en lista
      keywords = map(keywords, ~ str_trim(tolower(.x))),   # Limpiar y pasar a minúsculas
      Location = str_trim(tolower(Location))               # Normalizar Location
    ) %>%
    unnest(keywords)
  # Reemplaza "" o "NA" (como texto) por NA en todo el dataframe
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



  get_native_country <- function(native_str) {
    if (is.na(native_str) || native_str == "") return(NA)

    native_items <- str_trim(tolower(unlist(strsplit(native_str, ";"))))

    matched_locations <- c()

    for (item in native_items) {
      # Coincidencia exacta con Location
      exact_match <- locations_table$Location[locations_table$Location == item]

      # Coincidencia exacta con keyword
      keyword_exact <- locations_table$Location[locations_table$keywords == item]

      # Coincidencia parcial (ej: "korea" en "republic of korea")
      keyword_partial <- locations_table$Location[str_detect(locations_table$keywords, fixed(item))]

      matches <- unique(c(exact_match, keyword_exact, keyword_partial))
      matched_locations <- c(matched_locations, matches)
    }

    matched_locations <- unique(na.omit(matched_locations))
    if (length(matched_locations) == 0) return(NA)
    return(paste(matched_locations, collapse = "; "))
  }

  # Separar mecanismos múltiples, limpiar
  MasterList <- MasterList %>%
    mutate(NativeRange = str_split(NativeRange, ";")) %>%
    unnest(NativeRange) %>%
    mutate(
      NativeRange = tolower(str_trim(NativeRange)))


  MasterList$NativeCountry <- sapply(tolower(trimws(MasterList$NativeRange)), get_native_country)
  MasterList$NativeCountryISO3 <- sapply(MasterList$NativeCountry, get_ISO3)
  MasterList$NativeRangeBioregions <- map_chr(
    tolower(MasterList$NativeRange),
    ~find_region(.x, regions_table$keywords, regions_table$Location)
  )
names(MasterList)
  ############### SIIIIIIIIIIIIIIIIIIIIII
  #MasterList %>% filter(grepl("korea republic of", NativeCountry))
  #MasterList %>% filter(grepl("korea", NativeRange))

  # ─────────────────────────────────────────────────────────────
  # 6. Validaciones y exportación de casos NA
  # ─────────────────────────────────────────────────────────────
  export_na <- function(df, column, filename) {
    NA_rows <- df %>% filter(is.na(.data[[column]]) | .data[[column]] == "")
    if (nrow(NA_rows) > 0) {
      write_xlsx(NA_rows, path = file.path("OutputFiles", "Check", filename))
    }
  }

  final_columns <- c(
    "OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF",
    "Kingdom", "Order", "Family", "Phylum", "Class", "EstablishmentMeans", "Pathways",
    "FunctionalGroup", "Group", "NativeRange", "NativeCountry",
    "NativeCountryISO3", "NativeRangeBioregions", "AffectedNativeSpecies",
    "EICATImpact", "Mechanisms", "EICAT_by_Mechanism", "Habitat",
    "FirstRecords", "OldestDate", "InvadedRange", "InvadedRangeISO3",
    "InvadedBioregions", "Source_Data"
  )
  MasterList <- MasterList[,final_columns]
  MasterList$InvadedRange <- as.character(MasterList$InvadedRange)

  export_na(MasterList, "NativeCountry", "NA_NativeCountry_MasterList.xlsx")
  export_na(MasterList, "NativeCountryISO3", "NA_NativeCountryISO3_MasterList.xlsx")
  export_na(MasterList, "NativeRangeBioregions", "NA_NativeCountryBioregion_MasterList.xlsx")
  export_na(MasterList, "InvadedRange", "NA_InvadedCountry_MasterList.xlsx")
  export_na(MasterList, "InvadedRangeISO3", "NA_InvadedCountryISO3_MasterList.xlsx")
  export_na(MasterList, "InvadedBioregions", "NA_InvadedBioregions_MasterList.xlsx")

  # ─────────────────────────────────────────────────────────────
  # 7. Eliminar duplicados por especie
  # ─────────────────────────────────────────────────────────────
  source(file.path("R", "noduplicates.r"))
  MASTERLIST_noduplicados <- noduplicates(MasterList, "AcceptedNameGBIF")

  final_columns <- c(
    "OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF",
    "Kingdom", "Order", "Family", "Phylum", "Class", "EstablishmentMeans", "Pathways",
    "FunctionalGroup", "Group", "NativeRange", "NativeCountry",
    "NativeCountryISO3", "NativeRangeBioregions", "AffectedNativeSpecies",
    "EICATImpact", "Mechanisms", "EICAT_by_Mechanism", "Habitat",
    "FirstRecords", "OldestDate", "InvadedRange", "InvadedRangeISO3",
    "InvadedBioregions", "Source_Data"
  )

  MASTERLIST_noduplicados <- MASTERLIST_noduplicados[, final_columns]

  # ─────────────────────────────────────────────────────────────
  # 8. Exportar resultados
  # ─────────────────────────────────────────────────────────────
  write.table(MASTERLIST_noduplicados,file.path("OutputFiles", "Intermediate", "Step9_StandardLocationNames_MasterList.csv"),
    sep = ";",
    row.names = FALSE
  )

  write_xlsx(MASTERLIST_noduplicados,"OutputFiles/Intermediate/Step9_StandardLocationNames_MasterList.xlsx")
}
