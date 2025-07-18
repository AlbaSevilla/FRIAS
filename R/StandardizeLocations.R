StandardizeLocations <- function() {
  ### CARGAMOS EL DATASET DE LA MASTERLIST
  MasterList <- read_excel("OutputFiles/Intermediate/Step3_FillGapsinLocations_MasterList.xlsx")
  MasterList$invaded_country_list <- gsub("[()]", "", MasterList$invaded_country_list) #Eliminamos () innecesarios
  MasterList$invaded_country_list <- gsub(",", ";", MasterList$invaded_country_list) #reemplazamos , por ;
  MasterList$invaded_country_list <- gsub("; ", ";", MasterList$invaded_country_list) #reemplazamos , por ;
  MasterList$Mechanism <- gsub("; ", ";", MasterList$Mechanism)

  ### CARGAMOS LA TABLA CON LAS LOCALIZACIONES Y LOS CÓDIGOS PARA LA CORRESPONDENCIA
  locations_table <- read.xlsx("TablesToStandardize/Standardization_Locations.xlsx")
  locations_table$keywords <- gsub("[()]", "", locations_table$keywords) #Eliminamos () innecesarios

  ############################################################################################
  ############################################################################################
  ############################################################################################
  ############################################################################################
  #EN LA COLUMNA DE PAISES INVADIDOS HAY ISO2 QUE SE CUELAN, LOS PASAMOS A PAISES Y LOS DEJAMOS
  #EN LA COLUMNA DE PAISES INVADIDOS YA COMO PAISES:
  ############################################################################################
  ############################################################################################
  ############################################################################################
  # Crear tabla de referencia: código ISO2 -> nombre del país
  translator <- locations_table %>%
    select(ISO2, Location) %>%
    distinct()

  # Vector nombrado para traducción rápida
  iso_to_country <- setNames(translator$Location, translator$ISO2)

  ###3. TRADUCIR LOS CÓDIGOS ISO2 A NOMBRES DE PAÍSES
  translate_iso_list <- function(iso_string) {
    iso_codes <- unlist(strsplit(iso_string, ";"))
    countries <- iso_to_country[iso_codes]
    countries <- countries[!is.na(countries)]  # Quitar valores NA
    paste(countries, collapse = ";")
  }

  # Aplicamos la función para crear columna traducida
  df_master <- MasterList %>%
    mutate(invaded_country_list_full = sapply(invaded_country_list, translate_iso_list))

  ###4. LIMPIAR CÓDIGOS ISO2 DE LA COLUMNA ORIGINAL
  MasterList <- MasterList %>%
    mutate(
      invaded_country_list = invaded_country_list %>%
        str_split(";\\s*") %>%                             # Separar por ;
        lapply(function(x) x[!str_detect(x, "^[A-Z]{2}$")]) %>%  # Eliminar ISO2
        lapply(str_trim) %>%
        sapply(function(x) paste(x, collapse = ";"))      # Volver a unir
    )

  ###5. FUSIONAR Y ELIMINAR DUPLICADOS
  # Creamos una función para fusionar ambas listas y eliminar duplicados
  combine_and_dedup <- function(original, translated) {
    all_items <- c(unlist(strsplit(original, ";")), unlist(strsplit(translated, ";")))
    all_items <- trimws(all_items)
    unique_items <- unique(all_items[!is.na(all_items) & all_items != ""])
    paste(unique_items, collapse = ";")
  }

  # Aplicar la función fila por fila
  MasterList$invaded_country_list_clean <- mapply(
    combine_and_dedup,
    MasterList$invaded_country_list,
    df_master$invaded_country_list_full
  )

  # Eliminar la columna vieja si ya no es necesaria
  MasterList <- MasterList %>% select(-invaded_country_list)

  ###6. VERIFICAR RESULTADOS
  head(MasterList$invaded_country_list_clean,10)

  #Renombramos para dejar la columna con el nombre original
  # Renombrar la columna final
  MasterList <- MasterList %>%
    dplyr::rename(invaded_country_list = invaded_country_list_clean)

  ################################################################################
  ################################################################################
  ################################################################################
  ################################################################################
  ################################################################################
  ################################################################################

  ############################################################################
  ########### AHORA OBTENEMOS LOS ISO 3 ######################################
  ############################################################################
  MasterList$InvadedCountriesISO3_List <- mapply(function(countries_str) {
    countries <- unlist(strsplit(countries_str, ";"))  # Separar los países por ";"

    ISO3 <- unlist(sapply(countries, function(c) {
      c <- trimws(c)  # Limpiar espacios

      # Buscar coincidencia exacta por nombre
      iso_match <- locations_table$ISO3[locations_table$Location == c]

      # Si no hay coincidencia, buscar en los keywords
      if (length(iso_match) == 0) {
        iso_match <- locations_table$ISO3[str_detect(locations_table$keywords, c)]
      }

      if (length(iso_match) == 0) {
        return(NA)  # Si no se encuentra, devolver NA
      }

      # Separar ISO3 múltiples si vienen con ";"
      iso_split <- unlist(strsplit(iso_match, ";"))
      iso_split <- trimws(iso_split)  # Limpiar cada ISO3
      return(iso_split)
    }))

    ISO3 <- ISO3[!is.na(ISO3)]  # Eliminar NA
    ISO3 <- unique(ISO3)        # Eliminar duplicados

    if (length(ISO3) == 0) {
      return(NA)
    } else {
      return(paste(ISO3, collapse = "; "))
    }
  }, MasterList$invaded_country_list)

  MasterListWithLocations <- MasterList

  ###5. FUSIONAR Y ELIMINAR DUPLICADOS
  # Creamos una función para fusionar ambas listas y eliminar duplicados
  combine_and_dedup <- function(original, translated) {
    all_items <- c(unlist(strsplit(original, ";")), unlist(strsplit(translated, ";")))
    all_items <- trimws(all_items)
    # Filtrar NAs, vacíos y "NA" literales
    all_items <- all_items[!is.na(all_items) & all_items != "" & toupper(all_items) != "NA"]
    unique_items <- unique(all_items)
    if (length(unique_items) == 0) {
      return(NA_character_)
    } else {
      return(paste(unique_items, collapse = ";"))
    }
  }


  MasterListWithLocations$InvadedCountriesISO3_List <- mapply(
    combine_and_dedup,
    MasterListWithLocations$InvadedCountriesISO3_List,
    MasterListWithLocations$ISO3_invaded_country_list
  )


  MasterListWithLocations <- MasterListWithLocations %>%
    mutate(
      InvadedCountriesISO3_List = case_when(
        is.na(InvadedCountriesISO3_List) & is.na(ISO3_invaded_country_list) ~ NA_character_,
        is.na(InvadedCountriesISO3_List) ~ ISO3_invaded_country_list,
        is.na(ISO3_invaded_country_list) ~ InvadedCountriesISO3_List,
        TRUE ~ paste(InvadedCountriesISO3_List, ISO3_invaded_country_list, sep = "; ")
      )
    ) %>%
    select(-ISO3_invaded_country_list)

  names(MasterListWithLocations)
  MasterListWithLocations_Final <- MasterListWithLocations[
    c("OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF",
      "Kingdom", "Order", "Family", "establishmentMeans", "pathway",
      "FunctionalGroup", "Group", "NativeRangeofIAS_list", "AffectedNativeSpecies",
      "EICATImpact", "Mechanism", "EICAT_by_Mechanism","habitat", "DateList", "OldestDate",
      "invaded_country_list", "InvadedCountriesISO3_List", "Source_Data")]

  for (col in names(MasterListWithLocations_Final)) {
    if (is.character(MasterListWithLocations_Final[[col]])) {
      MasterListWithLocations_Final[[col]] <- gsub("^\\s*NA\\s*$", "", MasterListWithLocations_Final[[col]])
    }
  }
  for (col in names(MasterListWithLocations_Final)) {
    if (is.character(MasterListWithLocations_Final[[col]])) {
      MasterListWithLocations_Final[[col]] <- gsub("; ", "", MasterListWithLocations_Final[[col]])
    }
  }

  unique(MasterListWithLocations_Final$Mechanism)
  ################################################################################
  ################################################################################
  ################################################################################


  #################################################################################
  ############# AHORA OBTENEMOS LA COLUMNA DE LAS REGIONES ########################
  ########################### INVADIDAS ###########################################
  #################################################################################
  regions_table <- read.xlsx("TablesToStandardize/Standardization_Regions.xlsx")

  regions_table <- regions_table %>%
    mutate(keywords = str_split(keywords, ";")) %>%      # Separar por ";"
    unnest(keywords) %>%                                 # Crear una fila por cada keyword
    mutate(keywords = str_trim(tolower(keywords)))       #

  head(regions_table)

  MasterList2 <- MasterListWithLocations_Final
  MasterList2$InvadedCountriesISO3_List <- tolower(MasterList2$InvadedCountriesISO3_List)


  # Función para encontrar regiones
  find_region <- function(invaded_str, keywords, locations) {
    if (is.na(invaded_str) || invaded_str == "") return(NA)
    invaded_list <- unlist(str_split(invaded_str, ";"))
    idx <- which(keywords %in% invaded_list)
    if (length(idx) > 0) {
      return(paste(unique(locations[idx]), collapse = ", "))
    } else {
      return(NA)
    }
  }

  # Aplicar la función a cada fila
  MasterList2$InvadedBioregions <- map_chr(
    MasterList2$InvadedCountriesISO3_List,
    ~find_region(.x, regions_table$keywords, regions_table$Location)
  )

  MasterListWithLocationsandBioregions <- MasterList2
names(MasterListWithLocationsandBioregions)
  MasterListWithCountriesAndBioregions_Final <- MasterListWithLocationsandBioregions[
    c("OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF",
      "Kingdom", "Order", "Family", "establishmentMeans", "pathway",
      "FunctionalGroup", "Group", "NativeRangeofIAS_list", "AffectedNativeSpecies",
      "EICATImpact", "Mechanism","EICAT_by_Mechanism", "habitat", "DateList", "OldestDate",
      "invaded_country_list", "InvadedCountriesISO3_List", "InvadedBioregions", "Source_Data")]


  #################################################################################
  ############# AHORA OBTENEMOS LA COLUMNA DE LAS REGIONES ########################
  ########################### NATIVAS #############################################
  #################################################################################
  regions_table <- read.xlsx("TablesToStandardize/Standardization_Regions.xlsx")

  regions_table <- regions_table %>%
    mutate(keywords = str_split(keywords, ";")) %>%      # Separar por ";"
    unnest(keywords) %>%                                 # Crear una fila por cada keyword
    mutate(keywords = str_trim(tolower(keywords)))       #

  MasterList3 <- MasterListWithLocationsandBioregions
  MasterList3$NativeRangeofIAS_list <- tolower(MasterList3$NativeRangeofIAS_list)
  MasterList3 <- MasterList3 %>%
    mutate(NativeRangeofIAS_list = str_split(NativeRangeofIAS_list, ";")) %>%      # Separar por ";"
    unnest(NativeRangeofIAS_list) %>%                                 # Crear una fila por cada keyword
    mutate(NativeRangeofIAS_list = str_trim(tolower(NativeRangeofIAS_list)))       #

  # countries <- c(
  #
  # MasterList3$NativeRangeofIAS_list <- sapply(
  #   MasterList3$NativeRangeofIAS_list,
  #   replace_with_reference,
  #   reference = reference,
  #   continents = continents
  # )

  # Función para encontrar regiones
  find_region <- function(invaded_str, keywords, locations) {
    if (is.na(invaded_str) || invaded_str == "") return(NA)
    invaded_list <- unlist(str_split(invaded_str, ";"))
    idx <- which(keywords %in% invaded_list)
    if (length(idx) > 0) {
      return(paste(unique(locations[idx]), collapse = "; "))
    } else {
      return(NA)
    }
  }

  # Aplicar la función a cada fila
  MasterList3$NativeRangeBioregions <- map_chr(
    MasterList3$NativeRangeofIAS_list,
    ~find_region(.x, regions_table$keywords, regions_table$Location)
  )

  MasterListWithLocationsandBioregionsandNative <- MasterList3

  MasterListWithLocationsandBioregionsandNative_Final <- MasterListWithLocationsandBioregionsandNative[
    c("OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF",
      "Kingdom", "Order", "Family", "establishmentMeans", "pathway",
      "FunctionalGroup", "Group", "NativeRangeofIAS_list", "NativeRangeBioregions",
      "AffectedNativeSpecies",
      "EICATImpact", "Mechanism", "EICAT_by_Mechanism","habitat", "DateList", "OldestDate",
      "invaded_country_list", "InvadedCountriesISO3_List", "InvadedBioregions", "Source_Data")]

  #################################################################################
  ############# AHORA OBTENEMOS LA COLUMNA DE LOS PAISES   ########################
  ########################### NATIVAS #############################################
  #################################################################################
  regions_table <- read.xlsx("TablesToStandardize/Standardization_Locations.xlsx")

  regions_table <- regions_table %>%
    mutate(keywords = str_split(keywords, ";")) %>%      # Separar por ";"
    unnest(keywords) %>%                                 # Crear una fila por cada keyword
    mutate(keywords = str_trim(tolower(keywords)))       #

  MasterList3 <- MasterListWithLocationsandBioregionsandNative

  # countries <- c(
  #
  # MasterList3$NativeRangeofIAS_list <- sapply(
  #   MasterList3$NativeRangeofIAS_list,
  #   replace_with_reference,
  #   reference = reference,
  #   continents = continents
  # )

  # Función para encontrar regiones
  find_region <- function(invaded_str, keywords, locations) {
    if (is.na(invaded_str) || invaded_str == "") return(NA)
    invaded_list <- unlist(str_split(invaded_str, ";"))
    idx <- which(keywords %in% invaded_list)
    if (length(idx) > 0) {
      return(paste(unique(locations[idx]), collapse = "; "))
    } else {
      return(NA)
    }
  }

  # Aplicar la función a cada fila
  MasterList3$NativeCountry <- map_chr(
    MasterList3$NativeRangeofIAS_list,
    ~find_region(.x, regions_table$keywords, regions_table$Location)
  )

  MasterListWithLocationsandBioregionsandNative <- MasterList3
  MasterListWithLocationsandBioregionsandNative_Final <- MasterListWithLocationsandBioregionsandNative[
    c("OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF",
      "Kingdom", "Order", "Family", "establishmentMeans", "pathway",
      "FunctionalGroup", "Group", "NativeRangeofIAS_list", "NativeCountry", "NativeRangeBioregions",
      "AffectedNativeSpecies",
      "EICATImpact", "Mechanism", "EICAT_by_Mechanism","habitat", "DateList", "OldestDate",
      "invaded_country_list", "InvadedCountriesISO3_List", "InvadedBioregions", "Source_Data")]


  ############################################################################
  ########### AHORA OBTENEMOS LOS ISO 3 ######################################
  ############################################################################
  Masterlist <- MasterListWithLocationsandBioregionsandNative_Final
  Masterlist$NativeCountries_ISO3 <- mapply(function(countries_str) {
    countries <- unlist(strsplit(countries_str, ";"))  # Separar los países por ";"

    ISO3 <- unlist(sapply(countries, function(c) {
      c <- trimws(c)  # Limpiar espacios

      # Buscar coincidencia exacta por nombre
      iso_match <- locations_table$ISO3[locations_table$Location == c]

      # Si no hay coincidencia, buscar en los keywords
      if (length(iso_match) == 0) {
        iso_match <- locations_table$ISO3[str_detect(locations_table$keywords, c)]
      }

      if (length(iso_match) == 0) {
        return(NA)  # Si no se encuentra, devolver NA
      }

      # Separar ISO3 múltiples si vienen con ";"
      iso_split <- unlist(strsplit(iso_match, ";"))
      iso_split <- trimws(iso_split)  # Limpiar cada ISO3
      return(iso_split)
    }))

    ISO3 <- ISO3[!is.na(ISO3)]  # Eliminar NA
    ISO3 <- unique(ISO3)        # Eliminar duplicados

    if (length(ISO3) == 0) {
      return(NA)
    } else {
      return(paste(ISO3, collapse = "; "))
    }
  }, Masterlist$NativeCountry)

  FinalMasterlist <- Masterlist

  #NO DUPLICADOS
  source(file.path("R", "colapsar_por_AcceptedNameGBIF.r"))
  MASTERLIST_noduplicados <- colapsar_por_AcceptedNameGBIF(FinalMasterlist)


  MASTERLIST_noduplicados <- MASTERLIST_noduplicados[, c("OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF",
                                                         "Kingdom", "Order", "Family", "establishmentMeans", "pathway",
                                                         "FunctionalGroup", "Group","NativeCountry","NativeCountries_ISO3", "NativeRangeBioregions",
                                                         "AffectedNativeSpecies",
                                                         "EICATImpact", "Mechanism", "EICAT_by_Mechanism","habitat", "DateList", "OldestDate",
                                                         "invaded_country_list", "InvadedCountriesISO3_List", "InvadedBioregions", "Source_Data")]

  sapply(MASTERLIST_noduplicados, class)

  # Verificar y guardar los casos donde ISO3 es NA o vacío
  NA_rows <- MASTERLIST_noduplicados %>% filter(is.na(NativeCountry) | NativeCountry == ""
                                                                   | is.na(NativeCountries_ISO3) | NativeCountries_ISO3 == ""
                                                                   | is.na(NativeRangeBioregions) | NativeRangeBioregions == ""
                                                                   | is.na(invaded_country_list) | invaded_country_list == ""
                                                                   | is.na(InvadedCountriesISO3_List) | InvadedCountriesISO3_List == ""
                                                                   | is.na(InvadedBioregions) | InvadedBioregions == "")
  head(NA_rows)
  if (nrow(NA_rows) > 0) {
    write.xlsx(NA_rows,
               file = file.path("OutputFiles", "Check", "NA_Locations_MasterList.xlsx"),
               rowNames = FALSE,
               colNames = TRUE)
  }



  # Guardar la nueva base de datos
  if (exists("MASTERLIST_noduplicados") && !is.null(MASTERLIST_noduplicados)) {
    write.table(MASTERLIST_noduplicados,
                file.path("OutputFiles", "Intermediate", paste0("Step4_StandardLocationNames_MasterList.csv")),
                sep = ";",
                row.names = FALSE,
                col.names = TRUE)
    write_xlsx(MASTERLIST_noduplicados, "OutputFiles/Intermediate/Step4_StandardLocationNames_MasterList.xlsx")
  }
}

