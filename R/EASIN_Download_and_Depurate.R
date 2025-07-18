EASIN_Download_and_Depurate <- function(){

  ##################################################
  ############ webscrapping ########################
  ##################################################
  url <- "https://easin.jrc.ec.europa.eu/apixg/catxg/env/FRW/skip/0/take/15000"
  res <- GET(url) # 1. Download JSON data

  json_raw <- content(res, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(json_raw, flatten = TRUE)# 2. Parse the JSON content

  dataset <- as.data.frame(json_data) # Convert to a dataframe

  # Select relevant columns
  dataset_freshwater <- dataset[,c("Name","Status", "EASINID", "PresentInCountries",
                                   "NativeRange", "Kingdom", "Order", "Family",
                                   "CBD_Pathways", "FirstIntroductionsInEU")]

  dataset_freshwater2 <- dataset_freshwater
  names(dataset_freshwater2)[names(dataset_freshwater2) == "PresentInCountries"] <- "Invaded_Countries_ISO2"
  names(dataset_freshwater2)[names(dataset_freshwater2) == "NativeRange"] <- "Native_Range_ISO2"


  dataset_freshwater2 <- dataset_freshwater2 %>%
    filter(Status == "A")
  datset_freshwater2 <- dataset_freshwater2 %>%
    select(-Status)
  dataset_freshwater2$Status <- "Alien"


  locations_table <- read.xlsx("TablesToStandardize/Standardization_Locations.xlsx")
  names(locations_table)
  locations_table <- locations_table[, c("ISO3", "ISO2", "Location")]
  locations_table$ISO2 <- trimws(locations_table$ISO2)
  locations_table$Location <- trimws(locations_table$Location)


  ################################################################
  ################ RANGO INVADIDO ################################
  ################################################################
  MasterList_separated <- data.frame()
  for (i in seq_along(dataset_freshwater2$Invaded_Countries_ISO2)) {
    entry <- dataset_freshwater2$Invaded_Countries_ISO2[[i]]
    if (!is.null(entry)) {
      countries <- entry$Country
      temp_df <- data.frame(
        Name = rep(dataset_freshwater2$Name[i], length(countries)),
        Invaded_Countries_ISO2 = countries,
        stringsAsFactors = FALSE
      )
      MasterList_separated <- rbind(MasterList_separated, temp_df)
    }
  }

  MasterList_separated$Invaded_Countries_ISO2 <- gsub("\"", "", MasterList_separated$Invaded_Countries_ISO2)

  merged_data <- merge(
    MasterList_separated,
    locations_table,
    by.x = "Invaded_Countries_ISO2",
    by.y = "ISO2",
    all.x = TRUE
  )

  #NO DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  dataset_final_colapsado <- noduplicates(merged_data,column_name_species = "Name")

  names(dataset_final_colapsado)[names(dataset_final_colapsado) == "ISO3"] <- "Invaded_Countries_ISO3"
  names(dataset_final_colapsado)[names(dataset_final_colapsado) == "Location"] <- "Invaded_Countries"

  ###############################################
  ########## RANGO NATIVO #######################
  ###############################################
  MasterList_separated2 <- data.frame()
  for (i in seq_along(dataset_freshwater2$Native_Range_ISO2)) {
    entry <- dataset_freshwater2$Native_Range_ISO2[[i]]
    if (!is.null(entry)) {
      countries <- entry$Country
      temp_df <- data.frame(
        Name = rep(dataset_freshwater2$Name[i], length(countries)),
        Native_Range_ISO2 = countries,
        stringsAsFactors = FALSE
      )
      MasterList_separated2 <- rbind(MasterList_separated2, temp_df)
    }
  }
  MasterList_separated2$Native_Range_ISO2 <- gsub("\"", "", MasterList_separated2$Native_Range_ISO2)
  merged_data2 <- merge(
    MasterList_separated2,
    locations_table,
    by.x = "Native_Range_ISO2",
    by.y = "ISO2",
    all.x = TRUE
  )

  #NO DUPLICADOS
  dataset_final_colapsado2 <- noduplicates(merged_data2,column_name_species = "Name")

  names(dataset_final_colapsado2)[names(dataset_final_colapsado2) == "ISO3"] <- "Native_Range_ISO3"
  names(dataset_final_colapsado2)[names(dataset_final_colapsado2) == "Location"] <- "Native_Range"

  ######################################################
  ############# DATASET FINAL ##########################
  ######################################################
  dataset_freshwater_final_1 <- merge(
      dataset_final_colapsado,
      dataset_final_colapsado2,
      by.x = "Name",
      by.y = "Name",
      all.x = TRUE
    )
  dataset_freshwater_final_2 <- merge(
    dataset_freshwater2,
    dataset_freshwater_final_1,
    by.x = "Name",
    by.y = "Name",
    all.x = TRUE
  )

  dataset_FINAL <- dataset_freshwater_final_2 %>%
    select(-"Invaded_Countries_ISO2.x") %>%
    select(-"Native_Range_ISO2.x")
  names(dataset_FINAL)[names(dataset_FINAL) == "Invaded_Countries_ISO2.y"] <- "Invaded_Countries_ISO2"
  names(dataset_FINAL)[names(dataset_FINAL) == "Native_Range_ISO2.y"] <- "Native_Range_ISO2"

  # Extraer solo el año desde la lista de dataframes
  dataset_FINAL$FirstIntroductionsInEU <- map_chr(dataset_FINAL$FirstIntroductionsInEU, function(df) {
    if (!is.null(df) && "Year" %in% colnames(df)) {
      as.character(df$Year[1])  # Primer año disponible
    } else {
      NA_character_
    }
  })
  dataset_FINAL$OldestDate <- dataset_FINAL$FirstIntroductionsInEU
  dataset_FINAL$Habitat <- "Freshwater"
  dataset_FINAL$CBD_Pathways <- map_chr(dataset_FINAL$CBD_Pathways,~ if (!is.null(.x)) .x$Name[1] else NA_character_)

  write.xlsx(dataset_FINAL,
             file.path("Inputfiles", paste0("Step0_OriginalDatabaseFreshwaterNODUPLICATES_EASIN.xlsx")),
             sep = ",",
             rowNames = FALSE,
             colNames = TRUE)
  cat("Archivo guardado correctamente : Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_EASIN.xlsx")
}
