easin_2025_Download_and_Depurate <- function(){
  #Download web scrapping
  url <- "https://easin.jrc.ec.europa.eu/apixg/catxg/env/FRW/skip/0/take/15000"
  res <- GET(url) # 1. Download JSON data
  json_raw <- content(res, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(json_raw, flatten = TRUE) # 2. Parse JSON content
  dataset <- as.data.frame(json_data) # Convert to a dataframe
  dataset_freshwater <- dataset[, c(
    "Name", "Status", "EASINID", "PresentInCountries",
    "NativeRange", "Kingdom", "Order", "Family",
    "CBD_Pathways", "FirstIntroductionsInEU"
  )]
  dataset_freshwater2 <- dataset_freshwater
  names(dataset_freshwater2)[names(dataset_freshwater2) == "PresentInCountries"] <- "Invaded_Countries_ISO2"
  names(dataset_freshwater2)[names(dataset_freshwater2) == "NativeRange"] <- "Native_Range_ISO2"
  dataset_freshwater2 <- dataset_freshwater2 %>%
    filter(Status == "A")
  dataset_freshwater2 <- dataset_freshwater2 %>%
    select(-Status)
  dataset_freshwater2$Status <- "Alien"
  locations_table <- read.xlsx("TablesToStandardize/standardization_tables.xlsx", sheet="countries_table")
  locations_table <- locations_table[, c("ISO3", "ISO2", "Location")]
  locations_table$ISO2 <- trimws(locations_table$ISO2)
  locations_table$Location <- trimws(locations_table$Location)
  MasterList_separated <- data.frame()
  for (i in seq_along(dataset_freshwater2$Invaded_Countries_ISO2)) {
    entry <- dataset_freshwater2$Invaded_Countries_ISO2[[i]]
    if (!is.null(entry)) {
      countries <- entry$Country
      temp_dataset <- data.frame(
        Name = rep(dataset_freshwater2$Name[i], length(countries)),
        Invaded_Countries_ISO2 = countries,
        stringsAsFactors = FALSE
      )
      MasterList_separated <- rbind(MasterList_separated, temp_dataset)
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
  final_dataset_collapse <- noduplicates(merged_data, column_name_species = "Name")
  names(final_dataset_collapse)[names(final_dataset_collapse) == "ISO3"] <- "Invaded_Countries_ISO3"
  names(final_dataset_collapse)[names(final_dataset_collapse) == "Location"] <- "Invaded_Countries"
  MasterList_separated2 <- data.frame()
  for (i in seq_along(dataset_freshwater2$Native_Range_ISO2)) {
    entry <- dataset_freshwater2$Native_Range_ISO2[[i]]
    if (!is.null(entry)) {
      countries <- entry$Country
      temp_dataset <- data.frame(
        Name = rep(dataset_freshwater2$Name[i], length(countries)),
        Native_Range_ISO2 = countries,
        stringsAsFactors = FALSE
      )
      MasterList_separated2 <- rbind(MasterList_separated2, temp_dataset)
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
  final_dataset_collapse2 <- noduplicates(merged_data2, column_name_species = "Name")
  names(final_dataset_collapse2)[names(final_dataset_collapse2) == "ISO3"] <- "Native_Range_ISO3"
  names(final_dataset_collapse2)[names(final_dataset_collapse2) == "Location"] <- "Native_Range"
  dataset_freshwater_final_1 <- merge(
    final_dataset_collapse,
    final_dataset_collapse2,
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
  final_dataset <- dataset_freshwater_final_2 %>%
    select(-"Invaded_Countries_ISO2.x") %>%
    select(-"Native_Range_ISO2.x")
  names(final_dataset)[names(final_dataset) == "Invaded_Countries_ISO2.y"] <- "Invaded_Countries_ISO2"
  names(final_dataset)[names(final_dataset) == "Native_Range_ISO2.y"] <- "Native_Range_ISO2"
  final_dataset$FirstIntroductionsInEU <- map_chr(final_dataset$FirstIntroductionsInEU, function(dataset) {
    if (!is.null(dataset) && "Year" %in% colnames(dataset)) {
      as.character(dataset$Year[1]) # Primer año disponible
    } else {
      NA_character_
    }
  })
  final_dataset$OldestDate <- final_dataset$FirstIntroductionsInEU
  final_dataset$Habitat <- "Freshwater"
  final_dataset$CBD_Pathways <- map_chr(final_dataset$CBD_Pathways, ~ if (!is.null(.x)) .x$Name[1] else NA_character_)

  #Save
  write.xlsx(final_dataset,"Inputfiles/originaldatabase_easin_2025.xlsx",rowNames = FALSE,colNames = TRUE)
  write.xlsx(final_dataset,"Inputfiles/freshwatersubset_easin_2025.xlsx",rowNames = FALSE,colNames = TRUE)
}
