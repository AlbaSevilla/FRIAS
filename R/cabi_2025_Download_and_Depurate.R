cabi_2025_Download_and_Depurate <- function() {
  #Depurate
  data <- read.csv("InputFiles/originaldatabase_cabi_2025.csv",sep=",")
  selected_columns <- c("Preferred.scientific.name","Kingdom", "Order", "Family","Phylum","Class","Habitat","Pathways", "Datasheet.URL....")
  data_subset_cabi_2025 <- data[, selected_columns]
  names(data_subset_cabi_2025)[names(data_subset_cabi_2025) == "Preferred.scientific.name"] <- "PreferredScientificName"
  data_subset_cabi_2025$column1 <- gsub(";", "", data_subset_cabi_2025$Datasheet.URL....)
  cabi_2025_noduplicates <- data_subset_cabi_2025 %>%
    dplyr::group_by(PreferredScientificName) %>%
    dplyr::summarise(across(everything(), ~ {
      text <- as.character(.)
      vals <- unlist(str_split(text, "[,;|]+|\\s{2,}"))
      vals <- str_trim(vals)
      vals <- vals[vals != ""]
      vals <- unique(vals)
      paste(vals, collapse = ", ")
    }), .groups = "drop")
  cabi_2025_noduplicates$Habitat <- "Freshwater"
  cabi_2025_noduplicates <- cabi_2025_noduplicates %>%
    select(-column1)
  names(cabi_2025_noduplicates)[names(cabi_2025_noduplicates) == "Datasheet.URL...."] <- "Url_Specie"
  cabi_2025_noduplicates$Url_Specie <- gsub("^https://doi\\.org/", "https://www.cabidigitallibrary.org/doi/", cabi_2025_noduplicates$Url_Specie)
  names <- cabi_2025_noduplicates$PreferredScientificName
  cabi_2025_noduplicates$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName

  #Save
  write.xlsx(cabi_2025_noduplicates, "./InputFiles/freshwatersubset_cabi_2025.xlsx")
}
