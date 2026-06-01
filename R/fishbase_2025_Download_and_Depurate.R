fishbase_2025_Download_and_Depurate <- function() {
  #Download
  fishbase_dataset <- species_names(server = c("fishbase", "sealifebase"), version = "latest")
  dataframe_species_dataset <- as.data.frame(fishbase_dataset)
  write.xlsx(dataframe_species_dataset, file.path("InputFiles", "originaldatabase_fishbase_2025.xlsx"))
  fish_names <- dataframe_species_dataset$Species
  fish_information <- ecosystem(species_list = fish_names)
  fish_information$Status <- factor(fish_information$Status)
  fish_information <- fish_information %>%
    filter(Status == "introduced")
  fish_information$Salinity <- factor(fish_information$Salinity)
  fish_information <- fish_information %>% filter(Salinity == "freshwater")
  fish_information$EcosystemType <- factor(fish_information$EcosystemType)
  fish_information <- fish_information %>%
    filter(EcosystemType == "River (basin)" | EcosystemType == "Lake" | EcosystemType == "Zoogreographic realm")
  fish_subset <- fish_information
  merged_dataset_fish <- fishbase_dataset %>%
    full_join(fish_subset, by = "SpecCode") %>%
    filter(!is.na(Status))
  selected_columns <- c("SpecCode","Species.x","Family","Order","Class","Status","Salinity","EcosystemType","EcosystemName")
  data_subset_fishbase <- merged_dataset_fish %>%
    select(all_of(selected_columns))
  dataset_noduplicates <- noduplicates(data_subset_fishbase,column_name_species = "Species.x")
  write.xlsx(dataset_noduplicates, file.path("InputFiles", "originaldatabase_fishbase_2025.xlsx"))

  #Depurate
  dataset_noduplicates <- read_excel("InputFiles/originaldatabase_fishbase_2025.xlsx")
  especies <- unique(dataset_noduplicates$Species.x)
  intro_df <- map_dfr(especies, function(sp) {
    tbl <- introductions(sp)
    if (nrow(tbl) > 0 && any(c("From", "RangeMin", "Year") %in% names(tbl))) {
      tbl <- tbl %>%
        mutate(
          Year_fusion = ifelse(!is.na(RangeMin), RangeMin, as.numeric(stringr::str_extract(Year, "\\d{4}")))
        ) %>%
        filter(!is.na(Year_fusion))
      if (nrow(tbl) > 0) {
        min_year <- which.min(tbl$Year_fusion)
        native_range <- tbl$From[min_year]
        native_year <- tbl$Year_fusion[min_year]
        # Si el país es "Unknown", ponerlo como NA
        if (!is.na(native_range) && native_range == "Unknown") native_range <- NA_character_
        tibble(Species.x = sp, native_range = native_range, native_year = native_year)
      } else {
        tibble(Species.x = sp, native_range = NA_character_, native_year = NA_real_)
      }
    } else {
      tibble(Species.x = sp, native_range = NA_character_, native_year = NA_real_)
    }
  })
  dataset_noduplicates <- dataset_noduplicates %>%
    left_join(intro_df, by = "Species.x")
  especies <- dataset_noduplicates$Species.x
  res <- ecosystem(especies)
  NativeRange <- res %>%
    filter(Status == "native") %>%
    select(Location, SpecCode)
  NativeRange <- noduplicates(NativeRange, "SpecCode")
  NativeRange <- NativeRange %>%
    mutate(NativeRange = Location) %>%
    select(-Location)
  NativeRange$SpecCode <- as.character(NativeRange$SpecCode)
  InvadedRange2 <- res %>%
    filter(Status == "introduced") %>%
    select(Location, SpecCode)
  InvadedRange2 <- noduplicates(InvadedRange2, "SpecCode")
  InvadedRange2 <- InvadedRange2 %>%
    mutate(InvadedRange2 = Location) %>%
    select(-Location)
  InvadedRange2$SpecCode <- as.character(InvadedRange2$SpecCode)
  dataset_noduplicates2 <- dataset_noduplicates %>%
    left_join(NativeRange, by = "SpecCode") %>%
    left_join(InvadedRange2, by= "SpecCode")
  dataset_noduplicates3 <- dataset_noduplicates2 %>%
    select(Species.x, SpecCode, Family, Order, Class, Status,
           Salinity,NativeRange, EcosystemName, native_year, InvadedRange2) %>%
    mutate(IntroducedRange = paste0(EcosystemName, InvadedRange2)) %>%
    select(-EcosystemName, -InvadedRange2)
  dataset_noduplicates3$NativeRange <- gsub("NA;", "", dataset_noduplicates3$NativeRange)
  dataset_noduplicates3$NativeRange <- gsub("NA,", "", dataset_noduplicates3$NativeRange)
  dataset_noduplicates3$NativeRange <- gsub(";NA", "", dataset_noduplicates3$NativeRange)
  dataset_noduplicates3$NativeRange <- gsub(",NA", "", dataset_noduplicates3$NativeRange)
  dataset_noduplicates3$NativeRange <- gsub(",", ";", dataset_noduplicates3$NativeRange)
  dataset_noduplicates3$Name <- paste(dataset_noduplicates3$Species.x)

  #Save
  write.xlsx(dataset_noduplicates3, "InputFiles/freshwatersubset_fishbase_2025.xlsx")
}

