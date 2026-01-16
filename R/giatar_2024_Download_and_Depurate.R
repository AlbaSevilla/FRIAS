giatar_2024_Download_and_Depurate <- function(){
  #Download
  url <- "https://zenodo.org/records/17123201/files/GIATAR.zip?download=1"
  destfile <- "InputFiles/originaldatabase_giatar_2024.zip"
  download.file(url, destfile, mode = "wb")
  unzip("InputFiles/originaldatabase_giatar_2024.zip", exdir = "InputFiles")
  original_path <- getwd()

  #Depurate
  native_ranges <- read.csv("InputFiles/dataset/native ranges/all_sources_native_ranges.csv")
  native_ranges <- native_ranges[,-1]
  selected_columns <- c("genus_species", "bioregion", "DAISIE_region", "usageKey")
  native_ranges <- native_ranges[,selected_columns]
  colnames(native_ranges) <- c("ScientificName", "Bioregion", "Native_Range", "usageKey")
  native_ranges_noduplicates <- noduplicates(native_ranges, "ScientificName")
  first_records <- read.csv("InputFiles/dataset/occurrences/all_records.csv")
  selected_columns2 <- c("usageKey", "location", "year", "ISO3")
  first_records <- first_records[,selected_columns2]
  first_records_noduplicates <- noduplicates(first_records, "usageKey")
  colnames(first_records_noduplicates) <- c("usageKey", "Invaded_country", "Year", "ISO3")
  dataset <- merge(first_records_noduplicates, native_ranges_noduplicates, by = "usageKey")
  dataset <- as.data.frame(dataset)
  dataset <- dataset %>%
    select(ScientificName, everything())
  dataset2 <- dataset %>% select(-usageKey)
  write_xlsx(dataset2, "InputFiles/originaldatabase_giatar_2024.xlsx")

  dataset <- dataset2
  species_list0 <- dataset$ScientificName
  results <- character(length(species_list0))
  for (i in seq_along(species_list0)) {
    especie <- species_list0[i]
    tryCatch({
      res <- name_backbone_checklist(especie)
      results[i] <- res$canonicalName
    }, error = function(e) {
      message("Species not found ", especie, ": ", e$message)
      results[i] <- NA
    })
    Sys.sleep(0.2)
  }
  species_list <- results
  dat_act <- check_habitat(species_list, dataset)
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh <- OldestDate(dat_fresh,"Year")

  #Save
  write.xlsx(dat_fresh, "InputFiles/freshwatersubset_giatar_2024.xlsx")
}
