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

  CABI_firstrecords <- read.csv("InputFiles/originaldatabase_giatar_2024/dataset/CABI data/CABI_tables/tointroductions.csv")
  #Eliminate inconsistences in year field (like 1970s, <1900, etc..)
  CABI_firstrecords <- CABI_firstrecords %>%
    mutate(
      Year = ifelse(grepl("^[0-9]+$", Year), Year, NA)
    )
  CABI_firstrecords <- noduplicates(CABI_firstrecords, "usageKey")
  CABI_firstrecords <- CABI_firstrecords %>%
    filter(grepl("^[0-9]+$", usageKey))


  EPPO_first_reports <- read.csv("InputFiles/originaldatabase_giatar_2024/dataset/EPPO data/EPPO_first_reports.csv")
  EPPO_first_reports <- noduplicates(EPPO_first_reports, "usageKey")

  #Merge CABI_firstrecords and EPPO_first_reports by usageKey
  merged <- full_join(
    CABI_firstrecords,
    EPPO_first_reports,
    by = "usageKey"
  )
  merged <- merged[,c("usageKey", "Introduced.to", "Introduced.from", "Year", "year", "location")]
  merged <- merged %>%
    mutate(
      RecipientRange = paste(Introduced.to, location, sep = ", "),
      RecipientRange = gsub("^, |, $", "", RecipientRange),
      ReportYear = paste(Year, year, sep = ", "),
      ReportYear = gsub("^, |, $", "", ReportYear),
      ReportYear = gsub(", NA|NA,", "", ReportYear),
      ReportYear = trimws(ReportYear)
    ) %>%
    select(
      usageKey,
      RecipientRange,
      ReportYear
    )
  first_records_noduplicates <- noduplicates(merged, "usageKey")



  dataset <- full_join(
    native_ranges_noduplicates,
    first_records_noduplicates,
    by = "usageKey"
  )
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
    message("Procesando ", i, "/", length(species_list0), ": ", especie)
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
  dat_fresh <- OldestDate(dat_fresh,"ReportYear")

  #Save
  write.xlsx(dat_fresh, "InputFiles/freshwatersubset_giatar_2024.xlsx")
}
