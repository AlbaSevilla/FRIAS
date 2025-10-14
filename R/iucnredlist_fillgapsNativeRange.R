iucnredlist_fillgapsNativeCountry <- function(){

  MasterList <- read_excel("OutputFiles/Intermediate/Step7_ObtainFuctionalGroup_Masterlist.xlsx")
  names <- MasterList$AcceptedNameGBIF
  taxonomy_iucn <- read.csv("InputFiles/taxonomy_iucn.csv", sep=";")
  taxonomy_iucn <- taxonomy_iucn[,c(1:15)]
  assessments_iucn <- read.csv("InputFiles/assessments_iucn.csv", sep=";")
  assessments_iucn <- assessments_iucn[,c(1:15)]
  subset_taxonomy_iucn <- taxonomy_iucn[taxonomy_iucn$scientificName %in% names, ]
  subset_assessments_iucn <- assessments_iucn[assessments_iucn$scientificName %in% names, ]
  columnas_taxonomy <- c("internalTaxonId", "scientificName")
  subset_subset_taxonomy_iucn <- subset_taxonomy_iucn[,columnas_taxonomy]
  columnas_taxonomy <- c("assessmentId", "scientificName")
  subset_subset_assessments_iucn <- subset_assessments_iucn[,columnas_taxonomy]
  final <- merge(subset_subset_taxonomy_iucn, subset_subset_assessments_iucn, by="scientificName")


  #build urls
  final$URL <- paste0("https://www.iucnredlist.org/species/pdf/", final$assessmentId)
  head(final[, c("scientificName", "URL")])


  #Obtain the native Country with the IUCN by web scraping the free PDFs available on the website for the species of interest.
  results <- list()
  for (i in 1:nrow(final)) {
    url <- final$URL[i]
    scientificName <- final$scientificName[i]
    message(sprintf("Processing %d/%d: %s", i, nrow(final), scientificName))
    text <- tryCatch({
      pdf_text(url)
    }, error = function(e) {
      message("Error reading PDF for species : ", scientificName)
      return(NA)
    })
    if (is.na(text)[1]) next
    page1 <- text[1]
    lines <- strsplit(page1, "\n")[[1]]
    line_name <- grep(", ", lines, value = TRUE)
    if (length(line_name) > 0) {
      scientificNameinIUCN <- sub(",.*", "", line_name[1])
      scientificNameinIUCN <- trimws(scientificNameinIUCN)
    } else {
      scientificNameinIUCN <- NA
    }
    all_Text <- paste(text, collapse = "\n")
    pattern <- "Country Occurrence:[\\s\\S]*?(?=\\n\\n|$)"
    match <- regmatches(all_Text, regexpr(pattern, all_Text, perl = TRUE))
    if (length(match) == 0) {
      country_occurrence <- NA
    } else {
      country_occurrence <- trimws(match)
    }
    results[[i]] <- data.frame(
      scientificName_in_table = scientificName,
      scientificName_in_pdf = scientificNameinIUCN,
      country_occurrence = country_occurrence,
      stringsAsFactors = FALSE
    )
  }

  results_masterlist <- do.call(rbind, results)

  #Save
  write_xlsx(results_masterlist,"Inputfiles/NativeCountrysMasterlistByIUCN.xlsx")

  #Final Result:
  results_masterlist <- read_excel("Inputfiles/NativeCountrysMasterlistByIUCN.xlsx")
  results_masterlist$country_occurrence <- results_masterlist$country_occurrence %>%
    str_replace_all("\\s*\\([^\\)]+\\)", "") %>%
    str_replace_all(c(
      "Country Occurrence:\\nNative, :" = "",
      "Country Occurrence:\\nNative:" = "",
      "Country Occurrence:Native, Extant non-breeding: " = "",
      "Country Occurrence:" = "",
      "Extant \\(resident\\)" = "",
      "Native, :" = "",
      "Native; Extant:" = ""
    )) %>%
    str_replace_all(",", ";") %>%
    str_replace_all("[\\(\\)]", "") %>%
    str_replace_all("\n", "") %>%
    str_trim()


  # Merge MasterList with cleaned results_masterlist
  merged_results <- MasterList %>%
    left_join(results_masterlist, by = c("AcceptedNameGBIF" = "scientificName_in_table")) %>%
    mutate(NativeRange = paste(NativeRange, country_occurrence, sep = "; ")) %>%
    mutate(NativeRange = str_replace_all(NativeRange, c(
      "NA; " = "",
      "NA; Native, : " = "",
      "Native, : " = "",
      "Native, Extant non-breeding: " = "",
      "NA" = "",
      "mainland" = "",
      "Peninsular" = "",
      "coast" = "",
      "pacific" = "",
      "Province" = "",
      "-" = ""
    ))) %>%
    select(-scientificName_in_pdf, -country_occurrence)


  write_xlsx(merged_results,"OutputFiles/Intermediate/Step8_FillGapsinLocations_MasterList.xlsx")
}



