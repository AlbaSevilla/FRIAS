great_britain_non_native_species_information_portal_2025_Download_and_Depurate <- function(){
  #Download
  html_text <- readLines("https://www.nonnativespecies.org/non-native-species/id-sheets")
  writeLines(html_text, "Inputfiles/html_great_britain_non_native_species_information_portal_2025.html")
  html_parsed <- read_html(paste(html_text, collapse = "\n"))
  species <- html_parsed %>%
    html_elements("td > em") %>%
    html_text() %>%
    trimws()
  relative_links <- html_parsed %>%
    html_elements("a") %>%
    html_attr("href") %>%
    na.omit() %>%
    .[grepl("^/assets/Uploads/.*\\.pdf$", .)]  # Only PDF links in /assets/Uploads/
  base_url <- "https://www.nonnativespecies.org"
  full_links <- paste0(base_url, relative_links)
  results <- data.frame(
    Species = character(),
    Native_to = character(),
    Habitat = character(),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(full_links)) {
    sp <- species[i]
    url <- full_links[i]
      cat("Processing", i, "/", length(full_links), ":", sp, "\n")
      text_pdf <- tryCatch(pdf_text(url), error = function(e) return(NA))
    if (all(is.na(text_pdf))) next
      lines <- unlist(strsplit(text_pdf, "\n"))
    native_line <- grep("Native to:", lines, value = TRUE)
    native_to <- if (length(native_line)) sub(".*Native to:\\s*", "", native_line[1]) else NA
    habitat_line <- grep("Habitat:", lines, value = TRUE)
    habitat <- if (length(habitat_line)) sub(".*Habitat:\\s*", "", habitat_line[1]) else NA
    results <- rbind(results, data.frame(
      Species = sp,
      Native_to = native_to,
      Habitat = habitat,
      stringsAsFactors = FALSE
    ))
  }
  results$Invaded_country <- "Great Britain"
  results$Habitat_Database <- results$Habitat

  # Save
  write.xlsx(results, "Inputfiles/originaldatabase_great_britain_non_native_species_information_portal_2025.xlsx")

  # -------------------------
  # HABITAT PROCESSING PIPELINE
  # -------------------------
  source(file.path("R", "check_habitat.r"))

  dataset <- results
  names_species <- dataset$Species

  # Taxonomic verification
  accepted_names <- name_backbone_checklist(names_species)$canonicalName

  # Standardize habitat information
  updated_data <- check_habitat(accepted_names, dataset)

  # Filter freshwater species
  freshwater_subset <- updated_data %>% filter(grepl("FRESHWATER", Habitat))

  # Save freshwater dataset
  write.xlsx(freshwater_subset, "Inputfiles/freshwatersubset_great_britain_non_native_species_information_portal_2025.xlsx")
}
