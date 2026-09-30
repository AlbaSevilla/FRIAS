iucnredlist_NativeRecipientRange <- function(){
 #csvs has been downloaded from: https://www.iucnredlist.org/search

  MasterList <- read_excel("OutputFiles/Intermediate/step6_standardizedinformalgroup_masterlist.xlsx")
  names <- MasterList$AcceptedNameGBIF
  taxonomy_iucn <- read.csv("InputFiles/taxonomy.csv", sep=",")
  head(taxonomy_iucn)
  taxonomy_iucn <- taxonomy_iucn[,c(1:15)]
  assessments_iucn <- read.csv("InputFiles/assessments.csv", sep=",")
  assessments_iucn <- assessments_iucn[,c(1:15)]
  subset_taxonomy_iucn <- taxonomy_iucn[taxonomy_iucn$scientificName %in% names, ]
  subset_assessments_iucn <- assessments_iucn[assessments_iucn$scientificName %in% names, ]
  columnas_taxonomy <- c("internalTaxonId", "scientificName")
  subset_subset_taxonomy_iucn <- subset_taxonomy_iucn[,columnas_taxonomy]
  columnas_taxonomy <- c("assessmentId", "scientificName")
  subset_subset_assessments_iucn <- subset_assessments_iucn[,columnas_taxonomy]
  final <- merge(subset_subset_taxonomy_iucn, subset_subset_assessments_iucn, by="scientificName")
  dim(final)

  #build urls
  final$URL <- paste0("https://www.iucnredlist.org/species/pdf/", final$assessmentId)
  head(final[, c("scientificName", "URL")])

  #Obtain the native Country with the IUCN by web scraping the free PDFs available on the website for the species of interest.
  get_iucn_country_occurrence <- function(final) {
    stopifnot(all(c("scientificName", "assessmentId") %in% names(final)))
    results <- list()
    for (i in seq_len(nrow(final))) {
      sci_name <- final$scientificName[i]
      url <- paste0("https://www.iucnredlist.org/species/pdf/", final$assessmentId[i])
      message(sprintf("Processing %d/%d: %s", i, nrow(final), sci_name))
      tmpfile <- tempfile(fileext = ".pdf")
      tryCatch({
        download.file(url, tmpfile, mode = "wb", quiet = TRUE)
        text <- pdf_text(tmpfile)
      }, error = function(e) {
        message("Error downloading PDF of: ", sci_name)
        return(NULL)
      })
      if (is.null(text)) next
      all_text <- paste(text, collapse = "\n")
      match <- str_match(all_text, "Country Occurrence[\\s\\S]*?(?=Population|$)")[,1]
      if (is.na(match)) next
      block <- gsub("\n", " ", match)
      native_match <- str_match(block, "Native[^:]*:\\s*(.*?)\\s*(?=Extant|Introduced|$)")[,2]
      clean_list <- function(x) {
        if (is.na(x) || x == "") return(NA)
        x <- gsub("\\s+", " ", x)         # elimina espacios múltiples
        x <- gsub("\\(.*?\\)", "", x)     # elimina todo lo que esté entre paréntesis
        x <- gsub(",\\s*", ";", x)        # reemplaza comas por ;
        x <- gsub(";+", ";", x)           # elimina ; repetidos
        x <- trimws(x)                     # elimina espacios al inicio y fin
        countries <- unlist(strsplit(x, ";"))
        countries <- trimws(countries)
        countries <- countries[countries != ""]
        paste(unique(countries), collapse = "; ")
      }
      native_countries <- clean_list(native_match)
      results[[i]] <- data.frame(
        scientificName = sci_name,
        native_occurrence = native_countries,
        stringsAsFactors = FALSE
      )
    }

    bind_rows(results)
  }
  clean_list <- function(x) {
    if (is.na(x) || x == "") return(NA)
    x <- gsub("\\s+", " ", x)
    x <- gsub(",\\s*", ";", x)
    x <- gsub(";+", ";", x)
    x <- trimws(x)
    x <- gsub("© The IUCN Red List.*?(?=$)", "", x)
    x <- gsub("http://dx.doi.org/[^ ]+", "", x)
    countries <- unlist(strsplit(x, ";"))
    countries <- trimws(countries)
    countries <- countries[countries != ""]
    paste(unique(countries), collapse = "; ")
  }
  native_range <- get_iucn_country_occurrence(final)
  native_range <- native_range %>%
    mutate(native_occurrence = str_replace_all(native_occurrence,"© The IUCN Red List.*", "")) %>%
    mutate(native_occurrence = str_replace_all(native_occurrence,"http://dx.doi.org/[^ ]+", "")) %>%
    mutate(native_occurrence = str_trim(native_occurrence))

  #obtain introduced ranges
  extract_extant_introduced_dataset <- function(dataset) {

    extract_extant_introduced <- function(assessmentId) {
      tryCatch({
        url <- paste0("https://www.iucnredlist.org/species/pdf/", assessmentId)
        text <- paste(pdf_text(url), collapse = " ")

        # --- Original part: Extant Introduced ---
        pattern <- "Extant[^\n]*Introduced[^\n]*:[\\s\\S]*?(?=\\n\\n|©|Distribution Map|Country Occurrence:|$)"
        extant_introduced <- regmatches(text, regexpr(pattern, text, perl = TRUE))
        countries_vector <- c()
        if(length(extant_introduced) > 0) {
          countries_text <- sub(".*?Extant[^\n]*Introduced[^\n]*:\\s*", "", extant_introduced, perl = TRUE)
          countries_vector <- str_split(countries_text, ";\\s*")[[1]]
          countries_vector <- str_trim(countries_vector)
        }

        # --- New part: detect "was introduced to" or "widely distributed" ---
        intro_pattern <- "(was introduced to|widely distributed)[\\s\\S]*?\\."
        intro_matches <- gregexpr(intro_pattern, text, perl = TRUE)
        introduced_vector <- c()
        if(intro_matches[[1]][1] != -1) {
          phrases <- regmatches(text, intro_matches)[[1]]
          for(phrase in phrases) {
            phrase <- sub("^(was introduced to|widely distributed)\\s*", "", phrase, perl = TRUE)
            words <- unlist(str_split(phrase, "\\s+"))
            words_upper <- words[str_detect(words, "^[A-Z]")]
            introduced_vector <- c(introduced_vector, paste(words_upper, collapse = ";"))
          }
        }

        # --- Merge both vectors ---
        combined_vector <- c(countries_vector, introduced_vector)

        # --- Remove parentheses and content inside ---
        combined_vector <- str_replace_all(combined_vector, "\\([^)]*\\)", "")
        combined_vector <- str_replace_all(combined_vector, "[()]", "")
        combined_vector <- str_trim(combined_vector)

        if(length(combined_vector) == 0) combined_vector <- NA

        return(combined_vector)

      }, error = function(e) {
        warning(paste("Error in assessmentId:", assessmentId))
        return(NA)
      })
    }

    dataset$extant_introduced <- vector("list", nrow(dataset))

    for(i in 1:nrow(dataset)) {
      dataset$extant_introduced[[i]] <- extract_extant_introduced(dataset$assessmentId[i])
      cat(sprintf("Processing %d of %d (%.1f%%)\n", i, nrow(dataset), i / nrow(dataset) * 100))
    }

    return(dataset)
  }


  invaded_range <- extract_extant_introduced_dataset(final)
  invaded_range <- invaded_range %>%
    unnest(extant_introduced)
  invaded_range <- noduplicates(invaded_range, "scientificName")
  invaded_range$extant_introduced <- gsub(",",";",invaded_range$extant_introduced)

  results_masterlist <- merge(
    native_range,
    invaded_range,
    by= "scientificName",
    all = TRUE
  )

  names(results_masterlist)
  names(results_masterlist)[names(results_masterlist) == "native_occurrence"] <- "country_occurrence"
  names(results_masterlist)[names(results_masterlist) == "extant_introduced"] <- "invaded_country"

  results_masterlist$invaded_occurrence <- NULL
  names(results_masterlist)

  #Save
  results <- as.data.frame(results_masterlist)
  results$invaded_country <- sapply(results$invaded_country, function(x) {
    if (is.null(x) || all(is.na(x))) {
      return(NA)
    } else {
      return(paste(x, collapse = "; "))
    }
  })

  write.csv(results, "InputFiles/Native_Recipient_Range_IUCN.csv", row.names = FALSE)
  write.xlsx(results, "InputFiles/Native_Recipient_Range_IUCN.xlsx")

  #Final Result:
  MasterList <- read_excel("OutputFiles/Intermediate/step6_standardizedinformalgroup_masterlist.xlsx")
  results_masterlist <- read_csv("InputFiles/Native_Recipient_Range_IUCN.csv")
  names(results_masterlist)
  names(MasterList)
  # Merge MasterList with cleaned results_masterlist

  merged_results <- MasterList %>%
    left_join(results_masterlist, by = c("AcceptedNameGBIF" = "scientificName")) %>%
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
    mutate(RecipientRange = paste(RecipientRange, invaded_country, sep = "; ")) %>%
    select(-country_occurrence, -invaded_country, -internalTaxonId, -URL, -assessmentId)

  #Date
  old_locale <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  today <- Sys.Date()
  formatted_date <- format(today, "%d %B %Y")
  current_year <- format(today, "%Y")
  Sys.setlocale("LC_TIME", old_locale)

  iucn_text <- paste0(
    "iucn ", current_year,
    " (the iucn red list of threatened species, version ", current_year, " - ",
    "https://www.iucnredlist.org. accessed on ", formatted_date, ")"
  )

  # Añadir la referencia a Source_Data solo para especies con info IUCN
  MasterList <- MasterList %>%
    mutate(Source_Data = if_else(
      AcceptedNameGBIF %in% results_masterlist$scientificName,
      paste0(Source_Data, "; ", iucn_text),
      Source_Data
    ))

  write_xlsx(merged_results,"OutputFiles/Intermediate/step7_additionalNativeRecipientRangeIUCN_masterlist.xlsx")
  write_csv(merged_results,"OutputFiles/Intermediate/step7_additionalNativeRecipientRangeIUCN_masterlist.csv")
}



