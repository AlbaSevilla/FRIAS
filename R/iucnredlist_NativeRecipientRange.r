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
    final$URL <- paste0("https://www.iucnredlist.org/species/pdf/", final$assessmentId)
    results <- list()
    for (i in seq_len(nrow(final))) {
      url <- final$URL[i]
      sci_name <- final$scientificName[i]
      message(sprintf("Processing %d/%d: %s", i, nrow(final), sci_name))
      text <- tryCatch(
        pdf_text(url),
        error = function(e) {
          message("error to: ", sci_name)
          return(NA)
        }
      )
      if (is.na(text)[1]) next
      all_text <- paste(text, collapse = "\n")
      match <- str_match(all_text, "Country Occurrence:[\\s\\S]*?(?=\\n\\n|$)")[, 1]
      if (is.na(match)) next
      block <- gsub("\n", " ", match)  # eliminar saltos de línea dentro del bloque
      native_match  <- str_match(block, "Native,\\s*Extant \\(resident\\):\\s*(.*?)\\s*(?=Extant|Introduced|$)")[, 2]
      invaded_match <- str_match(block, "Extant\\s*(?:&|and)\\s*Introduced \\(resident\\):\\s*(.*?)(?=$)")[, 2]
      clean_list <- function(x) {
        if (is.na(x) || x == "") return(NA)
        x <- gsub("\\s+", " ", x)
        x <- gsub(",\\s*", ";", x)
        x <- gsub(";+", ";", x)
        x <- trimws(x)
        countries <- unlist(strsplit(x, ";"))
        countries <- trimws(countries)
        countries <- countries[countries != ""]
        paste(unique(countries), collapse = "; ")
      }
      native_countries  <- clean_list(native_match)
      invaded_countries <- clean_list(invaded_match)
      lines <- strsplit(text[1], "\n")[[1]]
      line_name <- grep(", ", lines, value = TRUE)
      sci_name_pdf <- if (length(line_name) > 0) trimws(sub(",.*", "", line_name[1])) else NA
      results[[i]] <- data.frame(
        scientificName_in_table = sci_name,
        scientificName_in_pdf = sci_name_pdf,
        native_occurrence = native_countries,
        invaded_occurrence = invaded_countries,
        stringsAsFactors = FALSE
      )
    }
    results_masterlist <- bind_rows(results)
    return(results_masterlist)
  }

  native_range <- get_iucn_country_occurrence(final)


  extraer_extant_introducido_df <- function(df) {
    extraer_extant_introducido <- function(assessmentId) {
      tryCatch({
        url <- paste0("https://www.iucnredlist.org/species/pdf/", assessmentId)
        texto <- paste(pdf_text(url), collapse = " ")
        pattern <- "Extant[^\n]*Introduced[^\n]*:[\\s\\S]*?(?=\\n\\n|©|Distribution Map|Country Occurrence:|$)"
        extant_introduced <- regmatches(texto, regexpr(pattern, texto, perl = TRUE))
        if(length(extant_introduced) == 0) return(NA)
        paises_texto <- sub(".*?Extant[^\n]*Introduced[^\n]*:\\s*", "", extant_introduced, perl = TRUE)
        paises_vector <- str_split(paises_texto, ";\\s*")[[1]]
        paises_vector <- str_trim(paises_vector)
        return(paises_vector)
      }, error = function(e) {
        warning(paste("Error in assessmentId:", assessmentId))
        return(NA)
      })
    }
    df$extant_introduced <- vector("list", nrow(df))
    for(i in 1:nrow(df)) {
      df$extant_introduced[[i]] <- extraer_extant_introducido(df$assessmentId[i])
      cat(sprintf("Processing %d of %d (%.1f%%)\n", i, nrow(df), i / nrow(df) * 100))
    }
    return(df)
  }

  invaded_range <- extraer_extant_introducido_df(final)


  results_masterlist <- merge(
    native_range,
    invaded_range,
    by.x = "scientificName_in_table",
    by.y = "scientificName",
    all = TRUE  # Esto asegura que se incluyan todas las filas, incluso si no hay match
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
    mutate(InvadedRange = paste(RecipientRange, invaded_country, sep = "; ")) %>%
    select(-scientificName_in_pdf, -country_occurrence, -invaded_country)

  #Date
  old_locale <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  today <- Sys.Date()
  formatted_date <- format(today, "%d %B %Y")  # ej: "02 December 2025"
  current_year <- format(today, "%Y")          # ej: "2025"
  Sys.setlocale("LC_TIME", old_locale)

  iucn_text <- paste0(
    "iucn ", current_year,
    " (the iucn red list of threatened species, version ", current_year, " - ",
    "https://www.iucnredlist.org. accessed on ", formatted_date, ")"
  )

  # Añadir la referencia a Source_Data solo para especies con info IUCN
  MasterList <- MasterList %>%
    mutate(Source_Data = if_else(
      AcceptedNameGBIF %in% results_masterlist$scientificName_in_table,
      paste0(Source_Data, "; ", iucn_text),
      Source_Data
    ))

  write_xlsx(merged_results,"OutputFiles/Intermediate/step7_additionalNativeRecipientRangeIUCN_masterlist.xlsx")
  write_csv(merged_results,"OutputFiles/Intermediate/step7_additionalNativeRecipientRangeIUCN_masterlist.csv")
}



