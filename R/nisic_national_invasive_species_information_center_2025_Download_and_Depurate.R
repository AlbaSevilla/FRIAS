nisic_national_invasive_species_information_center_2025_Download_and_Depurate <- function(){
  #Download
  get_species_from_url <- function(url) {
    url <- sub("/+$", "", url)
    last_part <- sub(".*/", "", url)
    species <- gsub("-", " ", last_part)
    species <- tools::toTitleCase(species)
    return(species)
  }
  get_species_section <- function(url) {
    especie <- get_species_from_url(url)
    res <- read_html(url)
    clean <- res
    xml_remove(xml_find_all(clean, "//script|//style"))
    full_text <- clean %>% html_element("body") %>% html_text2()
    full_text <- full_text %>%
      str_replace_all("\r", "") %>%
      str_replace_all("\n{2,}", "\n\n") %>%
      str_trim()
    pattern <- paste0("(?s)", especie, ".*?(?=Image use policy)")
    out <- str_extract(full_text, pattern)
    if (is.na(out)) return(NA_character_)
    out <- str_trim(out)
    return(out)
  }
  extract_categories <- function(text_clean, especie) {
    out <- list()
    patterns <- list(
      ScientificName = "Scientific Name",
      NativeTo = "Native To",
      DateUSIntro = "Date of U\\.S\\. Introduction",
      MeansIntro = "Means of Introduction",
      Impact = "Impact"
    )
    get_section <- function(text, start, end = NULL) {
      if (is.null(end)) {
        pattern <- paste0("(?i)(?s)", start, "\\s*(.*)")
      } else {
        pattern <- paste0("(?i)(?s)", start, "\\s*(.*?)(?=", end, ")")
      }
      res <- str_extract(text, pattern)
      if (!is.na(res)) {
        res <- str_replace(res, regex(start, ignore_case = TRUE), "")
        res <- str_trim(res)
      } else {
        res <- ""
      }
      return(res)
    }
    out$ScientificName <- get_section(text_clean, patterns$ScientificName, patterns$NativeTo)
    out$NativeTo <- get_section(text_clean, patterns$NativeTo, patterns$DateUSIntro)
    out$DateOfUSIntroduction <- get_section(text_clean, patterns$DateUSIntro, patterns$MeansIntro)
    out$MeansOfIntroduction <- get_section(text_clean, patterns$MeansIntro, patterns$Impact)
    out$Impact <- get_section(text_clean, patterns$Impact, NULL)
    return(out)
  }
  main_url <- "https://www.invasivespeciesinfo.gov/species-profiles-list"
  html <- read_html(main_url)
  species_table <- html %>% html_table()
  table_data <- as.data.frame(species_table)
  links <- html %>% html_nodes("a") %>% html_attr("href")
  species_links <- links[grepl("^/(terrestrial|aquatic)/[a-z-]+/[a-z-]+$", links)]
  species_links <- unique(species_links) # eliminar duplicados
  get_full_url <- function(link) {
    paste0("https://www.invasivespeciesinfo.gov", link)
  }
  total_species <- length(species_links)
  dataset_all <- imap_dataset(species_links, function(link, i) {
    cat(sprintf("Procesando especie %d/%d: %s\n", i, total_species, link))
    url <- paste0("https://www.invasivespeciesinfo.gov", link)
    especie <- get_species_from_url(url)
    text_clean <- get_species_section(url)
    info <- extract_categories(text_clean, especie)
    as.data.frame(info, stringsAsFactors = FALSE)
  })
  scientific_name <- dataset_all$ScientificName
  scientific_name2 <- gsub("\\s+", " ", scientific_name)
  scientific_name3 <- gsub("[ ,.-]", "_", scientific_name2)
  scientific_name4 <- gsub("_+", "_", scientific_name3)
  scientific_name5 <- sub("^([^_]+_[^_]+).*", "\\1", scientific_name4)
  scientific_name6 <- gsub("_", " ", scientific_name5)
  dataset_all$ScientificName_withoutAuthor <- scientific_name6
  dataset_all <-
    dataset_all %>%
    mutate(
      Native_To = NativeTo %>%
        str_replace_all("\\(.*?\\)", "") %>%  # quitar referencias entre paréntesis
        str_replace_all("\\n", "") %>%        # quitar saltos de línea
        str_replace_all("\\.", "") %>%        # quitar puntos
        str_trim() %>%
        str_replace_all("(?i)first (discovered|identified|noted).*?in ", "") %>%
        str_replace_all("(?i)originally from ", "") %>%
        str_replace_all("(?i)native to ", "") %>%
        str_replace_all("(?i)believed to have originated in ", "") %>%
        str_replace_all("(?i)origin unknown.*", "") %>%
        str_replace_all("(?i)unknown.*", "") %>%
        str_trim()
    )
  remove_text_in_parentheses <- function(text) {
    return(str_replace_all(text, "\\(.*?\\)", ""))
  }
  extract_all_years <- function(text) {
    text_cleaned <- remove_text_in_parentheses(text)
    years <- str_extract_all(text_cleaned, "\\d{4}")
    if (length(years[[1]]) > 0) {
      return(paste(years[[1]], collapse = ", "))
    } else {
      return(NA)
    }
  }
  dataset_all$Date_of_US_Introduction <- sapply(dataset_all$DateOfUSIntroduction, extract_all_years)
  write.xlsx(dataset_all, "./InputFiles/originaldatabase_nisic_national_invasive_species_information_center_2025.xlsx")
  write.csv2(dataset_all, "./InputFiles/originaldatabase_nisic_national_invasive_species_information_center_2025.csv")

  #Depurate
  dataset <- dataset_all
  species_list0 <- dataset$ScientificName
  species_list <- name_backbone_checklist(species_list0)$canonicalName
  updated_dataset <- check_habitat(species_list, dataset)
  updated_dataset2 <- updated_dataset %>%
    select(-Species)
  dataset_freshwater <- updated_dataset2 %>%
    filter(grepl("FRESHWATER", Habitat))
  dataset_freshwater$RecipientCountry <- "United States of America"
  dataset_freshwater[] <- lapply(dataset_freshwater, function(x) {
    if (is.character(x) || is.factor(x)) {
      x <- as.character(x)          # por si es factor
      x <- gsub(",", ";", x)        # reemplaza todas las comas
    }
    x
  })
  dataset_freshwater$ScientificName <- sub(" \\(ITIS\\).*", "", dataset_freshwater$ScientificName)
  dataset_freshwater$DateOfUSIntroduction <- sapply(dataset_freshwater$DateOfUSIntroduction, function(x) {
    x <- gsub("\\(.*?\\)", "", x)
    years <- str_extract_all(x, "\\b\\d{4}\\b")[[1]]
    paste(years, collapse = ";")
  })

  #Save
  write.xlsx(dataset_freshwater, "./InputFiles/freshwatersubset_nisic_national_invasive_species_information_center_2025.xlsx")
}


