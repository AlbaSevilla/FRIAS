fao_2025_Download_and_Depurate <- function(){
  #Download
  extract_regions <- function(text_column) {
    continents <- unique(countrycode::codelist$continent)
    countries <- na.omit(unique(countrycode::codelist$country.name.en))
    map(text_column, function(text) {
      if (is.na(text)) return(NA_character_)
      results <- c(
        continents[str_detect(text, fixed(continents))],
        countries[str_detect(text, fixed(countries))]
      )
      extract_words <- str_extract_all(text, "\\b[A-Z][a-z]+(?:\\s[A-Z][a-z]+)*\\b")[[1]]
      subregions <- setdiff(extract_words, results)
      unique(c(results, subregions))
    })
  }
  links <- c(
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e08.htm",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e09.htm",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0a.htm",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0b.htm",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0c.htm",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0d.htm",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0e.htm",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0f.htm"
  )

  columns <- c("Native range", "From", "To", "Year", "Reason")
  processing_link <- function(url) {
    res <- read_html(url)
    names <- res %>%
      html_nodes("h2") %>%
      html_text() %>%
      str_trim()
    content <- res %>%
      html_nodes("h2") %>%
      map(function(node) {
        merged_node <- node %>% html_nodes(xpath = "following-sibling::*")
        info <- merged_node %>%
          keep(~ html_name(.x) != "h2") %>%
          map_chr(html_text) %>%
          str_replace_all("\r\n", " ") %>%
          str_squish() %>%
          paste(collapse = " ")
        stract_range <- str_split(info, "Native range", n = 3)[[1]]
        if (length(stract_range) >= 2) {
          info <- paste0("Native range", stract_range[2])
        }
        info
      })
    dataset <- tibble(species = names, info = content) %>%
      mutate(info = str_replace_all(info, "\r\n", " ")) %>%
      mutate(
        `Native range` = str_extract_all(info, "(?<=Native range: ).*?(?=To:|From:|Year:|Reason:|$)"),
        `From` = str_extract_all(info, "(?<=From: ).*?(?=To:|Native range:|Year:|Reason:|$)"),
        `To` = str_extract_all(info, "(?<=To: ).*?(?=From:|Native range:|Year:|Reason:|$)"),
        `Year` = str_extract_all(info, "(?<=Year: ).*?(?=To:|From:|Native range:|Reason:|$)"),
        `Reason` = str_extract_all(info, "(?<=Reason: ).*?(?=To:|From:|Native range:|Year:|$)")
      ) %>%
      mutate(across(all_of(columns), ~ sapply(.x, function(x) paste(unique(x), collapse = "; ")))) %>%
      select(species, all_of(columns))
    dataset
  }
  final_dataset <- map_datasetr(links, processing_link)
  final_dataset$Habitat_Database <- "Freshwater"
  species <- final_dataset$species
  final_dataset$AcceptedNameGBIF <- name_backbone_checklist(species)$canonicalName
  final_dataset <- final_dataset %>%
    mutate(across(everything(), ~ str_split(.x, ";") %>%
                    map_chr(~ {
                      values <- trimws(.x)
                      values <- values[values != "Unknown"]
                      if(length(values) == 0) NA_character_
                      else paste(values, collapse = "; ")
                    })))
  final_dataset2 <- final_dataset %>%
    mutate(Native_range_clean = str_extract(`Native range`, "^[^.]+") %>% str_trim())
  final_dataset2$regions <- extract_regions(final_dataset2$Native_range_clean)
  final_dataset2$united_regions <- sapply(final_dataset2$regions, function(x) {
    if (all(is.na(x))) return(NA_character_)
    paste(x[!is.na(x)], collapse = "; ")
  })
  columns <- c("AcceptedNameGBIF", "united_regions", "To", "Year", "Reason", "Habitat_Database")
  final_dataset2 <- final_dataset2[, columns]
  colnames(final_dataset2) <- c("AcceptedNameGBIF", "NativeRange", "RecipientCountry", "Year", "Reason", "Habitat_Database")
  continents <- unique(countrycode::codelist$continent)
  countries <- na.omit(unique(countrycode::codelist$country.name.en))
  river_string <- c("River", "Lake", "Sea", "Bay", "Creek", "Delta", "Estuary")
  clean_native_range <- function(text) {
    if (is.na(text)) return(NA_character_)
    extract_words <- str_extract_all(text, "\\b[A-Z][a-z]+(?:\\s[A-Z][a-z]+)*\\b")[[1]]
    extract_words <- extract_words[!str_detect(extract_words, "(ish|ese|ian|ic|ite)$")]
    selection <- extract_words[
      extract_words %in% countries |
        extract_words %in% continents |
        str_detect(extract_words, paste0(river_string, collapse = "|")) |
        extract_words %in% c("Amazon", "Nile", "Danube")
    ]
    if (length(selection) == 0) return(NA_character_)
    paste(unique(selection), collapse = "; ")
  }
  final_dataset2 <- final_dataset2 %>%
    mutate(NativeRange = sapply(NativeRange, clean_native_range))

  #Save
  write_xlsx(final_dataset2, "InputFiles/originaldatabase_fao_2025.xlsx")
  write_xlsx(final_dataset2, "InputFiles/freshwatersubset_fao_2025.xlsx")
}
