garcia_berthou_et_al_2004_Download_and_Depurate <- function() {
  #Download
  taxonomic_urls <- paste0("https://invasiber2.org/fitxa_llista.php?taxonomic=", 1:11)
  get_species_links <- function(url) {
    page <- read_html(url)
    tables <- html_table(page)
    if(length(tables) < 5) return(NULL)
    table5 <- tables[[5]]
    species_names <- table5 %>%
      select(X1, X2) %>%
      filter(row_number() > 1) %>%
      mutate(scientific_name = paste(X1, X2)) %>%
      select(-X1, -X2)
    links <- html_nodes(page, "a") %>% html_attr("href")
    species_links <- links[grepl("fitxa_detalls\\.php", links)]
    list(names = species_names$scientific_name, links = paste0("https://invasiber2.org/", unique(species_links)))
  }
  species_data <- map(taxonomic_urls, get_species_links)
  all_species_links <- unlist(map(species_data, "links"))
  process_link <- function(url) {
    tryCatch({
      page <- read_html(url)
      tables <- html_table(page, fill = TRUE)
      species_name <- page %>% html_nodes("em") %>% html_text(trim = TRUE) %>% .[1:2] %>% paste(collapse = " ")
      table <- NULL
      if(length(tables) >= 8) {
        raw_table <- tables[[8]]
        if(ncol(raw_table) >= 2) {
          table <- raw_table %>%
            select(X1, X2) %>%
            filter(!is.na(X1) & X1 != "") %>%
            filter(!duplicated(X1)) %>%
            pivot_wider(names_from = X1, values_from = X2) %>%
            mutate(Species = species_name, .before = 1)
        }
      }
      native_distribution <- data.frame(NativeDistribution = NA)
      if(!is.null(table)) {
        last_table <- raw_table %>% mutate(across(everything(), trimws)) %>% filter(rowSums(!is.na(.)) > 0)
        region_row <- last_table %>% filter(X1 == "Native geographic distribution:")
        if(nrow(region_row) > 0) {
          region_row$X2 <- sapply(region_row$X2, function(x) {
            words <- str_extract_all(x, "\\b[A-Z][a-zA-Z]*\\b")[[1]]
            paste(words, collapse = ";")
          })
          native_distribution <- data.frame(NativeDistribution = region_row$X2)
        }
      }
      list(
        species_name = species_name,
        url = url,
        table8 = table,
        NativeDistribution = native_distribution
      )
    }, error = function(e) {
      warning(paste("Error processing:", url))
      return(NULL)
    })
  }
  results <- map(all_species_links, process_link)
  valid_results <- compact(results)
  final_df <- map_dfr(valid_results, function(x) {
    table <- x$table8
    if(!is.null(table)) {
      table$Species <- x$species_name
      table$URL <- x$url
      table$NativeDistribution <- x$NativeDistribution$NativeDistribution
      return(table)
    } else {
      return(NULL)
    }
  })
  dataset <- as.data.frame(final_df)
  colnames(dataset) <- c("Species", "Description", "BiologyAndHabitat", "Distribution", "IntroductionMechanism",
                         "EcologicalImpact", "SocioEconomicImpact", "Species", "URL", "NativeDistribution")
  write_xlsx(dataset,"InputFiles/originaldatabase_garcia_berthou_et_al_2004.xlsx")

  #Depurate
  dataset <- read.xlsx("InputFiles/originaldatabase_garcia_berthou_et_al_2004.xlsx", sheet="Sheet1")
  names <- dataset$Species
  accepted_names <- name_backbone_checklist(names)$canonicalName
  dataset_habitat <- check_habitat(accepted_names, dataset)
  dataset_habitat <- dataset_habitat[,-1]
  dataset_freshwater <- dataset_habitat %>% filter(grepl("FRESHWATER", Habitat))
  dataset_freshwater$InvadedCountry <- "Spain; Portugal"
  selected_columns <- c("Species", "AcceptedNameGBIF", "Habitat", "NativeDistribution", "InvadedCountry")
  dataset_freshwater <- dataset_freshwater[, selected_columns]
  write_xlsx(dataset_freshwater, "InputFiles/freshwatersubset_garcia_berthou_et_al_2004.xlsx")
}
