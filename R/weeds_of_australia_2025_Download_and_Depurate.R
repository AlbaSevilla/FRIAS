weeds_of_australia_2025_Download_and_Depurate <- function(){
  #Download
  obtain_valid_urls <- function(base_url = "https://weeds.org.au/weeds-profiles/page/", quiet = FALSE) {
    n <- 1
    urls <- c()
  repeat {
      url <- paste0(base_url, n, "/")
      response <- try(GET(url), silent = TRUE)
  if (inherits(response, "try-error") || status_code(response) != 200) {
        if (!quiet) cat("Error or page not accessible:", url, "\n")
        break
      }
      html <- read_html(response)
      no_dataset_msg <- html %>%
        html_nodes("h2.no-filter") %>%
        html_text(trim = TRUE)
      if (any(grepl("no matching filter results", no_dataset_msg, ignore.case = TRUE))) {
        if (!quiet) cat("Empty page:", url, "\n")
        break
      } else {
        if (!quiet) cat("Valid page:", url, "\n")
        urls <- c(urls, url)
        n <- n + 1
      }
    }
    return(urls)
  }
  extract_scientific_names <- function(urls, quiet = FALSE) {
    dataset <- data.frame(Page = integer(), Scientific_Name = character(), stringsAsFactors = FALSE)
    for (i in seq_along(urls)) {
      url <- urls[i]
      if (!quiet) cat("Extracting from:", url, "\n")
      response <- try(GET(url), silent = TRUE)
      if (inherits(response, "try-error") || status_code(response) != 200) next
      html <- read_html(response)
      names <- html %>%
        html_nodes("p") %>%
        html_text(trim = TRUE)
      filtered_names <- names[nchar(names) > 3 & grepl("^[A-Z][a-z]+ [a-z]+", names)]
      if (length(filtered_names) > 0) {
        dataset <- bind_rows(dataset,
                             data.frame(Page = i,
                                        Scientific_Name = filtered_names,
                                        stringsAsFactors = FALSE))
      }
    }
    return(dataset)
  }
  urls <- obtain_valid_urls()
  dataset <- extract_scientific_names(urls)
  write.xlsx(dataset, "Inputfiles/originaldatabase_weeds_of_australia_2025.xlsx")

  #Depurate
  dataset$Invaded_Country <- "Australia"
  species_names <- dataset$Scientific_Name
  accepted_names <- name_backbone_checklist(species_names)$canonicalName
  dataset <- check_habitat(accepted_names, dataset)
  dataset <- dataset %>% filter(grepl("FRESHWATER", Habitat))
  dataset <- dataset[, c("Species", setdiff(names(dataset), "Species"))]

  #Save
  write.xlsx(dataset, "Inputfiles/freshwatersubset_weeds_of_australia_2025.xlsx")
}
