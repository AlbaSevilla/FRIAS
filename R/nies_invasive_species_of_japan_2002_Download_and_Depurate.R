nies_invasive_species_of_japan_2002_Download_and_Depurate <- function(){
  #Download and depurate
  base_url <- "https://www.nies.go.jp/biodiversity/invasive/IASDB/ni_searchResult_en.php"
  microHabitat <- c("freshwater", "stream", "limnetic", "swamp")
  urls <- paste0(base_url, "?PAGE=", 1:11,
                 paste0("&microHabitat%5B%5D=", microHabitat, collapse = ""))
  all_tables <- list()
  for (i in seq_along(urls)) {
    cat("Processing page", i, "\n")
    res <- httr::GET(urls[i])
    if (httr::status_code(res) != 200) {
      cat("Error on page", i, "\n")
      next
    }

    page <- tryCatch(read_html(res), error = function(e) NA)
    if (is.na(page)) {
      cat("Error reading HTML on page", i, "\n")
      next
    }

    tables <- tryCatch(html_table(page, fill = TRUE), error = function(e) list())
    if (length(tables) == 0) {
      cat("No table found on page", i, "\n")
      next
    }

    dataset <- tables[[1]]

    rows_html <- page %>%
      html_nodes("table") %>%
      .[[1]] %>%
      html_nodes("tr")

    links_per_row <- sapply(rows_html, function(tr) {
      a_href <- tr %>% html_nodes("a") %>% html_attr("href")
      if (length(a_href) == 0) return(NA_character_)
      paste(a_href, collapse = "; ")
    })

    links_per_row <- links_per_row[-1]

    if(length(links_per_row) < nrow(dataset)) {
      links_per_row <- c(links_per_row, rep(NA, nrow(dataset) - length(links_per_row)))
    } else if (length(links_per_row) > nrow(dataset)) {
      links_per_row <- links_per_row[1:nrow(dataset)]
    }

    dataset$links <- links_per_row
    all_tables[[i]] <- dataset
  }
  dataset_complete <- bind_rows(all_tables)
  colnames(dataset_complete) <- dataset_complete[1,]
  dataset_complete <- dataset_complete[-1,]
  dataset_complete$Habitat_Database <- "Freshwater"
  dataset_complete$Invaded_country <- "Japan"
  colnames(dataset_complete)[4] <- "Species_Links"
  process_link <- function(url) {
    tryCatch({
      res <- read_html(url)
      tables <- html_table(res, fill = TRUE)
      if (length(tables) < 3) return(NULL)
      dataset2 <- tables[[2]]
      dataset3 <- tables[[3]]
      dataset <- bind_rows(dataset2, dataset3)
      dataset <- as.data.frame(dataset)
      dataset <- t(dataset)
      dataset <- as.data.frame(dataset)
      colnames(dataset) <- dataset[1, ]
      dataset <- dataset[-1, , drop = FALSE]
      dataset <- dataset[, -1, drop = FALSE]
      rownames(dataset) <- NULL
      return(dataset)
    }, error = function(e) {
      cat("Error processing URL:", url, "\n")
      return(NULL)
    })
  }
  results <- list()
  for (i in seq_along(dataset_complete$Species_Links)) {
    cat("Processing:", i, "/", length(dataset_complete$Species_Links), "\n")
    results[[i]] <- process_link(dataset_complete$Species_Links[i])
  }
  dataset_results <- bind_rows(results)
  dataset_results <- as.data.frame(dataset_results)
  names(dataset_results) <- gsub(" ", "_", names(dataset_results))
  dataset_results <- dataset_results[, c("Scientific_name")]
  dataset_results <- data.frame(dataset_results)
  dataset_results$Scientific_name <- dataset_results$Scientific_name
  dataset_noduplicates <- noduplicates(dataset_results, "Scientific_name")
  dataset_noduplicates$Habitat_Database <- "Freshwater"
  dataset_noduplicates$Invaded_Country <- "Japan"
  dataset_noduplicates <- dataset_noduplicates[-1,]

  #Save
  write.xlsx(dataset_noduplicates, "Inputfiles/originaldatabase_nies_invasive_species_of_japan_2002.xlsx")
  write.xlsx(dataset_noduplicates, "Inputfiles/freshwatersubset_nies_invasive_species_of_japan_2002.xlsx")
}
