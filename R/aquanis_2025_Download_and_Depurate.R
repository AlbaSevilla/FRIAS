aquanis_2025_Download_and_Depurate <- function(){
  #Download
  url <- "https://aquanisresearch.com/index.php/aquanis/species/open/page/ALL"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[3]]
  res3 <- as.data.frame(res3)
  rownames(res3) <- NULL
  dataset <- res3
  species <- dataset$Species
  accepted_names <- name_backbone_checklist(species)$canonicalName
  dataset_habitat <- check_habitat(accepted_names, dataset)
  dataset_habitat2 <- noduplicates(dataset_habitat, "AcceptedNameGBIF")
  write.xlsx (dataset_habitat, "./InputFiles/originaldatabase_aquanis_2025.xlsx")


  #Depurate
  dataset_HAB <- read_excel("InputFiles/originaldatabase_aquanis_2025.xlsx")
  freshwater_dataset <- dataset_HAB %>% filter(grepl("FRESHWATER", Habitat))
  freshwater_species_names <- freshwater_dataset$AcceptedNameGBIF
  nodes <- res %>% html_nodes("a")
  names <- nodes %>% html_text(trim = TRUE)
  links <- nodes %>% html_attr("href")
  link_table <- tibble(
    Species = str_squish(names),
    Link = links
  ) %>%
    filter(str_detect(Link, "/species/view/id/"))
  filtered_table <- link_table %>%
    filter(Species %in% freshwater_dataset$AcceptedNameGBIF)
  extract_information <- function(url, especie, idx, total) {
    message(sprintf("[%d/%d] %s", idx, total, species))
    valid_lme <- c(
      "East Bering Sea", "Gulf of Alaska", "California Current", "Gulf of California",
      "Gulf of Mexico", "Southeast U.S. Continental Shelf", "Northeast U.S. Continental Shelf",
      "Scotian Shelf", "Newfoundland-Labrador Shelf", "Insular Pacific?Hawaiian",
      "Pacific Central-American Coastal", "Caribbean Sea", "Humboldt Current",
      "Patagonian Shelf", "South Brazil Shelf", "East Brazil Shelf", "North Brazil Shelf",
      "West Greenland Shelf", "East Greenland Shelf", "Barents Sea", "Norwegian Sea",
      "North Sea", "Baltic Sea", "Celtic?Biscay Shelf", "Iberian Coastal", "Mediterranean Sea",
      "Canary Current", "Guinea Current", "Benguela Current", "Agulhas Current",
      "Somali Coastal Current", "Arabian Sea", "Red Sea", "Bay of Bengal", "Gulf of Thailand",
      "South China Sea", "Sulu-Celebes Sea", "Indonesian Sea", "North Australian Shelf",
      "Northeast Australian Shelf (Great Barrier Reef)", "East-Central Australian Shelf",
      "Southeast Australian Shelf", "Southwest Australian Shelf", "West-Central Australian Shelf",
      "Northwest Australian Shelf", "New Zealand Shelf", "East China Sea", "Yellow Sea",
      "Kuroshio Current", "Sea of Japan", "Oyashio Current", "Sea of Okhotsk",
      "West Bering Sea", "Chukchi Sea", "Beaufort Sea", "East Siberian Sea", "Laptev Sea",
      "Kara Sea", "Iceland Shelf and Sea", "Faroe Plateau", "Antarctica", "Black Sea",
      "Hudson Bay", "Arctic Ocean", "Greenland Sea", "Canadian High Arctic and North Greenland",
      "Aleutian Islands"
    )

    tryCatch({
      res <- read_html(url)
      res2 <- res %>% html_table(fill = TRUE)
      dataset <- suppressWarnings(bind_rows(res2[[2]], res2[[3]]))
      if (is.null(dataset)) return(tibble(Species = species))
      dataset <- dataset[, -3, drop = FALSE]
      dataset <- as.data.frame(t(dataset))
      columnas <- as.character(unlist(dataset[1, ]))
      columnas <- columnas[!is.na(columnas) & columnas != ""]
      n_col <- ncol(dataset)
      n_names <- length(columnas)
      if (n_names < n_col) {
        columnas <- c(columnas, paste0("V", seq_len(n_col - n_names)))
      } else if (n_names > n_col) {
        columnas <- columnas[seq_len(n_col)]
      }
      colnames(dataset) <- columnas
      dataset <- dataset[-1, ]
      selected_columns <- c(
        "Species", "Family", "Order", "Class", "Phylum",
        "Native origin (?)", "Characteristic feeding method / Life stage (?)"
      )
      missing_cols <- setdiff(selected_columns, colnames(dataset))
      if (length(missing_cols) > 0) dataset[missing_cols] <- NA
      dataset_sel <- dataset[, selected_columns, drop = FALSE]
      clean_native_origin <- function(text) {
        if (is.na(text)) return(NA)
        text <- sub("References:.*", "", text)
        text <- str_replace_all(text, "Not entered", "")
        ocean <- str_match(text, "Ocean:\\s*(\\w+)")[,2]
        country <- str_match(text, "Country:\\s*(\\w+)")[,2]
        founded_lmes <- valid_lme[str_detect(text, fixed(valid_lme, ignore_case = TRUE))]
        results <- c(ocean, country, founded_lmes)
        results <- results[!is.na(results) & results != ""]
        paste(unique(results), collapse = "; ")
      }
      dataset_sel$`Native origin (?)` <- sapply(dataset_sel$`Native origin (?)`, clean_native_origin)
      dataset_sel$Species <- sapply(strsplit(dataset_sel$Species, " "), function(x) paste(x[1:2], collapse = " "))
      names(dataset_sel)[names(dataset_sel) == "Native origin (?)"] <- "Native_Range"
      keywords <- c(
        "Characteristic feeding method", "Life stage", "Adult", "Juvenile", "Larvae", "Eggs",
        "Resting stage", "Photoautotroph", "Mixotroph", "Suspension feeder – Active",
        "Suspension feeder – Passive", "Deposit feeder – Surface",
        "Deposit feeder – Sub-surface", "Omnivore", "Herbivore", "Scavenger",
        "Symbiont contribution", "Planktotroph", "Chemoautotroph", "Predator", "Grazer"
      )
      content <- function(tbl) {
        text <- paste(unlist(tbl), collapse = " ")
        all(str_detect(text, regex(keywords, ignore_case = TRUE)))
      }
      objective_table <- keep(res2, content)
      if (length(objective_table) > 0) {
        final_table <- objective_table[[1]] %>%
          mutate(across(everything(), ~trimws(.))) %>%
          filter(X1 != "" & !is.na(X1))
        feeding_types <- final_table$X1[rowSums(final_table == "X", na.rm = TRUE) > 0]
        dataset_sel$FeedingType <- paste(feeding_types, collapse = "; ")
      } else {
        dataset_sel$FeedingType <- NA
      }
      return(dataset_sel)

    }, error = function(e) {
      message(sprintf("ERROR %s: %s", species, e$message))
      return(tibble(Species = species))
    })
  }
  total_species <- nrow(filtered_table)
  list_results <- pmap(
    list(
      url = filtered_table$Link,
      species = filtered_table$Species,
      idx = seq_len(total_species),
      total = total_species
    ),
    extract_information
  )
  dataset_results <- bind_rows(list_results)
  dataset_results <- dataset_results %>% filter(!(Species == "NA NA"))
  pattern <- paste(c("References", "Country", "Comments"), collapse = "|")
  dataset_results <- as.data.frame(lapply(dataset_results, \(x) gsub(pattern, "", x)))
  dataset <- dataset_results
  species <- dataset$Species
  names_aceptados <- name_backbone_checklist(species)$canonicalName
  dataset_final <- check_habitat(species, dataset)
  dataset_final$Characteristic.feeding.method...Life.stage.... <- NULL
  dataset_final$FeedingType <- gsub(
    "Characteristic feeding method / Life stage (?); ",
    "",
    dataset_final$FeedingType,
    fixed = TRUE
  )
  dataset_final$FeedingType <- trimws(dataset_final$FeedingType)
  dataset_final$FeedingType[dataset_final$FeedingType == ""] <- NA
  aquanis_final <- dataset_final
  names_species <- aquanis_final$Species
  res <- read_html("https://aquanisresearch.com/index.php/aquanis/introductions/open/page/ALL")
  res2 <- res %>% html_table
  res2 <- as.data.frame(res2)
  res3 <- res2 %>%
    select(Species, Recipient.region, Date.of.the.first.record)
  res3$Recipient.region <- sub(" /.*$", "", res3$Recipient.region)
  expand_year_range <- function(x) {
    x <- trimws(x)
    if (is.na(x) || x == "") return(NA_character_)
    if (grepl("-", x)) {
      parts <- strsplit(x, "-")[[1]]
      start <- as.integer(parts[1])
      end <- as.integer(parts[2])
      if (is.na(start) || is.na(end)) return(NA_character_)
      return(paste(seq(start, end), collapse = "; "))
    } else {
      return(x)
    }
  }
  res3$Date.of.the.first.record <- sapply(res3$Date.of.the.first.record, expand_year_range)
  res3$Date.of.the.first.record <- sub("To ", "", res3$Date.of.the.first.record)
  res3$Date.of.the.first.record <- sub("From ", "", res3$Date.of.the.first.record)
  source(file.path("R", "noduplicates.r"))
  res4 <- noduplicates(res3, "Species")
  merged <- merge(aquanis_final, res4, by="Species")

  #Save
  write.xlsx (merged, "./InputFiles/freshwatersubset_aquanis_2025.xlsx")
}
