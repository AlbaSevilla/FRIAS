picker_and_griffiths_2017_Download_and_Depurate <- function() {
  #Download
  file <- "InputFiles/originaldatabase_picker_and_griffiths_2017.pdf"
  selected_text <- pdf_text(file)[9:19]
  table_text <- selected_text
  text <- unlist(strsplit(table_text, "\n"))
  text <- trimws(text)
  text <- text[text != ""]
  text <- text[!grepl("Page|Original Research|APPENDIX|Alien animals|Group +Species|Common name|Origin|Habitat|Date of introduction|first detection", text, ignore.case = TRUE)]
  current_group <- NA
  current_family <- NA
  dataset_list <- list()
  for (l in text) {
    if (grepl("^[A-Z][a-z]+$", l)) {
      current_group <- l
      next
    }
    if (grepl("^\\s*[A-Z][a-z]+idae", l)) {
      current_family <- trimws(l)
      next
    }
    if (grepl("^\\s{2,}", l)) {
      l <- paste(current_family, trimws(l))
    }
    words <- unlist(strsplit(l, "\\s+"))
    if (length(words) < 5) next
    habitat <- words[length(words) - 1]
    date_intro <- words[length(words)]
    fam <- words[1]
    if (length(words) >= 4) {
      if (grepl("^[A-Z]", words[3])) {
        scientific_name <- paste(words[2], words[3], sep = " ")
        start_common <- 4
      } else {
        scientific_name <- words[2]
        start_common <- 3
      }
    } else {
      scientific_name <- words[2]
      start_common <- 3
    }
    if (length(words) > start_common + 1) {
      common_origin <- paste(words[start_common:(length(words)-2)], collapse = " ")
    } else {
      common_origin <- NA
    }
    dataset_list[[length(dataset_list) + 1]] <- data.frame(
      Group = current_group,
      Family = fam,
      Scientific_name = scientific_name,
      Common_and_Origin = common_origin,
      Habitat = habitat,
      Date_of_introduction = date_intro,
      stringsAsFactors = FALSE
    )
  }
  dataset <- do.call(rbind, dataset_list)
  rownames(dataset) <- NULL
  dataset <- dataset %>% select(Scientific_name, everything())
  rownames(dataset) <- NULL
  dataset$Common_and_Origin <- sapply(dataset$Common_and_Origin, function(x) {
    words <- unlist(strsplit(x, "\\s+"))
    capitals_only <- words[grepl("^[A-Z]", words)]
    paste(capitals_only, collapse = " ")
  })
  write.xlsx(dataset, "InputFiles/originaldatabase_picker_and_griffiths_2017.xlsx", rowNames = FALSE)

  #Depurate
  dataset_fresh <- dataset %>% filter(Habitat == "F")
  dataset_fresh$Habitat_Database <- "Freshwater"
  dataset_fresh$Invaded_country <- "South Africa"
  final_table <- as.data.frame(dataset_fresh)
  names_list <- final_table$Scientific_name
  final_table$AcceptedNameGBIF <- name_backbone_checklist(names_list)$canonicalName
  final_table <- final_table %>%
    mutate(Date_of_introduction = gsub("[^0-9]", "", Date_of_introduction),
           Habitat = gsub("F", "FRESHWATER", Habitat))

  #Save
  write.xlsx(final_table, "InputFiles/freshwatersubset_picker_and_griffiths_2017.xlsx")
}
