hui_et_al_2020_Download_and_Depurate <- function(){
  #Depurate
  file_path <- "Inputfiles/originaldatabase_hui_et_al_2020.pdf"
  pdf_pages <- pdf_text(file_path)[3:5]
  table_text <- pdf_pages
  lines <- unlist(strsplit(table_text, "\n"))
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  lines <- lines[!grepl("Tan et al.|Table 1a|Order and Family|Region of Origin|Remarks", lines, ignore.case = TRUE)]
  current_order <- NA
  current_family <- NA
  dataset_list <- list()
  for (i in seq_along(lines)) {
    line <- lines[i]

    if (grepl("^ORDER ", line)) {
      current_order <- gsub("^ORDER\\s+", "", line)
      next
    }

    if (grepl("^Family ", line)) {
      current_family <- gsub("^Family\\s+", "", line)
      next
    }

    if (grepl("^\\s", line)) {
      line <- paste(current_family, trimws(line))
    }

    cols <- unlist(strsplit(line, " {2,}"))

    if (length(cols) >= 2 && grepl("^[A-Z][a-z]+idae$", cols[1])) {
      family <- cols[1]
      species <- cols[2]
      origin <- ifelse(length(cols) > 2, cols[3], NA)
      remarks <- ifelse(length(cols) > 3, cols[4], NA)
    } else if (length(cols) >= 1) {
      family <- current_family
      species <- cols[1]
      origin <- ifelse(length(cols) > 1, cols[2], NA)
      remarks <- ifelse(length(cols) > 2, cols[3], NA)
    } else {
      next
    }

    dataset_list[[length(dataset_list) + 1]] <- data.frame(
      Order = current_order,
      Family = family,
      Species = species,
      Region_of_Origin = origin,
      Remarks = remarks,
      stringsAsFactors = FALSE
    )
  }

  dataset <- do.call(rbind, dataset_list)
  rownames(dataset) <- NULL
  dataset <- dataset[, c(3:5)]
  write.xlsx(dataset, "Inputfiles/originaldatabase_hui_et_al_2020.xlsx", rowNames = FALSE)
  dataset <- dataset %>% filter(Remarks == "established")
  dataset$Habitat <- "Freshwater"
  dataset$Invaded_Country <- "Singapore"

  #Save
  write.xlsx(dataset, "Inputfiles/freshwatersubset_hui_et_al_2020.xlsx")
}
