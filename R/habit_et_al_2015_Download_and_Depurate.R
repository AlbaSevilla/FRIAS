habit_et_al_2015_Download_and_Depurate <- function(){
  #Download
  url <- "https://www.revistaecosistemas.net/index.php/ecosistemas/article/download/1008/874/3237"
  destfile <- "InputFiles/originaldatabase_habit_et_al_2015.pdf"
  download.file(url, destfile, mode = "wb")
  text_pages <- pdf_text(destfile)[2]
  text <- text_pages
  lines <- strsplit(text, "\n")[[1]]
  start_line <- grep("ORDEN.*FAMILIA.*ESPECIE", lines)[1]
  table_lines <- lines[start_line:length(lines)]
  table_lines <- table_lines[nzchar(table_lines)]
  parse_line <- function(line) {
    c(
      order = trimws(substr(line, 1, 22)),
      family = trimws(substr(line, 23, 41)),
      species = trimws(substr(line, 42, 80)),
      native_range = trimws(substr(line, 81, 105)),
      presence = trimws(substr(line, 106, nchar(line)))
    )
  }
  raw_data <- t(sapply(table_lines[-1], parse_line))
  dataset <- as.data.frame(raw_data, stringsAsFactors = FALSE)
  for (col in c("order", "family")) {
    for (i in 2:nrow(dataset)) {
      if (dataset[i, col] == "") dataset[i, col] <- dataset[i - 1, col]
    }
  }
  selected_cols <- c("species", "native_range")
  dataset <- dataset[, selected_cols]
  rownames(dataset) <- NULL
  dataset$native_range <- sub(".*?\\)", "", dataset$native_range)
  dataset$species <- sub("\\(.*", "", dataset$species)
  write.xlsx(dataset, "InputFiles/originaldatabase_habit_et_al_2015.xlsx")

  #Depurate
  dataset <- dataset
  names_vec <- dataset$species
  accepted_names <- name_backbone_checklist(names_vec)$canonicalName
  habitat_dat <- check_habitat(accepted_names, dataset)
  freshwater_species <- habitat_dat %>% filter(grepl("FRESHWATER", Habitat))
  freshwater_species$Invaded_country <- "Chile"

  #Save
  write.xlsx(freshwater_species, "InputFiles/freshwatersubset_habit_et_al_2015.xlsx")
}
