lista_nacional_de_especies_invasoras_de_portugal_2019_Download_and_Depurate <- function(){
  #Download
  url <- "https://dre.pt/application/conteudo/123025739"
  destfile <- "InputFiles/originaldatabase_lista_nacional_de_especies_invasoras_de_portugal_2019.pdf"
  download.file(url, destfile, mode = "wb")
  text_pages <- pdf_text(destfile)[12:15]
  text_content <- text_pages
  lines <- unlist(strsplit(text_content, "\n"))
  lines <- lines[lines != ""]
  dataset <- data.frame(line = lines, stringsAsFactors = FALSE)
  split_on_large_spaces <- function(dataset, col = "line", space_threshold = 5) {
    pattern <- paste0("\\s{", space_threshold, ",}")
    split_lines <- unlist(strsplit(dataset[[col]], pattern))
    result_dataset <- data.frame(line = trimws(split_lines), stringsAsFactors = FALSE)
    return(result_dataset)
  }
  dataset_split <- split_on_large_spaces(dataset)
  count_words <- function(x) {
    str_count(x, "\\S+")
  }
  species_list <- dataset_split[count_words(dataset_split$line) %in% 2:4, ]
  DATASET_Portugal <- as.data.frame(species_list)
  colnames(DATASET_Portugal)[1] <- "Species"
  write.xlsx(DATASET_Portugal, "./InputFiles/originaldatabase_lista_nacional_de_especies_invasoras_de_portugal_2019.xlsx")

  #Depurate
  dataset <- DATASET_Portugal
  species_names0 <- DATASET_Portugal$Species
  species_names <- name_backbone_checklist(species_names0)$canonicalName
  dataset_updated <- check_habitat(species_names, dataset)
  dataset_freshwater <- dataset_updated %>% filter(grepl("FRESHWATER", Habitat))
  dataset_freshwater$Invaded_Country <- "Portugal"

  #Save
  write.xlsx(dataset_freshwater, "./InputFiles/freshwatersubset_lista_nacional_de_especies_invasoras_de_portugal_2019.xlsx")
}
