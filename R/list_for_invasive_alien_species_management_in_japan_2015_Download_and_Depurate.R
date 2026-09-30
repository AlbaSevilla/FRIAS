list_for_invasive_alien_species_management_in_japan_2015_Download_and_Depurate <- function(){
  #Download
  url <- "https://www.env.go.jp/nature/intro/2outline/list/list.pdf"
  destfile <- "InputFiles/originaldatabase_list_for_invasive_alien_species_management_in_japan_2015.pdf"
  download.file(url, destfile, mode = "wb")

  #Depurate
  text_pages <- pdf_text(destfile)
  full_text <- paste(text_pages, collapse = "\n")
  pattern <- "\\b([A-Z][a-z]+\\s[a-z]+)\\b"
  species <- str_extract_all(full_text, pattern)[[1]]
  species_list <- unique(species)
  dataset <- as.data.frame(species_list)
  write.xlsx(dataset, "./InputFiles/originaldatabase_list_for_invasive_alien_species_management_in_japan_2015.xlsx")
  species_names <- dataset$species_list
  accepted_names <- name_backbone_checklist(species_names)$canonicalName
  dataset_updated <- check_habitat(accepted_names, dataset)
  dataset_fresh <- dataset_updated %>% filter(grepl("FRESHWATER", Habitat))
  dataset_fresh$Invaded_Country <- "Japan"

  #Save
  write.xlsx(dataset_fresh, "./InputFiles/freshwatersubset_list_for_invasive_alien_species_management_in_japan_2015.xlsx")
}
