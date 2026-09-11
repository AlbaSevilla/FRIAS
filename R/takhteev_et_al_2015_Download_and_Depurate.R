takhteev_et_al_2015_Download_and_Depurate <- function(){
  #Download
  url <- "http://kmkjournals.com/upload/PDF/ArthropodaSelecta/24/24_3_335_370_Takhteev_et_al_for_Inet.pdf"
  destfile <- "InputFiles/originaldatabase_takhteev_et_al_2015.pdf"
  download.file(url, destfile, mode = "wb")

  #Depurate
  text_pages <- pdf_text(destfile)[30]
  text <- text_pages
  completed_text <- paste(text, collapse = "\n")
  lines <- unlist(strsplit(completed_text, "\n"))
  pattern <- "^[ ]*([A-Z][a-z]+)\\s([a-z]+)"
  species_lines <- grep(pattern, lines, value = TRUE)
  species_names <- str_match(species_lines, pattern)[,2:3]
  species_names <- apply(species_names, 1, paste, collapse = " ")
  Dataset <- as.data.frame(species_names)
  colnames(Dataset) <- "Species"
  Dataset <- as.data.frame(Dataset)
  dataset <- Dataset
  species_list0 <- Dataset$Species
  species_list <- name_backbone_checklist(species_list0)$canonicalName
  dataset_updated <- check_habitat(species_list, dataset)
  especies <- dataset_updated$AcceptedNameGBIF
  range <- name_backbone_checklist(especies)$rank
  dataset_updated2 <- cbind(dataset_updated,range)
  dataset_freshwater <- dataset_updated2 %>%
    filter(grepl("FRESHWATER", Habitat))
  dataset_freshwater$Invaded_Country <- "Russia"

  write.xlsx(dataset_freshwater, "./InputFiles/freshwatersubset_takhteev_et_al_2015.xlsx")
}
