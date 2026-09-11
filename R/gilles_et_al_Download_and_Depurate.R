gilles_et_al_Download_and_Depurate <- function(){
  #Depurate
  file <- "InputFiles/originaldatabase_gilles_et_al.pdf"
  text_pages <- pdf_text(file)[5:6]
  text <- text_pages
  lines <- str_split(text, "\n")[[1]]
  lines_species <- lines %>%
    str_trim() %>%
    .[str_count(., "\\s{2,}") >= 7]
  data <- str_split(lines_species, "\\s{2,}")
  clean_row <- function(row) {
    if (length(row) > 8) {
      taxon <- row[1]
      common <- row[2]
      while (length(row) > 8) {
        taxon <- paste(taxon, row[2])
        row <- row[-2]
      }
      return(c(taxon, row[-1]))
    } else {
      return(row)
    }
  }
  table_clean <- lapply(data, clean_row)
  dataset <- do.call(rbind, table_clean) %>% as.data.frame()
  colnames(dataset) <- c("Taxon_name", "Common_name", "FishBase", "GISD", "CABI", "IESNA", "GScholar", "Outcome")
  dataset <- dataset[-1,]
  dataset <- dataset %>% filter(Outcome == "Invasive")
  dataset <- dataset
  names <- dataset$Taxon_name
  name_accepted <- name_backbone_checklist(names)$canonicalName
  dataset_habitat <- check_habitat(name_accepted, dataset)
  dataset_frw <- dataset_habitat %>%
    filter(grepl("FRESHWATER", Habitat))

  #Save
  write_xlsx(dataset_frw, "InputFiles/freshwatersubset_gilles_et_al.xlsx")
  write_xlsx(dataset_frw, "InputFiles/originaldatabase_gilles_et_al.xlsx")
}

