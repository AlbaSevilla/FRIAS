petrosyan_et_al_2023_Download_and_Depurate <- function() {
  #Download
  url <- "https://neobiota.pensoft.net/article/96282/download/suppl/33/"
  destfile <- "InputFiles/originaldatabase_petrosyan_et_al_2023.xlsx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dataset <- read.xlsx(destfile)
  dataset <- dataset[, -1]
  colnames(dataset) <- c("Species_name", "Native_Range", "Introduction_year",
                         "Species_occurrence_records", "Invasive_range", "Year_of_creation")
  dataset <- dataset[-c(1:2), ]  # remove header rows
  dataset_unique <- noduplicates(dataset, column_name_species = "Species_name")
  species_list0 <- dataset_unique$Species_name
  species_list <- name_backbone_checklist(species_list0)$canonicalName
  dataset_updated <- check_habitat(species_list, dataset_unique)
  dataset_freshwater <- dataset_updated %>%
    filter(grepl("FRESHWATER", Habitat))
  dataset <- dataset_freshwater[, c(1, 2, 8, 9)]
  names(dataset)[1] <- "Species_Name"
  names(dataset)[2] <- "Native_Range"
  dataset$Invaded_region <- "Northern Eurasia"
  dataset <- dataset %>%
    mutate(
      Native_Range = str_extract_all(Native_Range, "\\b[A-Z][a-z]+\\b"),
      Native_Range = lapply(Native_Range, function(x) unique(x)),
      Native_Range = sapply(Native_Range, function(x) paste(x, collapse = ", "))
    )
  dataset$Native_Range <- sapply(dataset$Native_Range, saveLocations)

  #Save
  write.xlsx(dataset, "./InputFiles/freshwatersubset_petrosyan_et_al_2023.xlsx")
}
