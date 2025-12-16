clarke_and_mcgeoch_2023_Download_and_Depurate <- function(){
  #Download
  url <- "https://zenodo.org/records/7508641/files/DavidAClarke/Impacts_invasive_insects-v2.0.zip?download=1"
  destfile <- "InputFiles/originaldatabase_clarkeandmcgeoch_2023.zip"
  download.file(url, destfile, mode = "wb")

  #unzip downloaded file
  unzip("InputFiles/originaldatabase_clarkeandmcgeoch_2023.zip", exdir = "InputFiles")
  original_path <- getwd()
  dataset2 <- read.xlsx("InputFiles/DavidAClarke-Impacts_invasive_insects-f3b0791/Data/Assessment_information.xlsx", sheet="Assessment_results_input")
  dataset3 <- read.xlsx("InputFiles/DavidAClarke-Impacts_invasive_insects-f3b0791/Data/Assessment_information.xlsx", sheet="Impact_distribution")
  mergedataset <- merge(dataset2, dataset3, by.x="scientificName", by.y="scientificName")
  mergedataset <- mergedataset %>%
    mutate(Country = paste(Country.x, Country.y, sep=" ;")) %>%
    select(-Country.x, -Country.y) %>%
    mutate(ISO3codes = paste(ISO3, Alpha_3, sep=" ;")) %>%
    select(-ISO3, -Alpha_3)
  mergedataset <- mergedataset %>%
    mutate(EICAT_by_Mechanism = paste(Severity, Mechanism, sep="-"))
  merge_noduplicates <- noduplicates(mergedataset, "scientificName")
  write.xlsx(merge_noduplicates , "InputFiles/originaldatabase_clarke_and_mcgeoch_2023.xlsx")
  dataset <- read_excel("InputFiles/originaldatabase_clarke_and_mcgeoch_2023.xlsx")
  species_list0 <- dataset$scientificName
  species_list <- name_backbone_checklist(species_list0)$canonicalName
  dataset_updated <- check_habitat(species_list, dataset)
  dataset_freshwater <- dataset_updated %>%
    filter(grepl("FRESHWATER", Habitat))

  #Save
  write.xlsx(dataset_freshwater, "./InputFiles/freshwatersubset_clarke_and_mcgeoch_2023.xlsx")
  write.csv(dataset_freshwater, "./InputFiles/freshwatersubset_clarke_and_mcgeoch_2023.csv")
}
