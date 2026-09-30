oldoni_et_al_2024_Download_and_Depurate <- function(){
  #Download
  url <- "https://zenodo.org/records/10897501/files/ranking_emerging_status_hierarchical_strategy_Wallonia.tsv?download=1"
  destfile <- "InputFiles/ranking_emerging_status_points_strategy_Wallonia.tsv"
  download.file(url, destfile, mode = "wb")
  url <- "https://zenodo.org/records/10897501/files/ranking_emerging_status_hierarchical_strategy_Flanders.tsv?download=1"
  destfile <- "InputFiles/ranking_emerging_status_points_strategy_Flanders.tsv"
  download.file(url, destfile, mode = "wb")
  url <- "https://zenodo.org/records/10897501/files/ranking_emerging_status_hierarchical_strategy_Brussels.tsv?download=1"
  destfile <- "InputFiles/ranking_emerging_status_points_strategy_Brussels.tsv"
  download.file(url, destfile, mode = "wb")
  url <- "https://zenodo.org/records/10897501/files/ranking_emerging_status_hierarchical_strategy_Belgium.tsv?download=1"
  destfile <- "InputFiles/ranking_emerging_status_points_strategy_Belgium.tsv"
  download.file(url, destfile, mode = "wb")
  dataset_wallonia <- read.csv("InputFiles/ranking_emerging_status_points_strategy_Wallonia.tsv", fill=TRUE, header=TRUE, sep="\t")
  dataset_wallonia$region <- "Wallonia"
  dataset_wallonia <- dataset_wallonia %>% select("canonicalName", "kingdom", "region")
  dataset_flanders <- read.csv("InputFiles/ranking_emerging_status_points_strategy_Flanders.tsv", fill=TRUE, header=TRUE, sep="\t")
  dataset_flanders$region <- "Flanders"
  dataset_flanders <- dataset_flanders %>% select("canonicalName", "kingdom", "region")
  dataset_brussels <- read.csv("InputFiles/ranking_emerging_status_points_strategy_Brussels.tsv", fill=TRUE, header=TRUE, sep="\t")
  dataset_brussels$region <- "Brussels"
  dataset_brussels <- dataset_brussels %>% select("canonicalName", "kingdom", "region")
  dataset_belgium <- read.csv("InputFiles/ranking_emerging_status_points_strategy_Belgium.tsv", fill=TRUE, header=TRUE, sep="\t")
  dataset_belgium$region <- "Belgium"
  dataset_belgium <- dataset_belgium %>% select("canonicalName", "kingdom", "region")
  dataset_final <- rbind(dataset_wallonia, dataset_flanders, dataset_brussels, dataset_belgium)
  dataset_unique <- dataset_final %>%
    dplyr::group_by(canonicalName) %>%
    dplyr::summarise(across(everything(), ~ {
      text <- as.character(.)
      vals <- unlist(str_split(text, "[,;|]+|\\s{2,}"))
      vals <- str_trim(vals)
      vals <- vals[vals != ""]
      vals <- unique(vals)
      paste(vals, collapse = ", ")
    }), .groups = "drop")
  write.xlsx(dataset_unique, "./InputFiles/originaldatabase_oldoni_et_al_2024.xlsx")

  #Depurate
  dataset <- dataset_unique
  species_list0 <- dataset$canonicalName
  species_list <- name_backbone_checklist(species_list0)$canonicalName
  dataset_updated <- check_habitat(species_list, dataset)
  dataset_freshwater <- dataset_updated %>%
    filter(grepl("FRESHWATER", Habitat)) %>%
    select(-"Species")

  #Save
  write.xlsx(dataset_freshwater, "./InputFiles/freshwatersubset_oldoni_et_al_2024.xlsx")
}
