mikkola_et_al_2012_Download_and_Depurate <- function(){
  #Download
  url <- "https://www.intechopen.com/chapters/38666"
  res <- read_html(url)
  res2 <- res %>% html_table()
  res3 <- res2[[1]]
  dataset <- as.data.frame(res3)
  colnames(dataset) <- as.character(unlist(dataset[1, ]))
  dataset <- dataset[-1, ]
  dataset$Invaded_country <- "Kyrgyzstan"
  names(dataset) <- gsub(" ", "_", names(dataset))
  write.xlsx(dataset, "InputFiles/originaldatabase_mikkola_et_al_2012.xlsx")

  #Depurate
  species_names <- dataset$Scientific_name
  acep_names <- name_backbone_checklist(species_names)$canonicalName
  dataset_updated <- check_habitat(acep_names, dataset)
  dataset_fresh <- dataset_updated %>% filter(grepl("FRESHWATER", Habitat))
  dataset_fresh2 <- dataset_fresh %>% filter(Introduced == "+")
  names(dataset_fresh2) <- gsub(" ", "_", names(dataset_fresh2))

  #Save
  write.xlsx(dataset_fresh2, "InputFiles/freshwatersubset_mikkola_et_al_2012.xlsx")
}
