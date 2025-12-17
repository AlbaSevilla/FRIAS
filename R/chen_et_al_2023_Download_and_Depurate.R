chen_et_al_2023_Megafauna_Download_and_Depurate <- function(){
  #Depurate
  dataset <- read_excel("InputFiles/originaldatabase_chen_et_al_2023.xlsx",
                                                              col_names = TRUE)
  dataset <- as.data.frame(dataset)
  dataset_sinduplicados <- noduplicates(dataset, "Binomial name")
  dataset_sinduplicados$Habitat_Database <- "Freshwater"
  colnames(dataset_sinduplicados) <- gsub(" ", "_", colnames(dataset_sinduplicados))
  names <- dataset_sinduplicados$Binomial_name
  dataset_sinduplicados$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName
  #Save
  write.xlsx(dataset_sinduplicados, "InputFiles/freshwatersubset_chen_et_al_2023.xlsx")
}
