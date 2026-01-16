xiong_et_al_2023_Download_and_Depurate <- function(){
  #Download
  url <- "https://aquaticinvasions.arphahub.com/article/103610/download/suppl/31/"
  destfile <- "InputFiles/originaldatabase_xiong_et_al_2023.docx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  doc <- read_docx("InputFiles/originaldatabase_xiong_et_al_2023.docx")
  text <- docx_summary(doc)$text
  species <- text[c(108:210)]
  TaxonomicGroup <- text[c(212:314)]
  Origin <- text[c(316:418)]
  FirstRecord <- text[c(524:626)]
  PotentialImpacts <- text[c(628:730)]
  Pathways <- text[c(420:522)]
  Dataset <- cbind(species, TaxonomicGroup, Origin, FirstRecord, PotentialImpacts, Pathways)
  Dataset <- as.data.frame(Dataset)
  dat_noduplicates <- noduplicates(Dataset, "species")
  dataset <- dat_noduplicates
  names <- dataset$species
  accepted_names <- name_backbone_checklist(names)$canonicalName
  habitat_dat <- check_habitat(accepted_names, dataset)
  freshwater_species <- habitat_dat %>% dplyr::filter(grepl("FRESHWATER", Habitat))
  freshwater_species$Invaded_country <- "China"

  #Save
  write.xlsx(freshwater_species, "InputFiles/freshwatersubset_xiong_et_al_2023.xlsx")
}
