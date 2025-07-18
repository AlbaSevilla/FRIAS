NonNativeSpeciesinPoyangLakeBasin_Download_and_Depurate <- function(){
  ########### DESCARGAR ####################
  ##########################################
  #A mano:
  #url <- "https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2F1755-0998.12528&file=men12528-sup-0002-TableS1.xlsx"
  url <- "https://aquaticinvasions.arphahub.com/article/103610/download/suppl/31/"
  destfile <- "InputFiles/Step0_OriginalDatabase_NonNativeSpeciesinPoyangLakeBasin.docx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")
  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  # Leer el documento
  doc <- read_docx("Inputfiles/Step0_OriginalDatabase_NonNativeSpeciesinPoyangLakeBasin.docx")
  # Extraer todo el texto como vector
  texto <- docx_summary(doc)$text
  especies <- texto[c(108:210)]
  TaxonomicGroup <- texto[c(212:314)]
  Origin <- texto[c(316:418)]
  FirstRecord <- texto[c(524:626)]
  PotentialImpacts <- texto[c(628:730)]
  Pathways <- texto[c(420:522)]
  Dataset <- cbind(especies, TaxonomicGroup, Origin, FirstRecord, PotentialImpacts, Pathways)
  Dataset <- as.data.frame(Dataset)
  #No duplicados
  source(file.path("R", "noduplicates.r"))
  dat_noduplicates <- noduplicates(Dataset, "especies")

  #Habitat
  source(file.path("R", "check_habitat.r"))
  dataset <- dat_noduplicates
  nombres <- dataset$especies
  nom_acep <- name_backbone_checklist(nombres)$canonicalName
  habitat_dat <- check_habitat(nom_acep, dataset)

  #Freshwater
  freshwater_species <- habitat_dat %>% dplyr::filter(grepl("FRESHWATER", Habitat))
  freshwater_species$Invaded_country <- "China"

  write.xlsx(freshwater_species, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_NonNativeSpeciesinPoyangLakeBasin.xlsx")
  cat("Descargado Step0_OriginalDatabaseFreshwaterNODUPLICATES_NonNativeSpeciesinPoyangLakeBasin.xlsx")
}
