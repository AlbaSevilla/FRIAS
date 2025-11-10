arts_data_banken_Download_and_Depurate <- function(){
  #DOWNLOAD
  #in web https://lister.artsdatabanken.no/fremmedartslista/2023?TaxonRank=AssessedAtSameRank

  #DEPURATE:
  dat <- read_excel("InputFiles/originaldatabase_arts_data_banken.xlsx")
  names(dat)

  #NO DUPLICADOS:
  source(file.path("R","noduplicates.r"))
  dat_noduplicates <- noduplicates(dat, "Nombre_Cientifico")

  #Son invasoras en Noruega:
  dat_noduplicates$invaded_country <- "Norway"

  write.xlsx(dat_noduplicates, "InputFiles/originaldatabase_arts_data_banken.xlsx")
  #######################################################
  ############ habitat ##################################
  #######################################################
  source(file.path("R", "check_habitat.r"))
  dataset <- dat_noduplicates
  nombres <- dataset$Nombre_Cientifico
  nombres_aceptados <- name_backbone_checklist(nombres)$canonicalName
  dataset_actualizado <- check_habitat(nombres_aceptados, dataset)

  #Obtenemos freshwater
  dat_fresh <- dataset_actualizado %>% filter(grepl("FRESHWATER", Habitat))
  write.xlsx(dat_fresh, "InputFiles/freshwatersubset_arts_data_banken.xlsx")
}
