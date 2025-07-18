InBUyUruguay_Download_and_Depurate <- function(){
  #Descargar
  #En la web https://sieei.udelar.edu.uy/especies

  #Depurar
  dat <- read_excel("Inputfiles/Step0_OriginalDatabase_InBUyUruguay.xlsx")
  names(dat)
  not_selected_columns <- c("author")
  dat <- dat %>% select(-author)

  #No duplicados
  source(file.path("R","noduplicates.r"))
  dat_noduplicates <- noduplicates(dat, "scientific_name")

  #Invaded_country
  invaded_country <- "Uruguay"
  dat_noduplicates$invaded_country <- invaded_country

  #Habitat
  source(file.path("R", "check_habitat.r"))
  dataset <- dat_noduplicates
  nombres <- dataset$scientific_name
  nombres_gbif <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(nombres_gbif, dataset)

  #Nos quedamos con los freshwater
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))


  write.xlsx(dat_fresh, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_InBUyUruguay.xlsx")
  cat("Archivo descargado : Step0_OriginalDatabaseFreshwaterNODUPLICATES_InBUyUruguay.xlsx")
}
