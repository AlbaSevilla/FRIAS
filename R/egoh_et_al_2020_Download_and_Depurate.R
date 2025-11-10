egoh_et_al_2020_Download_and_Depurate <- function(){

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://ars.els-cdn.com/content/image/1-s2.0-S0301479720301079-mmc1.docx"
  destfile <- "InputFiles/originaldatabase_egoh_et_al_2020.docx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")


  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  # Leer el documento .docx
  dat <- read_docx("Inputfiles/originaldatabase_egoh_et_al_2020.docx")

  # Extraer contenido de celdas de tabla
  content <- docx_summary(dat)
  paragraphs <- content %>%
    filter(content_type == "table cell") %>%
    select(text)

  # Crear dataframe
  dataset <- as.data.frame(paragraphs, stringsAsFactors = FALSE)
  colnames(dataset) <- "Especies"
  #NO DUPLICADOS
  # source(file.path("R","noduplicates.r"))
  # egoh_et_al_2020_sinduplicados <- noduplicates(egoh_et_al_2020_freshwater, "scientificName")
  # nrow(egoh_et_al_2020_sinduplicados)
  #

  #NO TIENE DUBPLICADOS YA DE BASE


  #Los que son freshwater para GBIF
  source(file.path("R", "check_habitat.r"))
  dataset <- dataset
  nombres <- dataset$Especies
  nombres_acep <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(nombres_acep, dataset)
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh$Invaded_country <- "South Africa"
  dat_fresh2 <- dat_fresh %>% select(-Especies)


  write.xlsx(dat_fresh2, "Inputfiles/freshwatersubset_egoh_et_al_2020.xlsx")
}
