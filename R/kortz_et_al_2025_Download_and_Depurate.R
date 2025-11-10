kortz_et_al_2025_Download_and_Depurate <- function(){

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://neobiota.pensoft.net/article/151156/download/suppl/31/"
  destfile <- "InputFiles/originaldatabase_kortz_et_al_2025.docx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")


  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  # Leer el documento .docx
  dat <- read_docx("Inputfiles/originaldatabase_kortz_et_al_2025.docx")
  content <- docx_summary(dat)
  paragraphs <- content %>%
    filter(content_type == "table cell") %>%
    select(text)

  dataset <- as.data.frame(paragraphs, stringsAsFactors = FALSE)
  colnames(dataset) <- "Especies"
  species_dataset <- dataset %>%
    filter(str_detect(Especies, "^[A-Z][a-z]+ [a-z]+$"))

  #Los que son freshwater para GBIF
  source(file.path("R", "check_habitat.r"))
  dataset <- species_dataset
  nombres <- dataset$Especies
  nombres_acep <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(nombres_acep, dataset)
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh2 <- dat_fresh %>% select(-Especies)

  write.xlsx(dat_fresh2, "Inputfiles/freshwatersubset_kortz_et_al_2025.xlsx")
}
