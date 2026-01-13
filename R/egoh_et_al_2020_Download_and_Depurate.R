egoh_et_al_2020_Download_and_Depurate <- function(){
  #Download
  url <- "https://ars.els-cdn.com/content/image/1-s2.0-S0301479720301079-mmc1.docx"
  destfile <- "InputFiles/originaldatabase_egoh_et_al_2020.docx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dat <- read_docx("Inputfiles/originaldatabase_egoh_et_al_2020.docx")
  content <- docx_summary(dat)
  paragraphs <- content %>%
    filter(content_type == "table cell") %>%
    select(text)
  dataset <- as.data.frame(paragraphs, stringsAsFactors = FALSE)
  colnames(dataset) <- "Species"
  dataset <- dataset
  names <- dataset$Species
  names_acep <- name_backbone_checklist(names)$canonicalName
  dat_act <- check_habitat(names_acep, dataset)
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh$Invaded_country <- "South Africa"
  dat_fresh2 <- dat_fresh %>% select(-Species)

  #Save
  write.xlsx(dat_fresh2, "Inputfiles/freshwatersubset_egoh_et_al_2020.xlsx")
}
