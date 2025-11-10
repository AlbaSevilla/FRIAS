bambaradeniya_et_al_2002_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  url <- "https://zoosprint.org/index.php/zpj/article/view/6050/5467"
  destfile <- "InputFiles/originaldatabase_bambaradeniya_et_al_2002.pdf"
  download.file(url, destfile, mode = "wb")

  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  text_pages <- pdf_text(destfile)[4:6]
  texto <- text_pages
  especies <- unlist(regmatches(texto, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+\\b", texto)))
  dataset <- as.data.frame(especies)
  colnames(dataset) <- "Especies"

  write.xlsx(dataset, "Inputfiles/originaldatabase_bambaradeniya_et_al_2002.xlsx")

  #Habitat
  source(file.path("R", "check_habitat.r"))
  nombres <- dataset$Especies
  nom_acep <- name_backbone_checklist(nombres)$canonicalName
  habitat_dat <- check_habitat(nom_acep, dataset)

  #Freshwater
  freshwater_species <- habitat_dat %>% filter(grepl("FRESHWATER", Habitat))
  freshwater_species$invaded_country <- "Sri Lanka"

  write.xlsx(freshwater_species, "Inputfiles/freshwatersubset_bambaradeniya_et_al_2002.xlsx")
}
