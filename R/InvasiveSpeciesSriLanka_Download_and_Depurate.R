InvasiveSpeciesSriLanka_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  url <- "https://zoosprint.org/index.php/zpj/article/view/6050/5467"
  destfile <- "InputFiles/Step0_OriginalDatabase_InvasiveSpeciesSriLanka.pdf"
  download.file(url, destfile, mode = "wb")

  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  text_pages <- pdf_text(destfile)[4:6]
  texto <- text_pages
  especies <- unlist(regmatches(texto, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+\\b", texto)))
  dataset <- as.data.frame(especies)
  colnames(dataset) <- "Especies"

  write.xlsx(dataset, "Inputfiles/Step0_OriginalDatabase_InvasiveSpeciesSriLanka.xlsx")

  #Habitat
  source(file.path("R", "check_habitat.r"))
  nombres <- dataset$Especies
  nom_acep <- name_backbone_checklist(nombres)$canonicalName
  habitat_dat <- check_habitat(nom_acep, dataset)

  #Freshwater
  freshwater_species <- habitat_dat %>% filter(grepl("FRESHWATER", Habitat))

  freshwater_species$invaded_country <- "Sri Lanka"

  write.xlsx(freshwater_species, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_InvasiveSpeciesSriLanka.xlsx")
}
