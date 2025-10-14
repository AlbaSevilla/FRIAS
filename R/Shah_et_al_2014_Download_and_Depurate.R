Shah_et_al_2014_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  # url <- "https://d1wqtxts1xzle7.cloudfront.net/33643039/Shah___Reshi_2014_published_tropical_ecol-libre.pdf?1399396684=&response-content-disposition=inline%3B+filename%3DCharacterization_of_alien_aquatic_flora.pdf&Expires=1748446666&Signature=fG1xouDgDHNVddwOYH6sqPN7mUotZRPk7u3E2pKkOtNq2x2zOrA8LBso-DyeNPBCQ10DkYIET7v3XmLI1v2NpkUx09ibQRNKO-Srd6FszYJ-dYq~kifSCGHQPsp2JSJAKucMA0fe8sxaybWJprCh5tM8r7cRE1UhDrQuUXy7JW79jWSTxa2BYiGtiKVSq8Escdmhm8Y7D56WW-yHxMlfxKksuFU3SzZAJtL9NVRhAwKnPDUpLAgDQD-fKfCWLQe0rBcqa8pRAXt5lX9xfWl6LqPAmq3BQ4YGjlFIWCZehP30TRmRSO9bPDvxDFS7Lm11-ggU~DuOryHo-2VvgDrJCQ__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA"
  # destfile <- "InputFiles/Step0_OriginalDatabase_Shah_et_al_2014.pdf"
  # download.file(url, destfile, mode = "wb")

  #No funciona el link ahora, a mano

  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  # text_pages <- pdf_text(destfile)[11:15]
  # texto <- text_pages
  # cat(texto)
  # especies <- str_extract_all(texto, "\\b[A-Z][a-z]+\\s[a-z\\-]+\\b") |> unlist()
  # dataset <- as.data.frame(especies)
  # colnames(dataset) <- "Especies"
  # write.xlsx(dataset, "Inputfiles/Step0_OriginalDatabase_Shah_et_al_2014.xlsx")


  dataset <- read_excel("Inputfiles/Step0_OriginalDatabase_Shah_et_al_2014.xlsx")
  #Habitat
  source(file.path("R", "check_habitat.r"))
  nombres <- dataset$Especies
  nom_acep <- name_backbone_checklist(nombres)$canonicalName
  habitat_dat <- check_habitat(nom_acep, dataset)

  #Freshwater
  freshwater_species <- habitat_dat %>% filter(grepl("FRESHWATER", Habitat))

  freshwater_species$invaded_country <- "India"

  write.xlsx(freshwater_species, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Shah_et_al_2014.xlsx")
}
