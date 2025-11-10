list_for_invasive_alien_species_management_in_japan_2015_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  url <- "https://www.env.go.jp/nature/intro/2outline/list/list.pdf"
  destfile <- "InputFiles/originaldatabase_list_for_invasive_alien_species_management_in_japan_2015.pdf"
  download.file(url, destfile, mode = "wb")


  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  text_pages <- pdf_text(destfile)
  texto <- text_pages
  texto_completo <- paste(texto, collapse = "\n")
  patron <- "\\b([A-Z][a-z]+\\s[a-z]+)\\b"
  especies <- str_extract_all(texto_completo, patron)[[1]]
  lista_especies <- unique(especies)
  lista_especies <- as.data.frame(lista_especies)

  DATASET_JAPON <- lista_especies

  write.xlsx(DATASET_JAPON, "./Inputfiles/originaldatabase_list_for_invasive_alien_species_management_in_japan_2015.xlsx")

  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  #Obtain habitats
  # Obtener nombres científicos desde el campo "Especie"
  dataset <- DATASET_JAPON
  especies_lista0 <- DATASET_JAPON$lista_especies
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)


  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))

  Invaded_Country <- "Japan"
  dataset_freshwater$Invaded_Country <- Invaded_Country

  write.xlsx(dataset_freshwater, "./Inputfiles/freshwatersubset_list_for_invasive_alien_species_management_in_japan_2015.xlsx")
}
