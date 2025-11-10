takhteev_et_al_2015_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  url <- "http://kmkjournals.com/upload/PDF/ArthropodaSelecta/24/24_3_335_370_Takhteev_et_al_for_Inet.pdf"
  destfile <- "InputFiles/originaldatabase_takhteev_et_al_2015.pdf"
  download.file(url, destfile, mode = "wb")


  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
   #Tenemos que extraer la Table 3
   text_pages <- pdf_text(destfile)[30]
   texto <- text_pages
   texto_completo <- paste(texto, collapse = "\n")
   lineas <- unlist(strsplit(texto_completo, "\n"))
   # lines es el vector que tienes con todas las líneas


   # Buscar líneas que tengan un patrón típico de nombre científico:
   # empiezan con mayúscula, luego minúscula, luego espacio, luego minúscula
   pattern <- "^[ ]*([A-Z][a-z]+)\\s([a-z]+)"

   # Extraer líneas que cumplen ese patrón
   species_lines <- grep(pattern, lineas, value = TRUE)

   # Extraer solo el nombre de especie (género y especie)
   species_names <- str_match(species_lines, pattern)[,2:3]

   # Unir en un vector de caracteres
   species_names <- apply(species_names, 1, paste, collapse = " ")

  #Hemos hecho el excel a mano
  Dataset <- as.data.frame(species_names)
  colnames(Dataset) <- "Species"
  Dataset <- as.data.frame(Dataset)
  Dataset

  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  #Obtain habitats
  # Obtener nombres científicos desde el campo "Especie"
  dataset <- Dataset
  especies_lista0 <- Dataset$Species
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)

  especies <- dataset_actualizado$AcceptedNameGBIF
  rango <- name_backbone_checklist(especies)$rank
  dataset_actualizado2 <- cbind(dataset_actualizado,rango)


  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado2 %>%
    filter(grepl("FRESHWATER", Habitat))

  Invaded_Country <- "Russia"
  dataset_freshwater$Invaded_Country <- Invaded_Country

  write.xlsx(dataset_freshwater, "./Inputfiles/freshwatersubset_takhteev_et_al_2015.xlsx")
}
