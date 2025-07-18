InvasiveSpeciesInPortugal_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  url <- "https://dre.pt/application/conteudo/123025739"
  destfile <- "InputFiles/Step0_OriginalDatabase_InvasiveSpeciesInPortugal.pdf"
  download.file(url, destfile, mode = "wb")


  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  text_pages <- pdf_text(destfile)[12:15]
  texto <- text_pages
  library(stringr)


  #las lineas son confusas, contienen información de ambas columnas, las separamos:
  lineas <- unlist(strsplit(texto, "\n"))
  lineas <- lineas[lineas != ""]
  df <- data.frame(line = lineas, stringsAsFactors = FALSE)
  split_on_large_spaces <- function(df, col = "line", space_threshold = 5) {
    pattern <- paste0("\\s{", space_threshold, ",}")
    split_lines <- unlist(strsplit(df[[col]], pattern))
    result_df <- data.frame(line = trimws(split_lines), stringsAsFactors = FALSE)
    return(result_df)
  }
  df_split <- split_on_large_spaces(df)

  #AHORA NOS VAMOS A QUEDAR CON LAS FILAS DE 2,3, Y 4 PALABRAS, QUE SON LAS ESPECIES MAYORITARIAMENTE
  # Función para contar palabras
  count_words <- function(x) {
    str_count(x, "\\S+")
  }
  lista_especies <- df_split[count_words(df_split$line) %in% 2:4, ]


  DATASET_Portugal <- as.data.frame(lista_especies)
  colnames(DATASET_Portugal)[1] <- "Species"

  write.xlsx(DATASET_Portugal, "./Inputfiles/Step0_OriginalDatabase_InvasiveSpeciesInPortugal.xlsx")

  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  #Obtain habitats
  # Obtener nombres científicos desde el campo "Especie"
  dataset <- DATASET_Portugal
  especies_lista0 <- DATASET_Portugal$Species
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)


  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado %>%  filter(grepl("FRESHWATER", Habitat))

  Invaded_Country <- "Portugal"
  dataset_freshwater$Invaded_Country <- Invaded_Country

  write.xlsx(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_InvasiveSpeciesInPortugal.xlsx")
  cat("Archivo descargado correctamente: /Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_InvasiveSpeciesInPortugal.xlsx")

}
