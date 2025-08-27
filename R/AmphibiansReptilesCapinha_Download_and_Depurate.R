AmphibiansReptilesCapinha_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  # Definir la URL y el archivo de destino
  # archivo descargado en la url: https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fddi.12617&file=ddi12617-sup-0001-AppendixS1.xlsx

  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  ruta_InputFiles <- file.path("InputFiles")
  data_name_reptiles0 <- list.files(path = ruta_InputFiles, pattern = "Step0_OriginalDatabasewithHabitat_AmphibiansReptilesCapinha_Cesar.xlsx", full.names=TRUE)
  dat0 <- read.xlsx(data_name_reptiles0) # Aquí estamos indicando que sustraiga los excel de las bases de datos iniciales de la carpeta 'InputFiles'.

  especies_seleccion <- dat0 %>%
    filter(`1_Cesar` == "1")
  nombres_especies_freshwater <- especies_seleccion$Species

  data_name_reptiles <- list.files(path = ruta_InputFiles, pattern = "Step0_OriginalDatabase_AmphibiansReptilesCapinha.xlsx", full.names=TRUE)
  dat <- read.xlsx(data_name_reptiles, sheet=2) # Aquí estamos indicando que sustraiga los excel de las bases de datos iniciales de la carpeta 'InputFiles'.
  dat <- dat[dat$Species %in% nombres_especies_freshwater, ]


  #NO DUPLICADOS
  amfibios_sin_duplicados <- dat %>%
    dplyr::group_by(Species) %>%
    dplyr::summarise(across(everything(), ~ {
      texto <- as.character(.)
      vals <- unlist(str_split(texto, "[,;|]+|\\s{2,}"))
      vals <- str_trim(vals)
      vals <- vals[vals != ""]
      vals <- unique(vals)
      paste(vals, collapse = ", ")
    }), .groups = "drop")

  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  # source(file.path("R", "check_habitat.r"))
  #
  # #Obtain habitats
  # dataset <- amfibios_sin_duplicados
  # especies_lista0 <- dataset$Species
  # especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  # dataset_actualizado <- check_habitat(especies_lista, dataset)
  #
  # ##############################################################
  # ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  # ##############################################################
  # dataset_freshwater <- dataset_actualizado %>%
  #   filter(grepl("FRESHWATER", Habitat))
  dataset_freshwater <- amfibios_sin_duplicados
  dataset_freshwater$Habitat <- "Freshwater"

  # Mover la columna 'Species' al inicio
  dataset_freshwater <- dataset_freshwater[, c("Species", setdiff(names(dataset_freshwater), "Species"))]

  write.xlsx(dataset_freshwater, "./InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_AmphibiansReptilesCapinha.xlsx")
  cat("Archivo descargado correctamente: /InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_AmphibiansReptilesCapinha.xlsx")
}
