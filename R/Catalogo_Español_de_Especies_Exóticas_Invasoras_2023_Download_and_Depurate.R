Catalogo_Español_de_Especies_Exóticas_Invasoras_2023_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/conservacion-de-especies/Tabla%20CEEEIcompleta.xlsx"
  destfile <- "InputFiles/Step0_OriginalDatabase_Catalogo_Español_de_Especies_Exóticas_Invasoras_2023.xlsx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  # Leer el archivo de Excel
  mi_excel <- read_excel("Inputfiles/Step0_OriginalDatabase_Catalogo_Español_de_Especies_Exóticas_Invasoras_2023.xlsx")  # Cambia esta ruta por la ubicación de tu archivo
  # Guardar como archivo CSV
  write.csv2(mi_excel, "Inputfiles/Step0_OriginalDatabase_Catalogo_Español_de_Especies_Exóticas_Invasoras_2023.csv", row.names = FALSE)







  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  ruta_inputfiles <- file.path("Inputfiles")
  dat <- read.csv("Inputfiles/Step0_OriginalDatabase_Catalogo_Español_de_Especies_Exóticas_Invasoras_2023.csv",sep=";")#Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.
  head(dat)
  names(dat)
  colnames(dat)[colnames(dat) == "Grupo.taxonómico"] <- "Group"
  colnames(dat)[colnames(dat) == "Rango.taxonómico"] <- "Rango_taxonomico"
  colnames(dat)[colnames(dat) == "Nombre.científico.CEEEI..BOE"] <- "Nombre_cientifico_CEEEI"
  colnames(dat)[colnames(dat) == "Nombre.científico.actualizado"] <- "Nombre_cientifico_actualizado"
  colnames(dat)[colnames(dat) == "Ámbito.de.aplicación"] <- "Ambito_de_aplicacion"
  colnames(dat)[colnames(dat) == "Nombre.común"] <- "Nombre_comun"
  colnames(dat)[colnames(dat) == "Situación.anterior"] <- "Situacion_anterior"

  Catalogo_Español_de_Especies_Exóticas_Invasoras_2023_rangoespecie <- dat %>%
    filter(Rango_taxonomico %in% c("especie")) %>%
    select(Group, Nombre_cientifico_CEEEI)


  invasive_country <- c("Spain")
  Catalogo_Español_de_Especies_Exóticas_Invasoras_2023_rangoespecie$invasive_country <- invasive_country



  #CUÁLES SON FRESHWATER?
  #NO DUPLICADOS
  Catalogo_Español_de_Especies_Exóticas_Invasoras_2023_subset_sinduplicados <- Catalogo_Español_de_Especies_Exóticas_Invasoras_2023_rangoespecie %>%
    dplyr::group_by(Nombre_cientifico_CEEEI) %>%
    dplyr::summarise(across(everything(), ~ {
      texto <- as.character(.)
      texto <- str_remove_all(texto, "https://doi[^[:space:]]+")
      vals <- unlist(str_split(texto, "[,;|]+|\\s{2,}"))
      vals <- str_trim(vals)
      vals <- vals[vals != ""]
      vals <- unique(vals)
      paste(vals, collapse = ", ")
    }), .groups = "drop")



  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  #Obtain habitats
  dataset <- Catalogo_Español_de_Especies_Exóticas_Invasoras_2023_subset_sinduplicados
  especies_lista0 <- dataset$Nombre_cientifico_Catalogo_Español_de_Especies_Exóticas_Invasoras_2023
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)

  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))

  # Reordenar columnas colocando 'Species' al inicio
  dataset_freshwater <- dataset_freshwater[, c("Species", setdiff(names(dataset_freshwater), "Species"))]

  write.xlsx(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Catalogo_Español_de_Especies_Exóticas_Invasoras_2023.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_Catalogo_Español_de_Especies_Exóticas_Invasoras_2023.xlsx", "\n")
}

