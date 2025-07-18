ClarkeAndMcGeoch2023_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  library(pdftools)

  # Leer texto de las páginas 10 a 19 del PDF
  pdf_file <- "Inputfiles/Step0_OriginalDatabase_ClarkeAndMcGeoch2023.pdf"

  # Extraer todas las páginas
  all_pages <- pdf_text(pdf_file)

  # Extraer páginas 10 a 19 (índice empieza en 1)
  pages_10_19 <- all_pages[10:19]

  # Unir el texto de esas páginas
  text_10_19 <- paste(pages_10_19, collapse = "\n")
  cat(text_10_19)

  txt <- text_10_19
  # Dividir por líneas y quitar líneas vacías
  lines <- strsplit(txt, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[lines != ""]

  # Inicializar vector para datos procesados
  data <- list()

  # Variables temporales para manejar nombres en dos líneas
  current_order <- NA
  current_family <- NA
  current_sci_name <- NA
  current_mechanism <- NA
  current_severity <- NA

  for (line in lines) {
    # Si la línea empieza con texto en primera columna, es línea completa
    # Si no, es continuación del nombre científico

    # Dividir la línea por espacios múltiples
    parts <- unlist(strsplit(line, "\\s{2,}"))

    if (nchar(parts[1]) > 0) {
      # Línea completa con todas las columnas

      # Guardar la fila anterior si existe
      if (!is.na(current_order)) {
        data[[length(data) + 1]] <- list(
          Order = current_order,
          Family = current_family,
          Scientific_name = current_sci_name,
          Mechanism = current_mechanism,
          Severity = current_severity
        )
      }

      # Actualizar variables temporales
      current_order <- parts[1]
      current_family <- parts[2]
      current_sci_name <- parts[3]
      # Si nombre científico tiene más palabras (ej. Merizodus soledadinus), las juntamos luego
      if (length(parts) > 5) {
        # mecanismo puede ocupar espacios, tratamos separado
        # pero en este dataset el mecanismo y severity parecen una sola palabra y una sigla
        # asumimos que scientific name son todas las palabras entre Family y Mechanism
        # En el split no hay más de 5 columnas, pero por si acaso:
        current_sci_name <- paste(parts[3:(length(parts) - 2)], collapse = " ")
        current_mechanism <- parts[length(parts) - 1]
        current_severity <- parts[length(parts)]
      } else {
        current_mechanism <- parts[4]
        current_severity <- parts[5]
      }
    } else {
      # Línea de continuación del nombre científico
      current_sci_name <- paste(current_sci_name, trimws(line))
    }
  }

  # Guardar la última fila
  if (!is.na(current_order)) {
    data[[length(data) + 1]] <- list(
      Order = current_order,
      Family = current_family,
      Scientific_name = current_sci_name,
      Mechanism = current_mechanism,
      Severity = current_severity
    )
  }

  # Convertir lista a dataframe
  df <- do.call(rbind, lapply(data, as.data.frame))

  # Mostrar resultado
  print(df)

  write.xlsx(df, "Inputfiles/Step0_OriginalDatabase_ClarkeAndMcGeoch2023.xlsx")
  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################

  dataset <- read_excel("Inputfiles/Step0_OriginalDatabase_ClarkeAndMcGeoch2023.xlsx")
  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  #Obtain habitats
  # Obtener nombres científicos desde el campo "Especie"
  especies_lista0 <- dataset$Scientific_name
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)


  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))

  #REPETIDAS NO METER BD

  write.xlsx(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_ClarkeAndMcGeoch2023.xlsx")
  cat("Archivo descargado correctamente: /Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_ClarkeAndMcGeoch2023.xlsx")

}
