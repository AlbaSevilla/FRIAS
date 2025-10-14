Iriarte_et_al_2005_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  url <- "https://www.scielo.cl/pdf/rchnat/v78n1/art10.pdf"
  destfile <- "InputFiles/Step0_OriginalDatabase_Iriarte_et_al_2005_Download_and_Depurate.pdf"
  download.file(url, destfile, mode = "wb")


  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  text_pages <- pdf_text(destfile)[5:6]
  texto <- text_pages
  texto_completo <- paste(texto, collapse = "\n")

  # Dividir en líneas
  lineas <- strsplit(texto_completo, "\n")[[1]]

  # Buscar posición de "Scientific (common) names"
  pos_scn <- grep("Scientific \\(common\\) names", lineas)

  if (length(pos_scn) > 0) {
    # Quedarse desde esa línea hacia adelante (incluyendo la línea)
    lineas <- lineas[pos_scn[1]:length(lineas)]
  } else {
    warning("No se encontró la línea 'Scientific (common) names'")
  }

  # Buscar posición de "TABLE 2"
  pos_table2 <- grep("TABLE 2", lineas)

  if (length(pos_table2) > 0) {
    # Quedarse hasta justo antes de "TABLE 2"
    lineas <- lineas[1:(pos_table2[1] - 1)]
  } else {
    warning("No se encontró la línea 'TABLE 2'")
  }

  # Mostrar resultado
  cat(lineas, sep = "\n")
  texto_modificado <- gsub(" {3,}", ";", lineas)
  texto_modificado
  # Función para contar campos separados por ';'
  count_fields <- function(text) {
    sapply(strsplit(text, ";"), length)
  }

  fusionar_lineas <- function(texto) {
    resultado <- c()

    for (linea in texto) {
      if (nchar(trimws(linea)) == 0) {
        # Omitir líneas vacías
        next
      }
      campos <- count_fields(linea)
      if (length(resultado) == 0) {
        resultado <- c(resultado, linea)
      } else {
        if (campos < 3) {
          # Fusionar con la línea anterior
          resultado[length(resultado)] <- paste0(resultado[length(resultado)], " ", trimws(linea))
        } else {
          resultado <- c(resultado, linea)
        }
      }
    }
    return(resultado)
  }

  texto_fusionado <- fusionar_lineas(texto_modificado)
  texto_fusionado2 <- gsub(";IRIARTE ET AL.", "", texto_fusionado)
  cat(texto_fusionado2)
  # Paso 2: Convertir a data.frame
  dataset <- read.table(text = texto_fusionado2, sep = ";", header = TRUE, fill = TRUE, strip.white = TRUE, quote = "")
  View(dataset)
  # Desplazar las columnas una posición a la izquierda
  dataset_corregido <- data.frame(
    Scientific_name = dataset[, 0],
    Origin = dataset[, 1],
    Motivation = dataset[, 2],
    When = dataset[, 3],
    Introduced_to = dataset[, 5],
    stringsAsFactors = FALSE
  )

  # Revisa el resultado
  dataset_corregido
  dataset_corregido$Scientific_Name <- rownames(dataset_corregido)
  rownames(dataset_corregido) <- NULL     # o bien: row.names(dataset) <- NULL

  write.csv2(dataset_corregido, "./Inputfiles/Step0_OriginalDatabase_Iriarte_et_al_2005.csv")

  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  Database <- read.csv("Inputfiles/Step0_OriginalDatabase_Iriarte_et_al_2005.csv", sep=";")
  Database$Habitat_Database <- "Freshwater"
  Database$Invaded_Country <- "Chile"
  Database <- Database %>% select(-X)
  Database <- Database[, c("Scientific_Name", setdiff(names(Database), "Scientific_Name"))]
  Database <- Database %>% filter(!When=="Undated")
  Database <- Database %>% filter(!When=="No data")

  write.xlsx(Database, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Iriarte_et_al_2005.xlsx")
  cat("Archivo descargado correctamente: /Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Iriarte_et_al_2005.xlsx")

}
