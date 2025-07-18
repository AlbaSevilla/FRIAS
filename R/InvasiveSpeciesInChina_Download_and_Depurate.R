InvasiveSpeciesInChina_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
    url <- "https://link.springer.com/content/pdf/10.1023/A%3A1016695609745.pdf"
    destfile <- "InputFiles/Step0_OriginalDatabase_InvasiveSpeciesInChina.pdf"
    download.file(url, destfile, mode = "wb")


    ##########################################################
    ############### obtener información de interés ###########
    ##########################################################
    text_pages <- pdf_text(destfile)[14:23]
    texto <- text_pages

    extraer_info_especies <- function(texto) {
      bloques <- str_split(texto, "\n\n\n|\\n\\n")[[1]]  #Dividir por especies usando saltos de línea dobles o triples

      resultados <- lapply(bloques, function(bloque) {
        bloque_limpio <- str_remove_all(bloque, "\\([^\\)]*\\)") #Eliminamos el contenido entre paréntesis
        nombre <- str_extract( #Extraemos el nombre común y científico
          bloque,
          "^[-A-Za-z\\s,'’]+\\s*\\([A-Za-z\\s\\.]+(?: spp\\.| sp\\.| [a-z]+)?\\)(\\s*(and|&)\\s*[-A-Za-z\\s,'’]+\\s*\\([A-Za-z\\s\\.]+(?: spp\\.| sp\\.| [a-z]+)?\\))?"
        )
        if (is.na(nombre)) return(NULL)
        nativo <- str_extract(bloque_limpio, "(?:a native of|is native to|[Nn]ative to|was introduced to the former USSR from|was introduced to|from) [^.]+\\.")
        paises_nativos <- if (!is.na(nativo)) { #Obtenemos el Rango Nativo
          paises <- str_extract_all(nativo, "[A-Z][a-z]+(?: [A-Z][a-z]+)*")[[1]]
          paises_limpios <- paises[!grepl("Native", paises)]
          if (length(paises_limpios) > 0) paises_limpios[1] else NA
        } else NA
        First_Record <- str_extract_all(bloque_limpio, "\\b(mid-)?\\d{4}s?|[A-Z][a-z]+ \\d{4}\\b")[[1]] #Extraemos las fechas
        #Dataset final:
        list(
          especie = nombre,
          nativos = paises_nativos,
          First_Record = unique(First_Record)
        )
      })

      #Quitamos los nulos y convertimos a dataframe
      resultados <- resultados[!sapply(resultados, is.null)]
      df <- do.call(rbind, lapply(resultados, function(x) {
        data.frame(
          Especie = x$especie,
          Rango_Nativo = x$nativos,
          First_Record = paste(x$First_Record, collapse = ", "),
          stringsAsFactors = FALSE
        )
      }))

      return(df)
    }
    DATASET_CHINA <- extraer_info_especies(paste(text_pages, collapse = "\n"))
    write.xlsx(DATASET_CHINA, "./Inputfiles/Step0_OriginalDatabase_InvasiveSpeciesInChina.xlsx")

    #####################################################################################
    ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
    #####################################################################################
    DATASET_CHINA <- read_excel("Inputfiles/Step0_OriginalDatabase_InvasiveSpeciesInChina.xlsx")

    source(file.path("R", "check_habitat.r"))

    #Obtain habitats
    # Obtener nombres científicos desde el campo "Especie"
    dataset <- DATASET_CHINA
    especies_lista0 <- DATASET_CHINA$Especie
    especies_lista1 <- str_match(especies_lista0, "\\(([^\\)]+)\\)")[,2]  # Solo el primero
    especies_lista <- name_backbone_checklist(especies_lista1)$canonicalName
    dataset_actualizado <- check_habitat(especies_lista, dataset)


    ##############################################################
    ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
    ##############################################################
    dataset_freshwater <- dataset_actualizado %>%
      filter(grepl("FRESHWATER", Habitat) | is.na(Habitat))

    dataset_freshwater$Especie <- str_match(dataset_freshwater$Especie, "\\(([^\\)]+)\\)")[,2]


    Invaded_Country <- "China"
    dataset_freshwater$Invaded_Country <- Invaded_Country

    ##########################################################
    #PARA OBTENER LA FECHA MÁS ANTIGUA #######################
    ##########################################################
    dataset_freshwater$OldestDate <- dataset_freshwater$First_Record
    dataset_freshwater$OldestDate <- str_replace_all(dataset_freshwater$OldestDate,
                                                       c("Since " = "",
                                                         "s" = "",
                                                         "By " = "",
                                                         "In " = "",
                                                         "mid-" = ""))
    dataset_freshwater$First_Record <- str_replace_all(dataset_freshwater$First_Record,
                                                     c("Since " = "",
                                                       "s" = "",
                                                       "By " = "",
                                                       "In " = ""))

    #OLDEST DATE
    source(file.path("R", "OldestDate.r"))
    dataset_freshwater <- OldestDate(dataset_freshwater, "OldestDate")


    write.xlsx(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_InvasiveSpeciesInChina.xlsx")
    cat("Archivo descargado correctamente: /Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_InvasiveSpeciesInChina.xlsx")

}
