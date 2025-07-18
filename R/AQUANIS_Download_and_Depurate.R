AQUANIS_Download_and_Depurate <- function(){
  ##################################################################################
  ##################### DESCARGAR ##################################################
  ##################################################################################
  #AQUANIS
  url <- "https://aquanisresearch.com/index.php/aquanis/species/open/page/ALL"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[3]]
  res3 <- as.data.frame(res3)
  res3
  rownames(res3) <- NULL


  #obtener habitat
  source(file.path("R", "check_habitat.R"))
  dataset <- res3
  especies <- dataset$Species
  especies_accep <- name_backbone_checklist(especies)$canonicalName
  dataset_habitat <- check_habitat(especies_accep, dataset)

  write.xlsx (dataset_habitat, "./InputFiles/Step0_OriginalDatabase_AQUANIS.xlsx")


  ###################################################################################
  ######################### freshwater ##############################################
  ###################################################################################
  dataset_HAB <- read_excel("InputFiles/Step0_OriginalDatabase_AQUANIS.xlsx")
  dataset_freshwater <- dataset_HAB %>% filter(grepl("FRESHWATER", Habitat))
  dataset_freshwater_especies <- dataset_freshwater$AcceptedNameGBIF

  #De las que nos quedemos, cogemos:
  # 1 -. Link
  # 2 -. Species, Family, Order, Native origin
  # Librerías necesarias

  #####################################################################
  ########## EXTRAER LINKS DE LAS FRESHWATER ##########################
  #####################################################################
  nodos <- res %>% html_nodes("a")
  nombres <- nodos %>% html_text(trim = TRUE)
  links <- nodos %>% html_attr("href")

  # Crear tabla con nombre y link
  tabla_links <- tibble(Species = str_squish(nombres),
                        Link = links) %>%
    filter(str_detect(Link, "/species/view/id/"))  # Solo los enlaces relevantes
  tabla_filtrada <- tabla_links %>%
    filter(Species %in% dataset_freshwater$AcceptedNameGBIF)

  # CASO INDIVIDUAL:

  # url <- "https://aquanisresearch.com/aquanis/species/view/id/1986" #Link que nos sale en tabla_filtrada$Link
  # res <- read_html(url)
  # res2 <- res %>% html_table
  # res2 <- bind_rows(res2[[2]], res2[[3]])  # o rbind(res[[2]], res[[3]])
  # res2 <- res2[,-3]
  # res2 <- as.data.frame(t(res2))
  # res2
  # columnas <- as.character(unlist(res2[1,]))
  # colnames(res2) <- columnas
  # res2 <- res2[-1,]
  # seleccionar_columnas <- c("Species", "Family", "Order", "Class", "Phylum", "Native origin (?)")
  # res3 <- res2[,seleccionar_columnas]
  # res3
  # res3$`Native origin (?)` <- sub("References:.*", "", res3$`Native origin (?)`)
  # res3$Species <- sapply(strsplit(res3$Species, " "), function(x) paste(x[1:2], collapse = " "))
  # names(res3)[names(res3) == "Native origin (?)"] <- "Native_Range"
  # res3




  extraer_info_especie <- function(url, especie, idx, total) {
    library(rvest)
    library(dplyr)
    library(stringr)

    # Tu lista de LME válidos (pegada aquí directamente)
    LME_validos <- c(
      "East Bering Sea", "Gulf of Alaska", "California Current", "Gulf of California",
      "Gulf of Mexico", "Southeast U.S. Continental Shelf", "Northeast U.S. Continental Shelf",
      "Scotian Shelf", "Newfoundland-Labrador Shelf", "Insular Pacific?Hawaiian",
      "Pacific Central-American Coastal", "Caribbean Sea", "Humboldt Current",
      "Patagonian Shelf", "South Brazil Shelf", "East Brazil Shelf", "North Brazil Shelf",
      "West Greenland Shelf", "East Greenland Shelf", "Barents Sea", "Norwegian Sea",
      "North Sea", "Baltic Sea", "Celtic?Biscay Shelf", "Iberian Coastal", "Mediterranean Sea",
      "Canary Current", "Guinea Current", "Benguela Current", "Agulhas Current",
      "Somali Coastal Current", "Arabian Sea", "Red Sea", "Bay of Bengal", "Gulf of Thailand",
      "South China Sea", "Sulu-Celebes Sea", "Indonesian Sea", "North Australian Shelf",
      "Northeast Australian Shelf (Great Barrier Reef)", "East-Central Australian Shelf",
      "Southeast Australian Shelf", "Southwest Australian Shelf", "West-Central Australian Shelf",
      "Northwest Australian Shelf", "New Zealand Shelf", "East China Sea", "Yellow Sea",
      "Kuroshio Current", "Sea of Japan", "Oyashio Current", "Sea of Okhotsk",
      "West Bering Sea", "Chukchi Sea", "Beaufort Sea", "East Siberian Sea", "Laptev Sea",
      "Kara Sea", "Iceland Shelf and Sea", "Faroe Plateau", "Antarctica", "Black Sea",
      "Hudson Bay", "Arctic Ocean", "Greenland Sea", "Canadian High Arctic and North Greenland",
      "Aleutian Islands"
    )

    message(sprintf("Procesando especie %d de %d: %s", idx, total, especie))
    res <- read_html(url)
    res2 <- res %>% html_table()
    df <- bind_rows(res2[[2]], res2[[3]])
    df <- df[, -3]
    df <- as.data.frame(t(df))

    columnas <- as.character(unlist(df[1, ]))
    colnames(df) <- columnas
    df <- df[-1, ]

    seleccionar_columnas <- c("Species", "Family", "Order", "Class", "Phylum", "Native origin (?)")
    missing_cols <- setdiff(seleccionar_columnas, colnames(df))
    if(length(missing_cols) > 0){
      df[missing_cols] <- NA
    }
    df_sel <- df[, seleccionar_columnas]

    # Limpiar y extraer de la columna "Native origin (?)"
    clean_native_origin <- function(texto) {
      if (is.na(texto)) return(NA)

      # Eliminar 'References' y 'Not entered'
      texto <- sub("References:.*", "", texto)
      texto <- str_replace_all(texto, "Not entered", "")

      # Extraer lo que sigue a 'Ocean:'
      ocean <- str_match(texto, "Ocean:\\s*(\\w+)")[,2]

      # Extraer lo que sigue a 'Country:'
      country <- str_match(texto, "Country:\\s*(\\w+)")[,2]

      # Buscar coincidencias con LME válidos
      lmes_encontrados <- LME_validos[str_detect(texto, fixed(LME_validos, ignore_case = TRUE))]

      # Combinar resultados eliminando NAs
      valores <- c(ocean, country, lmes_encontrados)
      valores <- valores[!is.na(valores) & valores != ""]

      # Unir todo en una sola cadena
      return(paste(unique(valores), collapse = "; "))
    }

    df_sel$`Native origin (?)` <- sapply(df_sel$`Native origin (?)`, clean_native_origin)

    # Arreglar nombre de especie (dos primeras palabras)
    df_sel$Species <- sapply(strsplit(df_sel$Species, " "), function(x) paste(x[1:2], collapse = " "))

    # Renombrar columna
    names(df_sel)[names(df_sel) == "Native origin (?)"] <- "Native_Range"

    return(df_sel)
  }


  total_especies <- nrow(tabla_filtrada)

  resultados_lista <- mapply(
    FUN = extraer_info_especie,
    url = tabla_filtrada$Link,
    especie = tabla_filtrada$Species,
    idx = seq_len(total_especies),
    MoreArgs = list(total = total_especies),
    SIMPLIFY = FALSE
  )

  resultados_df <- bind_rows(resultados_lista)
  rownames(resultados_df) <- NULL
  resultados_df <- resultados_df %>% filter(!(Species == "NA NA"))

  # Cadenas a eliminar
  strings_to_remove <- c("References", "Country", "Comments")

  # Crear un patrón regex que una todas las cadenas a eliminar
  pattern <- paste(strings_to_remove, collapse = "|")

  # Aplicar la eliminación a todo el data frame
  resultados_df2 <- as.data.frame(lapply(resultados_df, function(col) {
    gsub(pattern, "", col)
  }), stringsAsFactors = FALSE)


  dataset <- resultados_df2
  especies <- dataset$Species
  nombres_aceptados <- name_backbone_checklist(especies)$canonicalName
  dataset_final <- check_habitat(especies, dataset)


  write.xlsx (dataset_final, "./InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_AQUANIS.xlsx")
}
