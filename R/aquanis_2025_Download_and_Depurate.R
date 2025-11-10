aquanis_2025_Download_and_Depurate <- function(){
  ##################################################################################
  ##################### DESCARGAR ##################################################
  ##################################################################################
  #AQUANIS_2025
  url <- "https://aquanisresearch.com/index.php/aquanis/species/open/page/ALL"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[3]]
  res3 <- as.data.frame(res3)
  rownames(res3) <- NULL


  #obtener habitat
  source(file.path("R", "check_habitat.r"))
  dataset <- res3
  especies <- dataset$Species
  especies_accep <- name_backbone_checklist(especies)$canonicalName
  dataset_habitat <- check_habitat(especies_accep, dataset)

  #No duplicados
  source(file.path("R", "noduplicates.r"))
  dataset_habitat2 <- noduplicates(dataset_habitat, "AcceptedNameGBIF")

  write.xlsx (dataset_habitat, "./InputFiles/originaldatabase_aquanis_2025.xlsx")


  ###################################################################################
  ######################### freshwater ##############################################
  ###################################################################################
  dataset_HAB <- read_excel("InputFiles/originaldatabase_aquanis_2025.xlsx")
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

  extraer_info_especie <- function(url, especie, idx, total) {
    library(rvest)
    library(dplyr)
    library(stringr)

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
    dataset <- bind_rows(res2[[2]], res2[[3]])
    dataset <- dataset[, -3]
    dataset <- as.data.frame(t(dataset))

    columnas <- as.character(unlist(dataset[1, ]))
    colnames(dataset) <- columnas
    dataset <- dataset[-1, ]

    seleccionar_columnas <- c("Species", "Family", "Order", "Class", "Phylum", "Native origin (?)")
    missing_cols <- setdiff(seleccionar_columnas, colnames(dataset))
    if(length(missing_cols) > 0){
      dataset[missing_cols] <- NA
    }
    dataset_sel <- dataset[, seleccionar_columnas]
    clean_native_origin <- function(texto) {
      if (is.na(texto)) return(NA)
      texto <- sub("References:.*", "", texto)
      texto <- str_replace_all(texto, "Not entered", "")
      ocean <- str_match(texto, "Ocean:\\s*(\\w+)")[,2]
      country <- str_match(texto, "Country:\\s*(\\w+)")[,2]
      lmes_encontrados <- LME_validos[str_detect(texto, fixed(LME_validos, ignore_case = TRUE))]
      valores <- c(ocean, country, lmes_encontrados)
      valores <- valores[!is.na(valores) & valores != ""]

      return(paste(unique(valores), collapse = "; "))
    }
    dataset_sel$`Native origin (?)` <- sapply(dataset_sel$`Native origin (?)`, clean_native_origin)
    dataset_sel$Species <- sapply(strsplit(dataset_sel$Species, " "), function(x) paste(x[1:2], collapse = " "))
    names(dataset_sel)[names(dataset_sel) == "Native origin (?)"] <- "Native_Range"

    return(dataset_sel)
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

  resultados_dataset <- bind_rows(resultados_lista)
  rownames(resultados_dataset) <- NULL
  resultados_dataset <- resultados_dataset %>% filter(!(Species == "NA NA"))
  strings_to_remove <- c("References", "Country", "Comments")
  pattern <- paste(strings_to_remove, collapse = "|")
  resultados_dataset2 <- as.data.frame(lapply(resultados_dataset, function(col) {
    gsub(pattern, "", col)
  }), stringsAsFactors = FALSE)


  dataset <- resultados_dataset2
  especies <- dataset$Species
  nombres_aceptados <- name_backbone_checklist(especies)$canonicalName
  dataset_final <- check_habitat(especies, dataset)


  write.xlsx (dataset_final, "./InputFiles/freshwatersubset_aquanis_2025.xlsx")
}
