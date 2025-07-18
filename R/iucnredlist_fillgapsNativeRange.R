iucnredlist_fillgapsNativeRange <- function(){

  #PRIMERO ABRIMOS NUESTRA MASTERLIST
  MasterList <- read_excel("OutputFiles/Intermediate/Step2_MasterList.xlsx")
  names <- MasterList$AcceptedNameGBIF
  #names es los nombres de las especies que me quiero quedar por filas de final$scientificName


  #NO FUNCIONA BIEN, PROBAMOS MEZCLANDO ASSESSMENT CON TAXONOMY
  taxonomy_iucn <- read.csv("InputFiles/taxonomy_iucn.csv", sep=";")
  names(taxonomy_iucn)
  taxonomy_iucn <- taxonomy_iucn[,c(1:15)]
  head(taxonomy_iucn)
  names(taxonomy_iucn)

  assessments_iucn <- read.csv("InputFiles/assessments_iucn.csv", sep=";")
  names(assessments_iucn)
  assessments_iucn <- assessments_iucn[,c(1:15)]
  head(assessments_iucn)
  names(assessments_iucn)

  #NOS QUEDAMOS CON LAS ESPECIES DE LA MASTERLIST
  subset_taxonomy_iucn <- taxonomy_iucn[taxonomy_iucn$scientificName %in% names, ]
  subset_assessments_iucn <- assessments_iucn[assessments_iucn$scientificName %in% names, ]


  #LA TABLA CON LOS CODIGOS PARA CONSTRUIR LAS URLS DE PDF
  names(subset_taxonomy_iucn)
  columnas_taxonomy <- c("internalTaxonId", "scientificName")
  subset_subset_taxonomy_iucn <- subset_taxonomy_iucn[,columnas_taxonomy]
  head(subset_subset_taxonomy_iucn)
  names(subset_assessments_iucn)
  columnas_taxonomy <- c("assessmentId", "scientificName")
  subset_subset_assessments_iucn <- subset_assessments_iucn[,columnas_taxonomy]
  head(subset_subset_assessments_iucn)
  final <- merge(subset_subset_taxonomy_iucn, subset_subset_assessments_iucn, by="scientificName")
  final



  #CONSTRUIMOS LA COLUMNA DE URLS
  final$URL <- paste0("https://www.iucnredlist.org/species/pdf/", final$assessmentId)
  head(final[, c("scientificName", "URL")])


  #
  # #DESCARGAMOS LOS PDFS
  # # Iterar sobre cada fila del data.frame final
  # for (i in 1:nrow(final)) {
  #
  #   # Obtener la URL
  #   url <- final$URL[i]
  #
  #   # Usar directamente el nombre científico
  #   nombre_especie <- final$scientificName[i]
  #
  #   # Crear la ruta de destino (entre comillas para soportar espacios en nombres de archivo)
  #   destfile <- paste0("InputFiles/Step0_IUCNSpecies_", nombre_especie, ".pdf")
  #
  #   # Descargar el archivo PDF
  #   tryCatch({
  #     download.file(url, destfile, mode = "wb")
  #     message("✅ Descargado: ", nombre_especie)
  #   }, error = function(e) {
  #     message("❌ Error con: ", nombre_especie)
  #   })
  # }



  #OBTENER EL RANGO NATIVO CON IUCN WEB SCRAPPEANDO LOS PDFS LIBRES DISPONIBLES EN LA WEB DE LAS ESPECIES DE NUESTRO INTERES
  resultados <- list()
  # Iterar sobre cada especie en el data.frame final
  for (i in 1:nrow(final)) {
    url <- final$URL[i]
    nombre_estandar <- final$scientificName[i]
    message(sprintf("Procesando %d/%d: %s", i, nrow(final), nombre_estandar))

    #Leemos el pdf directamente online para no hacer una descarga de aprox 4000 archivos
    texto <- tryCatch({
      pdf_text(url)
    }, error = function(e) {
      message("❌ Error al leer PDF para: ", nombre_estandar)
      return(NA)
    })

    # Si falló la lectura, continuar con la siguiente especie
    if (is.na(texto)[1]) next

    ## ---- 1. Extraer nombre científico del PDF ----
    pagina1 <- texto[1]
    lineas <- strsplit(pagina1, "\n")[[1]]
    linea_nombre <- grep(", ", lineas, value = TRUE)

    if (length(linea_nombre) > 0) {
      nombre_extraido <- sub(",.*", "", linea_nombre[1])
      nombre_extraido <- trimws(nombre_extraido)
    } else {
      nombre_extraido <- NA
    }

    ## ---- 2. Extraer “Country Occurrence” ----
    todo_el_texto <- paste(texto, collapse = "\n")
    pattern <- "Country Occurrence:[\\s\\S]*?(?=\\n\\n|$)"
    match <- regmatches(todo_el_texto, regexpr(pattern, todo_el_texto, perl = TRUE))

    if (length(match) == 0) {
      country_occurrence <- NA
    } else {
      country_occurrence <- trimws(match)
    }

    ## ---- 3. Guardar en la lista ----
    resultados[[i]] <- data.frame(
      scientificName_in_table = nombre_estandar,
      scientificName_in_pdf = nombre_extraido,
      country_occurrence = country_occurrence,
      stringsAsFactors = FALSE
    )
  }

  # Unir todos los resultados en un único data.frame
  resultados_df <- do.call(rbind, resultados)
  head(resultados_df)

  #Guardamos
  write_xlsx(resultados_df,"Inputfiles/NativeRangesMasterlistByIUCN.xlsx")

  #Resultado unido:
  #Eliminamos "Country Occurrence:\nNative:" y eliminamos "Extant (resident)", y también "()" y las "," las
  #cambiamos por
  resultados_df <- read_excel("Inputfiles/NativeRangesMasterlistByIUCN.xlsx")
  resultados_df2 <- resultados_df
  resultados_df2$country_occurrence <- gsub("Country Occurrence:\nNative, :", "", resultados_df2$country_occurrence)
  resultados_df2$country_occurrence <- gsub("Country Occurrence:\nNative:", "", resultados_df2$country_occurrence)
  resultados_df2$country_occurrence <- gsub("Country Occurrence:Native, Extant non-breeding: ", "", resultados_df2$country_occurrence)
  resultados_df2$country_occurrence <- gsub("Country Occurrence:", "", resultados_df2$country_occurrence)
  resultados_df2$country_occurrence <- gsub("\n", "", resultados_df2$country_occurrence)
  resultados_df2$country_occurrence <- gsub("Extant \\(resident\\)", "", resultados_df2$country_occurrence)
  resultados_df2$country_occurrence <- gsub("Native, :", "", resultados_df2$country_occurrence)
  resultados_df2$country_occurrence <- gsub("\\(|\\)", "", resultados_df2$country_occurrence)
  resultados_df2$country_occurrence <- trimws(resultados_df2$country_occurrence)
  head(resultados_df2)

  resultado_unido <- merge(
    MasterList,
    resultados_df2,
    by.x = "AcceptedNameGBIF",
    by.y = "scientificName_in_table",
    all.x = TRUE
  )
  resultado_unido <- resultado_unido %>%
    mutate(NativeRangeofIAS_list = paste(NativeRangeofIAS_list, country_occurrence, sep = "; "))

  resultado_unido$NativeRangeofIAS_list <- gsub("NA; ", "", resultado_unido$NativeRangeofIAS_list)
  resultado_unido$NativeRangeofIAS_list <- gsub("NA; Native, : ", "", resultado_unido$NativeRangeofIAS_list)
  resultado_unido$NativeRangeofIAS_list <- gsub("Native, : ", "", resultado_unido$NativeRangeofIAS_list)
  resultado_unido$NativeRangeofIAS_list <- gsub("Native, Extant non-breeding: ", "", resultado_unido$NativeRangeofIAS_list)
  resultado_unido <- resultado_unido %>%
    select(-scientificName_in_pdf) %>%
    select(-country_occurrence)
  names(resultado_unido)
  write_xlsx(resultado_unido,"OutputFiles/Intermediate/Step3_FillGapsinLocations_MasterList.xlsx")

}
