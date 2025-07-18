SwazilandsAlienPlantsDatabase_Download_and_Depurate <- function(){
  #DESCARGAR
  # Leer la página web
  url <- "http://eswatinibiodiversity.com/alienplants/specieslist.asp"
  html <- read_html(url)

  # Todos los enlaces <a>
  tags_a <- html_elements(html, "a")

  # Filtrar los <a> que contienen speciesinfo.asp?spid=
  enlaces_validos <- tags_a[grepl("speciesinfo\\.asp\\?spid=", html_attr(tags_a, "href"))]

  # Extraer href y texto visible
  hrefs <- html_attr(enlaces_validos, "href")
  nombres <- html_text(enlaces_validos)

  # Limpiar texto (quitar &nbsp;, espacios extra)
  nombres <- gsub("\u00a0", " ", nombres)
  nombres <- trimws(nombres)

  # Filtro: mantener solo si parecen nombre científico (2 palabras, ambas con inicial mayúscula o binomial)
  es_nombre_cientifico <- grepl("^[A-Z][a-z]+ [a-z]+$", nombres)

  # Aplicar filtro
  dataset <- data.frame(
    especie = nombres[es_nombre_cientifico],
    enlace = paste0("http://eswatinibiodiversity.com/alienplants/", hrefs[es_nombre_cientifico]),
    stringsAsFactors = FALSE
  )

  resultados <- data.frame(
    Species = character(),
    Origin = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(dataset))) {
    especie <- dataset$especie[i]
    url <- dataset$enlace[i]
    cat("Procesando", i, "/", nrow(dataset), ":", especie, "\n")

    # Intentar leer el HTML de la ficha
    pagina <- tryCatch(read_html(url), error = function(e) return(NA))
    if (is.na(pagina)) next

    # Extraer todos los textos visibles
    texto <- pagina %>% html_text()

    # Dividir en líneas por salto de línea
    lineas <- unlist(strsplit(texto, "\n"))
    lineas <- trimws(lineas)

    # Buscar la línea que contiene "Origin:"
    linea_origin <- grep("Origin:", lineas, value = TRUE)
    origin <- if (length(linea_origin)) sub(".*Origin:\\s*", "", linea_origin[1]) else NA

    # Agregar al data.frame
    resultados <- rbind(resultados, data.frame(
      Species = especie,
      Origin = origin,
      stringsAsFactors = FALSE
    ))
  }

  resultados$Invaded_country <- "Swaziland"
  #Guardamos
  write.xlsx(resultados, "Inputfiles/Step0_OriginalDatabase_SwazilandsAlienPlantsDatabase.xlsx")


  #######################################
  ############# HABITAT #################
  #######################################
  source(file.path("R", "check_habitat.r"))
  dataset <- resultados
  nombres <- dataset$Species
  nom_acep <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(nom_acep, dataset)

  #Freshwater?
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))

  #Guardamos
  write.xlsx(dat_fresh, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_SwazilandsAlienPlantsDatabase.xlsx")

}
