FAODIAS_Download_and_Depurate <- function(){

  #######################################################################
  ############## PARA TODOS LOS ENLACES #################################
  #######################################################################


  enlaces <- c(
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e08.htm#register%20of%20international%20introductions%20of%20inland%20aquatic%20species%20(a%20%20%20b)",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e09.htm#register%20of%20international%20introductions%20of%20inland%20aquatic%20species%20%C2%A9",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0a.htm#register%20of%20international%20introductions%20of%20inland%20aquatic%20species%20(d%20%20%20e%20%20%20f%20%20%20g",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0b.htm#register%20of%20international%20introductions%20of%20inland%20aquatic%20species%20(i%20%20%20l%20%20%20m)",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0c.htm#register%20of%20international%20introductions%20of%20inland%20aquatic%20species%20(n%20%20%20o)",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0d.htm#register%20of%20international%20introductions%20of%20inland%20aquatic%20species%20(p%20%20%20r)",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0e.htm#register%20of%20international%20introductions%20of%20inland%20aquatic%20species%20(s)",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0f.htm#register%20of%20international%20introductions%20of%20inland%20aquatic%20species%20(t%20%20%20u%20%20%20x)"
  )

  extraer_datos <- function(url) {
    res <- read_html(url)

    nombres <- res %>%
      html_nodes("h2") %>%
      html_text() %>%
      .[grepl(" ", .)]  # Filtrar nombres que contienen espacio (ajustar si es necesario)

    descripciones <- res %>%
      html_nodes("p") %>%
      html_text() %>%
      .[grepl("Native range", .)]  # Filtrar descripciones que contengan "Native range"

    # En caso que nombres y descripciones no tengan igual longitud, tomar el mínimo para evitar error
    n <- min(length(nombres), length(descripciones))

    data.frame(
      nombre = nombres[1:n],
      descripcion = descripciones[1:n],
      stringsAsFactors = FALSE
    )
  }

  # Aplicar la función a todos los enlaces y unir
  lista_datos <- lapply(enlaces, extraer_datos)
  dataset_unificado <- bind_rows(lista_datos)

  # Ver las primeras filas
  View(dataset_unificado)


  #TRANSFORMACIONES EN LA COLUMNA DESCRIPCION
  dataset_unificado$descripcion <- gsub("Native range: ", "", dataset_unificado$descripcion)
  dataset_unificado$descripcion <- gsub(" and ", ";", dataset_unificado$descripcion)
  dataset_unificado$descripcion <- gsub(",", ";", dataset_unificado$descripcion)
  dataset_unificado$descripcion <- gsub("Native range; ", "", dataset_unificado$descripcion)
  dataset_unificado$descripcion <- gsub("\r\n", "", dataset_unificado$descripcion)
  dataset_unificado$nombre <- str_extract(dataset_unificado$nombre, "^\\S+\\s+\\S+")
  dataset_unificado$Habitat_Database <- "Freshwater"

  especies <- dataset_unificado$nombre
  dataset_unificado$AcceptedNameGBIF <- name_backbone_checklist(especies)$canonicalName

  #Guardamos el dataset
  write_xlsx(dataset_unificado, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_FAODIAS.xlsx")


  # # Enlace base
  # enlace <- "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e00.htm"
  # base_url <- "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/"
  #
  # # Leer HTML
  # res <- read_html(enlace)
  #
  # # Extraer nombres y enlaces
  # nodos <- res %>% html_nodes("a")
  # nombres <- nodos %>% html_text(trim = TRUE)
  # links <- nodos %>% html_attr("href")
  #
  # # Construir links completos
  # links_full <- sapply(links, function(link) {
  #   if (is.na(link) || grepl("^#", link)) return(NA)
  #
  #   parts <- strsplit(link, "#")[[1]]
  #   file <- parts[1]
  #   fragment <- ifelse(length(parts) > 1, URLencode(parts[2], reserved = TRUE), "")
  #
  #   paste0(base_url, file, ifelse(fragment != "", paste0("#", fragment), ""))
  # })
  #
  # # Crear data.frame con nombres y enlaces
  # df <- tibble(Nombre = nombres, Link = links_full)
  #
  # # Supongamos que tu tibble se llama df_links
  # df_especies <- df %>%
  #   filter(
  #     # Nombre contiene al menos dos palabras, ambas con letras (minúsculas o mayúsculas)
  #     str_detect(Nombre, "^[A-Z][a-z]+\\s+[a-z]+")
  #   )
  #
  # View(df_especies)
  #
  #
  #
  #
  # # Función para extraer datos de una sola página
  # extraer_info <- function(url) {
  #   # Intentar leer la página
  #   tryCatch({
  #     html <- read_html(url)
  #
  #     # Extraer párrafos de texto
  #     parrafos <- html %>% html_nodes("p") %>% html_text(trim = TRUE)
  #
  #     # Buscar el texto que contiene "Native range:"
  #     native_line <- parrafos[str_detect(parrafos, regex("Native range:", ignore_case = TRUE))][1]
  #     native_range <- str_remove(native_line, ".*[Nn]ative range:\\s*") %>% str_trim()
  #
  #     # Extraer tablas (To:, From:, etc.)
  #     tablas <- html %>% html_nodes("table")
  #
  #     # Extraer todas las celdas de texto de la primera tabla (usualmente la que contiene To:)
  #     celdas <- tablas[[1]] %>% html_nodes("td") %>% html_text(trim = TRUE)
  #
  #     # Buscar el valor de "To:"
  #     to_index <- which(str_detect(celdas, regex("^To:", ignore_case = TRUE)))
  #     to_value <- if (length(to_index) > 0 && to_index + 1 <= length(celdas)) celdas[to_index + 1] else NA
  #
  #     tibble(Native_Range = native_range, To = to_value, Link = url)
  #   }, error = function(e) {
  #     # Si falla, devolver NA
  #     tibble(Native_Range = NA, To = NA, Link = url)
  #   })
  # }
  #
  #
  # df_especies2 <- df_especies[c(5:6),]
  # # Aplicar la función a todos los enlaces
  # resultado <- map_dfr(df_especies2$Link, extraer_info)
  #
  # # Unir con nombres si quieres
  # df_final <- df_especies2 %>%
  #   left_join(resultado, by = "Link")
  # View(df_final)
  #
  #
  #
  #
  #
  #
  #
  #
  # enlace1 <- "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e08.htm#register%20of%20international%20introductions%20of%20inland%20aquatic%20species%20(a%20%20%20b)"
  # enlace2 <- "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e09.htm#register%20of%20international%20introductions%20of%20inland%20aquatic%20species%20%C2%A9"
  # enlace3 <- "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0a.htm#register%20of%20international%20introductions%20of%20inland%20aquatic%20species%20(d%20%20%20e%20%20%20f%20%20%20g"
  # enlace4 <- "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0b.htm#register%20of%20international%20introductions%20of%20inland%20aquatic%20species%20(i%20%20%20l%20%20%20m)"
  # enlace5 <- "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0c.htm#register%20of%20international%20introductions%20of%20inland%20aquatic%20species%20(n%20%20%20o)"
  # enlace6 <- "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0d.htm#register%20of%20international%20introductions%20of%20inland%20aquatic%20species%20(p%20%20%20r)"
  # enlace7 <- "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0e.htm#register%20of%20international%20introductions%20of%20inland%20aquatic%20species%20(s)"
  # enlace8 <- "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0f.htm#register%20of%20international%20introductions%20of%20inland%20aquatic%20species%20(t%20%20%20u%20%20%20x)"
  #
  # res1 <- read_html(enlace1)
  # # Extraer nombres de especies (ejemplo, adaptar selector CSS)
  # nombres <- res1 %>% html_nodes("h2") %>% html_text()
  # nombres <- nombres[grepl(" ", nombres)]
  # length(nombres)
  #
  # # Extraer descripciones (ejemplo, adaptar selector CSS)
  # descripciones <- res1 %>% html_nodes("p") %>% html_text()
  # descripciones <- descripciones[grepl("Native range", descripciones)]
  # length(descripciones)
  #
  # dataset <- cbind(nombres, descripciones)
  #
}




