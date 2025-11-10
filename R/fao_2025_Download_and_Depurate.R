fao_2025_Download_and_Depurate <- function(){
  # DESCARGAR
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

  dataset <- dataset_unificado %>%
    mutate(
      descripcion = str_extract_all(descripcion, "\\b[A-Z][a-z]+\\b"),
      descripcion = lapply(descripcion, function(x) unique(x)),
      descripcion = sapply(descripcion, function(x) paste(x, collapse = ", "))
    )

  write_xlsx(dataset, "Inputfiles/originaldatabase_fao_2025.xlsx")
  write_xlsx(dataset, "Inputfiles/freshwatersubset_fao_2025.xlsx")

}




