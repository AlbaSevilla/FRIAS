department_of_conservation_new_zealand_2025_Download_and_Depurate <- function(){

  url <- "https://www.doc.govt.nz/nature/pests-and-threats/freshwater-pests/other-freshwater-pest-species/"
  res <- read_html(url)
  tags_a <- html_elements(res, "a")
  tags_a_html <- as.character(tags_a)
  enlaces_validos <- tags_a[grepl("<em>", tags_a_html)]

  #Obtenemos los nombres científicos:
  nombres_cientificos <- sapply(enlaces_validos, function(tag) {
    em_nodo <- html_element(tag, "em")
    html_text(em_nodo)
  })
  nombres_cientificos <- as.data.frame(nombres_cientificos)

  #Obtenemos los enlaces de las especies
  enlaces_especies <- html_attr(enlaces_validos, "href")
  enlaces_especies <- as.character(enlaces_especies)
  enlaces_especies <- as.data.frame(enlaces_especies)


  Dataset <- cbind(nombres_cientificos, enlaces_especies)

  resultados <- data.frame(
    Species = character(),
    Origin = character(),
    Habitat = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(Dataset))) {
    nombres_cientificos <- Dataset$nombres_cientificos[i]
    url <- Dataset$enlaces_especies[i]
    cat("Procesando", i, "/", nrow(Dataset), ":", nombres_cientificos, "\n")

    # Intentar leer el HTML de la ficha
    pagina <- tryCatch(read_html(url), error = function(e) return(NA))
    if (is.na(pagina)) next

    # Extraer todos los textos visibles
    texto <- pagina %>% html_text()
    lineas <- unlist(strsplit(texto, "\n"))
    lineas <- trimws(lineas)

    # --- ORIGIN ---
    linea_origin <- grep("Origin:", lineas, value = TRUE)
    if (length(linea_origin)) {
      origin <- sub(".*Origin:\\s*", "", linea_origin[1])
    } else {
      h4s <- html_elements(pagina, "h4")
      h4_origin <- h4s[html_text(h4s) %in% c("Origin", "Where is it originally from?")]

      origin <- NA
      if (length(h4_origin)) {
        for (header in h4_origin) {
          siguiente <- html_node(header, xpath = "following-sibling::*[1]")
          if (!is.na(siguiente) && length(siguiente) > 0) {
            texto <- html_text(siguiente)
            if (nchar(texto) > 0) {
              origin <- texto
              break
            }
          }
        }
      }
    }


    # --- HABITAT ---
    linea_habitat <- grep("Habitats:", lineas, value = TRUE)
    if (length(linea_habitat)) {
      habitat <- sub(".*Habitats:\\s*", "", linea_habitat[1])
    } else {
      h4s <- html_elements(pagina, "h4")
      h4_habitat <- h4s[html_text(h4s) %in% c("Habitat", "Which habitats is it likely to invade?")]

      h2s <- html_elements(pagina, "h2")
      h2_habitat <- h2s[html_text(h2s) == "Habitats"]

      header_habitat <- c(h4_habitat, h2_habitat)

      habitat <- NA  # Por defecto

      if (length(header_habitat)) {
        for (header in header_habitat) {
          siguiente <- html_node(header, xpath = "following-sibling::*[1]")
          if (!is.na(siguiente) && length(siguiente) > 0) {
            texto <- html_text(siguiente)
            if (nchar(texto) > 0) {
              habitat <- texto
              break  # Nos quedamos con el primero válido
            }
          }
        }
      }
    }


    # Agregar al data.frame
    resultados <- rbind(resultados, data.frame(
      Species = nombres_cientificos,
      Origin = origin,
      Habitat = habitat,
      stringsAsFactors = FALSE
    ))
  }

  resultados$Invaded_country <- "New Zealand"
  resultados$Habitat_Database <- "Freshwater"

  #GUARDAMOS
  write.xlsx(resultados, "Inputfiles/originaldatabase_department_of_conservation_new_zealand_2025.xlsx")
  write.xlsx(resultados, "Inputfiles/freshwatersubset_department_of_conservation_new_zealand_2025.xlsx")
}
