habit_et_al_2015_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  url <- "https://www.revistaecosistemas.net/index.php/ecosistemas/article/download/1008/874/3237"
  destfile <- "InputFiles/originaldatabase_habit_et_al_2015.pdf"
  download.file(url, destfile, mode = "wb")

  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  text_pages <- pdf_text(destfile)[2]
  texto <- text_pages
  cat(texto)
  # Leer líneas
  lineas <- strsplit(texto, "\n")[[1]]

  # Buscar la línea donde empieza la tabla (línea que contiene "ORDEN" y "FAMILIA" y "ESPECIE")
  linea_inicio <- grep("ORDEN.*FAMILIA.*ESPECIE", lineas)[1]

  # Extraer solo desde esa línea hasta el final
  lineas_tabla <- lineas[linea_inicio:length(lineas)]

  # Eliminar líneas vacías
  lineas_tabla <- lineas_tabla[nzchar(lineas_tabla)]

  # Función para separar columnas (como antes)
  parse_linea <- function(linea) {
    c(
      orden = trimws(substr(linea, 1, 22)),
      familia = trimws(substr(linea, 23, 41)),
      especie = trimws(substr(linea, 42, 80)),
      rango_nativo = trimws(substr(linea, 81, 105)),
      presencia = trimws(substr(linea, 106, nchar(linea)))
    )
  }

  # Aplicar a todas las líneas excepto la primera (cabecera)
  datos_raw <- t(sapply(lineas_tabla[-1], parse_linea))

  # Convertir a data.frame
  database <- as.data.frame(datos_raw, stringsAsFactors = FALSE)

  # Rellenar 'orden' y 'familia' vacíos con el valor anterior
  for (col in c("orden", "familia")) {
    for (i in 2:nrow(database)) {
      if (database[i, col] == "") database[i, col] <- database[i - 1, col]
    }
  }

  # Mostrar resultado
  columnas_seleccion <- c("especie","rango_nativo")
  database <- database[,columnas_seleccion]
  rownames(database) <- NULL
  str(database)
  database$rango_nativo <- sub(".*?\\)", "", database$rango_nativo)
  database$especie <- sub("\\(.*", "", database$especie)

  write.xlsx(database, "Inputfiles/originaldatabase_habit_et_al_2015.xlsx")

  #Habitat
  source(file.path("R", "check_habitat.r"))
  dataset <- database
  nombres <- dataset$especie
  nom_acep <- name_backbone_checklist(nombres)$canonicalName
  habitat_dat <- check_habitat(nom_acep, dataset)

  #Freshwater
  freshwater_species <- habitat_dat %>% filter(grepl("FRESHWATER", Habitat))
  freshwater_species$Invaded_country <- "Chile"
  write.xlsx(freshwater_species, "Inputfiles/freshwatersubset_habit_et_al_2015.xlsx")
}
