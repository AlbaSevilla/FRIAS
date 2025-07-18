InvasiveSpeciesOfJapan_Download_and_Depurate <- function(){
  base_url <- "https://www.nies.go.jp/biodiversity/invasive/IASDB/ni_searchResult_en.php"
  microHabitat <- c("freshwater", "stream", "limnetic", "swamp")

  urls <- paste0(base_url, "?PAGE=", 1:11,
                 paste0("&microHabitat%5B%5D=", microHabitat, collapse = ""))

  all_tables <- list()

  for (i in seq_along(urls)) {
    cat("Procesando página", i, "\n")
    res <- httr::GET(urls[i])
    if (httr::status_code(res) != 200) {
      cat("Error en la página", i, "\n")
      next
    }

    pagina <- tryCatch(read_html(res), error = function(e) NA)
    if (is.na(pagina)) {
      cat("Error leyendo HTML en la página", i, "\n")
      next
    }

    tablas <- tryCatch(html_table(pagina, fill = TRUE), error = function(e) list())
    if (length(tablas) == 0) {
      cat("No se encontró tabla en la página", i, "\n")
      next
    }

    tabla <- tablas[[1]]  # Primera tabla

    # Extraer enlaces fila por fila (en la tabla, un enlace por fila asumiendo estructura)
    filas_html <- pagina %>%
      html_nodes("table") %>%
      .[[1]] %>%
      html_nodes("tr")

    # Sacar enlaces por fila (si existen)
    enlaces_por_fila <- sapply(filas_html, function(tr) {
      a_href <- tr %>% html_nodes("a") %>% html_attr("href")
      if (length(a_href) == 0) return(NA_character_)
      paste(a_href, collapse = "; ")  # si hay varios, separarlos
    })

    # La primera fila normalmente es header, la tabla en R la tiene sin header repetido
    # Así que alineamos enlaces sin la primera fila
    enlaces_por_fila <- enlaces_por_fila[-1]  # quitamos header

    # Si filas y enlaces no coinciden, ajustamos
    if(length(enlaces_por_fila) < nrow(tabla)) {
      enlaces_por_fila <- c(enlaces_por_fila, rep(NA, nrow(tabla) - length(enlaces_por_fila)))
    } else if (length(enlaces_por_fila) > nrow(tabla)) {
      enlaces_por_fila <- enlaces_por_fila[1:nrow(tabla)]
    }

    tabla$links <- enlaces_por_fila

    all_tables[[i]] <- tabla
  }

  # Unimos todas las tablas con enlaces ya alineados
  tabla_completa <- bind_rows(all_tables)

  # Ajustar nombres de columnas si fuera necesario
  colnames(tabla_completa) <- tabla_completa[1,]
  tabla_completa <- tabla_completa[-1,]

  # Agregar columnas fijas
  tabla_completa$Habitat_Database <- "Freshwater"
  tabla_completa$Invaded_country <- "Japan"
  colnames(tabla_completa)[4] <- "Enlaces_especie"

  url <- "https://www.nies.go.jp/biodiversity/invasive/DB/detail/10020e.html"
  res <- read_html(url)
  res2 <- res %>% html_table()
  res3 <- res2[[2]]
  res4 <- res2[[3]]
  res5 <- bind_rows(res2,res3)
  res5 <- as.data.frame(res5)
  # Trasponer
  df_t <- t(res5)

  # Convertir a dataframe y asignar nombres de columnas
  df_t <- as.data.frame(df_t)

  # Los nombres de columnas originales eran filas, podemos renombrar:
  colnames(df_t) <- paste0("V", 1:ncol(df_t))
  colnames(df_t) <- df_t[1,]
  df_t <- df_t[-1,]
  df_t <- df_t[,-1]
  rownames(df_t) <- NULL

  # Función para procesar un solo enlace con índice externo
  procesar_enlace <- function(url) {
    tryCatch({
      res <- read_html(url)
      tablas <- html_table(res, fill = TRUE)

      if (length(tablas) < 3) {
        return(NULL)
      }

      res2 <- tablas[[2]]
      res3 <- tablas[[3]]

      df <- bind_rows(res2, res3)
      df <- as.data.frame(df)

      # Trasponer
      df_t <- t(df)
      df_t <- as.data.frame(df_t)

      colnames(df_t) <- df_t[1, ]
      df_t <- df_t[-1, , drop = FALSE]
      df_t <- df_t[, -1, drop = FALSE]  # Opcional, si la primera columna es redundante
      rownames(df_t) <- NULL

      return(df_t)

    }, error = function(e) {
      cat("Error procesando URL:", url, "\n")
      return(NULL)
    })
  }

  # Iterar con índice para mostrar progreso
  resultados <- list()

  for (i in seq_along(tabla_completa$Enlaces_especie)) {
    cat("Procesando:", i, "/", length(tabla_completa$Enlaces_especie), "\n")
    resultados[[i]] <- procesar_enlace(tabla_completa$Enlaces_especie[i])
  }

  resultados2 <- bind_rows(resultados)
  resultados3 <- as.data.frame(resultados2)
  names(resultados3) <- gsub(" ", "_", names(resultados3))
  columnas_seleccionar<-c("Scientific_name")
  resultados4 <- resultados3[,columnas_seleccionar]
  head(resultados4)

  #No duplicados
  source(file.path("R", "noduplicates.r"))
  dat_noduplicados <- noduplicates(resultados4, "Scientific_name")

  dat_noduplicados$Habitat_Database <- "Freshwater"
  dat_noduplicados$Invaded_Country <- "Japan"
  dat_noduplicados <- dat_noduplicados[-1,]

  write.xlsx(dat_noduplicados, "Inputfiles/Step0_OriginalDatabase_InvasiveSpeciesOfJapan.xlsx")
  write.xlsx(dat_noduplicados, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_InvasiveSpeciesOfJapan.xlsx")

}
