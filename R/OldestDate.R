OldestDate <- function(df, columna_fechas) {
  # 2. Reemplazar todo lo que NO sea número ni ; por ;
  df[[columna_fechas]] <- gsub("[^0-9;]+", ";", df[[columna_fechas]])

  # 3. Ordenar los años dentro del campo
  df[[columna_fechas]] <- sapply(strsplit(df[[columna_fechas]], ";"), function(x) {
    x <- trimws(x)
    x <- x[x != ""]              # eliminar vacíos
    x_sorted <- sort(x)
    paste(x_sorted, collapse = ";")
  })

  # 4. Eliminar punto y coma inicial si aparece
  df[[columna_fechas]] <- sub("^;", "", df[[columna_fechas]])

  # 5. Obtener el primer año (más antiguo)
  df$OldestDate <- sapply(strsplit(df[[columna_fechas]], ";"), function(x) {
    x <- trimws(x)
    x <- x[x != ""]  # eliminar vacíos
    if (length(x) == 0 || is.na(x[1]) || x[1] == "") {
      NA
    } else {
      x[1]
    }
  })

  return(df)
}
