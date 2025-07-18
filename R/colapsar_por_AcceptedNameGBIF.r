# colapsar_por_AcceptedNameGBIF <- function(df, clave = "AcceptedNameGBIF", separador = ";") {
#   # Verificar que la columna clave exista
#   if (!(clave %in% names(df))) {
#     stop(paste("La columna", clave, "no existe en el dataframe"))
#   }
#
#   # Filas con valor en la columna clave
#   df_con_valor <- df[!is.na(df[[clave]]) & trimws(df[[clave]]) != "", ]
#
#   # Filas sin valor en la columna clave
#   df_sin_valor <- df[is.na(df[[clave]]) | trimws(df[[clave]]) == "", ]
#
#   # Agrupar por el valor de la clave
#   grupos <- split(df_con_valor, df_con_valor[[clave]])
#
#   # Colapsar cada grupo
#   resultado_colapsado <- lapply(grupos, function(grupo) {
#     # Para cada columna, colapsar valores únicos separados por el separador
#     sapply(grupo, function(col) {
#       elementos <- unlist(strsplit(as.character(col), paste0("\\s*", separador, "\\s*")))
#       elementos <- unique(trimws(elementos[elementos != "" & elementos != "NA"]))
#       paste(elementos, collapse = separador)
#     }, simplify = FALSE)
#   })
#
#   # Combinar todos los resultados en un dataframe
#   df_colapsado <- as.data.frame(do.call(rbind, resultado_colapsado), stringsAsFactors = FALSE)
#
#   # Unir datos colapsados con los que no tenían valor en la clave
#   resultado_final <- rbind(df_colapsado, df_sin_valor)
#   resultado_final[] <- lapply(resultado_final, function(col) {
#     if (is.list(col)) {
#       sapply(col, toString)  # Convierte listas en texto separado por comas
#     } else {
#       col
#     }
#   })
#
#   rownames(resultado_final) <- NULL
#   resultado_final <- as.data.frame(resultado_final)
#   return(resultado_final)
# }
colapsar_por_AcceptedNameGBIF <- function(df, clave = "AcceptedNameGBIF", separador = ";") {
  # Verificar que la columna clave exista
  if (!(clave %in% names(df))) {
    stop(paste("La columna", clave, "no existe en el dataframe"))
  }

  # Normalizar valores vacíos y "NA" como texto a NA real
  df[] <- lapply(df, function(col) {
    col[trimws(col) == "" | tolower(trimws(col)) == "na"] <- NA
    return(col)
  })

  # Filas con valor en la columna clave
  df_con_valor <- df[!is.na(df[[clave]]), ]

  # Filas sin valor en la columna clave
  df_sin_valor <- df[is.na(df[[clave]]), ]

  # Agrupar por el valor de la clave
  grupos <- split(df_con_valor, df_con_valor[[clave]])

  # Colapsar cada grupo
  resultado_colapsado <- lapply(grupos, function(grupo) {
    sapply(grupo, function(col) {
      elementos <- unlist(strsplit(as.character(col), paste0("\\s*", separador, "\\s*")))
      elementos <- unique(trimws(elementos[!is.na(elementos) & elementos != ""]))
      paste(elementos, collapse = separador)
    }, simplify = FALSE)
  })

  # Combinar todos los resultados en un dataframe
  df_colapsado <- as.data.frame(do.call(rbind, resultado_colapsado), stringsAsFactors = FALSE)

  # Unir datos colapsados con los que no tenían valor en la clave
  resultado_final <- rbind(df_colapsado, df_sin_valor)

  # Convertir listas a texto plano si fuera necesario
  resultado_final[] <- lapply(resultado_final, function(col) {
    if (is.list(col)) sapply(col, toString) else col
  })

  rownames(resultado_final) <- NULL
  return(as.data.frame(resultado_final, stringsAsFactors = FALSE))
}
