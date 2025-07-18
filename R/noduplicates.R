noduplicates <- function(data, column_name_species) {
  # Verifica que la columna exista
  if (!(column_name_species %in% colnames(data))) {
    stop(paste("La columna", column_name_species, "no existe en el dataframe."))
  }

  # Columnas a procesar (excluye la columna de agrupamiento)
  cols_to_process <- setdiff(names(data), column_name_species)

  data %>%
    dplyr::group_by(across(all_of(column_name_species))) %>%
    dplyr::summarise(across(all_of(cols_to_process), ~ {
      texto <- as.character(.)
      texto <- str_remove_all(texto, "https://doi[^[:space:]]+")
      vals <- unlist(str_split(texto, "[,;|]+|\\s{2,}"))
      vals <- str_trim(vals)
      vals <- vals[vals != ""]
      vals <- unique(vals)
      paste(vals, collapse = ", ")
    }), .groups = "drop")
}
