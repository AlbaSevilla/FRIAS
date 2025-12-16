noduplicates <- function(data, column_name_species) {
  if (!(column_name_species %in% colnames(data))) {
    stop(paste("Column with species name", column_name_species, "doesn't exist on dataset"))
  }
  columns <- setdiff(names(data), column_name_species)
  data %>%
    dplyr::group_by(across(all_of(column_name_species))) %>%
    dplyr::summarise(across(all_of(columns), ~ {
      text <- as.character(.)
      text <- str_remove_all(text, "https://doi[^[:space:]]+")
      vals <- unlist(str_split(text, "[,;|]+|\\s{2,}"))
      vals <- str_trim(vals)
      vals <- vals[vals != ""]
      vals <- unique(vals)
      paste(vals, collapse = ", ")
    }), .groups = "drop")
}
