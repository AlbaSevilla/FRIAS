colapse_through_AcceptedNameGBIF <- function(df, colkey = "AcceptedNameGBIF", sep = ";") {
  if (!(colkey %in% names(df))) {
    stop(paste("Column ", colkey, "doesn't exists in this dataframe"))
  }
  rows_with_acceptedname <- df[!is.na(df[[colkey]]) & trimws(df[[colkey]]) != "", ]
  rows_without_acceptedname <- df[is.na(df[[colkey]]) | trimws(df[[colkey]]) == "", ]
  groups <- split(rows_with_acceptedname, rows_with_acceptedname[[colkey]])
  final_result <- lapply(groups, function(group) {
    sapply(group, function(col) {
      elements <- unlist(strsplit(as.character(col), paste0("\\s*", sep, "\\s*")))
      elements <- unique(trimws(elements[elements != "" & elements != "NA"]))
      paste(elements, collapse = sep)
    }, simplify = FALSE)
  })
  final_result2 <- as.data.frame(do.call(rbind, final_result), stringsAsFactors = FALSE)
  final_result3 <- rbind(final_result2, rows_without_acceptedname)
  rownames(final_result3) <- NULL
  return(final_result3)
}
