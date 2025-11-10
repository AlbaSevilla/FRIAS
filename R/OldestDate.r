OldestDate <- function(database, dates_column) {
  database[[dates_column]] <- gsub("[^0-9;]+", ";", database[[dates_column]])

  database[[dates_column]] <- sapply(strsplit(database[[dates_column]], ";"), function(x) {
    x <- trimws(x)
    x <- x[x != ""]
    x_sorted <- sort(x)
    paste(x_sorted, collapse = ";")
  })

  database[[dates_column]] <- sub("^;", "", database[[dates_column]])

  database$OldestDate <- sapply(strsplit(database[[dates_column]], ";"), function(x) {
    x <- trimws(x)
    x <- x[x != ""]  # eliminar vacíos
    if (length(x) == 0 || is.na(x[1]) || x[1] == "") {
      NA
    } else {
      x[1]
    }
  })

  return(database)
}
