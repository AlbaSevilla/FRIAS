saveLocations <- function(text) {
  words <- unlist(strsplit(text, " "))
  filter_words <- words[grepl("^[A-Z]", words)]
  result <- paste(filter_words, collapse = " ")
  return(result)
}
