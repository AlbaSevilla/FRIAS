ConservarLocalizaciones <- function(texto) {
  # Separa el texto en palabras (usando espacios como separador)
  palabras <- unlist(strsplit(texto, " "))

  # Filtra palabras que empiezan con mayúscula
  palabras_filtradas <- palabras[grepl("^[A-Z]", palabras)]

  # Une las palabras filtradas en un solo string, separadas por espacio
  resultado <- paste(palabras_filtradas, collapse = " ")

  return(resultado)
}
#
# source(file.path("R", "ConservarLocalizaciones.r"))
# dataset$Native_Range <- sapply(dataset$Native_Range, ConservarLocalizaciones)
# dataset$Native_Range
