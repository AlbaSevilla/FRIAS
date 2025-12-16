van_wilgen_et_al_2022_Download_and_Depurate <- function(){
  #Download
  url <- "https://static-content.springer.com/esm/art%3A10.1007%2Fs10530-021-02623-3/MediaObjects/10530_2021_2623_MOESM1_ESM.pdf"
  destfile <- "InputFiles/originaldatabase_van_wilgen_et_al_2022.pdf"
  download.file(url, destfile, mode = "wb")

  #DEpurate

  # Extraer texto de las primeras 7 páginas
  texto <- pdf_text(destfile)[1:7]

  # Función para extraer tablas de cada página
  extraer_tablas <- function(texto_pdf){
    lineas <- str_split(texto_pdf, "\n")[[1]]
    # Filtrar las líneas que parecen filas de tabla (separadas por espacios múltiples)
    lineas_tabla <- lineas[grepl("\\s{2,}", lineas)]
    # Separar columnas por 2 o más espacios
    tabla <- str_split_fixed(lineas_tabla, "\\s{2,}", 4)
    df <- as.data.frame(tabla, stringsAsFactors = FALSE)
    colnames(df) <- c("Category", "Impact", "Species", "References")
    return(df)
  }

  # Aplicar a todas las páginas
  lista_tablas <- lapply(texto, extraer_tablas)

  # Unir todas las tablas en un solo data.frame
  tabla_final <- do.call(rbind, lista_tablas)

  # --- Rellenar categorías vacías con el valor anterior ---
  tabla_final$Category[tabla_final$Category == ""] <- NA
  tabla_final$References[tabla_final$References == ""] <- NA

  tabla_final <- tabla_final %>% filter(!is.na(References))

  tabla_final <- as.data.frame(tabla_final, stringsAsFactors = FALSE)

  tabla_final <- tabla_final %>% fill(Category, .direction = "down")

  # Ver las primeras filas
  head(tabla_final, 20)
  tabla_final <- tabla_final[-c(1:2),]
  tabla_final <- tabla_final %>% filter(References!="")

}
