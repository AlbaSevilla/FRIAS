convertCSVtoExcelINPUTFILES <- function() {
  #COMPROBAMOS QUE LA CARPETA INPUTFILES EXISTA Y EN CASO DE QUE NO, QUE SE CREE
  if (!dir.exists("InputFiles")) {
    stop("La carpeta 'InputFiles' no existe.")
  }
  
  # OBTENEMOS LA LISTA DE LOS ARCHIVOS CSV QUE HAY EN LA CARPETA
  csv_files <- list.files("InputFiles", pattern = "\\.csv$", full.names = TRUE)
  
  
  # CADA CSV TIENE UNA FORMA DIFERENTE DE ESTABLECER LOS LIMITADORES DE COLUMNAS, ASÍ QUE OBTENEMOS QUÉ LIMITADOR TIENE CADA CSV
  detect_delimiter <- function(file_path) {
    
    # Lista de delimitadores comunes
    delimitadores <- c(",", ";", "\t", "|")
    
    # Intentar leer el archivo con cada delimitador
    for (delim in delimitadores) {
      # Intentamos leer el archivo con el delimitador actual
      test_data <- tryCatch(
        read.csv(file_path, sep = delim, header = TRUE, nrows = 10), # Leer las primeras 10 filas para prueba
        error = function(e) NULL
      )
      if (!is.null(test_data) && ncol(test_data) > 1) {
        return(delim)
      }
    }
    
    # Si no se detectó un delimitador adecuado
    return(NULL)
  }
  delimitador_detectado <- sapply(csv_files, detect_delimiter) #AQUÍ SE GUARDAN LOS LIMITADORES DE CADA CSV
  
  
  # Si no hay csvs, mostramos un mensaje por pantalla
  if (length(csv_files) == 0) {
    cat("No se encontraron archivos CSV en la carpeta 'InputFiles'.\n")
    return()
  }
  
  # Para los csv que haya, los convertimos en excel teniendo en cuenta el limitador de cada uno
  for (i in seq_along(csv_files)) {
    excel_file <- sub("\\.csv$", ".xlsx", csv_files[i]) #Aquí cambiamos la extensión de cada archivo de csv_files de csv a xlsx
    dataset <- read.csv(csv_files[i], sep = delimitador_detectado[i])
    write.xlsx(dataset, excel_file)
    cat("Archivo convertido a Excel:", excel_file, "\n")
  }
}
