convertCSVtoExcelINTERMEDIATE <- function() {

  ##############################################################
  # Verificamos si la carpeta 'OutputFiles/Intermediate' existe #####
  ##############################################################
  if (!dir.exists("OutputFiles/Intermediate")) {
    stop("La carpeta 'OutputFiles/Intermediate' no existe.")
  }
  csv_files <- list.files("OutputFiles/Intermediate", pattern = "\\.csv$", full.names = TRUE) # Obtenemos la lista de archivos CSV en la carpeta 'Intermediate'

  # Si no hay archivos CSV, salta un mensaje
  if (length(csv_files) == 0) {
    cat("No se encontraron archivos CSV en la carpeta 'Intermediate'.\n")
    return()
  }

  # Función para detectar el limitador de columnas para cada csv de la base de datos
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
  delimitador_detectado <- sapply(csv_files, detect_delimiter) #Aquí se guarda el limitador de cada archivo csv de la carpeta


  # Convertimos los csv en excel
  for (i in seq_along(csv_files)) {
    excel_file <- sub("\\.csv$", ".xlsx", csv_files[i]) #Aquí cambia la extensión del archivo .csv a .xlsx del nuevo archivo
    dataset <- read.csv(csv_files[i], sep = delimitador_detectado[i])
    write.xlsx(dataset, excel_file) #Guardamos el archivo nuevo .xlsx
    cat("Archivo convertido a Excel:", excel_file, "\n")
  }
}
