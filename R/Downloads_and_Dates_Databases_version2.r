Downloads_and_Dates_Databases <- function() {

  path_inputfiles <- file.path("Inputfiles")
  output_dir <- "./TablesToStandardize"
  output_file <- file.path(output_dir, "Dates_of_use_database.xlsx")

  if (!file.exists(output_file)) {
    cat("No existe Dates_of_use_database.xlsx, creando...\n")

      databases <- list.files(
        path = path_inputfiles,
        pattern = "^Step0_OriginalDatabase.*\\.xlsx$",
        full.names = TRUE
      )

      # Quedarse solo con los que contengan "FreshwaterNODUPLICATES"
      databases <- databases[grepl("FreshwaterNODUPLICATES", databases)]

      Dates_Hours <- lapply(databases, function(Database) {
        atime <- file.info(Database)$mtime
        list(
          Database = Database,
          Date = format(atime, "%Y-%m-%d"),
          Hour = format(atime, "%H:%M:%S")
        )
      })

      Dates_Hours_databases <- do.call(rbind, lapply(Dates_Hours, as.data.frame))
      rownames(Dates_Hours_databases) <- NULL

      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      write.xlsx(Dates_Hours_databases, output_file)
      cat("Archivo creado:", output_file, "\n")
  }

  # Leer archivo existente
  dates_db <- read.xlsx(output_file)

  # Leer Downloads_Frequency
  freq_file <- file.path(output_dir, "Downloads_Frequency.xlsx")
  if (!file.exists(freq_file)) {
    stop("No existe el archivo Downloads_Frequency.xlsx")
  }
  freq_db <- read.xlsx(freq_file)


  head(freq_db)
  head(dates_db)
  # Merge (usar solo columnas 1:3 de freq_db)
  merged <- merge(dates_db, freq_db[, 1:3], by = "Database", all.x = TRUE)
  head(merged)

  # Calcular Days_since desde Date
  merged$Days_since <- as.numeric(Sys.Date() - as.Date(merged$Date))

  # Seleccionar las que necesitan descarga
  to_download <- merged[is.na(merged$Days_since) | merged$Days_since >= merged$Frequency_days, ]

  # Ejecutar funciones
  for (i in seq_len(nrow(to_download))) {
    script_name <- to_download$Function[i]

    if (!is.na(script_name)) {
      script_path <- file.path("R", script_name)

      if (file.exists(script_path)) {
        cat("Ejecutando script:", script_path, "para", to_download$Database[i], "\n")

        # Cargar el script
        source(script_path, local = TRUE)

        # Obtener el nombre de la función (sin extensión)
        func_name <- sub("\\.r$", "", script_name, ignore.case = TRUE)

        # Ejecutar la función
        do.call(func_name, list())

        # Actualizar fecha/hora en merged
        merged$Date[merged$Database == to_download$Database[i]] <- as.character(Sys.Date())
        merged$Hour[merged$Database == to_download$Database[i]] <- format(Sys.time(), "%H:%M:%S")
      } else {
        cat("No se encuentra el script:", script_path, "\n")
      }
    } else {
      cat("No se encuentra el script: NA\n")
    }
  }

  # Guardar archivo actualizado (solo Database, Date, Hour)
  write.xlsx(merged[, c("Database", "Date", "Hour")], output_file)
  cat("Archivo actualizado:", output_file, "\n")
}
