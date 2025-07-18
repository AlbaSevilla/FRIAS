Prepare_Datasets_ToErrorsInHabitatClasifications <- function (FileInfo){

  # Lista de archivos
  data_files <- list.files(path = "Output/Intermediate", pattern = "^Step1_Prepared.*\\.csv$", full.names = TRUE)

  # Crear la carpeta 'Check' si no existe
  output_dir <- "Output/Check/CheckHabitat"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  # Recorremos todos los archivos en la lista
  for (file_path in data_files) {

    # 1. Cargar el dataset utilizando read.csv
    dat <- read.csv(file_path, sep=";")

    # Verificar si la columna 'habitat' existe en el dataset
    if ("habitat" %in% colnames(dat)) {

      # 2. Extraer el nombre de la base de datos del nombre del archivo
      db_name <- file_path %>%
        str_extract("Step1_Prepared_.*?\\.csv") %>%
        str_remove_all("Step1_Prepared_|\\.csv")

      # 3. Procesar la columna habitat
      dat <- dat %>%
        mutate(habitat = paste0(habitat, "(", db_name, ")")  # Añadir el nombre de la base de datos
        )

      dat <- dat[,c("OriginalNameDB", "AcceptedNameGBIF","habitat", "Source_Date")]

      # 4. Crear el nuevo nombre de archivo para guardar en la carpeta de destino
      new_file_path <- file.path(output_dir, basename(file_path))

      # 5. Guardar el archivo modificado en la nueva carpeta
      write.csv2(dat, new_file_path, row.names = FALSE)

      # Mensaje de progreso
      cat("✅ Procesado y guardado:", new_file_path, "\n")

    } else {
      # Si la columna habitat no existe, mostrar un mensaje de advertencia
      cat("⚠️ La columna 'habitat' no se encuentra en:", file_path, "\n")
    }
  }


  ##############################################################
  ###########         MERGE DATABASES           ################
  ##############################################################

  ruta_intermediate <- "Output/Check/CheckHabitat"
  if (!dir.exists(ruta_intermediate)) dir.create(ruta_intermediate, recursive = TRUE)

  data_files <- list.files(path = ruta_intermediate, pattern = "^Step1_Prepared.*\\.csv$", full.names = TRUE)
  db_names <- sub("^.*Step1_Prepared_(.*)\\.csv$", "\\1", data_files)

  cat("Uniendo bases de datos \n")
  for (i in seq_along(db_names)) {
    cat("Procesando la base de datos: ", db_names[i], "\n")
    dat <- read.csv(data_files[i], sep = ";", stringsAsFactors = FALSE)

    if (i == 1) {
      alldat <- dat
    } else {
      alldat <- merge(alldat, dat, by = "OriginalNameDB", all = TRUE)

      # Resolver columnas duplicadas con ".x" y ".y"
      while (any(grepl("\\.y$", names(alldat)))) {
        dupl_base <- sub("\\.y$", "", grep("\\.y$", names(alldat), value = TRUE))

        for (col in dupl_base) {
          col_x <- paste0(col, ".x")
          col_y <- paste0(col, ".y")

          if (col == "eventDate") {
            alldat[[col]] <- apply(alldat[, c(col_x, col_y)], 1, function(x) {
              x <- as.numeric(x)
              if (all(is.na(x))) NA else min(x, na.rm = TRUE)
            })
          } else {
            alldat[[col]] <- paste(alldat[[col_x]], alldat[[col_y]], sep = "; ")
          }

          alldat[[col]] <- gsub("; NA|NA;|NA", "", alldat[[col]])
          alldat[[col]] <- gsub("^; |; $", "", alldat[[col]])
          alldat[[col]] <- trimws(alldat[[col]])
        }

        # Eliminar columnas originales duplicadas
        alldat <- alldat[ , !grepl("\\.x$|\\.y$", names(alldat))]
        numero_especies <- nrow(alldat)
      }
    }
  }

  cat("Limpiando valores duplicados dentro de las celdas \n")
  # Limpieza de valores duplicados dentro de celdas
  clean_cells <- function(col, sep = ";") {
    sapply(strsplit(as.character(col), paste0("\\s*", sep, "\\s*")), function(x) {
      x <- unique(trimws(x))
      x <- x[x != "" & x != "NA"]
      paste(x, collapse = sep)
    })
  }

  alldat <- as.data.frame(lapply(alldat, clean_cells), stringsAsFactors = FALSE)

  cat("Procesando primeros registros \n")
  # Procesar OldestDate
  if ("OldestDate" %in% names(alldat)) {
    alldat$OldestDate <- sapply(as.character(alldat$OldestDate), function(x) {
      if (is.na(x) || x == "") return("")
      years <- as.numeric(unlist(strsplit(x, ";\\s*")))
      years <- years[!is.na(years)]
      if (length(years) == 0) "" else min(years)
    })
  }

  # Reemplazar separadores y volver a limpiar
  alldat[] <- lapply(alldat, function(x) {
    x <- gsub(";", ",", x)
    sapply(strsplit(x, ","), function(y) {
      y <- unique(trimws(y))
      y <- y[y != "" & y != "NA"]
      paste(y, collapse = ";")
    })
  })
  ######
  cat("Colapsando filas por AcceptedNameGBIF \n")
  alldat <- read.csv("Output/Check/CheckHabitat/CheckMasterList_Habitat.csv")
  # Separar registros con y sin AcceptedNameGBIF
  AllDat_sinNA <- alldat[!is.na(alldat$AcceptedNameGBIF) & alldat$AcceptedNameGBIF != "", ]
  AllDat_NA <- alldat[is.na(alldat$AcceptedNameGBIF) | alldat$AcceptedNameGBIF == "", ]

  # Colapsar por AcceptedNameGBIF
  collapse_fun <- function(x) {
    x <- unique(unlist(strsplit(x, ";")))
    x <- trimws(x[x != "" & x != "NA"])
    paste(x, collapse = ";")
  }

  # Aplicar la función a cada grupo
  collapsed <- aggregate(. ~ AcceptedNameGBIF, data = AllDat_sinNA, FUN = collapse_fun)

  # Reagregar filas sin AcceptedNameGBIF
  final_data <- rbind(collapsed, AllDat_NA)

  # Opcional: reordenar si quieres dejar primero las filas con nombre
  final_data <- final_data[order(final_data$AcceptedNameGBIF != "")&!is.na(final_data$AcceptedNameGBIF), ]

  # Verifica el resultado
  cat("Total de registros después de colapsar: ", nrow(final_data), "\n")
  write.csv(final_data, file.path("Output/Check/CheckHabitat", "CheckMasterList_Habitat.csv"), row.names = FALSE)
  write_xlsx(final_data, file.path("Output/Check/CheckHabitat", "CheckMasterList_Habitat.xlsx"))


}
