Habitat_DB_vs_GBIF <- function(){

  #Primero tenemos que obtener los archivos prepared con una columna nueva indicando el dataset del que salen esos resultados
  data_files <- list.files(
    path = "OutputFiles/Intermediate",
    pattern = "^Step1_Prepared.*\\.csv$",
    full.names = TRUE
  )

  #Vamos a excluir aquellas bases de datos de las que desde un principio hemos obtenido su habitat a través de Gbif, no tiene sentido incluirlas en la comparación
  excluir_nombres <- c(
    "Step1_Prepared_AmphibiansAndReptilesCesarCapinha.csv",
    "Step1_Prepared_CEEEI.csv",
    "Step1_Prepared_FirstRecords.csv",
    "Step1_Prepared_Global_Horizon_Scanning_IUCN.csv",
    "Step1_Prepared_GloNAF.csv",
    "Step1_Prepared_INVASIBER.csv",
    "Step1_Prepared_LIFEINVASAQUACANOBARBACID_ALERTLIST.csv",
    "Step1_Prepared_LIFEINVASAQUACANOBARBACID_BLACKLIST.csv",
    "Step1_Prepared_NISIC.csv",
    "Step1_Prepared_NONNATIVEFISHESINDIALOLITHKUMAR.csv",
    "Step1_Prepared_USRIIS.csv",
    "Step1_Prepared_InvasiveNonNativeSpeciesInBrazilRafaelZenni.csv"
  )
  nombres_archivos <- basename(data_files)
  data_files <- data_files[!(nombres_archivos %in% excluir_nombres)]


  #PROCEDEMOS A LA CREACIÓN DE LOS ARCHIVOS STEP1_PREPARED..... EN LA CARPETA CHECK/CHECKHABITAT
  OutputFiles_dir <- "OutputFiles/Check/CheckHabitat"
  if (!dir.exists(OutputFiles_dir)) {
    dir.create(OutputFiles_dir)
  }

  for (file_path in data_files) {
    dat <- read.csv(file_path, sep=";")
    if ("habitat" %in% colnames(dat)) {
      db_name <- file_path %>%
        str_extract("Step1_Prepared_.*?\\.csv") %>%
        str_remove_all("Step1_Prepared_|\\.csv")
      dat <- dat %>%
        mutate(habitat = paste0(habitat, "(", db_name, ")")  # Añadir el nombre de la base de datos
        )
      dat <- dat[,c("OriginalNameDB", "AcceptedNameGBIF","habitat", "Source_Date")]
      names(dat)[names(dat) == "Source_Date"] <- "Source_Data"

      new_file_path <- file.path(OutputFiles_dir, basename(file_path))
      write.csv2(dat, new_file_path, row.names = FALSE)

      cat("Procesado y guardado:", new_file_path, "\n")

    } else {
      # Si la columna habitat no existe, mostrar un mensaje de advertencia
      cat("La columna del Habitat no se encuentra en:", file_path, "\n")
    }
  }


  ##############################################################
  ###########         MERGE DATABASES           ################
  ##############################################################

  ruta_intermediate <- "OutputFiles/Check/CheckHabitat"
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
  clean_cells_puntocoma <- function(col, sep = ";") {
    sapply(strsplit(as.character(col), paste0("\\s*", sep, "\\s*")), function(x) {
      x <- unique(trimws(x))
      x <- x[x != "" & x != "NA"]
      paste(x, collapse = sep)
    })
  }

  alldat <- as.data.frame(lapply(alldat, clean_cells_puntocoma), stringsAsFactors = FALSE)

  clean_cells_coma <- function(col, sep = ",") {
    sapply(strsplit(as.character(col), paste0("\\s*", sep, "\\s*")), function(x) {
      x <- unique(trimws(x))
      x <- x[x != "" & x != "NA"]
      paste(x, collapse = sep)
    })
  }

  alldat <- as.data.frame(lapply(alldat, clean_cells_coma), stringsAsFactors = FALSE)

  source(file.path("R", "colapsar_por_AcceptedNameGBIF.r"))
  alldat <- colapsar_por_AcceptedNameGBIF(alldat)
  write.csv(alldat, file.path("OutputFiles/Check/CheckHabitat", "CheckMasterList_Habitat.csv"), row.names = FALSE)
  write_xlsx(alldat, file.path("OutputFiles/Check/CheckHabitat", "CheckMasterList_Habitat.xlsx"))


  ############################################################################################################
  ############################################################################################################
  ############################################################################################################
  ############################################################################################################
  Masterlist <- read_excel("OutputFiles/Check/CheckHabitat/CheckMasterList_Habitat.xlsx")

  #############################################################
  ######### OBTENCION HABITAT #################################
  #############################################################
  source(file.path("R", "check_habitat.r"))
  #Obtain habitats
  dataset <- Masterlist
  especies_lista <- dataset$AcceptedNameGBIF
  dataset_actualizado <- check_habitat(especies_lista, dataset)


  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat) | is.na(Habitat))

  dataset_no_freshwater <- dataset_actualizado %>%
    filter(!grepl("FRESHWATER", Habitat) & !is.na(Habitat))

  ###### GUARDAMOS
  write.xlsx(dataset_actualizado, "./OutputFiles/Check/CheckHabitat/HabitatwithFunctionMasterList2.xlsx")
  write.xlsx(dataset_freshwater, "./OutputFiles/Check/CheckHabitat/FreshwaterHabitatwithFunctionMasterList2.xlsx")
  write.xlsx(dataset_no_freshwater, "./OutputFiles/Check/CheckHabitat/No_FreshwaterHabitatwithFunctionMasterList2.xlsx")



  #COMPROBAR
  #tabla_resultados <- name_lookup(query = "Abbottina rivularis", rank = "species", habitat = "freshwater")$data
  #especie_habitat <- tabla_resultados$habitats
  #Si hay NA em especie_habitat es porque GBIF no tiene registros, por tanto ponemos que en la columna Habitat ponga 'no hay registros en gbif para contrastar'
  #tabla_resultados2 <- tabla_resultados %>% filter(canonicalName == "Aedes polynesiensis")
  #tabla_resultados2$habitats
  #View(tabla_resultados)


  ###########################################################################
  ######### ANÁLISIS ########################################################
  ###########################################################################
  CheckHabitatMasterlist <- read_excel("OutputFiles/Check/CheckHabitat/No_FreshwaterHabitatwithFunctionMasterList2.xlsx")
  CheckHabitatMasterlist_subset <- CheckHabitatMasterlist %>%
    select(OriginalNameDB, AcceptedNameGBIF, habitat, Habitat)
  CheckHabitatMasterlist_subset <- CheckHabitatMasterlist_subset %>%
    separate_rows(habitat, sep = ";")
  CheckHabitatMasterlist_subset$Source_Data <- sub(".*\\(([^)]+)\\).*", "\\1", CheckHabitatMasterlist_subset$habitat)

  counts <- as.data.frame(table(CheckHabitatMasterlist_subset$Source_Data))
  colnames(counts) <- c("Source_Data", "Freq")
  counts <- counts[order(-counts$Freq), ]


  CheckHabitatMasterlist_subset$Source_Data <- factor(CheckHabitatMasterlist_subset$Source_Data,
                                                      levels = counts$Source_Data)

  levels(CheckHabitatMasterlist_subset$Source_Data)[
    levels(CheckHabitatMasterlist_subset$Source_Data) == "COBIO"
  ] <- "CONABIO"
  data_freq <- CheckHabitatMasterlist_subset %>%
    dplyr::count(Source_Data)

  ggplot(data_freq, aes(x = reorder(Source_Data, -n), y = n, fill = n)) +
    geom_col() +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) +  # direction = 1 → colores fuertes para valores altos
    theme_minimal() +
    labs(title = "Species incoherences habitat between Databases and GBIF",
         x = "Source_Data",
         y = "Rows' number") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####################################################################
######### OBTENER TABLAS DE CONTINGENCIA ############################
#####################################################################
  # CheckHabitatMasterlist <- read_excel("OutputFiles/Check/CheckHabitat/HabitatwithFunctionMasterList2.xlsx")
  # CheckHabitatMasterlist_subset <- CheckHabitatMasterlist %>%
  #   select(OriginalNameDB, AcceptedNameGBIF, habitat, Habitat)
  # CheckHabitatMasterlist_subset <- CheckHabitatMasterlist_subset %>%
  #   separate_rows(habitat, sep = ";")
  # CheckHabitatMasterlist_subset$Source_Data <- sub(".*\\(([^)]+)\\).*", "\\1", CheckHabitatMasterlist_subset$habitat)
  # write.xlsx(CheckHabitatMasterlist_subset, "./OutputFiles/Check/CheckHabitat/SeparatedFreshwaterHabitatwithFunctionMasterList2.xlsx")
  #
  #
  # counts <- as.data.frame(table(CheckHabitatMasterlist_subset$Source_Data))
  # colnames(counts) <- c("Source_Data", "Freq")
  # counts <- counts[order(-counts$Freq), ]
  #
  #
  # CheckHabitatMasterlist_subset$Source_Data <- factor(CheckHabitatMasterlist_subset$Source_Data,
  #                                                     levels = counts$Source_Data)
  #
  # remove_duplicates <- function(label) {
  #   label <- as.character(label)
  #   parts <- unlist(strsplit(label, "\\s*/\\s*|\\s+"))  # divide por espacio o slash
  #   unique_parts <- unique(parts[parts != ""])  # quitar vacíos
  #   cleaned_label <- paste(unique_parts, collapse = " ")
  #   return(cleaned_label)
  # }
  #
  # # Obtener todas las categorías únicas de Source_Data
  # Source_Datas <- unique(CheckHabitatMasterlist_subset$Source_Data)
  #
  # # Iterar sobre cada categoría
  # for (Source_Data in Source_Datas) {
  #
  #   # Filtrar los datos según el Source_Data actual
  #   dataset <- CheckHabitatMasterlist_subset %>%
  #     filter(Source_Data == Source_Data)
  #
  #   # Crear la tabla de contingencia entre 'habitat' y 'Habitat'
  #   tabla <- table(Habitat = dataset$habitat, GBIF_Group = dataset$Habitat)
  #   tabla_df <- as.data.frame.matrix(tabla)
  #
  #   # Imprimir la tabla como verificación
  #   cat("\n==== Tabla para:", Source_Data, "====\n")
  #   print(tabla_df)
  #
  #   # Convertir a formato largo para ggplot2
  #   tabla_long <- reshape2::melt(as.matrix(tabla_df))
  #   colnames(tabla_long) <- c("Habitat", "GBIF_Group", "Freq")
  #
  #   # Limpiar etiquetas duplicadas
  #   tabla_long$Habitat <- sapply(tabla_long$Habitat, remove_duplicates)
  #   tabla_long$GBIF_Group <- sapply(tabla_long$GBIF_Group, remove_duplicates)
  #
  #   # Truncar etiquetas largas para evitar solapamientos
  #   tabla_long$Habitat <- str_trunc(tabla_long$Habitat, 30)
  #   tabla_long$GBIF_Group <- str_trunc(tabla_long$GBIF_Group, 30)
  #
  #   # Crear el heatmap
  #   p <- ggplot(tabla_long, aes(x = GBIF_Group, y = Habitat, fill = Freq)) +
  #     geom_tile(color = "white") +
  #     geom_text(aes(label = Freq), color = "black", size = 4) +
  #     scale_fill_gradient(low = "white", high = "orange") +
  #     labs(title = paste("Mapa de calor:", Source_Data),
  #          x = "GBIF Group", y = "Habitat") +
  #     theme_minimal() +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
  #           axis.text.y = element_text(size = 8)) +
  #     coord_fixed()
  #
  #   # Mostrar el gráfico
  #   print(p)
  # }
  #
  # #VALE PUES AHORA VAMOS A CALCULAR EL PORCENTAJE DE FALLOS Y ACIERTOS:
  #
  # # Filtrar datos por 'Source_Data' para este ciclo
  # attach(CheckHabitatMasterlist_subset)
  #
  # # Obtener los valores únicos de Source_Data
  # # Instalar y cargar el paquete writexl si no lo tienes instalado
  # # install.packages("writexl")
  # library(writexl)
  #
  # # Definir el archivo de salida Excel
  # OutputFiles_file <- "OutputFiles/Check/CheckHabitat/resultado_tablas.xlsx"
  #
  # # Lista para almacenar las tablas
  # tablas <- list()
  #
  # # Obtener los valores únicos de Source_Data
  # Source_Datas <- unique(CheckHabitatMasterlist_subset$Source_Data)
  #
  # # Iterar sobre cada valor de Source_Data
  # for (Source_Data in Source_Datas) {
  #
  #   # Filtrar el dataset por Source_Data
  #   dataset_filtered1 <- CheckHabitatMasterlist_subset %>%
  #     filter(Source_Data == Source_Data)
  #
  #   # Separar los valores en la columna 'habitat'
  #   dataset_filtered <- dataset_filtered1
  #
  #   # Obtener niveles únicos de habitat que contienen "freshwater"
  #   niveles_habitat <- unique(dataset_filtered$habitat[grepl("freshwater", dataset_filtered$habitat, ignore.case = TRUE)])
  #
  #   # Inicializar vectores para almacenar resultados
  #   habitat_vec <- c()
  #   freshwater_by_db_vec <- c()
  #   no_freshwater_by_gbif_vec <- c()
  #   porcentaje_incorrecto_vec <- c()
  #
  #   # Iterar por cada nivel de habitat
  #   for (nivel in niveles_habitat) {
  #
  #     # Filtrar registros para este nivel de hábitat
  #     registros_nivel <- dataset_filtered[dataset_filtered$habitat == nivel, ]
  #
  #     # Calcular total y registros sin 'FRESHWATER' en la columna Habitat
  #     total <- nrow(registros_nivel)
  #     no_fresh <- sum(!grepl("FRESHWATER", registros_nivel$Habitat, ignore.case = TRUE))
  #
  #     # Calcular el porcentaje de registros incorrectos
  #     porcentaje <- if (total > 0) round(100 * no_fresh / total, 2) else NA
  #
  #     # Guardar los resultados en los vectores
  #     habitat_vec <- c(habitat_vec, nivel)
  #     freshwater_by_db_vec <- c(freshwater_by_db_vec, total)
  #     no_freshwater_by_gbif_vec <- c(no_freshwater_by_gbif_vec, no_fresh)
  #     porcentaje_incorrecto_vec <- c(porcentaje_incorrecto_vec, porcentaje)
  #   }
  #
  #   # Crear el dataframe con los resultados
  #   resultado <- data.frame(
  #     habitat = habitat_vec,
  #     Freshwater_By_db = freshwater_by_db_vec,
  #     No_Freshwater_By_GBIF = no_freshwater_by_gbif_vec,
  #     Porcentaje_Incorrecto_Freshwater = porcentaje_incorrecto_vec
  #   )
  #
  #   # Guardar la tabla en la lista con el nombre de la fuente de datos
  #   tablas[[Source_Data]] <- resultado
  # }
  #
  # # Guardar todas las tablas en un archivo Excel
  # write_xlsx(tablas, path = OutputFiles_file)
  #
  # cat("Las tablas se han guardado correctamente en el archivo:", OutputFiles_file, "\n")

}
