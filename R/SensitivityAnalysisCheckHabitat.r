Habitat_DB_vs_GBIF <- function(){

  #Primero tenemos que obtener los archivos prepared con una columna nueva indicando el dataset del que salen esos resultados
  data_files <- list.files(
    path = "OutputFiles/Intermediate",
    pattern = "^Step1_Prepared.*\\.csv$",
    full.names = TRUE
  )

  #Vamos a excluir aquellas bases de datos de las que desde un principio hemos obtenido su Habitat a través de Gbif, no tiene sentido incluirlas en la comparación
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


  #PROCEDEMOS A LA CREACIÓN DE LOS ARCHIVOS STEP1_PREPARED..... EN LA CARPETA CHECK/CHECKHabitat
  OutputFiles_dir <- "OutputFiles/Check/CheckHabitat"
  if (!dir.exists(OutputFiles_dir)) {
    dir.create(OutputFiles_dir)
  }

  for (file_path in data_files) {
    dat <- read.csv(file_path, sep=";")
    if ("Habitat" %in% colnames(dat)) {
      db_name <- file_path %>%
        str_extract("Step1_Prepared_.*?\\.csv") %>%
        str_remove_all("Step1_Prepared_|\\.csv")
      dat <- dat %>%
        mutate(Habitat = paste0(Habitat, "(", db_name, ")")  # Añadir el nombre de la base de datos
        )
      dat <- dat[,c("OriginalNameDB", "AcceptedNameGBIF","Habitat", "Source_Date")]
      names(dat)[names(dat) == "Source_Date"] <- "Source_Data"

      new_file_path <- file.path(OutputFiles_dir, basename(file_path))
      write.csv2(dat, new_file_path, row.names = FALSE)

      cat("Procesado y guardado:", new_file_path, "\n")

    } else {
      # Si la columna Habitat no existe, mostrar un mensaje de advertencia
      cat("La columna del Habitat no se encuentra en:", file_path, "\n")
    }
  }


  ##############################################################
  ###########         MERGE DATABASES           ################
  ##############################################################

  ruta_intermediate <- "Validation/CheckHabitat"
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
  write.csv(alldat, file.path("Validation/CheckHabitat", "Merged_Habitat.csv"), row.names = FALSE)
  write_xlsx(alldat, file.path("Validation/CheckHabitat", "Merged_Habitat.xlsx"))


  ############################################################################################################
  ############################################################################################################
  ############################################################################################################
  ############################################################################################################
  Masterlist <- read_excel("Validation/CheckHabitat/Merged_Habitat.xlsx") %>%
    select(-Source_Date, -Source_Data)#Hemos creado una columna nueva indicando el nombre de las bases de datos de las que proceden los habitats asignados, sin embargo, no vamos a eliminar los () porque
  #a la hora del plot, se tergiversaba la información al desglosar por habitat, que llevaba a asignar habitats incorrectos a bases de datos que no ofrecian ese dato sino otro.
  #Así que a la hora de hacer el plot, vamos a desglosar por ; en habitat, y ahí si, vamos a obtener la fuente de datos.

  head(Masterlist)

  #############################################################
  ######### OBTENCION Habitat #################################
  #############################################################
  source(file.path("R", "check_habitat.r"))
  #Obtain Habitats
  dataset <- Masterlist
  especies_lista <- dataset$AcceptedNameGBIF
  dataset_actualizado <- check_habitat(especies_lista, dataset)

  dataset_actualizado2 <- dataset_actualizado %>%
    filter(!is.na(Habitat))

  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat) | is.na(Habitat))

  dataset_no_freshwater <- dataset_actualizado %>%
    filter(!grepl("FRESHWATER", Habitat) & !is.na(Habitat))

  ###### GUARDAMOS
  write.xlsx(dataset_actualizado, "./Validation/CheckHabitat/Masterlist_HabitatComparative.xlsx")
  write.xlsx(dataset_freshwater, "./Validation/CheckHabitat/Masterlist_HabitatMatches.xlsx")
  write.xlsx(dataset_no_freshwater, "./Validation/CheckHabitat/Masterlist_HabitatMismatches.xlsx")


  ###############################################################
  #########               PLOT                ###################
  ###############################################################
  CheckHabitatMasterlist <- read_excel("Validation/CheckHabitat/Masterlist_HabitatMismatches.xlsx") %>%
    separate_rows(habitat, sep = ";")
  CheckHabitatMasterlist$Source_Data <- sub(".*\\(([^)]+)\\).*", "\\1", CheckHabitatMasterlist$habitat)
  CheckHabitatMasterlist$Source_Data <- trimws(CheckHabitatMasterlist$Source_Data)

  # Generar tabla de frecuencias
  counts <- as.data.frame(table(CheckHabitatMasterlist$Source_Data))
  colnames(counts) <- c("Source_Data", "Freq")
  counts <- counts[order(-counts$Freq), ]
  counts$Source_Data <- as.character(counts$Source_Data)

  # Reemplazar los nombres de las bases de datos de manera segura
  counts <- counts %>%
    mutate(Source_Data = recode(Source_Data,
                                "GRIIS" = "GRIIS",
                                "EASIN" = "EASIN",
                                "USGS" = "USGS",
                                "InvasiveSpeciesOfJapan" = "List_for_Invasive_Alien_Species_Management_in_Japan",
                                "GIDIAS" = "GIDIAS",
                                "COBIO" = "CONABIO",
                                "CABI" = "CABI",
                                "DptOfConservationNewZealand" = "ONZPR_Official_New_Zealand_Pest_Register",
                                "FRESHWATERECOLOGYINFO" = "Freshwater_EcologyInfo",
                                "AliensInCaves" = "AliensInCaves",
                                "ChenetAll2023" = "Chen_et_al_2023",
                                "InvasiveSpeciesKorea" = "Ryu_et_al_2017",
                                "JELLYFISH" = "Global_literature_database_for_freshwater_jellyfish_research_between_1880_and_2023",
                                .default = NA_character_))  # Valores no listados se vuelven NA

  # Aplicar los mismos nombres al dataset original, reemplazando correctamente
  CheckHabitatMasterlist <- CheckHabitatMasterlist %>%
    mutate(Source_Data = recode(Source_Data,
                                "GRIIS" = "GRIIS",
                                "EASIN" = "EASIN",
                                "USGS" = "USGS",
                                "InvasiveSpeciesOfJapan" = "List_for_Invasive_Alien_Species_Management_in_Japan",
                                "GIDIAS" = "GIDIAS",
                                "COBIO" = "CONABIO",
                                "CABI" = "CABI",
                                "DptOfConservationNewZealand" = "ONZPR_Official_New_Zealand_Pest_Register",
                                "FRESHWATERECOLOGYINFO" = "Freshwater_EcologyInfo",
                                "AliensInCaves" = "AliensInCaves",
                                "ChenetAll2023" = "Chen_et_al_2023",
                                "InvasiveSpeciesKorea" = "Ryu_et_al_2017",
                                "JELLYFISH" = "Global_literature_database_for_freshwater_jellyfish_research_between_1880_and_2023"))
  # Asegurarnos de que Source_Data es carácter
  CheckHabitatMasterlist <- CheckHabitatMasterlist %>%
    mutate(Source_Data = as.character(Source_Data))

  # Generar tabla de frecuencias
  data_freq <- CheckHabitatMasterlist %>%
    dplyr::count(Source_Data) %>%   # usar dplyr::count() para evitar conflictos
    arrange(desc(n))

  # Verificar
  head(data_freq)

  ggplot(data_freq, aes(x = reorder(Source_Data, -n), y = n, fill = n)) +
    geom_col() +
    geom_text(aes(label = n), vjust = -0.5, size = 4) +  # Números encima de las barras
    scale_fill_distiller(palette = "Blues", direction = 1) +
    labs(title = "Freshwater Habitat Mismatches: Databases vs GBIF",
         x = "Data Source",
         y = "Mismatches number") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 20, hjust = 1, size = 13),  # Tamaño de los nombres de eje X
      axis.text.y = element_text(size = 13),                         # Tamaño de los nombres de eje Y
      axis.title.x = element_text(size = 15),                        # Tamaño del título del eje X
      axis.title.y = element_text(size = 15),                        # Tamaño del título del eje Y
      legend.position = "none",                                      # Quita la leyenda
      plot.title = element_text(size = 16, face = "bold")            # Tamaño del título del gráfico
    )



  #########################################################################################

  #  Para evaluar las discrepancias entre el campo habitat extraido directamente de la base de datos y la extracción del habitat por la función creada en base a los datos de GBIF, se ha llevado a cabo el siguiente análisis que recoge:

    #### 1-. Análisis exploratorio de los datos:

  #  Con el fin de observar las frecuencias de errores en las clasificaciones de habitats de la base de datos respecto a GBIF, para ello se obtiene la tabla de frecuencias y su representación en un gráfico de barras. Como se puede ver, no todas las bases de datos contienen discrepancias con GBIF, debido a que el habitat de éstas se ha obtenido con GBIF. Aquí partimos de la base de datos MasterList unicamente con los registros que no coinciden en el habitat.


  #### 2-. Tablas de contingencia:

  #Obtenemos las tablas de contingencia que cruzan los habitats dados por la fuente original y los habitats dados por GBIF. Lo hacemos para todas las bases de datos que se quieren comparar. Aquí partimos de la base de datos MasterList completa, con y sin coincidencias en el habitat.

  #Establecemos de nuevo el directorio ya que el rmarkdown 'pierde' la información de donde está el directorio de celda en celda
  CheckHabitatMasterlist <- read_excel("Validation/CheckHabitat/Masterlist_HabitatMismatches.xlsx") %>%
    separate_rows(habitat, sep = ";") %>%
    separate_rows(habitat, sep = "-")
  CheckHabitatMasterlist$Source_Data <- sub(".*\\(([^)]+)\\).*", "\\1", CheckHabitatMasterlist$habitat)
  CheckHabitatMasterlist$Source_Data <- trimws(CheckHabitatMasterlist$Source_Data)

  #Cuando se generaban las tablas de frecuencias, a veces el nombre de la categoria del habitat era tan largo, que estropeaba la representación de la tabla, esto ocurre porque hay muchas palabras repetidas dentro de un habitat porque son la combinación de diferentes etiquetas obtenidas de la web (el caso de CABI), por ello se van a eliminar los duplicados en las categorias, si antes era 'freshwater/lakes freshwater/river' se queda en 'freshwater lakes river'
  remove_duplicates <- function(label) {
    label <- as.character(label)
    parts <- unlist(strsplit(label, "\\s*/\\s*|\\s+"))  # divide por espacio o slash
    unique_parts <- unique(parts[parts != ""])  # quitar vacíos
    cleaned_label <- paste(unique_parts, collapse = " ") #Categoría final
    return(cleaned_label)
  }

  CheckHabitatMasterlist_subset <- CheckHabitatMasterlist
  #Le aplicamos la función a la MasterList en la variable habitat y en la variable Source_Date
  CheckHabitatMasterlist_subset$habitat <- sapply(CheckHabitatMasterlist_subset$habitat, remove_duplicates)
  CheckHabitatMasterlist_subset$Source_Data <- sapply(CheckHabitatMasterlist_subset$Source_Data, remove_duplicates)
  unique(CheckHabitatMasterlist_subset$Source_Data)

  #Creamos la tabla de frecuencias para cada Base de Datos
  counts <- as.data.frame(table(CheckHabitatMasterlist_subset$Source_Data))
  colnames(counts) <- c("Source_Date", "Freq")
  counts <- counts[order(-counts$Freq), ]
  CheckHabitatMasterlist_subset$Source_Data <- factor(CheckHabitatMasterlist_subset$Source_Data, levels = counts$Source_Date)
  source_dates <- unique(CheckHabitatMasterlist_subset$Source_Data)

  for (source_date in source_dates) {
    dataset <- CheckHabitatMasterlist_subset %>%
      filter(Source_Data == source_date)
    tabla <- table(Habitat = dataset$habitat, GBIF_Group = dataset$Habitat)
    tabla_db <- as.data.frame.matrix(tabla)
    cat("\n Tabla para:", source_date, "====\n")
    print(tabla_db)
  }

  ggplot(data_freq, aes(x = reorder(Source_Data, -n), y = n, fill = n)) +
    geom_col() +
    geom_text(aes(label = n), vjust = -0.5, size = 4) +  # Números encima de las barras
    scale_fill_distiller(palette = "Blues", direction = 1) +
    labs(title = "Freshwater Habitat Mismatches: Databases vs GBIF",
         x = "Data Source",
         y = "Mismatches number") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 20, hjust = 1, size = 13),  # Tamaño de los nombres de eje X
      axis.text.y = element_text(size = 13),                         # Tamaño de los nombres de eje Y
      axis.title.x = element_text(size = 15),                        # Tamaño del título del eje X
      axis.title.y = element_text(size = 15),                        # Tamaño del título del eje Y
      legend.position = "none",                                      # Quita la leyenda
      plot.title = element_text(size = 16, face = "bold")            # Tamaño del título del gráfico
    )
  #### 3-. Cálculamos el ratio de error por habitat:

  #Para ello, vamos primero a obtener una muestra de cada categoría de habitat de la MasterList, para ello, al ser una muestra bastante heterogénea, no podemos realizar un muestreo aleatorio simple directamente, ya que este muestreo no tiene en cuenta la heterogeneidad de la población. Para no caer en este error, realizamos un Muestreo Estratificado aleatorio, en donde cada estrato es independiente de los demás. Se realiza un Muestreo Estratificado aleatorio con método proporcional

  #1.  Primero calculamos los tamaño de muestra de cada habitat

  #Los parámetros utilizados a continuación se definen de la siguiente manera:

  # -   $N_h: Número~total~de~unidades~en~el~estrato~h$

  # -   $Wh=\frac{N_h}{N}~Es~la~ponderacion~del~estrato~h$

  # -   $n_h:Número~de~unidades~de~la~muestra~en~el~estrato~h$

  # Se lleva a cabo un muestreo con afijación proporcional porque la nh es proporcional al tamaño de cada estrato. Una afijación de Neymann, se usa cuando queremos minimizar la varianza entre estratos. Usar esta afijación es mejor cuando hay grandes diferencias de variabilidad entre los estratos. En este caso, los estratos no se componen de las mismas categorías por lo que evaluar la varianza entre estratos no es lo correcto. Función obtenida de <https://github.com/DFJL/SamplingUtil/blob/master/R/nstrata.R>

  #OBTENCIÓN DE Nh y Wh
  Estratos <- CheckHabitatMasterlist_subset[, c("AcceptedNameGBIF", "habitat", "Habitat")] #Extraemos las columnas estrictamente necesarias para facilitar la visualización de la tabla
  tabla <- table(Estratos$habitat)
  Estratos <- as.data.frame(tabla)
  names(Estratos) <- c("habitat", "n")
  Estratos$p <- Estratos$n / sum(Estratos$n)
  Estratos

  # Función nstrata para el cálculo del tamaño de la muestra
  nstrata <- function(n, wh, sh = NULL, ch = NULL, method = "proportional") {
    nh <- rep(0, length(wh))
    method <- match.arg(method, c("proportional", "neyman"))

    if (method == "proportional") {
      nh <- ceiling(n * wh)
    } else {
      if (method == "neyman" && is.null(ch) && !is.null(sh)) {
        nh <- ceiling(n * ((wh * sh) / sum(wh * sh)))
      }
    }
    return(nh)
  }
  resultados_df <- data.frame() #Aquí almacenaremos los resultados


  #Obtención de las muestras por muestreo estratificado para cada categoria de habitat
  for (hab in unique(Estratos$habitat)) {
    CheckHabitatMasterlist_subset_GRIIS_hab <- CheckHabitatMasterlist_subset %>%
      filter(habitat == hab)
    #OBTENCIÓN nh
    nsizeProp <- nstrata(n = Estratos$n[Estratos$habitat == hab], wh = Estratos$p[Estratos$habitat == hab], method = "proportional")
    tabla_tamaño_estratificado <- cbind(Estratos[Estratos$habitat == hab, ], n_sample = nsizeProp)
    nsizeProp <- tabla_tamaño_estratificado$n_sample
    #DATASET FINAL
    resultados_habitat <- data.frame(
      habitat = hab,
      nh = nsizeProp
    )
    resultados_df <- rbind(resultados_df, resultados_habitat)

  }

  colnames(Estratos)[colnames(Estratos) == "n"] <- "Nh"
  colnames(Estratos)[colnames(Estratos) == "p"] <- "Wh"
  resultados_df <- resultados_df %>% select(-habitat)
  resultadofinal <- cbind(Estratos, resultados_df)
  resultadofinal

  #2.  Extraemos las muestras de cada habitat con el tamaño de muestra obtenido antes:

  #Extraemos cada muestra del habitat correspondiente
  muestras_aleatorias_por_habitat <- list()

  for (i in seq_len(nrow(resultadofinal))) {
    hab <- resultadofinal$habitat[i]
    size_muestra <- resultadofinal$nh[i]

    # Filtramos por habitat
    registros_por_habitat <- dplyr::filter(CheckHabitatMasterlist_subset, habitat == hab)

    numero_total_registros <- nrow(registros_por_habitat)

    if (numero_total_registros == 0) {
      next   # si no hay registros para ese habitat, pasa al siguiente
    }

    # Ajustar tamaño de muestra si es mayor al total disponible
    size_muestra <- min(size_muestra, numero_total_registros)

    # m.a.s sin reemplazo de cada habitat
    muestra_aleatoria_por_habitat <- registros_por_habitat[
      sample.int(numero_total_registros, size = size_muestra, replace = FALSE),
    ]

    muestras_aleatorias_por_habitat[[hab]] <- muestra_aleatoria_por_habitat
  }


  # Para ver las muestras individuales por hábitat
  # muestras_aleatorias_por_habitat

  muestra_total_aleatoria_por_habitat <- do.call(rbind, muestras_aleatorias_por_habitat)

  muestra_total_aleatoria_por_habitat

  conteo_sources <- table(muestra_total_aleatoria_por_habitat$Source_Data)
  print(conteo_sources)

  write.xlsx(muestra_total_aleatoria_por_habitat, "./Validation/CheckHabitat/sample_10%_DB_vs_GBIF_CheckHabitat_SIyNOcoincidentes.xlsx")


  #3.  Obtenemos los ratios de error para cada categoría respecto a GBIF

  unique(muestra_total_aleatoria_por_habitat$habitat)
  muestra_total_aleatoria_por_habitat$habitat_freshwater <- grepl("freshwater|dulceacuicola", muestra_total_aleatoria_por_habitat$habitat, ignore.case = TRUE) #Creamos una columna de TRUE o FALSE si la columna
  #habitat contiene 'freshwater'
  muestra_total_aleatoria_por_habitat$Habitat_freshwater <- grepl("freshwater|dulceacuicola", muestra_total_aleatoria_por_habitat$Habitat, ignore.case = TRUE) #Creamos una columna de TRUE o FALSE si la columna
  #Habitat contiene 'freshwater'

  #Creamos una columna TRUE o FALSE que indique si coinciden las dos columnas que hemos creado
  muestra_total_aleatoria_por_habitat$coincidencia_habitat <- muestra_total_aleatoria_por_habitat$habitat_freshwater == muestra_total_aleatoria_por_habitat$Habitat_freshwater
  #SERÁ:
  #TRUE si coinciden las columnas creadas, es decir que ambas funciones dan freshwater
  #FALSE si no coinciden las columnas creadas, es decir una columna dice que la especie es freshwater y la otra no y viceversa

  # #PROBAMOS MANUALMENTE A CONTAR LOS FALSE del habitat 'terrestrial|freshwater'
  # unique(muestra_total_aleatoria_por_habitat$habitat) #Obtenemos todas las categorías de habitat de GRIIS
  # terrestrialfreshwater <- muestra_total_aleatoria_por_habitat %>% filter(habitat=="terrestrial|freshwater(GRIIS)") #hacemos un subset de los registros terrestrial|freshwater de la muestra
  # error_terrestrialfreshwater <- terrestrialfreshwater %>% filter(coincidencia_habitat=="FALSE") #obtenemos de ese subset otro subset de los FALSE, es decir que no coincide el resultado de GBIF con                                                                                                #el de GRIIS
  # n_error_terrestrialfreshwater <- nrow(error_terrestrialfreshwater) #numero total de no coincidencias
  # n_error_terrestrialfreshwater
  # p_error_terrestrialfreshwater <- n_error_terrestrialfreshwater/nrow(terrestrialfreshwater) #porcentaje de no coincidencias
  # p_error_terrestrialfreshwater


  #OBTENEMOS LOS FALSE de manera automatizada
  categorias_habitat <- unique(muestra_total_aleatoria_por_habitat$habitat)


  resultados <- map_dfr(categorias_habitat, function(hab) {
    subset_habitat <- muestra_total_aleatoria_por_habitat %>% filter(habitat == hab)
    errores <- subset_habitat %>% filter(coincidencia_habitat == "FALSE")
    n_error <- nrow(errores)
    total <- nrow(subset_habitat)
    tibble(
      habitat = hab,
      n_error = n_error,
      total = total,
      p_error = ifelse(total > 0, n_error / total, NA)
    )
  })
  print(resultados)

  #Como vemos que los problemas los da GRIIS, vamos a proceder a un chequeo manual de los habitats de griis vs check_habitat de GBIF

  ###############################
  ###############################
  #Obtenemos los habitats de las especies de GRIIS por gbif:
  CheckHabitatMasterlist <- read_csv2("Validation/CheckHabitat/Step1_Prepared_GRIIS.csv") %>%
    mutate(
      habitat = str_remove_all(habitat, "\\([^)]*\\)") %>%
        str_trim()
    ) %>%
    mutate(Habitat_GRIIS = habitat) %>%
    select(-Source_Date, -habitat) %>%
    separate_rows(Habitat_GRIIS, sep = "-")

  #Obtenemos los Habitats de GRIIS segun check_habitat
  source(file.path("R", "check_habitat.r"))
  dataset <- CheckHabitatMasterlist
  nombres_aceptados <- dataset$AcceptedNameGBIF
  dataset_habitat <- check_habitat(nombres_aceptados, dataset)
  dataset_habitat2 <- dataset_habitat %>%
    mutate(Habitat_GBIF = Habitat) %>%
    select(-Habitat) #Aquí hemos obtenido los habitats dados por GBIF a través de la función check_habitat
  #Debemos eliminar aquellos NAS que GBIF no ha encontrado el habitat para la especie
  dataset_habitat3 <- dataset_habitat2 %>%
    filter(!is.na(Habitat_GBIF))%>%
    mutate(
      Habitat_GRIIS = str_replace_all(Habitat_GRIIS, " ", "") #Eliminamos los espacios innecesarios
    )
  nrow(dataset_habitat3)
  #######################################
  #######################################
  # Se lleva a cabo un muestreo con afijación proporcional porque la nh es
  #proporcional al tamaño de cada estrato. Una afijación de Neymann, se usa
  #cuando queremos minimizar la varianza entre estratos. Usar esta afijación
  #es mejor cuando hay grandes diferencias de variabilidad entre los estratos.
  #En este caso, los estratos no se componen de las mismas categorías por lo
  #que evaluar la varianza entre estratos no es lo correcto.
  #Función obtenida de <https://github.com/DFJL/SamplingUtil/blob/master/R/nstrata.R>

  #1 -. Obtención de los tamaños de muestra
  #OBTENCIÓN DE Nh y Wh
  Estratos <- dataset_habitat3 #Extraemos las columnas estrictamente necesarias para facilitar la visualización de la tabla
  tabla <- table(Estratos$Habitat_GRIIS)
  Estratos <- as.data.frame(tabla)
  names(Estratos) <- c("habitat", "n")
  Estratos$p <- Estratos$n / sum(Estratos$n)
  Estratos

  # Función nstrata para el cálculo del tamaño de la muestra
  nstrata <- function(n, wh, sh = NULL, ch = NULL, method = "proportional") {
    nh <- rep(0, length(wh))
    method <- match.arg(method, c("proportional", "neyman"))

    if (method == "proportional") {
      nh <- ceiling(n * wh)
    } else {
      if (method == "neyman" && is.null(ch) && !is.null(sh)) {
        nh <- ceiling(n * ((wh * sh) / sum(wh * sh)))
      }
    }
    return(nh)
  }
  resultados_df <- data.frame() #Aquí almacenaremos los resultados
  #Obtención de las muestras por muestreo estratificado para cada categoria de habitat
  for (hab in unique(Estratos$habitat)) {
    CheckHabitatMasterlist_subset_GRIIS_hab <- CheckHabitatMasterlist_subset %>%
      filter(habitat == hab)
    #OBTENCIÓN nh
    nsizeProp <- nstrata(n = Estratos$n[Estratos$habitat == hab], wh = Estratos$p[Estratos$habitat == hab], method = "proportional")
    tabla_tamaño_estratificado <- cbind(Estratos[Estratos$habitat == hab, ], n_sample = nsizeProp)
    nsizeProp <- tabla_tamaño_estratificado$n_sample
    #DATASET FINAL
    resultados_habitat <- data.frame(
      habitat = hab,
      nh = nsizeProp
    )
    resultados_df <- rbind(resultados_df, resultados_habitat)

  }
  colnames(Estratos)[colnames(Estratos) == "n"] <- "Nh"
  colnames(Estratos)[colnames(Estratos) == "p"] <- "Wh"
  resultados_df <- resultados_df %>% select(-habitat)
  resultadofinal <- cbind(Estratos, resultados_df)
  resultadofinal

  #2.  Obtención de la muestra
  #Extraemos las muestras de cada habitat con el tamaño de muestra obtenido antes:
  muestras_aleatorias_por_habitat <- list()
  for (i in 1:nrow(resultadofinal)) {
    hab <- resultadofinal$habitat[i]
    if(resultadofinal$nh[i]>1){
      size_muestra <- (resultadofinal$nh[i])# Tamaño de muestra definido para ese hábitat
    }
    else {
      size_muestra <- resultadofinal$nh[i]# Tamaño de muestra definido para ese hábitat
    }
    registros_por_habitat <- dataset_habitat3 %>% #Filtramos por habitat
      filter(Habitat_GRIIS == hab)

    numero_total_registros <- nrow(registros_por_habitat) #Número de filas
    if (size_muestra > numero_total_registros) {
      size_muestra <- numero_total_registros
    }
    #m.a.s sin reemplazo de cada habitat
    muestra_aleatoria_por_habitat <- registros_por_habitat[
      sample(numero_total_registros, size = size_muestra, replace = FALSE), ]

    muestras_aleatorias_por_habitat[[hab]] <- muestra_aleatoria_por_habitat
  }

  # Para ver las muestras individuales por hábitat
  # muestras_aleatorias_por_habitat

  muestra_total_aleatoria_por_habitat <- do.call(rbind, muestras_aleatorias_por_habitat)
  nombres <- muestra_total_aleatoria_por_habitat$AcceptedNameGBIF
  muestra_total_aleatoria_por_habitat$kingdom <- name_backbone_checklist(nombres)$kingdom
  muestra_total_aleatoria_por_habitat$phylum <- name_backbone_checklist(nombres)$phylum
  muestra_total_aleatoria_por_habitat$order <- name_backbone_checklist(nombres)$order
  muestra_total_aleatoria_por_habitat$family <- name_backbone_checklist(nombres)$family
  muestra_total_aleatoria_por_habitat$genus <- name_backbone_checklist(nombres)$genus

  muestra_total_aleatoria_por_habitat




  #3-. Obtención del 10% de cada tipo de habitat
  muestra_por_habitat <- muestra_total_aleatoria_por_habitat

  # Lista de hábitats únicos desde muestra_10_por_habitat
  habitats_de_interes <- unique(muestra_por_habitat$Habitat_GRIIS)

  # Filtrar y extraer el 10% de cada habitat, con al menos 1 fila
  muestra_filtrada <- muestra_por_habitat %>%
    filter(Habitat_GRIIS %in% habitats_de_interes) %>%
    group_by(Habitat_GRIIS) %>%
    group_map(~ {
      n_total <- nrow(.x)
      n_sample <- max(1, round(n_total * 0.10))
      .x %>% sample_n(n_sample)
    }, .keep = TRUE) %>%  # <-- esta parte es clave
    bind_rows()


  # Verifica que haya representación de todos los hábitats
  muestra_filtrada
  table(muestra_filtrada$Habitat_GRIIS)
  write.xlsx(muestra_filtrada, "Validation/CheckHabitat/sample_10%_GRIIS_GBIF.xlsx")



  ########################################


  #Una vez aplicado el checkeo manual. vamos a sacar la matriz de confusion de cada categioria
  checkeo <- read_excel("./Validation/CheckHabitat/sample_10%_GRIIS_GBIFbueno.xlsx")
  checkeo <- checkeo[0:93,]
  checkeo

  # Crear matriz de confusión para GRIIS
  # Crear tabla de conteos
  matriz_griis <- table(checkeo$Habitat_GRIIS1, checkeo$ACIERTO_GRIIS)

  # Convertir a data.frame
  matriz_griis <- as.data.frame(matriz_griis)

  # Separar Success/Fail y renombrar columnas
  matriz_griis <- matriz_griis %>%
    mutate(
      Habitat = Var1,
      Fail = ifelse(Var2 == 0, Freq, 0),
      Success = ifelse(Var2 == 1, Freq, 0)
    ) %>%
    select(Habitat, Success, Fail)

  # Calcular tasas por fila (por hábitat)
  matriz_griis <- matriz_griis %>%
    group_by(Habitat) %>%
    dplyr::summarise(
      Success = sum(Success),
      Fail = sum(Fail)
    ) %>%
    mutate(
      Total = Success + Fail,
      SuccessRate = Success / Total,
      FailRate = Fail / Total
    )

  print("Matriz de confusión GRIIS:")
  print(matriz_griis)



  # --- MATRIZ GBIF ---
  matriz_gbif <- table(checkeo$Habitat_CheckHabitatGBIF, checkeo$ACIERTO_GBIF)

  matriz_gbif <- as.data.frame(matriz_gbif) %>%
    mutate(
      Habitat = Var1,
      Fail = ifelse(Var2 == 0, Freq, 0),
      Success = ifelse(Var2 == 1, Freq, 0)
    ) %>%
    select(Habitat, Success, Fail) %>%
    group_by(Habitat) %>%
    dplyr::summarise(
      Success = sum(Success),
      Fail = sum(Fail)
    ) %>%
    mutate(
      Total = Success + Fail,
      SuccessRate = Success / Total,
      FailRate = Fail / Total
    )

  print("Matriz de confusión GBIF:")
  print(matriz_gbif)

  #Hacemos la matriz de confusión para GBIF
  expected_value <- factor(checkeo$Binary_CheckHabitatGBIF)
  predicted_value <- factor(checkeo$Binary_Final)
  Confusion_Matrix <- confusionMatrix(data=predicted_value, reference=expected_value)
  Confusion_Matrix
  Confusion_Matrix$byClass["Balanced Accuracy"]
  cm <- table(Predicho = predicted_value, Real = expected_value)
  cm
  # Proporciones sobre el total
  prop_total <- prop.table(cm)
  round(prop_total, 2)   # redondeado a 2 decimales

  # Imprimir los resultados
  tp <- cm[2,2]
  tn <- cm[1,1]
  fp <- cm[1,2]
  fn <- cm[2,1]

  cat("Verdaderos Positivos (TP):", tp, "\n")
  cat("Falsos Positivos (FP):", fp, "\n")
  cat("Verdaderos Negativos (TN):", tn, "\n")
  cat("Falsos Negativos (FN):", fn, "\n")

  # Tasa de Verdaderos Positivos (Sensibilidad o TPR)
  tpr <- tp / (tp + fn)
  # Tasa de Falsos Positivos (FPR)
  fpr <- fp / (fp + tn)
  # Tasa de Verdaderos Negativos (Especificidad o TNR)
  tnr <- tn / (tn + fp)
  # Tasa de Falsos Negativos (FNR)
  fnr <- fn / (fn + tp)

  # Mostrar los resultados
  cat("Tasa de Verdaderos Positivos (TPR o Sensibilidad):", tpr, "\n")
  cat("Tasa de Falsos Positivos (FPR):", fpr, "\n")
  cat("Tasa de Verdaderos Negativos (TNR o Especificidad):", tnr, "\n")
  cat("Tasa de Falsos Negativos (FNR):", fnr, "\n")

  #Error que se comete con el tamaño de muestra:
  # Especificar ocurrencias muestrales (x), tamaño de muestra (n) y nivel de confianza

  #####################################################################
  ##### Error de muestreo para la proporcion sin el 10% ###############
  #####################################################################
  n <- 242
  N <- 449
  confidence_level <- 0.95
  k <- confidence_level

  # Calcular estimación puntual, alfa, valor crítico z, error estándar y margen de error
  P <- n / N
  #P <- 0.5
  Q <- 1-P
  #error <- sqrt(((N-n)/(N-1))*((P*Q)/n))
  error <- k * sqrt((N-n)/(N-1)) * sqrt((P*(1-P))/n)

  # Calcular los límites inferior y superior del intervalo de confianza
  lower_bound <- P - error
  upper_bound <- P + error

  # Imprimir los resultados
  cat(sprintf("Estimación puntual: %.3f\n", P))
  cat(sprintf("Margen de error: %.3f\n", error))
  cat("Intervalo de confianza : (", lower_bound, "-", upper_bound, ")", "\n")

  #####################################################################
  ##### Error de muestreo para la proporcion con el 10% ###############
  #####################################################################
  n <- 91          # Submuestra
  N <- 871         # Tamaño poblacional
  n_exitos <- 79   # Número de éxitos observados
  confidence_level <- 0.95
  P <- n_exitos / n         # Proporción de acierto
  Q <- 1 - P                # Proporcion de fallo
  alpha <- 1 - confidence_level
  z_alpha_2 <- qnorm(1 - alpha/2)  # Valor crítico Z

  # Margen de error (con corrección por población finita)
  error <- z_alpha_2 * sqrt(((N - n) / (N - 1)) * ((P * Q) / n))

  # Intervalo de confianza
  lower_bound <- P - error
  upper_bound <- P + error

  # Resultados
  cat(sprintf("Proporcion de acierto (P) : %.4f\n", P))
  cat(sprintf("Proporcion de fallo (Q) : %.4f\n", Q))
  cat(sprintf("Margen de error: %.4f\n", error))
  cat(sprintf("Intervalo de confianza (%.0f%%): (%.4f, %.4f)\n",
              confidence_level * 100, lower_bound, upper_bound))

}
