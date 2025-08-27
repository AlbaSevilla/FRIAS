MergeDatasets <- function(FileInfo, OutputFiles = TRUE) {
  #PREPARAMOS ARCHIVOS ########################################################################
  ruta_intermediate <- "OutputFiles/Intermediate"
  if (!dir.exists(ruta_intermediate)) dir.create(ruta_intermediate, recursive = TRUE)

  data_files <- list.files(path = ruta_intermediate, pattern = "^Step1_Prepared.*\\.csv$", full.names = TRUE)
  db_names <- sub("^.*Step1_Prepared_(.*)\\.csv$", "\\1", data_files)

  #UNIMOS LOS ARCHIVOS ########################################################################
  cat("Uniendo bases de datos \n")
  for (i in seq_along(db_names)) {
    cat("Procesando la base de datos: ", db_names[i], "\n")
    dat <- read.csv(data_files[i], sep = ";", stringsAsFactors = FALSE)
    dat <- dat %>%
      dplyr::mutate(
        EICATImpact = tolower(str_trim(EICATImpact)),
        EICATImpact = gsub("[-_/]", "", EICATImpact)  # Eliminar caracteres especiales pero mantener espacios
      )
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

          alldat[[col]] <- gsub("NA; ", "", alldat[[col]])
          alldat[[col]] <- gsub("; NA", "", alldat[[col]])
          alldat[[col]] <- trimws(alldat[[col]])
        }

        # Eliminar columnas originales duplicadas
        alldat <- alldat[ , !grepl("\\.x$|\\.y$", names(alldat))]
        numero_especies <- nrow(alldat)
      }
    }
  }
  originalnames <- alldat$OriginalNameDB
  alldat$CorrectedAcceptedNameGBIF <- name_backbone_checklist(originalnames)$canonicalName
  alldat$AcceptedNameGBIF <- alldat$CorrectedAcceptedNameGBIF


  #Nos quedamos con el primer ID_GBIF (son iguales todos los de una celda)
  alldat$ID_GBIF <- sapply(strsplit(as.character(alldat$ID_GBIF), ";"), `[`, 1)

  #REEMPLAZAMOS LOS "NA" POR VACÍOS EN TODAS LAS COLUMNAS MENOS EN EL NOMBRE DE LA DATABASE
  cols <- setdiff(names(alldat), "Source_Date")
  alldat[cols] <- lapply(alldat[cols], function(x) {
    x[x == "" | x == "NA"] <- NA
    x
  })


  #CAMBIAMOS LOS , POR ; EN ALLDAT
  alldat[] <- lapply(alldat, function(x) gsub(",", ";", x))

  cat("Procesando fechas más antiguas \n")
  #ELIMINAMOS COSAS QUE NO SEAN NUMEROS EN LA COLUMNA FirstRecords:
  alldat <- alldat %>%
    mutate(
      FirstRecords = gsub("[^0-9;]", "", FirstRecords)
    )
  #AHORA ORDENAMOS LAS FECHAS DE FirstRecords DE MENOR A MAYOR PARA QUE SEA MÁS FACIL
  #COMPROBAR QUE OBTENEMOS LA FECHA MAS ANTIGUA EN OLDESTDATE
  alldat <- alldat %>%
    mutate(
      FirstRecords = sapply(strsplit(FirstRecords, ";"), function(x) {
        x <- str_trim(x)  # quitar espacios en blanco
        x_sorted <- sort(x)  # ordena como texto (si son años funciona bien)
        paste(x_sorted, collapse = ";")
      })
    )
  #Algunas veces sale ;1999 al principio de FirstRecords asi que eliminamos esos comienzos
  alldat <- alldat %>%
    mutate(
      FirstRecords = sub("^;", "", FirstRecords)
    )
  #OLDEST DATE
  alldat <- alldat %>%
    mutate(
      OldestDate = sapply(strsplit(FirstRecords, ";"), function(x) {
        x <- str_trim(x)
        if (length(x) == 0 || is.na(x[1]) || x[1] == "") {
          NA  # en caso de vacío o NA
        } else {
          x[1]  # primer valor
        }
      })
    )

  #LIMPIAMOS VALORES
  cat("Limpiando valores duplicados dentro de las celdas \n")
  eliminar_duplicados_por_fila <- function(df, separador = ";") {
    df_limpio <- as.data.frame(lapply(df, function(col) {
      sapply(col, function(celda) {
        # Si es NA o vacío, conservar tal cual
        if (is.na(celda) || trimws(celda) == "") return(celda)

        # Separar, limpiar, eliminar duplicados y recombinar
        partes <- unlist(strsplit(as.character(celda), paste0("\\s*", separador, "\\s*")))
        partes_unicos <- unique(trimws(partes[partes != "" & partes != "NA"]))
        paste(partes_unicos, collapse = separador)
      }, USE.NAMES = FALSE)
    }), stringsAsFactors = FALSE)

    return(df_limpio)
  }

  alldat <- eliminar_duplicados_por_fila(alldat)

  cat("Agrupando por AcceptedNameGBIF y colapsando \n")
  colapsar_por_AcceptedNameGBIF <- function(df, clave = "AcceptedNameGBIF", separador = ";") {
    # Verificar que la columna clave exista
    if (!(clave %in% names(df))) {
      stop(paste("La columna", clave, "no existe en el dataframe"))
    }

    # Filas con valor en la columna clave
    df_con_valor <- df[!is.na(df[[clave]]) & trimws(df[[clave]]) != "", ]

    # Filas sin valor en la columna clave
    df_sin_valor <- df[is.na(df[[clave]]) | trimws(df[[clave]]) == "", ]

    # Agrupar por el valor de la clave
    grupos <- split(df_con_valor, df_con_valor[[clave]])

    # Colapsar cada grupo
    resultado_colapsado <- lapply(grupos, function(grupo) {
      # Para cada columna, colapsar valores únicos separados por el separador
      sapply(grupo, function(col) {
        elementos <- unlist(strsplit(as.character(col), paste0("\\s*", separador, "\\s*")))
        elementos <- unique(trimws(elementos[elementos != "" & elementos != "NA"]))
        paste(elementos, collapse = separador)
      }, simplify = FALSE)
    })

    # Combinar todos los resultados en un dataframe
    df_colapsado <- as.data.frame(do.call(rbind, resultado_colapsado), stringsAsFactors = FALSE)

    # Unir datos colapsados con los que no tenían valor en la clave
    resultado_final <- rbind(df_colapsado, df_sin_valor)
    rownames(resultado_final) <- NULL

    return(resultado_final)
  }



  # colapsar_por_AcceptedNameGBIF <- function(df, clave = "AcceptedNameGBIF", separador = ";") {
  #   grupos <- split(df, df[[clave]])
  #   resultado <- lapply(grupos, function(grupo) {
  #     sapply(grupo, function(col) {
  #       elementos <- unlist(strsplit(as.character(col), paste0("\\s*", separador, "\\s*")))
  #       elementos <- unique(trimws(elementos[elementos != "" & elementos != "NA"]))
  #       paste(elementos, collapse = separador)
  #     }, simplify = FALSE)
  #   })
  #   do.call(rbind, resultado)
  # }
  alldat_sinNa <- alldat[!(is.na(alldat$AcceptedNameGBIF) | alldat$AcceptedNameGBIF == "" |
                             is.na(alldat$ID_GBIF) | alldat$ID_GBIF == ""), ]

  alldat_conNA <- alldat[(is.na(alldat$AcceptedNameGBIF) | alldat$AcceptedNameGBIF == "" |
                            is.na(alldat$ID_GBIF) | alldat$ID_GBIF == ""), ]

  names(alldat)
  alldat_final <- colapsar_por_AcceptedNameGBIF(alldat)

  cols <- setdiff(names(alldat_final), "Source_Date")
  alldat_final[cols] <- lapply(alldat_final[cols], function(x) {
    # Reemplaza "" y "NA" por NA usando gsub
    x <- gsub("NA", NA, x)
    x
  })


  # Función para asegurarse de que todas las
  #columnas sean texto antes de guardar
  Variables_Caracter <- function(dataset) {
    for (i in 1:ncol(dataset)) {
      if (is.list(dataset[[i]])) {
        dataset[[i]] <- sapply(dataset[[i]], function(x) paste(unlist(x), collapse = ", "))
      } else if (is.factor(dataset[[i]])) {
        dataset[[i]] <- as.character(dataset[[i]])
      }
    }
    return(dataset)
  }
  alldat_final <- Variables_Caracter(alldat_final)
  alldat_final2 <- rbind(alldat_final, alldat_conNA)

  ########################################################################################
  ########################################################################################
  #PREPARAR CORRECTAMENTE LA MASTERLIST:
  #1 ELIMINAR TODOS LOS ESPACIOS SIMPLES O DOBLES
  #DESPUÉS DE UNA COMA O PUNTO Y COMA
  Step2_MasterList <- alldat_final2
  #Eliminar todos los espacios despues de , o ;
  Step2_MasterList[] <- lapply(Step2_MasterList, function(x) {
    gsub("([;,])\\s+", "\\1", x)
  })

  #2 CAMBIAR TODAS LAS COMAS O PUNTO Y COMA
  #POR PUNTO Y COMA
  Step2_MasterList[] <- lapply(Step2_MasterList, function(x) {
    gsub("[,;]", ";", x)
  })

  #3 ELIMINAR CADENAS DE CARACTERES DUPLICADAS
  #DENTRO DE UNA MISMA CELDA PARA TODO EL DATASET
  elimina_duplicados <- function(x) {
    sapply(strsplit(x, ";"), function(y) paste(unique(y), collapse = ";"))
  }
  Step2_MasterList[] <- lapply(Step2_MasterList, elimina_duplicados)
  cols <- setdiff(names(Step2_MasterList), "Source_Date")
  Step2_MasterList[cols] <- lapply(Step2_MasterList[cols], function(x) {
    # Reemplaza "" y "NA" por NA usando gsub
    x <- gsub("NA", NA, x)
    x
  })

  #4 ORDENAR POR ORDEN ALFABETICO LAS
  #CADENAS DE CARACTERES DENTRO DE CADA CELDA
  ordena_cadenas <- function(x) {
    sapply(strsplit(x, ";"), function(y) paste(sort(y), collapse = ";"))
  }
  Step2_MasterList[] <- lapply(Step2_MasterList, ordena_cadenas)

  #5 ELIMINAMOS LO QUE ESTÁ ENTRE PARENTESIS
  Step2_MasterList[] <- lapply(Step2_MasterList, function(x) gsub("\\s*\\([^\\)]*\\)", "", x))
  Step2_MasterList <- Step2_MasterList %>%
    dplyr::select(-CorrectedAcceptedNameGBIF)

  #renombramos columna
  names(Step2_MasterList)[names(Step2_MasterList) == "Source_Date"] <- "Source_Data"

  names(Step2_MasterList)
  #ORDENAMOS LAS COLUMNAS
  ORDEN_columnas <- c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF",
                      "Kingdom","Order","Family","Phylum","Class","FunctionalGroup","Group",
                      "EstablishmentMeans","Pathways",
                      "NativeRange","InvadedRange","InvadedRangeISO3",
                      "AffectedNativeSpecies","EICATImpact","Mechanisms","EICAT_by_Mechanism",
                      "FirstRecords","OldestDate",
                      "Habitat","Source_Data")

  Step2_MasterList <- Step2_MasterList[,ORDEN_columnas]
  Step2_MasterList <- Step2_MasterList %>%
    filter(!is.na(Source_Data) & Source_Data != "")

  ###################################### NAS ####################################################
  cat("Eliminando NAs \n")
  # Eliminar registros con NA en campos clave
  Step2_MasterList_sinNa <- Step2_MasterList[!(is.na(Step2_MasterList$AcceptedNameGBIF) | Step2_MasterList$AcceptedNameGBIF == "" |
                                                 is.na(Step2_MasterList$ID_GBIF) | Step2_MasterList$ID_GBIF == ""), ]
  Step2_MasterList_sinNa_OriginalNameDB <- Step2_MasterList[!(is.na(Step2_MasterList$OriginalNameDB) | Step2_MasterList$OriginalNameDB == "" ), ]

  dim(Step2_MasterList_sinNa_OriginalNameDB)
  Step2_MasterList_conNA <- Step2_MasterList[(is.na(Step2_MasterList$AcceptedNameGBIF) | Step2_MasterList$AcceptedNameGBIF == "" |
                                                is.na(Step2_MasterList$ID_GBIF) | Step2_MasterList$ID_GBIF == ""), ]

  #################### GUARDAMOS RESULTADOS ###############################
  write.csv(Step2_MasterList_sinNa_OriginalNameDB, file.path("OutputFiles", "Intermediate", "Step2_MasterList.csv"), row.names = FALSE)
  write_xlsx(Step2_MasterList_sinNa_OriginalNameDB, file.path("OutputFiles", "Intermediate", "Step2_MasterList.xlsx"))

  if (nrow(Step2_MasterList_conNA) > 0) {
    write.xlsx(Step2_MasterList_conNA, file.path("OutputFiles", "Check", "NA_AcceptedNameGBIF_ID_GBIF_Masterlist.xlsx"))
    write.csv(Step2_MasterList_conNA, file.path("OutputFiles", "Check", "NA_AcceptedNameGBIF_ID_GBIF_Masterlist.csv"), row.names = FALSE)
  }
}
