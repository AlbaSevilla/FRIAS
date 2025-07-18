Obtain_TaxonNAS <- function() {
  dataset <- read.xlsx("OutputFiles/Intermediate/Step7_Standardizedpathway_Masterlist.xlsx")
  tax_cols <- c("Kingdom", "Order", "Family", "Class", "Phylum")
  for (col in tax_cols) {
    if (!col %in% colnames(dataset)) {
      dataset[[col]] <- NA_character_
    } else {
      dataset[[col]] <- trimws(dataset[[col]])
      dataset[[col]][dataset[[col]] == ""] <- NA
    }
  }
  dataset$AcceptedNameGBIF <- trimws(dataset$AcceptedNameGBIF)

  # Especies a consultar
  dataset_to_query <- dataset %>%
    filter(if_any(all_of(tax_cols), is.na)) %>%
    distinct(AcceptedNameGBIF) %>%
    pull(AcceptedNameGBIF)

  # Funcion que consulta y devuelve un named list
  consulta_taxonomia <- function(nombre_especie) {
    backbone <- name_backbone_checklist(name = nombre_especie)
    list(
      AcceptedNameGBIF = nombre_especie,
      Kingdom = if (!is.null(backbone$kingdom)) backbone$kingdom else NA,
      Order = if (!is.null(backbone$order)) backbone$order else NA,
      Family = if (!is.null(backbone$family)) backbone$family else NA,
      Class = if (!is.null(backbone$class)) backbone$class else NA,
      Phylum = if (!is.null(backbone$phylum)) backbone$phylum else NA
    )
  }

  # Paralelización
  cl <- makeCluster(detectCores() - 1)
  clusterExport(cl, c("name_backbone_checklist", "consulta_taxonomia", "dataset_to_query")) # Exporta la función a los workers

  # Barra de progreso sencilla
  pb <- txtProgressBar(min = 0, max = length(dataset_to_query), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  # Ejecuta en paralelo
  results <- parLapply(cl, seq_along(dataset_to_query), function(i) {
    res <- consulta_taxonomia(dataset_to_query[i])
    # Actualiza la barra de progreso (solo en el master)
    if (interactive()) progress(i)
    res
  })

  close(pb)
  stopCluster(cl)

  taxo_df <- bind_rows(results)

  # Rellena solo los NA
  for (col in tax_cols) {
    idx <- is.na(dataset[[col]]) & dataset$AcceptedNameGBIF %in% taxo_df$AcceptedNameGBIF
    dataset[[col]][idx] <- taxo_df[[col]][match(dataset$AcceptedNameGBIF[idx], taxo_df$AcceptedNameGBIF)]
  }


  dataset2 <- dataset

  ############################################################################
  ############# OBTENER NAS DE TAXONOMIA CON NCBI ############################
  ############################################################################

  # Configuración global de paralelización y barra de progreso
  future::plan(future::multisession, workers = 2) # NCBI recomienda pocos workers
  progressr::handlers("progress") # Puedes cambiar por "txtprogressbar" si prefieres

  # Suponiendo que dataset2 ya existe y está limpio
  tax_cols <- c("Kingdom", "Order", "Family", "Class", "Phylum")
  for (col in tax_cols) {
    dataset2[[col]][trimws(dataset2[[col]]) == ""] <- NA
  }
  dataset_na <- dataset2 %>%
    filter(if_any(all_of(tax_cols), is.na))
  species_to_query <- unique(dataset_na$AcceptedNameGBIF)

  # Función para consultar NCBI para una especie
  consulta_ncbi <- function(sp) {
    result <- tryCatch(
      tax_name(sci = sp, get = c("kingdom", "order", "family", "class", "phylum"), db = "ncbi"),
      error = function(e) return(data.frame(kingdom=NA, order=NA, family=NA, class=NA, phylum=NA))
    )
    if (is.data.frame(result) && nrow(result) > 0) {
      row <- result[1, ]
      row$species <- sp
      return(row)
    } else {
      return(data.frame(kingdom=NA, order=NA, family=NA, class=NA, phylum=NA, species=sp))
    }
  }

  # Consulta paralela con barra de progreso real
  progressr::with_progress({
    p <- progressr::progressor(along = species_to_query)
    results <- future_lapply(species_to_query, function(sp) {
      res <- consulta_ncbi(sp)
      p(sprintf("Procesando: %s", sp))
      res
    })
  })

  # Combinar resultados
  taxonomy_df <- bind_rows(results)
  names(taxonomy_df)[names(taxonomy_df) == "species"] <- "AcceptedNameGBIF"
  names(taxonomy_df)[names(taxonomy_df) == "kingdom"] <- "Kingdom"
  names(taxonomy_df)[names(taxonomy_df) == "order"] <- "Order"
  names(taxonomy_df)[names(taxonomy_df) == "family"] <- "Family"
  names(taxonomy_df)[names(taxonomy_df) == "class"] <- "Class"
  names(taxonomy_df)[names(taxonomy_df) == "phylum"] <- "Phylum"

  # Rellenar solo los NA en dataset2 con los valores de taxonomy_df
  for (col in tax_cols) {
    idx <- is.na(dataset2[[col]]) & dataset2$AcceptedNameGBIF %in% taxonomy_df$AcceptedNameGBIF
    dataset2[[col]][idx] <- taxonomy_df[[col]][match(dataset2$AcceptedNameGBIF[idx], taxonomy_df$AcceptedNameGBIF)]
  }

  # Ordenar y guardar
  dataset3 <- dataset2 %>% arrange(AcceptedNameGBIF)
  taxonomic_order <- c("Kingdom", "Phylum", "Class", "Order", "Family")
  other_columns <- setdiff(names(dataset3), taxonomic_order)
  dataset3 <- dataset3[, c(other_columns[1:2], taxonomic_order, other_columns[3:length(other_columns)])]



  write.xlsx(dataset3,
             file.path("OutputFiles", "Intermediate", "Step8_ObtainTaxonNAS_MasterList.xlsx"),
             row.names = FALSE,
             col.names = TRUE,
             quote = FALSE)



  #Resolver vacios de taxonomia para una misma familia
  dataset4 <- read_excel("Outputfiles/Intermediate/Step8_ObtainTaxonNAS_MasterList.xlsx")
  tax_cols <- c("Kingdom", "Phylum", "Class", "Order")  # Family ya está presente

  for (col in tax_cols) {
    # Encontramos las filas donde hay NA en la columna actual
    missing_idx <- which(is.na(dataset4[[col]]) & !is.na(dataset4$Family))

    for (i in missing_idx) {
      fam <- dataset4$Family[i]

      # Buscamos otras filas con la misma familia y el valor presente
      matches <- dataset4 %>%
        filter(Family == fam & !is.na(.data[[col]])) %>%
        distinct(.data[[col]]) %>%
        pull(.data[[col]])

      # Si hay un único valor no NA para esa familia, lo usamos
      if (length(matches) == 1) {
        dataset4[[col]][i] <- matches
      }
    }
  }

  View(dataset4)
  #Nos salian varios 'order' comprbamos que el que está bien es el segundo, nos lo quedamos
  dataset4 <- dataset4 %>%
    mutate(Order = sapply(str_split(Order, ";"), function(x) {
      if (length(x) >= 2) x[2] else x[1]
    }))


  #Reemplazamos 'Animalia;Metazoa' por 'Animalia'
  dataset4$Kingdom <- gsub("Animalia;Metazoa", "Animalia", dataset4$Kingdom)

  write.xlsx(dataset4,
             file.path("OutputFiles", "Intermediate", "Step8_ObtainTaxonNAS_MasterList.xlsx"),
             row.names = FALSE,
             col.names = TRUE,
             quote = FALSE)
}
