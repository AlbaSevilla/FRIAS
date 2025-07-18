Obtain_FunctionalGroup <- function(){
  ########################################
  ########################################
  ############## G L O B I ###############
  ########################################
  ########################################

  MasterList <- read.xlsx("OutputFiles/Intermediate/Step10_CorrectedAcceptedNameGBIF_Masterlist.xlsx")

  source(file.path("R", "noduplicates.r"))

  tabla_correspondenciasGlobi <- read_excel("TablesToStandardize/Standardization_FunctionalGroup.xlsx", sheet="GLOBI_traductable") %>%
    mutate(
      Kingdom_Target_clean = sapply(Kingdom_Target, function(k) {
        if (is.na(k)) return(NA_character_)
        k <- str_split(k, ",")[[1]]
        k <- trimws(k)
        k <- sort(k)
        paste(k, collapse = ", ")
      }),
      Kingdom_Source_clean = sapply(Kingdom_Source, function(k) {
        if (is.na(k)) return(NA_character_)
        k <- str_split(k, ",")[[1]]
        k <- trimws(k)
        k <- sort(k)
        paste(k, collapse = ", ")
      }),
      Phylum_Source_clean = sapply(Phylum_Source, function(p) {
        if (is.na(p)) return(NA_character_)
        p <- str_split(p, ",")[[1]]
        p <- trimws(p)
        p <- sort(p)
        paste(p, collapse = ", ")
      }),
      Class_Source_clean = sapply(Class_Source, function(c) {
        if (is.na(c)) return(NA_character_)
        c <- str_split(c, ",")[[1]]
        c <- trimws(c)
        c <- sort(c)
        paste(c, collapse = ", ")
      }),
      Order_Source_clean = sapply(Order_Source, function(o) {
        if (is.na(o)) return(NA_character_)
        o <- str_split(o, ",")[[1]]
        o <- trimws(o)
        o <- sort(o)
        paste(o, collapse = ", ")
      }),
      Family_Source_clean = sapply(Family_Source, function(f) {
        if (is.na(f)) return(NA_character_)
        f <- str_split(f, ",")[[1]]
        f <- trimws(f)
        f <- sort(f)
        paste(f, collapse = ", ")
      })
    ) %>%
    dplyr::rename(Functional_Group = `Functional Group`)


  clean_kingdom <- function(k) {
    if (is.na(k)) return(NA_character_)
    k <- str_split(k, ",")[[1]]
    k <- trimws(k)
    k <- sort(k)
    paste(k, collapse = ", ")
  }

  obtener_grupo_funcional <- function(nombre) {
    cat("Procesando:", nombre, "\n")

    resultado <- tryCatch({
      # Buscar taxonomía en MasterList
      taxonomia <- MasterList %>%
        filter(AcceptedNameGBIF == nombre) %>%
        select(Kingdom, Phylum, Class, Order, Family) %>%
        distinct()

      # Lista de columnas a probar en orden jerárquico
      columnas_taxo <- c("Kingdom", "Phylum", "Class", "Order", "Family")

      for (columna in columnas_taxo) {
        valor <- taxonomia[[columna]]
        if (length(valor) > 0 && !is.na(valor)) {
          valor_limpio <- clean_kingdom(valor)

          match_fg <- tabla_correspondenciasGlobi %>%
            filter(
              is.na(Interaction),
              !is.na(.data[[paste0(columna, "_Source_clean")]]),
              .data[[paste0(columna, "_Source_clean")]] == valor_limpio
            ) %>%
            pull(Functional_Group) %>%
            unique()

          if (length(match_fg) > 0 && !is.na(match_fg[1])) {
            return(match_fg[1])
          }
        }
      }

      # Si no se encontró por taxonomía, intentar con interacciones
      interacciones <- get_interactions(nombre)
      if (nrow(interacciones) == 0) return(NA)

      interacciones <- interacciones %>%
        filter(interaction_type %in% c(
          "eats", "preysOn", "parasiteOf", "endoparasiteOf",
          "ectoparasiteOf", "parasitoidOf", "pollinates",
          "pathogenOf", "rootparasiteOf", "hemiparasiteOf"
        )) %>%
        select(source_taxon_name, interaction_type, target_taxon_name)

      if (nrow(interacciones) == 0) return(NA)

      reinos <- name_backbone_checklist(interacciones$target_taxon_name)$kingdom
      interacciones$Kingdom_target <- reinos

      noduplicados <- noduplicates(interacciones, "interaction_type") %>%
        mutate(Kingdom_target_clean = sapply(Kingdom_target, clean_kingdom))

      resultado_join <- noduplicados %>%
        left_join(tabla_correspondenciasGlobi, by = c("interaction_type" = "Interaction", "Kingdom_target_clean" = "Kingdom_Target_clean")) %>%
        mutate(Functional_Group = ifelse(is.na(Functional_Group) & interaction_type == "preysOn", "Predator", Functional_Group))

      grupos <- resultado_join %>%
        filter(!is.na(Functional_Group)) %>%
        distinct(Functional_Group) %>%
        pull(Functional_Group) %>%
        sort() %>%
        paste(collapse = "; ")

      if (grupos == "") return(NA)
      return(grupos)

    }, error = function(e) {
      message("Error procesando ", nombre, ": ", e$message)
      return(NA)
    })

    return(resultado)
  }


  # Aplicar la función solo si está vacío el FunctionalGroup
  n <- nrow(MasterList)

  for (i in seq_len(n)) {
    nombre <- MasterList$AcceptedNameGBIF[i]
    ya_tiene <- MasterList$FunctionalGroup[i]

    # Solo procesar si está vacío o NA
    if (is.na(ya_tiene) || ya_tiene == "") {
      cat(sprintf("Procesando %d de %d: %s\n", i, n, nombre))

      res <- tryCatch({
        obtener_grupo_funcional(nombre)
      }, error = function(e) {
        message(sprintf("Error procesando %s: %s", nombre, e$message))
        NA_character_
      })

      MasterList$FunctionalGroup[i] <- res
    } else {
      cat(sprintf("Saltando %d de %d (ya tiene grupo): %s\n", i, n, nombre))
    }
  }
  write.xlsx(MasterList, "Outputfiles/Intermediate/Step11.1_ObtainFunctionalGroup_Masterlist.xlsx")

  ##############################################
  ##############################################
  ############## F I S H B A S E ###############
  ##############################################
  ##############################################

  MasterList2 <- read_excel("Outputfiles/Intermediate/Step11.1_ObtainFunctionalGroup_Masterlist.xlsx")
  tabla_correspondenciasFISHBASE <- read_excel("TablesToStandardize/Standardization_FunctionalGroup.xlsx", sheet="FISHBASE_traductable")

  # Extraer las especies con FunctionalGroup NA
  nas_functionalgroup <- MasterList2 %>%
    filter(is.na(FunctionalGroup))

  nas_species <- nas_functionalgroup$AcceptedNameGBIF

  fish_fooditems <- fooditems(nas_species, fields = c("Species", "FoodI"))
  fish_ecology <- ecology(nas_species, fields = c("Species", "FeedingType", "FoodTroph"))
  dataset_functionalgroup <- fish_fooditems %>%
    left_join(fish_ecology, by = "Species")

  names(dataset_functionalgroup)
  dataset_functionalgroup <- dataset_functionalgroup %>%
    select(Species, FeedingType, FoodI,FoodTroph)


  # Leer la tabla de correspondencia desde Excel
  mapping <- read_excel("TablesToStandardize/Standardization_FunctionalGroup.xlsx", sheet = 2)

  # Arreglar decimales si están con coma
  mapping <- mapping %>%
    mutate(FoodTroph = str_replace_all(FoodTroph, ",", "."))

  # Función para convertir rangos tipo "2.0-2.19" o ">2.8"
  parse_range <- function(range_str) {
    if (is.na(range_str)) return(c(NA_real_, NA_real_))
    if (str_detect(range_str, ">")) {
      min_val <- as.numeric(str_extract(range_str, "\\d+\\.\\d+"))
      return(c(min_val, Inf))
    } else if (str_detect(range_str, "-")) {
      parts <- str_split(range_str, "-", simplify = TRUE)
      return(as.numeric(parts))
    } else {
      return(c(NA_real_, NA_real_))
    }
  }

  # Aplicar la lógica de asignación
  dataset_functionalgroup <- dataset_functionalgroup %>%
    rowwise() %>%
    mutate(
      FunctionalGroup = {
        group <- NA_character_

        if (!is.na(FeedingType)) {
          # Filtro de mapping solo con este FeedingType
          type_map <- mapping %>% filter(FeedingType == FeedingType)

          if (!is.na(FoodTroph)) {
            # Buscar por rango de FoodTroph dentro de ese FeedingType
            for (i in seq_len(nrow(type_map))) {
              range_vals <- parse_range(type_map$FoodTroph[i])
              if (!any(is.na(range_vals)) && FoodTroph >= range_vals[1] && FoodTroph <= range_vals[2]) {
                group <- type_map$Functional_Groups[i]
                break
              }
            }
          }

          # Si no hay FoodTroph, usar solo FeedingType
          if (is.na(group) && nrow(type_map) > 0) {
            group <- type_map$Functional_Groups[1]  # Tomamos el primero que coincida
          }

        } else {
          # FeedingType es NA → buscar solo por FoodTroph (sin filtro)
          if (!is.na(FoodTroph)) {
            for (i in seq_len(nrow(mapping))) {
              range_vals <- parse_range(mapping$FoodTroph[i])
              if (!any(is.na(range_vals)) && FoodTroph >= range_vals[1] && FoodTroph <= range_vals[2]) {
                group <- mapping$Functional_Groups[i]
                break
              }
            }
          }
        }

        group
      }
    ) %>%
    ungroup()

  dataset_functionalgroup <- dataset_functionalgroup %>% select(-FeedingType, -FoodI, -FoodTroph)
  dataset_functionalgroup <- dataset_functionalgroup %>%
    rename(FunctionalGroupFishbase = FunctionalGroup)


  #Sin duplicados
  source(file.path("R", "noduplicates.r"))
  dataset_functionalgroup_sinduplicados <- noduplicates(dataset_functionalgroup, "Species")
  dataset_functionalgroup_sinduplicados$FunctionalGroupFishbase <- gsub("NA;", "", dataset_functionalgroup_sinduplicados$FunctionalGroupFishbase)
  dataset_functionalgroup_sinduplicados$FunctionalGroupFishbase <- gsub("NA", "", dataset_functionalgroup_sinduplicados$FunctionalGroupFishbase)


  nrow(MasterList2)
  nrow(dataset_functionalgroup_sinduplicados)
  mergedatasets <- MasterList2 %>%
    left_join(dataset_functionalgroup_sinduplicados,
              by = c("AcceptedNameGBIF" = "Species"))
  nrow(mergedatasets)

  mergedatasets <- mergedatasets %>%
    mutate(FunctionalGroupFinal = paste(FunctionalGroup, FunctionalGroupFishbase, sep = ";")) %>%
    select(-FunctionalGroup, -FunctionalGroupFishbase)
  mergedatasets$FunctionalGroupFinal <- gsub(";NA", "", mergedatasets$FunctionalGroupFinal)
  mergedatasets$FunctionalGroupFinal <- gsub("NA;", "", mergedatasets$FunctionalGroupFinal)
  mergedatasets$FunctionalGroupFinal <- gsub("NA", "", mergedatasets$FunctionalGroupFinal)

  head(mergedatasets$FunctionalGroupFinal)
  View(mergedatasets)

  nas_masterlistfunctionalgroup <- mergedatasets %>%
    filter(FunctionalGroupFinal=="")


 # Guardar resultado
   write.xlsx(mergedatasets, "OutputFiles/Intermediate/Step11_ObtainFuctionalGroup_Masterlist.xlsx")

}
