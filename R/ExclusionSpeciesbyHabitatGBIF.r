ExclusionSpeciesbyHabitatGBIF <- function(){

  ###################################################
  ####### OBTENER INCOHERENCIAS #####################
  ###################################################

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

    # Añadir el nombre de la base al final del habitat, si no es NA
    dat$habitat <- ifelse(
      is.na(dat$Habitat),
      NA,
      paste0(dat$Habitat, " (", db_names[i], ")")
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
  alldat$Group <- gsub("NA", "", alldat$Group)
  originalnames <- alldat$OriginalNameDB
  alldat$CorrectedAcceptedNameGBIF <- name_backbone_checklist(originalnames)$canonicalName
  alldat$AcceptedNameGBIF <- alldat$CorrectedAcceptedNameGBIF

  #Nos quedamos con el primer ID_GBIF (son iguales todos los de una celda)
  alldat$ID_GBIF <- sapply(strsplit(as.character(alldat$ID_GBIF), ";"), `[`, 1)

  #CAMBIAMOS LOS , POR ; EN ALLDAT
  alldat[] <- lapply(alldat, function(x) gsub(",", ";", x))

  #Colapsamos
  source(file.path("R", "colapsar_por_AcceptedNameGBIF.r"))
  alldat_final <- colapsar_por_AcceptedNameGBIF(alldat)
  source(file.path("R", "noduplicates.r"))
  alldat_final <- noduplicates(alldat_final, "OriginalNameDB")
  alldat_final <- alldat_final %>%
    rename(Habitat_FRIAS = habitat)
  names(alldat_final)
  dim(alldat_final)
  View(alldat_final)
  #################### GUARDAMOS RESULTADOS ###############################
  write.xlsx(alldat_final, file.path("OutputFiles", "Check", "Habitat_MasterList.xlsx"))
  write.csv(alldat_final, file.path("OutputFiles", "Check", "Habitat_MasterList.csv"), row.names = FALSE)




  #OBTENEMOS LAS COHERENCIAS/INCOHERENCIAS
  MasterList <- read_excel("OutputFiles/Check/Habitat_MasterList.xlsx")
  MasterList2 <- MasterList %>%
    filter(
      AcceptedNameGBIF == "NA" |              # conservar si es exactamente "NA" (como string)
        str_count(AcceptedNameGBIF, "\\S+") > 1 # o si tiene más de una palabra
    )
  source(file.path("R", "check_habitat.r"))
  especies <- MasterList2$OriginalNameDB
  nombre_aceptado <- name_backbone_checklist(especies)$canonicalName
  MasterList_Incoherencias <- check_habitat(nombre_aceptado, MasterList2)

  MasterList_Incoherencias2 <- MasterList_Incoherencias %>%
    mutate(Habitat_GBIF = Habitat) %>%
    mutate(Habitat_DDBB = Habitat_FRIAS) %>%
    select(-Habitat,-Species,-Habitat_FRIAS)
  write_xlsx(MasterList_Incoherencias2, "OutputFiles/Check/Check_Habitat_Masterlist.xlsx") #MATERLIST CON EL HABITAT POR GBIF Y EL HABITAT ORIGINAL


  MasterList_Incoherencias3 <- MasterList_Incoherencias2 %>%
    filter(!grepl("FRESHWATER", Habitat_GBIF))
  write_xlsx(MasterList_Incoherencias3, "OutputFiles/Check/Incoherences_Habitat_Masterlist.xlsx") #masterlist donde el habitat de gbif y de la bbdd NO coinciden


  MasterList_Incoherencias4 <- MasterList_Incoherencias2 %>%
    filter(grepl("FRESHWATER", Habitat_GBIF) | is.na(Habitat_GBIF))

  nombres_quedarnos <- MasterList_Incoherencias4$OriginalNameDB


  #############################################################
  ###### QUEDARNOS CON LAS FRESHWATER DE LA CHECKLIST #########
  #############################################################

  MasterList_anterior <- read_excel("OutputFiles/Intermediate/Step3_CorrectedAcceptedNameGBIF_Masterlist.xlsx")
  MasterList_anterior <- MasterList_anterior %>%
    rename(Habitat_DDBB = Habitat)
  source(file.path("R", "check_habitat.r"))
  nombres_Aceptados <- MasterList_anterior$AcceptedNameGBIF
  habitat_species <- check_habitat(nombres_Aceptados, MasterList_anterior)
  View(habitat_species)

  table(habitat_species$Habitat)

  #Nos quedamos con las freshwater y las que no encuentra el Habitat GBIF
  dat_fresh <- habitat_species %>%
    mutate(
      Habitat_DDBB_clean = str_trim(str_remove_all(Habitat_DDBB, "\\s*\\([^\\)]+\\)")),
      Habitat_DDBB_clean = sapply(
        strsplit(Habitat_DDBB_clean, ";|,"),
        function(x) paste(unique(trimws(x)), collapse = "; ")
      ),
      Habitat = if_else(is.na(Habitat) | Habitat == "", Habitat_DDBB_clean, Habitat)
    ) %>%
    filter(grepl("freshwater", Habitat, ignore.case = TRUE)) %>%
    select(-Habitat, -Habitat_DDBB_clean) %>%
    rename(Habitat = Habitat_DDBB)


  dat_no_fresh <- habitat_species %>%
    mutate(
      Habitat_DDBB_clean = str_trim(str_remove_all(Habitat_DDBB, "\\s*\\([^\\)]+\\)")),
      Habitat_DDBB_clean = sapply(
        strsplit(Habitat_DDBB_clean, ";|,"),
        function(x) paste(unique(trimws(x)), collapse = "; ")
      ),
      Habitat = if_else(is.na(Habitat) | Habitat == "", Habitat_DDBB_clean, Habitat)
    ) %>%
    filter(!grepl("freshwater", Habitat, ignore.case = TRUE))


#  View(dat_fresh)
  #View(dat_no_fresh)
  #Arreglamos columna OldestDate
  dat_fresh$FirstRecords <- sub("^(\\d{4})(\\d{4})$", "\\1,\\2", dat_fresh$FirstRecords)
  dat_fresh$OldestDate <- sub(",.*", "", dat_fresh$FirstRecords)

  names(dat_fresh)
  dim(dat_fresh)

  nas_names <- dat_fresh2 %>%
    filter(is.na(AcceptedNameGBIF) | AcceptedNameGBIF == "")

  dat_fresh2 <- dat_fresh %>%
    rowwise() %>%
    mutate(
      AcceptedNameGBIF = if (is.na(AcceptedNameGBIF) || AcceptedNameGBIF == "") {
        res <- name_backbone_checklist(OriginalNameDB)$canonicalName
        if (length(res) == 0 || is.na(res)) "" else res
      } else {
        AcceptedNameGBIF
      }
    ) %>%
    ungroup()


  dat_fresh2 <- dat_fresh2 %>%
    select(
      # 1. Identidad taxonómica
      OriginalNameDB,
      AcceptedNameGBIF,
      ID_GBIF,

      # 2. Clasificación taxonómica
      Kingdom,
      Phylum,
      Class,
      Order,
      Family,

      # 3. Ecología funcional
      FunctionalGroup,
      Group,

      # 4. Historia de introducción
      EstablishmentMeans,
      Pathways,
      FirstRecords,
      OldestDate,

      # 5. Distribución nativa
      NativeRange,

      # 6. Distribución invadida
      InvadedRange,
      InvadedRangeISO3,

      # 7. Impactos ecológicos
      AffectedNativeSpecies,
      EICATImpact,
      Mechanisms,
      EICAT_by_Mechanism,

      # 8.
      Habitat,
      Source_Data
    )

  write_xlsx(dat_fresh2, "OutputFiles/Intermediate/Step4_SelectedFreshwaterGBIF_Masterlist.xlsx") #masterlist donde el habitat de gbif y de la bbdd SI coinciden
}
