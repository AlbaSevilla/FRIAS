HabitatIncoherences_GBIF_FRIAS <- function(){

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

    seleccionarcolumnas <- c("OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF","Group", "habitat")
    dat <- dat[, seleccionarcolumnas]

    # Añadir el nombre de la base al final del habitat, si no es NA
    dat$habitat <- ifelse(
      is.na(dat$habitat),
      NA,
      paste0(dat$habitat, " (", db_names[i], ")")
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

  View(alldat)
  #Colapsamos
  source(file.path("R", "colapsar_por_AcceptedNameGBIF.r"))
  alldat_final <- colapsar_por_AcceptedNameGBIF(alldat)

  #################### GUARDAMOS RESULTADOS ###############################
  write.xlsx(alldat_final, file.path("OutputFiles", "Check", "Habitat_MasterList.xlsx"))
  write.csv(alldat_final, file.path("OutputFiles", "Check", "Habitat_MasterList.csv"), row.names = FALSE)



  MasterList <- read_excel("OutputFiles/Check/Habitat_MasterList.xlsx")

  #Eliminamos los registros que de AcceptedNameGBIF sea solo una palabra pero quedandonos con los NA
  MasterList2 <- MasterList %>%
    filter(
      AcceptedNameGBIF == "NA" |              # conservar si es exactamente "NA" (como string)
        str_count(AcceptedNameGBIF, "\\S+") > 1 # o si tiene más de una palabra
    )

  #Ahora le pasamos la función
  source(file.path("R", "check_habitat.r"))
  especies <- MasterList2$AcceptedNameGBIF
  MasterList_Incoherencias <- check_habitat(especies, MasterList2)

  #Cambiamos los nombres de las columnas
  MasterList_Incoherencias2 <- MasterList_Incoherencias %>%
    mutate(Habitat_GBIF = Habitat) %>%
    mutate(Habitat_DDBB = habitat) %>%
    select(-habitat,-Species,-Habitat)

  write_xlsx(MasterList_Incoherencias2, "OutputFiles/Check/Check_Habitat_Masterlist.xlsx")


  #Ahora sacamos los que el campo habitat no coincide con GBIF
  MasterList_Incoherencias3 <- MasterList_Incoherencias2 %>%
    filter(!grepl("FRESHWATER", Habitat_GBIF))

  #Lo guardamos en un archivo final
  write_xlsx(MasterList_Incoherencias3, "OutputFiles/Check/Incoherences_Habitat_Masterlist.xlsx")

}
