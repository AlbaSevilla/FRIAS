MergeDatasets <- function(FileInfo, OutputFiles = TRUE) {
  # Prepare files
  ruta_intermediate <- "OutputFiles/Intermediate"
  if (!dir.exists(ruta_intermediate)) dir.create(ruta_intermediate, recursive = TRUE)

  data_files <- list.files(path = ruta_intermediate, pattern = "^Step1_Prepared.*\\.csv$", full.names = TRUE)
  db_names <- sub("^.*Step1_Prepared_(.*)\\.csv$", "\\1", data_files)

  # Mix files
  cat("Merging databases \n")
  for (i in seq_along(db_names)) {
    cat("Processing the database: ", db_names[i], "\n")
    dat <- suppressMessages(
      read_csv2(data_files[i], show_col_types = FALSE)
    )
    issues <- problems(dat)
    if (nrow(issues) > 0) {
      cat("\n Problems detected in ", db_names[i], ":\n")
      issues_formatted <- issues %>%
        mutate(
          fila = row,
          columna = col,
          valor = actual,
          esperado = expected
        ) %>%
        select(fila, columna, valor, esperado)
      print(issues_formatted)
    }
    dat <- dat %>%
      mutate(
        EICATImpact = tolower(str_trim(EICATImpact)),
        EICATImpact = gsub("[-_/]", "", EICATImpact)
      )
    if (i == 1) {
      dat$OriginalNameDB[dat$OriginalNameDB == "" | is.na(dat$OriginalNameDB)] <-
        dat$AcceptedNameGBIF[dat$OriginalNameDB == "" | is.na(dat$OriginalNameDB)]
      alldat <- dat
    } else {
      dat$OriginalNameDB[dat$OriginalNameDB == "" | is.na(dat$OriginalNameDB)] <-
        dat$AcceptedNameGBIF[dat$OriginalNameDB == "" | is.na(dat$OriginalNameDB)]
      alldat <- merge(alldat, dat, by = "OriginalNameDB", all = TRUE)
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
          alldat[[col]] <- gsub(",", ";", alldat[[col]])
          alldat[[col]] <- trimws(alldat[[col]])
        }
        alldat <- alldat[ , !grepl("\\.x$|\\.y$", names(alldat))]
        numero_especies <- nrow(alldat)
      }
    }
  }


  #Correction of names using GBIF and save them in AcceptedNameGBIF.
  alldat <- alldat %>%
    mutate(
      CorrectedAcceptedNameGBIF = name_backbone_checklist(OriginalNameDB)$canonicalName,
      AcceptedNameGBIF = CorrectedAcceptedNameGBIF,
      ID_GBIF = sapply(strsplit(as.character(ID_GBIF), ";"), `[`, 1)
    )

  #Replaced ‘NA’ or ‘’ with NA in all columns except Source_Data
  cols <- setdiff(names(alldat), "Source_Data")
  alldat[cols] <- lapply(alldat[cols], function(x) {
    x[x == "" | x == "NA"] <- NA
    x
  })
  #Replace , to ;
  alldat[] <- lapply(alldat, function(x) gsub(",", ";", x))


  cat("Processing oldest dates \n")
  alldat <- alldat %>%
    mutate(
      FirstRecords = gsub("[^0-9;]", "", FirstRecords),
      FirstRecords = sapply(strsplit(FirstRecords, ";"), function(x) {
        x <- str_trim(x)
        paste(sort(x), collapse = ";")
      }),
      FirstRecords = sub("^;", "", FirstRecords),
      OldestDate = sapply(strsplit(FirstRecords, ";"), function(x) {
        x <- str_trim(x)
        if (length(x) == 0 || is.na(x[1]) || x[1] == "") NA else x[1]
      })
    )


  #LIMPIAMOS VALORES
  cat("Cleaning duplicate values within cells \n")
  alldat <- alldat %>%
    mutate(
      across(where(is.character), ~ {
        x <- na_if(.x, "")
        x <- na_if(x, "NA")
        sapply(x, function(cell) {
          if (is.na(cell)) return(NA)
          block <- unlist(strsplit(cell, "\\s*[,;]\\s*"))
          paste(unique(block), collapse = "; ")
        })
      })
    )



  cat("Grouping by AcceptedNameGBIF and collapsing columns \n")
  collapse_by_AcceptedNameGBIF <- function(dataset, key_col = "AcceptedNameGBIF", sep = ";") {
    if (!(key_col %in% names(dataset))) {
      stop(paste("The column", key_col, "does not exist in the dataset"))
    }
    dataset_with_value <- dataset[!is.na(dataset[[key_col]]) & trimws(dataset[[key_col]]) != "", ]
    dataset_without_value <- dataset[is.na(dataset[[key_col]]) | trimws(dataset[[key_col]]) == "", ]
    groups <- split(dataset_with_value, dataset_with_value[[key_col]])
    collapsed_result <- lapply(groups, function(group) {
      lapply(group, function(col) {
        elements <- unlist(strsplit(as.character(col), paste0("\\s*", sep, "\\s*")))
        elements <- unique(trimws(elements[elements != "" & elements != "NA"]))
        paste(elements, collapse = sep)
      })
    })
    dataset_collapsed <- as.data.frame(do.call(rbind, collapsed_result), stringsAsFactors = FALSE)
    final_result <- rbind(dataset_collapsed, dataset_without_value)
    rownames(final_result) <- NULL
    return(final_result)
  }

  alldat_withoutNA <- alldat[!(is.na(alldat$AcceptedNameGBIF) | alldat$AcceptedNameGBIF == "" |
                              is.na(alldat$ID_GBIF) | alldat$ID_GBIF == ""), ]
  alldat_withNA <- alldat[(is.na(alldat$AcceptedNameGBIF) | alldat$AcceptedNameGBIF == "" |
                               is.na(alldat$ID_GBIF) | alldat$ID_GBIF == ""), ]
  alldat_final <- collapse_by_AcceptedNameGBIF(alldat)


  # Replace "" and "NA" with actual NA in all columns except "Source_Data"
  cols <- setdiff(names(alldat_final), "Source_Data")
  alldat_final[cols] <- lapply(alldat_final[cols], function(x) {
    x[x == "" | x == "NA"] <- NA
    x
  })
  # Function to ensure all columns are character before saving
  ensure_character <- function(alldat_final) {
    for (i in seq_along(alldat_final)) {
      if (is.list(alldat_final[[i]])) {
        alldat_final[[i]] <- sapply(alldat_final[[i]], function(x) paste(unlist(x), collapse = ", "))
      } else if (is.factor(alldat_final[[i]])) {
        alldat_final[[i]] <- as.character(alldat_final[[i]])
      }
    }
    return(alldat_final)
  }
  alldat_final <- ensure_character(alldat_final)
  alldat_final <- rbind(alldat_final, alldat_withNA)


  ########################################################################################
  # PREPARE THE MASTERLIST - UNIFIED PIPELINE
  ########################################################################################

  masterlist <- alldat_final
  cols_to_clean <- setdiff(names(masterlist), "Source_Data")
  masterlist[] <- lapply(masterlist, function(col) {
    if (is.character(col)) {
      sapply(col, function(cell) {
        if (is.na(cell)) return(NA)
        cell <- gsub("([;,])\\s+", "\\1", cell)
        cell[cell == ","] <- ";"
        elements <- unlist(strsplit(cell, "\\s*[,;]\\s*"))
        elements <- unique(elements[elements != "" & elements != "NA"])
        elements <- sort(elements)
        cell <- paste(elements, collapse = "; ")
        cell <- gsub("\\s*\\([^\\)]*\\)", "", cell)
        if (cell == "") return(NA) else return(cell)
      })
    } else {
      col
    }
  })
  masterlist <- masterlist %>% dplyr::select(-any_of("CorrectedAcceptedNameGBIF"))
  names(masterlist)[names(masterlist) == "Source_Data"] <- "Source_Data"
  names(masterlist)
  alldat_withNA <- alldat_withNA %>%
    select(-CorrectedAcceptedNameGBIF)
  Step2_MasterList <- rbind(masterlist, alldat_withNA)


  # Order columns
  order_columns <- c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF",
                      "Kingdom","Order","Family","Phylum","Class","FunctionalGroup","Group",
                      "EstablishmentMeans","Pathways",
                      "NativeCountry","InvadedCountry","InvadedCountryISO3",
                      "AffectedNativeSpecies","EICATImpact","Mechanisms","EICAT_by_Mechanism",
                      "FirstRecords","OldestDate",
                      "Habitat","Source_Data")
  Step2_MasterList <- Step2_MasterList[,order_columns]
  Step2_MasterList <- Step2_MasterList %>%
    filter(!is.na(Source_Data) & Source_Data != "")

  ###################################### NAS ####################################################
  #We observe that records do not have accepted names and save them in the check file.

  Step2_MasterList_withoutNA <- Step2_MasterList[!(is.na(Step2_MasterList$AcceptedNameGBIF) | Step2_MasterList$AcceptedNameGBIF == "" |
                                                 is.na(Step2_MasterList$ID_GBIF) | Step2_MasterList$ID_GBIF == ""), ]
  Step2_MasterList_withoutNA_OriginalNameDB <- Step2_MasterList[!(is.na(Step2_MasterList$OriginalNameDB) | Step2_MasterList$OriginalNameDB == "" ), ]

  dim(Step2_MasterList_withoutNA_OriginalNameDB)

  Step2_MasterList_withNA <- Step2_MasterList[(is.na(Step2_MasterList$AcceptedNameGBIF) | Step2_MasterList$AcceptedNameGBIF == "" |
                                                is.na(Step2_MasterList$ID_GBIF) | Step2_MasterList$ID_GBIF == ""), ]

  #################### SAVE RESULTS ###############################
  write.csv(Step2_MasterList, file.path("OutputFiles", "Intermediate", "Step2_MasterList.csv"), row.names = FALSE)
  write_xlsx(Step2_MasterList, file.path("OutputFiles", "Intermediate", "Step2_MasterList.xlsx"))

  if (nrow(Step2_MasterList_withNA) > 0) {
    write.xlsx(Step2_MasterList_withNA, file.path("OutputFiles", "Check", "NA_AcceptedNameGBIF_ID_GBIF_Masterlist.xlsx"))
    write.csv(Step2_MasterList_withNA, file.path("OutputFiles", "Check", "NA_AcceptedNameGBIF_ID_GBIF_Masterlist.csv"), row.names = FALSE)
  }
}
