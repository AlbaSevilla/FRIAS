CorrectNamesGBIF <- function() {
  #Masterlist
  MasterList <- read.csv("OutputFiles/Intermediate/step2_masterlist.csv")
  MasterList <- MasterList %>%
    separate_rows(OriginalNameDB, sep=";")
  Obtain_AcceptedName_namelookup <- function(dataset, check_col = "AcceptedNameGBIF", lookup_col = "OriginalNameDB") {
    dataset_out <- dataset
    search <- !grepl(" ", dataset[[check_col]])
    names_to_lookup <- dataset[[lookup_col]][search]
    result <- character(length(names_to_lookup))
  for (i in seq_along(names_to_lookup)) {
      species <- names_to_lookup[i]
      cat("Processing:", species, "(", i, "/", length(names_to_lookup), ")\n")
      result[i] <- tryCatch({
        res <- name_lookup(species, limit = 1)
        if (!is.null(res$data) && nrow(res$data) > 0) {
          cn <- res$data$canonicalName[1]
          if (is.null(cn) || length(cn) == 0) NA else cn
        } else {
          NA
        }
      }, error = function(e) NA)
    }
    dataset_out$AcceptedName_GBIF_clean <- dataset[[check_col]]
    dataset_out$AcceptedName_GBIF_clean[search] <- result
    return(dataset_out)
  }

  MasterList <- Obtain_AcceptedName_namelookup(MasterList,
                                      check_col = "AcceptedNameGBIF",
                                      lookup_col = "OriginalNameDB")
  MasterList$AcceptedNameGBIF <- MasterList$AcceptedName_GBIF_clean
  MasterList$AcceptedName_GBIF_clean <- NULL
  update_gbif_id <- function(MasterList) {
    MasterList$ID_GBIF_NEW <- MasterList$ID_GBIF
    empty_rows <- which(is.na(MasterList$ID_GBIF) | MasterList$ID_GBIF == "")

    for (i in empty_rows) {
      species_name <- MasterList$OriginalNameDB[i]
      message("Processing species: ", species_name)
      result <- tryCatch({
        lookup <- name_lookup(species_name)
        if (length(lookup$data) > 0) {
          valid_keys <- na.omit(lookup$data$nameKey)
          if (length(valid_keys) > 0) {
            valid_keys[1]
          } else {
            ""
          }
        } else {
          ""
        }
      }, error = function(e) "")
      MasterList$ID_GBIF_NEW[i] <- result
    }
    return(MasterList)
  }

  MasterList2 <- update_gbif_id(MasterList)
  MasterList2$ID_GBIF <- MasterList2$ID_GBIF_NEW
  MasterList2$ID_GBIF_NEW <- NULL
  MasterList3 <- colapse_through_AcceptedNameGBIF(MasterList2)
  MasterList4 <- MasterList3 %>%
    mutate(AcceptedNameGBIF = sapply(AcceptedNameGBIF, function(x) if (length(x) == 0) NA_character_ else x)) %>%
    mutate(OriginalNameDB = sapply(OriginalNameDB, function(x) if (length(x) == 0) NA_character_ else x))
  names_table <- read.xlsx("TablesToStandardize/Table S2.xlsx", sheet="1. Accepted name") %>%
    rename(OriginalNameDB = Species_Name) %>%
    mutate(AcceptedNameGBIF = sapply(AcceptedNameGBIF, paste, collapse = ""))
  MasterList_final <- merge(
    MasterList4,
    names_table[, c("OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF")],
    by = "OriginalNameDB",
    all.x = TRUE,
    suffixes = c("", ".new")
  ) %>%
    mutate(
      AcceptedNameGBIF = ifelse(is.na(AcceptedNameGBIF) | AcceptedNameGBIF == "",
                                AcceptedNameGBIF.new, AcceptedNameGBIF),
      ID_GBIF = ifelse(is.na(ID_GBIF) | ID_GBIF == "",
                       ID_GBIF.new, ID_GBIF)
    ) %>%
    select(-AcceptedNameGBIF.new, -ID_GBIF.new) %>%
    filter(
      !grepl(";", AcceptedNameGBIF),
      !is.na(OriginalNameDB),
      lengths(strsplit(AcceptedNameGBIF, " ")) > 1
    )
  MasterList_final2 <- noduplicates(MasterList_final, "AcceptedNameGBIF")
  cols <- setdiff(names(MasterList_final2), "Source_Data")
  MasterList_final2[cols] <- lapply(MasterList_final2[cols], function(x) {
    x[x == "" | x == "NA"] <- NA
    x
  })
  MasterList_final3 <- MasterList_final2 %>%
    filter(!is.na(AcceptedNameGBIF))
  MasterList_final3 <- MasterList_final3[grepl("^[A-Za-z]+\\s[A-Za-z]+$", MasterList_final3$AcceptedNameGBIF), ]
  MasterList_final3[] <- lapply(MasterList_final3, function(x) {
    if (is.factor(x)) as.character(x)
    else if (is.list(x)) sapply(x, toString)  # colapsa listas en cadenas
    else x
  })
  MasterList_final3_NA <- MasterList_final2 %>%
    filter(
      is.na(AcceptedNameGBIF) |
        !grepl("^[A-Za-z]+\\s[A-Za-z]+$", AcceptedNameGBIF)
    )

  #Save
  write.xlsx(MasterList_final3,"OutputFiles/Intermediate/step3_correctedacceptednameGBIF_masterlist.xlsx",
    sep = ";", row.names = FALSE, col.names = TRUE)
  write.csv(MasterList_final3,"OutputFiles/Intermediate/step3_correctedacceptednameGBIF_masterlist.csv")

  #Save na cases
  write.xlsx(MasterList_final3_NA, "OutputFiles/Check/NA_AcceptedNamesGBIF_masterlist.xlsx")
  write.csv(MasterList_final3_NA,"OutputFiles/Check/NA_AcceptedNamesGBIF_masterlist.csv")
}
