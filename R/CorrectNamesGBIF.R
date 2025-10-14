CorrectNamesGBIF <- function() {
  #Masterlist
  MasterList <- read.xlsx("OutputFiles/Intermediate/Step2_MasterList.xlsx")

  source(file.path("R", "colapse_through_AcceptedNameGBIF.r"))
  MasterList <- colapse_through_AcceptedNameGBIF(MasterList)

  MasterList <- MasterList %>%
    mutate(AcceptedNameGBIF = sapply(AcceptedNameGBIF, function(x) if (length(x) == 0) NA_character_ else x)) %>%
    mutate(OriginalNameDB = sapply(OriginalNameDB, function(x) if (length(x) == 0) NA_character_ else x))

  #Correspondence table
  names_table <- read.xlsx("TablesToStandardize/standardization_tables.xlsx", sheet="errorsinacceptedname_table") %>%
    rename(OriginalNameDB = Specie_Name) %>%
    mutate(OriginalNameDB = sapply(OriginalNameDB, paste, collapse = "")) %>%
    mutate(AcceptedNameGBIF = sapply(AcceptedNameGBIF, paste, collapse = ""))

  #Apply corrections
  MasterList_final <- merge(
    MasterList,
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


  #Remove duplicates and merge
  source(file.path("R", "noduplicates.r"))
  MasterList_final2 <- noduplicates(MasterList_final, "AcceptedNameGBIF")
  cols <- setdiff(names(MasterList_final2), "Source_Data")
  MasterList_final2[cols] <- lapply(MasterList_final2[cols], function(x) {
    x[x == "" | x == "NA"] <- NA
    x
  })

  #Solve mistakes
  MasterList_final3 <- MasterList_final2 %>%
    filter(!is.na(AcceptedNameGBIF))
  MasterList_final3[] <- lapply(MasterList_final3, function(x) {
    if (is.factor(x)) as.character(x)
    else if (is.list(x)) sapply(x, toString)  # colapsa listas en cadenas
    else x
  })

  #Save result
  write.xlsx(
    MasterList_final3,
    file.path("OutputFiles", "Intermediate", "Step3_CorrectedAcceptedNameGBIF_Masterlist.xlsx"),
    sep = ";",
    row.names = FALSE,
    col.names = TRUE
  )
}
