CorrectNamesGBIF <- function(){

  MasterList <- read.xlsx("OutputFiles/Intermediate/Step9_NoMicroorganism_MasterList.xlsx")
  names_table <- read.xlsx("TablesToStandardize/Standardization_ErrorsAcceptedNameGBIF.xlsx")


  names_table <- names_table %>%
    dplyr::rename(OriginalNameDB = Specie_Name)

  MasterList <- MasterList %>%
    left_join(
      names_table %>% select(OriginalNameDB, AcceptedNameGBIF, ID_GBIF),
      by = "OriginalNameDB",
      suffix = c("", ".new")
    ) %>%
    mutate(
      AcceptedNameGBIF = ifelse(
        is.na(AcceptedNameGBIF) | AcceptedNameGBIF == "",
        AcceptedNameGBIF.new,
        AcceptedNameGBIF
      ),
      ID_GBIF = ifelse(
        is.na(ID_GBIF) | ID_GBIF == "",
        ID_GBIF.new,
        ID_GBIF
      )
    ) %>%
    select(-AcceptedNameGBIF.new, -ID_GBIF.new)

  nrow(MasterList)
  # Filtrar las filas donde AcceptedNameGBIF contiene exactamente un nombre (sin ";")
  MasterList_filtrada <- MasterList %>%
    filter(!grepl(";", AcceptedNameGBIF))
  nrow(MasterList_filtrada)

  MasterList_filtrada <- MasterList_filtrada %>%
    filter(!is.na(OriginalNameDB))

  nrow(MasterList_filtrada)

  source(file.path("R", "noduplicates.r"))
  MasterList_filtrada <- noduplicates(MasterList_filtrada, "OriginalNameDB")

  cols <- setdiff(names(MasterList_filtrada), "Source_Data")
  MasterList_filtrada[cols] <- lapply(MasterList_filtrada[cols], function(x) {
    x[x == "" | x == "NA"] <- NA
    x
  })
  MasterList_filtrada <- MasterList_filtrada %>%
    filter(!is.na(AcceptedNameGBIF))

  write.xlsx(MasterList_filtrada,
             file.path("OutputFiles", "Intermediate", paste0("Step10_CorrectedAcceptedNameGBIF_Masterlist.xlsx")),
             sep = ";",
             row.names = FALSE,
             col.names = TRUE)

}
