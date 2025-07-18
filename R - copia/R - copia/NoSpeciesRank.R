NoSpeciesRank <- function (){
  MasterList <- read_excel("OutputFiles/Intermediate/Step4_ObtainTaxonNAS_MasterList.xlsx")
  MasterList <- MasterList %>%
    mutate(NameToQuery = ifelse(is.na(AcceptedNameGBIF) | AcceptedNameGBIF == "",
                                OriginalNameDB,
                                AcceptedNameGBIF))

  # Función para obtener el rank de cada nombre
  get_rank <- function(name) {
    cat("Obteniendo el rango de: ", name, "\n")
    res <- tryCatch({
      out <- name_backbone_checklist(name = name)
      if (!is.null(out$rank)) {
        return(out$rank)
      } else {
        return(NA)
      }
    }, error = function(e) {
      return(NA)
    })
    return(res)
  }

  # Aplicar la función a todos los nombres
  MasterList$GBIF_Rank <- sapply(MasterList$NameToQuery, get_rank)

  table(MasterList$GBIF_Rank)

  MasterList <- MasterList %>%
    select(-NameToQuery)

  write.xlsx(MasterList,
             file.path("OutputFiles", "Intermediate", paste0("Step5_OnlySpeciesRank_MasterList.xlsx")),
             sep = ",",
             row.names = FALSE,
             col.names = TRUE)

  View(MasterList)
}
