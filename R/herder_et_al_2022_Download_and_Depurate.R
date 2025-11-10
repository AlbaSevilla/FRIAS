herder_et_al_2022_Download_and_Depurate <- function(){
  ################## DESCARGAR ##############################
  ###########################################################
  #A mano http://www.reabic.net/aquaticinvasions/2022/Supplements/AI_2022_Herder_etal_SupplementaryMaterial.pdf



  ###################################################
  ###### DEPURAR ####################################
  ###################################################
  archivo <- "Inputfiles/originaldatabase_herder_et_al_2022.pdf"
  lista9 <- pdf_text(archivo)
  texto_tabla <- lista9


  lineas <- unlist(strsplit(texto_tabla, "\n"))
  lineas <- trimws(lineas)
  lineas <- lineas[lineas != ""]
  lineas <- lineas[!grepl("Supplementary material|Recommended citation|Table S1|family\\s+genus / species|English name|Indonesian name|origin|first record|latest record", lineas, ignore.case = TRUE)]
  familias <- c("Anabantidae", "Aplocheilidae", "Channidae", "Cichlidae", "Clariidae", "Cyprinidae", "Osphronemidae", "Poeciliidae", "Synbranchidae", "Serrasalmidae")
  tabla_limpia <- c()
  i <- 1
  while (i <= length(lineas)) {
    l <- lineas[i]
    if (any(sapply(familias, function(fam) startsWith(l, fam)))) {
      actual <- l
      while (i < length(lineas) && !any(sapply(familias, function(fam) startsWith(lineas[i+1], fam))) && lineas[i+1] != "") {
        actual <- paste(actual, lineas[i+1])
        i <- i + 1
      }
      tabla_limpia <- c(tabla_limpia, actual)
    }
    i <- i + 1
  }

  tabla_lista <- strsplit(tabla_limpia, " {2,}")

  max_cols <- max(sapply(tabla_lista, length))
  tabla_lista <- lapply(tabla_lista, function(x) {length(x) <- max_cols; x})
  tabla_df <- as.data.frame(do.call(rbind, tabla_lista), stringsAsFactors = FALSE)

  colnames(tabla_df) <- c("Family", "Genus_Species", "English_name",
                          "Indonesian_name", "Origin", "First_record", "Latest_record")
  tabla_df <- tabla_df %>%
    select(Genus_Species, everything())
  write.xlsx(tabla_df, "Inputfiles/originaldatabase_herder_et_al_2022.xlsx", rowNames = FALSE)

  #Habitat ya sabemos que son freshwater
  tabla_df$Habitat_Database <- "Freshwater"
  tabla_df$Invaded_country <- "Indonesia"

  dataset <- tabla_df %>%
    mutate(
      Origin = str_extract_all(Origin, "\\b[A-Z][a-z]+\\b"),
      Origin = lapply(Origin, function(x) unique(x)),
      Origin = sapply(Origin, function(x) paste(x, collapse = ", "))
    )

  write.xlsx(dataset, "Inputfiles/freshwatersubset_herder_et_al_2022.xlsx")

}

