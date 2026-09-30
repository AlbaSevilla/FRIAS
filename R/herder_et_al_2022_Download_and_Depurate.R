herder_et_al_2022_Download_and_Depurate <- function(){
  #Depurate
  file_path <- "InputFiles/originaldatabase_herder_et_al_2022.pdf"
  pdf_pages <- pdf_text(file_path)
  table_text <- pdf_pages
  lines <- unlist(strsplit(table_text, "\n"))
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  lines <- lines[!grepl("Supplementary material|Recommended citation|Table S1|family\\s+genus / species|English name|Indonesian name|origin|first record|latest record",
                        lines, ignore.case = TRUE)]
  families <- c("Anabantidae", "Aplocheilidae", "Channidae", "Cichlidae", "Clariidae",
                "Cyprinidae", "Osphronemidae", "Poeciliidae", "Synbranchidae", "Serrasalmidae")
  clean_table <- c()
  i <- 1
  while (i <= length(lines)) {
    line <- lines[i]
    if (any(sapply(families, function(fam) startsWith(line, fam)))) {
      entry <- line
      while (i < length(lines) &&
             !any(sapply(families, function(fam) startsWith(lines[i+1], fam))) &&
             lines[i+1] != "") {
        entry <- paste(entry, lines[i+1])
        i <- i + 1
      }
      clean_table <- c(clean_table, entry)
    }
    i <- i + 1
  }
  split_table <- strsplit(clean_table, " {2,}")
  max_cols <- max(sapply(split_table, length))
  split_table <- lapply(split_table, function(x) {length(x) <- max_cols; x})
  dataset <- as.data.frame(do.call(rbind, split_table), stringsAsFactors = FALSE)
  colnames(dataset) <- c("Family", "Genus_Species", "English_Name",
                    "Indonesian_Name", "Origin", "First_Record", "Latest_Record")
  dataset <- dataset %>% select(Genus_Species, everything())
  write.xlsx(dataset, "InputFiles/originaldatabase_herder_et_al_2022.xlsx", rowNames = FALSE)

  #Depurate
  dataset$Habitat <- "Freshwater"
  dataset$Invaded_Country <- "Indonesia"
  dataset <- dataset %>%
    mutate(
      Origin = str_extract_all(Origin, "\\b[A-Z][a-z]+\\b"),
      Origin = lapply(Origin, function(x) unique(x)),
      Origin = sapply(Origin, function(x) paste(x, collapse = ", "))
    )
  names <- dataset$Genus_Species
  dataset$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName

  #Save
  write.xlsx(dataset, "InputFiles/freshwatersubset_herder_et_al_2022.xlsx")
}
