urrutia_et_al_2017_Download_and_Depurate <- function(){
  #Download
  url <- "https://gayanabotanica.cl/index.php/gb/article/download/245/81/1891"
  destfile <- "InputFiles/originaldatabase_urrutia_et_al_2017.pdf"
  download.file(url, destfile, mode = "wb")

  #Depurate
  list9 <- pdf_text(destfile)[5]
 text_table <- list9
  lines <- unlist(strsplit(texto_table, "\n"))
  lines <- lines[!grepl("^\\s*$", lines)]
  lines <- lines[!grepl("table 1|Family, geographical origin|Plantas acuáticas invasoras", lines)]
  table_clean <- c()
  for (i in seq_along(lines)) {
    if (i == 1) {
      table_clean <- lines[i]
    } else if (grepl("^\\s{10,}", lines[i])) { # Si la línea empieza con muchos espacios, es continuación
      table_clean[length(table_clean)] <- paste(table_clean[length(table_clean)], trimws(lines[i]))
    } else {
      table_clean <- c(table_clean, lines[i])
    }
  }
  table_clean <- table_clean[!grepl("^\\d+\\s*$", table_clean)]
  table_list <- strsplit(table_clean, " {2,}")
  table_dataset <- as.data.frame(do.call(rbind, table_list), stringsAsFactors = FALSE)
  table_dataset <- table_dataset[,c(1:4, 8)]
  colnames(table_dataset) <- table_dataset[1,]
  colnames(table_dataset) <- gsub(" ", "_", colnames(table_dataset))
  table_dataset <- table_dataset[-1,]
  write.xlsx(table_dataset, "Inputfiles/originaldatabase_urrutia_et_al_2017.xlsx")
  dataset <- table_dataset
  nombres <- dataset$Especie
  nom_acep <- name_backbone_checklist(nombres)$canonicalName
  habitat_dat <- check_habitat(nom_acep, dataset)
  habitat_dat <- habitat_dat[, !is.na(names(habitat_dat)) & names(habitat_dat) != ""]
  freshwater_species <- habitat_dat %>% dplyr::filter(grepl("FRESHWATER", Habitat))
  freshwater_species$Invaded_country <- "Chile"

  write.xlsx(freshwater_species, "Inputfiles/freshwatersubset_urrutia_et_al_2017.xlsx")
}
