cabi_2025_Download_and_Depurate <- function() {
  #Descargada a mano

  #Depurar
  datos <- read.csv(file.path("Inputfiles","originaldatabase_cabi_2025.csv"),sep=",")
  columnas_a_quedarse <- c("Preferred.scientific.name","Kingdom", "Order", "Family","Phylum","Class","Habitat","Pathways", "Datasheet.URL....")
  data_subset_cabi_2025 <- datos[, columnas_a_quedarse]
  names(data_subset_cabi_2025)[names(data_subset_cabi_2025) == "Preferred.scientific.name"] <- "PreferredScientificName"
  data_subset_cabi_2025$columna1 <- gsub(";", "", data_subset_cabi_2025$Datasheet.URL....)

  # NO DUPLICADOS
  cabi_2025_sinduplicados <- data_subset_cabi_2025 %>%
    dplyr::group_by(PreferredScientificName) %>%
    dplyr::summarise(across(everything(), ~ {
      texto <- as.character(.)
      vals <- unlist(str_split(texto, "[,;|]+|\\s{2,}"))
      vals <- str_trim(vals)
      vals <- vals[vals != ""]
      vals <- unique(vals)
      paste(vals, collapse = ", ")
    }), .groups = "drop")

  cabi_2025_sinduplicados$Habitat <- "Freshwater"
  cabi_2025_sinduplicados <- cabi_2025_sinduplicados %>%
    select(-columna1)
  names(cabi_2025_sinduplicados)[names(cabi_2025_sinduplicados) == "Datasheet.URL...."] <- "Url_Specie"

  #Obtenemos correctamente los enlaces:
  cabi_2025_sinduplicados$Url_Specie <- gsub("^https://doi\\.org/", "https://www.cabidigitallibrary.org/doi/", cabi_2025_sinduplicados$Url_Specie)

  write.xlsx(cabi_2025_sinduplicados, "./Inputfiles/freshwatersubset_cabi_2025.xlsx")
}
