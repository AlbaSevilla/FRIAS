CABI_2025_Download_and_Depurate <- function() {

  datos <- read.csv(file.path("Inputfiles","Step0_OriginalDatabase_CABI_2025 (2).csv"),sep=",")
  View(datos)
  columnas_a_quedarse <- c("Preferred.scientific.name","Kingdom", "Order", "Family","Phylum","Class","Habitat","Pathways", "Datasheet.URL....")
  data_subset_CABI_2025 <- datos[, columnas_a_quedarse]
  names(data_subset_CABI_2025)[names(data_subset_CABI_2025) == "Preferred.scientific.name"] <- "PreferredScientificName"
  data_subset_CABI_2025$columna1 <- gsub(";", "", data_subset_CABI_2025$Datasheet.URL....)

  # NO DUPLICADOS
  CABI_2025_sinduplicados <- data_subset_CABI_2025 %>%
    dplyr::group_by(PreferredScientificName) %>%
    dplyr::summarise(across(everything(), ~ {
      texto <- as.character(.)
      vals <- unlist(str_split(texto, "[,;|]+|\\s{2,}"))
      vals <- str_trim(vals)
      vals <- vals[vals != ""]
      vals <- unique(vals)
      paste(vals, collapse = ", ")
    }), .groups = "drop")

  CABI_2025_sinduplicados$Habitat <- "Freshwater"
  CABI_2025_sinduplicados <- CABI_2025_sinduplicados %>%
    select(-columna1)
  names(CABI_2025_sinduplicados)[names(CABI_2025_sinduplicados) == "Datasheet.URL...."] <- "Url_Specie"
  head(CABI_sinduplicados)

  #Obtenemos correctamente los enlaces:
  CABI_2025_sinduplicados$Url_Specie <- gsub("^https://doi\\.org/", "https://www.cabidigitallibrary.org/doi/", CABI_sinduplicados$Url_Specie)


  write.xlsx(CABI_2025_sinduplicados, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_CABI_2025.xlsx")
  cat("Archivo Excel de CABI_2025 guardado como 'Step0_OriginalDatabaseFreshwaterNODUPLICATES_CABI_2025.xlsx'\n")

  #WEB SCRAPPING

  #web : https://www.CABI_2025digitallibrary.org/journal/CABI_2025compendium/isdt#(Freshwater:ponds,reservoirs,riversStreams,springs,lakes;OR)
  #(vector:403028,403022,376455,376445,376454,376442,376451,376457,376438,376443,376448,403018,376434,376447,376440,376446,376444,376449,403021,376452,376450,376453,376441,376436,376456,376435,376437,376439,403027;OR)
  #https://doi.org/10.1079/CABI_2025compendium.5811
  # CABI_2025_sinduplicados$Url_Specie[1]
  #
  #
  # library(xml2)
  # library(rvest)
  # isc<-read_html("http://www.CABI_2025.org/isc/datasheet/50069")
  # isc %>%
  #   html_node("#toDistributionTable td:nth-child(1)") %>%
  #   html_text()
  #
  #
}
