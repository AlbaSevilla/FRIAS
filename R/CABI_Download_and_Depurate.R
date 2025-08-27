CABI_Download_and_Depurate <- function() {

  datos <- read.csv(file.path("Inputfiles","Step0_OriginalDatabase_CABI (2).csv"),sep=",")
  View(datos)
  columnas_a_quedarse <- c("Preferred.scientific.name","Kingdom", "Order", "Family","Phylum","Class","Habitat","Pathways", "Datasheet.URL....")
  data_subset_CABI <- datos[, columnas_a_quedarse]
  names(data_subset_CABI)[names(data_subset_CABI) == "Preferred.scientific.name"] <- "PreferredScientificName"
  data_subset_CABI$columna1 <- gsub(";", "", data_subset_CABI$Datasheet.URL....)

  # NO DUPLICADOS
  CABI_sinduplicados <- data_subset_CABI %>%
    dplyr::group_by(PreferredScientificName) %>%
    dplyr::summarise(across(everything(), ~ {
      texto <- as.character(.)
      vals <- unlist(str_split(texto, "[,;|]+|\\s{2,}"))
      vals <- str_trim(vals)
      vals <- vals[vals != ""]
      vals <- unique(vals)
      paste(vals, collapse = ", ")
    }), .groups = "drop")

  CABI_sinduplicados$Habitat <- "Freshwater"
  CABI_sinduplicados <- CABI_sinduplicados %>%
    select(-columna1)
  names(CABI_sinduplicados)[names(CABI_sinduplicados) == "Datasheet.URL...."] <- "Url_Specie"
  head(CABI_sinduplicados)

  #Obtenemos correctamente los enlaces:
  CABI_sinduplicados$Url_Specie <- gsub("^https://doi\\.org/", "https://www.cabidigitallibrary.org/doi/", CABI_sinduplicados$Url_Specie)


  write.xlsx(CABI_sinduplicados, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_CABI.xlsx")
  cat("Archivo Excel de CABI guardado como 'Step0_OriginalDatabaseFreshwaterNODUPLICATES_CABI.xlsx'\n")

  #WEB SCRAPPING

  #web : https://www.cabidigitallibrary.org/journal/cabicompendium/isdt#(Freshwater:ponds,reservoirs,riversStreams,springs,lakes;OR)
  #(vector:403028,403022,376455,376445,376454,376442,376451,376457,376438,376443,376448,403018,376434,376447,376440,376446,376444,376449,403021,376452,376450,376453,376441,376436,376456,376435,376437,376439,403027;OR)
  #https://doi.org/10.1079/cabicompendium.5811
  # CABI_sinduplicados$Url_Specie[1]
  #
  #
  # library(xml2)
  # library(rvest)
  # isc<-read_html("http://www.cabi.org/isc/datasheet/50069")
  # isc %>%
  #   html_node("#toDistributionTable td:nth-child(1)") %>%
  #   html_text()
  #
  #
}
