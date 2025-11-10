gilles_et_al_Download_and_Depurate <- function(){
  #Descargar a mano
  #url <- "https://bioone.org/journalArticle/Download?urlId=10.25225%2Fjvb.23032"


  ##############################################
  ########### DEPURAR ##########################
  ##############################################
  file <- "InputFiles/originaldatabase_gilles_et_al.pdf"
  text_pages <- pdf_text(file)[5:6]
  texto <- text_pages
  lineas <- str_split(texto, "\n")[[1]]
  lineas_especies <- lineas %>%
    str_trim() %>%
    .[str_count(., "\\s{2,}") >= 7]
  datos <- str_split(lineas_especies, "\\s{2,}")
  limpiar_fila <- function(fila) {
    if (length(fila) > 8) {
      taxon <- fila[1]
      common <- fila[2]
      while (length(fila) > 8) {
        taxon <- paste(taxon, fila[2])
        fila <- fila[-2]
      }
      return(c(taxon, fila[-1]))
    } else {
      return(fila)
    }
  }
  tabla_limpia <- lapply(datos, limpiar_fila)
  dataset <- do.call(rbind, tabla_limpia) %>% as.data.frame()
  colnames(dataset) <- c("Taxon_name", "Common_name", "FishBase", "GISD", "CABI", "IESNA", "GScholar", "Outcome")
  #Nos quedamos con los invasive
  dataset <- dataset[-1,]
  dataset <- dataset %>% filter(Outcome == "Invasive")

  #Sacamos cuales son freshwater para GBIF y nos quedamos con ellas
  source(file.path("R", "check_habitat.r"))
  dataset <- dataset
  nombres <- dataset$Taxon_name
  nombre_aceptado <- name_backbone_checklist(nombres)$canonicalName
  dataset_habitat <- check_habitat(nombre_aceptado, dataset)
  #Quedarnos con las freshwater
  dataset_frw <- dataset_habitat %>%
    filter(grepl("FRESHWATER", Habitat))

  write_xlsx(dataset_frw, "InputFiles/freshwatersubset_gilles_et_al.xlsx")
  write_xlsx(dataset_frw, "InputFiles/originaldatabase_gilles_et_al.xlsx")
}

