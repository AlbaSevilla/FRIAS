GillesJrEtAl_Download_and_Depurate <- function(){
  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://bioone.org/journalArticle/Download?urlId=10.25225%2Fjvb.23032"
  destfile <- "InputFiles/Step0_OriginalDatabase_GillesJrEtAl.pdf"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  ##############################################
  ########### DEPURAR ##########################
  ##############################################
  text_pages <- pdf_text(destfile)[5:6]
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
  df <- do.call(rbind, tabla_limpia) %>% as.data.frame()
  colnames(df) <- c("Taxon_name", "Common_name", "FishBase", "GISD", "CABI", "IESNA", "GScholar", "Outcome")
  #Nos quedamos con los invasive
  df <- df[-1,]
  df <- df %>% filter(Outcome == "Invasive")

  #Sacamos cuales son freshwater para GBIF y nos quedamos con ellas
  source(file.path("R", "check_habitat.r"))
  dataset <- df
  nombres <- df$Taxon_name
  nombre_aceptado <- name_backbone_checklist(nombres)$canonicalName
  dataset_habitat <- check_habitat(nombre_aceptado, dataset)
  #Quedarnos con las freshwater
  dataset_frw <- dataset_habitat %>%
    filter(grepl("FRESHWATER", Habitat))

  write_xlsx(df, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_GillesJrEtAl.xlsx")

  #########################################################
  #########################################################
  #Vemos qué especies están en la masterlist, y para esas especies, añadimos esta bd como referencia
  dataset <- read_excel("Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_GillesJrEtAl.xlsx")
  FRIAS_SpeciesList_MasterList <- read_excel("FinalFiles/FRIAS_SpeciesList_MasterList.xlsx")

  # Especies que están en dataset pero no en FRIAS_SpeciesList_MasterList
  setdiff(dataset$AcceptedNameGBIF, FRIAS_SpeciesList_MasterList$AcceptedNameGBIF)
}

