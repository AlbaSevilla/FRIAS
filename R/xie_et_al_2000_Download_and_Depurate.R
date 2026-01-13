xie_et_al_2000_Download_and_Depurate <- function(){
  #Depurate
  destfile <- "InputFiles/originaldatabase_xie_et_al_2000.pdf"
  text_pages <- pdf_text(destfile)[14:23]
  text <- text_pages
  extraer_info_Species <- function(text) {
    blocks <- str_split(text, "\n\n\n|\\n\\n")[[1]]  #Dividir por Species usando saltos de línea dobles o triples
      clean_block <- lapply(blocks, function(bloque) {
        bloque_limpio <- str_remove_all(bloque, "\\([^\\)]*\\)") #Eliminamos el contenido entre paréntesis
        nombre <- str_extract( #Extraemos el nombre común y científico
          bloque,
          "^[-A-Za-z\\s,'’]+\\s*\\([A-Za-z\\s\\.]+(?: spp\\.| sp\\.| [a-z]+)?\\)(\\s*(and|&)\\s*[-A-Za-z\\s,'’]+\\s*\\([A-Za-z\\s\\.]+(?: spp\\.| sp\\.| [a-z]+)?\\))?"
        )
        if (is.na(nombre)) return(NULL)
        nativo <- str_extract(bloque_limpio, "(?:a native of|is native to|[Nn]ative to|was introduced to the former USSR from|was introduced to|from) [^.]+\\.")
        paises_nativos <- if (!is.na(nativo)) { #Obtenemos el Rango Nativo
          paises <- str_extract_all(nativo, "[A-Z][a-z]+(?: [A-Z][a-z]+)*")[[1]]
          paises_limpios <- paises[!grepl("Native", paises)]
          if (length(paises_limpios) > 0) paises_limpios[1] else NA
        } else NA
        First_Record <- str_extract_all(bloque_limpio, "\\b(mid-)?\\d{4}s?|[A-Z][a-z]+ \\d{4}\\b")[[1]] #Extraemos las fechas

        list(
          Species = nombre,
          nativos = paises_nativos,
          First_Record = unique(First_Record)
        )
      })
      clean_block <- clean_block[!sapply(clean_block, is.null)]
      dataset <- do.call(rbind, lapply(clean_block, function(x) {
        data.frame(
          Species = x$Species,
          Rango_Nativo = x$nativos,
          First_Record = paste(x$First_Record, collapse = ", "),
          stringsAsFactors = FALSE
        )
      }))

      return(dataset)
    }
    DATASET_CHINA <- extraer_info_Species(paste(text_pages, collapse = "\n"))
    write.xlsx(DATASET_CHINA, "./Inputfiles/originaldatabase_xie_et_al_2000.xlsx")
    DATASET_CHINA <- read_excel("Inputfiles/originaldatabase_xie_et_al_2000.xlsx")
    dataset <- DATASET_CHINA
    Species_list0 <- DATASET_CHINA$Species
    Species_list1 <- str_match(Species_list0, "\\(([^\\)]+)\\)")[,2]  # Solo el primero
    Species_list <- name_backbone_checklist(Species_list1)$canonicalName
    dataset_actualizado <- check_habitat(Species_list, dataset)
    dataset_freshwater <- dataset_actualizado %>%
      filter(grepl("FRESHWATER", Habitat) | is.na(Habitat))
    Invaded_Country <- "China"
    dataset_freshwater$Invaded_Country <- Invaded_Country
    dataset_freshwater$OldestDate <- dataset_freshwater$First_Record
    dataset_freshwater$OldestDate <- str_replace_all(dataset_freshwater$OldestDate,
                                                       c("Since " = "",
                                                         "s" = "",
                                                         "By " = "",
                                                         "In " = "",
                                                         "mid-" = ""))
    dataset_freshwater$First_Record <- str_replace_all(dataset_freshwater$First_Record,
                                                     c("Since " = "",
                                                       "s" = "",
                                                       "By " = "",
                                                       "In " = ""))
    dataset_freshwater <- OldestDate(dataset_freshwater, "OldestDate")

    #Save
    write.xlsx(dataset_freshwater, "./Inputfiles/freshwatersubset_xie_et_al_2000.xlsx")
}
