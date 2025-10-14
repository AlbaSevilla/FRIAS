Ryu_et_al_2017_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  url <- "https://koreascience.kr/article/JAKO201726439088155.pdf"
  destfile <- "InputFiles/Step0_OriginalDatabase_Ryu_et_al_2017.pdf"
  download.file(url, destfile, mode = "wb")


  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  text_pages <- pdf_text(destfile)[11:16]
  texto <- text_pages

  texto <- str_split(paste(text_pages, collapse = "\n"), "\n")[[1]]
  texto <- texto[!str_detect(texto, "^\\s*$|^Appendix|^Scientific name")]
  familia_actual <- NA
  resultados <- list()
  for (linea in texto) {
    if (!str_detect(linea, "\\(")) {
      familia_actual <- str_trim(linea)
    } else {
      matches <- str_match(linea, "^\\s*(.*?)\\s*\\((.*?)\\)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)")
      if (!is.na(matches[1,1])) {
        resultados[[length(resultados)+1]] <- tibble(
          Familia = familia_actual,
          Nombre_cientifico = str_trim(matches[1,2]),
          Nombre_coreano = str_trim(matches[1,3]),
          OD = matches[1,4],
          HV = matches[1,5],
          WE = matches[1,6]
        )
      }
    }
  }
  tabla_pdf <- bind_rows(resultados)
  tabla_final1 <- as.data.frame(tabla_pdf)
  Tabla_fresh <- tabla_final1 %>% filter(grepl("FW", WE))
  Tabla_fresh$Habitat_database <- "FRESHWATER"
  Tabla_fresh <- Tabla_fresh %>% select(-WE)
  columnas_seleccion <- c("Nombre_cientifico", "Habitat_database")
  Tabla_final <- Tabla_fresh[,columnas_seleccion]
  Tabla_final$Invaded_country <- "Korea"
  write_xlsx(Tabla_final, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Ryu_et_al_2017.xlsx")
}
