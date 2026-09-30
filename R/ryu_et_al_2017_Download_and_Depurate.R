ryu_et_al_2017_Download_and_Depurate <- function(){
  #Download
  url <- "https://koreascience.kr/article/JAKO201726439088155.pdf"
  destfile <- "InputFiles/originaldatabase_ryu_et_al_2017.pdf"
  download.file(url, destfile, mode = "wb", method = "libcurl")


  #Depurate
  text_pages <- pdf_text(destfile)[11:16]
  text <- text_pages

  text <- str_split(paste(text_pages, collapse = "\n"), "\n")[[1]]
  text <- text[!str_detect(text, "^\\s*$|^Appendix|^Scientific name")]
  family_actual <- NA
  results <- list()
  for (linea in text) {
    if (!str_detect(linea, "\\(")) {
      family_actual <- str_trim(linea)
    } else {
      matches <- str_match(linea, "^\\s*(.*?)\\s*\\((.*?)\\)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)")
      if (!is.na(matches[1,1])) {
        results[[length(results)+1]] <- tibble(
          family = family_actual,
          scientific_name = str_trim(matches[1,2]),
          korean_name = str_trim(matches[1,3]),
          OD = matches[1,4],
          HV = matches[1,5],
          WE = matches[1,6]
        )
      }
    }
  }
  table_pdf <- bind_rows(results)
  table_final1 <- as.data.frame(table_pdf)
  table_fresh <- table_final1 %>% filter(grepl("FW", WE))
  table_fresh$Habitat_database <- "FRESHWATER"
  table_fresh <- table_fresh %>% select(-WE)
  selected_columns <- c("scientific_name", "Habitat_database")
  table_final <- table_fresh[,selected_columns]
  table_final$Invaded_country <- "Korea"
  names <- table_final$scientific_name
  table_final$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName

  #SAve
  write.xlsx(table_final, "InputFiles/freshwatersubset_ryu_et_al_2017.xlsx")
}
