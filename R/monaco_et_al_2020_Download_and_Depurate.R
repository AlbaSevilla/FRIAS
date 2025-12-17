monaco_et_al_2020_Download_and_Depurate <- function(){
  #Download
  url <- "https://www.lifeasap.eu/images/prodotti/6.1.11.1%20-%20Black%20list.pdf"
  destfile <- "InputFiles/originaldatabase_monaco_et_al_2020.pdf"
  download.file(url, destfile, mode = "wb")

  #Depurate
  text_pages <- pdf_text(destfile)[50:58]
  text1 <- text_pages
  text2 <- text1 |>
    paste(collapse = "\n") |>
    str_split("\n") |>
    unlist()
  text2 <- text2 |>
    str_trim() |>
    discard(~ .x == "" |
              str_detect(.x, "^Nome scientifico") |
              str_detect(.x, "^ALLEGATO") )
  text2_table <- text2 |>
    keep(~ str_detect(.x, "^\\d+\\s+"))
  table <- text2_table |>
    str_replace_all("\\s{2,}", "|") |>
    str_split_fixed("\\|", 4) |>
    as.data.frame(stringsAsFactors = FALSE)
  names(table) <- c("N", "Nome_scientifico", "Gruppo_tassonomico", "Classe_invasivita")
  table1 <- table |>
    mutate(across(everything(), ~str_trim(.))) |>
    mutate(N = as.integer(N))
  table1$Establishment <- "Presenti"
  text_pages <- pdf_text(destfile)[58:69]
  text1 <- text_pages
  text2 <- text1 |>
    paste(collapse = "\n") |>
    str_split("\n") |>
    unlist()
  text2 <- text2 |>
    str_trim() |>
    discard(~ .x == "" |
              str_detect(.x, "^Nome scientifico") |
              str_detect(.x, "^ALLEGATO") )
  text2_table <- text2 |>
    keep(~ str_detect(.x, "^\\d+\\s+"))
  table <- text2_table |>
    str_replace_all("\\s{2,}", "|") |>
    str_split_fixed("\\|", 4) |>
    as.data.frame(stringsAsFactors = FALSE)
  names(table) <- c("N", "Nome_scientifico", "Gruppo_tassonomico", "Classe_invasivita")

  table2 <- table |>
    mutate(across(everything(), ~str_trim(.))) |>
    mutate(N = as.integer(N))
  table2$Establishment <- "Assenti"
  merged <- merge(table1, table2, by = "Nome_scientifico", all = TRUE)
  table_nodup <- noduplicates(merged, "Nome_scientifico")
  dataset <- table_nodup
  names <- dataset$Nome_scientifico
  accep_names <- name_backbone_checklist(names)$canonicalName
  dat_hab <- check_habitat(accep_names, dataset)
  dat_frw <- dat_hab %>% filter(grepl("FRESHWATER", Habitat))
  dat_frw$RecipientRange <- "Italy"
  cols_x <- grep("\\.x$", names(dat_frw), value = TRUE)
  for (col_x in cols_x) {
    col_y <- sub("\\.x$", ".y", col_x)
    base_name <- sub("\\.x$", "", col_x)
    if (col_y %in% names(dat_frw)) {
      dat_frw[[base_name]] <- paste(dat_frw[[col_x]], dat_frw[[col_y]], sep = ", ")
      dat_frw[[base_name]] <- gsub("NA, |, NA|NA", "", dat_frw[[base_name]])
      dat_frw[[col_x]] <- NULL
      dat_frw[[col_y]] <- NULL
    }
  }
  names <- dat_frw$Nome_scientifico
  dat_frw$kingdom <- name_backbone_checklist(names)$kingdom
  dat_frw$order <- name_backbone_checklist(names)$order
  dat_frw$family <- name_backbone_checklist(names)$family
  dat_frw$phylum <- name_backbone_checklist(names)$phylum
  dat_frw$class <- name_backbone_checklist(names)$class

  #save
  write.xlsx(dat_frw, "InputFiles/freshwatersubset_monaco_et_al_2020.xlsx")
}
