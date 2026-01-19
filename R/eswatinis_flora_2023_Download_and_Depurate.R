eswatinis_flora_2023_Download_and_Depurate <- function(){
  #Download
  url <- "http://eswatinibiodiversity.com/alienplants/specieslist.asp"
  html <- read_html(url)
  tags_a <- html_elements(html, "a")
  valid_links <- tags_a[grepl("speciesinfo\\.asp\\?spid=", html_attr(tags_a, "href"))]
  hrefs <- html_attr(valid_links, "href")
  names <- html_text(valid_links)
  names <- gsub("\u00a0", " ", names)
  names <- trimws(names)
  scientific_name <- grepl("^[A-Z][a-z]+ [a-z]+$", names)
  dataset <- data.frame(
    especie = names[scientific_name],
    enlace = paste0("http://eswatinibiodiversity.com/alienplants/", hrefs[scientific_name]),
    stringsAsFactors = FALSE
  )
  results <- data.frame(
    Species = character(),
    Origin = character(),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(dataset))) {
    especie <- dataset$especie[i]
    url <- dataset$enlace[i]
    cat("Processing", i, "/", nrow(dataset), ":", especie, "\n")
    page <- tryCatch(read_html(url), error = function(e) return(NA))
    if (is.na(page)) next
    text <- page %>% html_text()
    lines <- unlist(strsplit(text, "\n"))
    lines <- trimws(lines)
    linea_origin <- grep("Origin:", lines, value = TRUE)
    origin <- if (length(linea_origin)) sub(".*Origin:\\s*", "", linea_origin[1]) else NA
    results <- rbind(results, data.frame(
      Species = especie,
      Origin = origin,
      stringsAsFactors = FALSE
    ))
  }
  results$Invaded_country <- "Swaziland"
  write.xlsx(results, "InputFiles/originaldatabase_eswatinis_flora_2023.xlsx")

  #Depurate
  dataset <- results
  names <- dataset$Species
  nom_acep <- name_backbone_checklist(names)$canonicalName
  dat_act <- check_habitat(nom_acep, dataset)
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dataset <- dat_fresh %>%
    mutate(
      Origin = str_extract_all(Origin, "\\b[A-Z][a-z]+\\b"),
      Origin = lapply(Origin, function(x) unique(x)),
      Origin = sapply(Origin, function(x) paste(x, collapse = ", "))
    )

  #Save
  write.xlsx(dataset, "InputFiles/freshwatersubset_eswatinis_flora_2023.xlsx")
}
