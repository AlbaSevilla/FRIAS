department_of_conservation_new_zealand_2025_Download_and_Depurate <- function(){
  #Download and Depurate
  url <- "https://www.doc.govt.nz/nature/pests-and-threats/freshwater-pests/other-freshwater-pest-species/"
  res <- read_html(url)
  tags_a <- html_elements(res, "a")
  tags_a_html <- as.character(tags_a)
  valid_links <- tags_a[grepl("<em>", tags_a_html)]
  scientific_names <- sapply(valid_links, function(tag) {
    em_node <- html_element(tag, "em")
    html_text(em_node)
  })
  scientific_names <- as.data.frame(scientific_names)
  species_links <- html_attr(valid_links, "href")
  species_links <- as.character(species_links)
  species_links <- as.data.frame(species_links)
  Dataset <- cbind(scientific_names, species_links)
  results <- data.frame(
    Species = character(),
    Origin = character(),
    Habitat = character(),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(Dataset))) {
    scientific_names <- Dataset$scientific_names[i]
    url <- Dataset$species_links[i]
    cat("Processing", i, "/", nrow(Dataset), ":", scientific_names, "\n")
    web <- tryCatch(read_html(url), error = function(e) return(NA))
    if (is.na(web)) next
    text <- web %>% html_text()
    lines <- unlist(strsplit(text, "\n"))
    lines <- trimws(lines)
    line_origin <- grep("Origin:", lines, value = TRUE)
    if (length(line_origin)) {
      origin <- sub(".*Origin:\\s*", "", line_origin[1])
    } else {
      h4s <- html_elements(web, "h4")
      h4_origin <- h4s[html_text(h4s) %in% c("Origin", "Where is it originally from?")]
      origin <- NA
      if (length(h4_origin)) {
        for (header in h4_origin) {
          siguiente <- html_node(header, xpath = "following-sibling::*[1]")
          if (!is.na(siguiente) && length(siguiente) > 0) {
            text <- html_text(siguiente)
            if (nchar(text) > 0) {
              origin <- text
              break
            }
          }
        }
      }
    }
    line_habitat <- grep("Habitats:", lines, value = TRUE)
    if (length(line_habitat)) {
      habitat <- sub(".*Habitats:\\s*", "", line_habitat[1])
    } else {
      h4s <- html_elements(web, "h4")
      h4_habitat <- h4s[html_text(h4s) %in% c("Habitat", "Which habitats is it likely to invade?")]
      h2s <- html_elements(web, "h2")
      h2_habitat <- h2s[html_text(h2s) == "Habitats"]
      header_habitat <- c(h4_habitat, h2_habitat)
      habitat <- NA
      if (length(header_habitat)) {
        for (header in header_habitat) {
          siguiente <- html_node(header, xpath = "following-sibling::*[1]")
          if (!is.na(siguiente) && length(siguiente) > 0) {
            text <- html_text(siguiente)
            if (nchar(text) > 0) {
              habitat <- text
              break
            }
          }
        }
      }
    }
    results <- rbind(results, data.frame(
      Species = scientific_names,
      Origin = origin,
      Habitat = habitat,
      stringsAsFactors = FALSE
    ))
  }
  results$Invaded_country <- "New Zealand"
  results$Habitat_Database <- "Freshwater"

  #Save
  write.xlsx(results, "Inputfiles/originaldatabase_department_of_conservation_new_zealand_2025.xlsx")
  write.xlsx(results, "Inputfiles/freshwatersubset_department_of_conservation_new_zealand_2025.xlsx")
}
