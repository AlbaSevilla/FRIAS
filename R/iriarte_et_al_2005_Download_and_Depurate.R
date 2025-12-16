iriarte_et_al_2005_Download_and_Depurate <- function(){
  #Download
  url <- "https://www.scielo.cl/pdf/rchnat/v78n1/art10.pdf"
  destfile <- "InputFiles/originaldatabase_iriarte_et_al_2005.pdf"
  download.file(url, destfile, mode = "wb")

  #Depurate
  text_pages <- pdf_text(destfile)[5:6]
  table_text <- text_pages
  full_text <- paste(table_text, collapse = "\n")
  lines <- strsplit(full_text, "\n")[[1]]
  pos_scn <- grep("Scientific \\(common\\) names", lines)
  if (length(pos_scn) > 0) {
    lines <- lines[pos_scn[1]:length(lines)]
  }
  pos_table2 <- grep("TABLE 2", lines)
  if (length(pos_table2) > 0) {
    lines <- lines[1:(pos_table2[1] - 1)]
  }
  modified_text <- gsub(" {3,}", ";", lines)
  count_fields <- function(text) {
    sapply(strsplit(text, ";"), length)
  }
  merge_lines <- function(text) {
    result <- c()
    for (line in text) {
      if (nchar(trimws(line)) == 0) next
      fields <- count_fields(line)
      if (length(result) == 0) {
        result <- c(result, line)
      } else {
        if (fields < 3) {
          result[length(result)] <- paste0(result[length(result)], " ", trimws(line))
        } else {
          result <- c(result, line)
        }
      }
    }
    return(result)
  }
  merged_text <- merge_lines(modified_text)
  merged_text <- gsub(";IRIARTE ET AL.", "", merged_text)
  dataset <- read.table(text = merged_text, sep = ";", header = TRUE, fill = TRUE, strip.white = TRUE, quote = "")
  dataset <- data.frame(
    Scientific_Name = dataset[, 0],
    Origin = dataset[, 1],
    Motivation = dataset[, 2],
    When = dataset[, 3],
    Introduced_To = dataset[, 5],
    stringsAsFactors = FALSE
  )
  dataset$Scientific_Name <- rownames(dataset)
  rownames(dataset) <- NULL
  write.csv2(dataset, "./Inputfiles/originaldatabase_iriarte_et_al_2005.csv")

  #Depurate
  dataset <- read.csv("./Inputfiles/originaldatabase_iriarte_et_al_2005.csv", sep = ";")
  dataset$Habitat <- "Freshwater"
  dataset$Invaded_Country <- "Chile"
  dataset <- dataset %>% select(-X)
  dataset <- dataset[, c("Scientific_Name", setdiff(names(dataset), "Scientific_Name"))]
  dataset <- dataset %>% filter(!When == "Undated")
  dataset <- dataset %>% filter(!When == "No data")

  #Save
  write.xlsx(dataset, "./Inputfiles/freshwatersubset_iriarte_et_al_2005.xlsx")
}
