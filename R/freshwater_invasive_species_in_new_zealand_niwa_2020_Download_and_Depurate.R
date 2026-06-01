freshwater_invasive_species_in_new_zealand_niwa_2020_Download_and_Depurate <- function(){
  #Download
  url <- "https://webstatic.niwa.co.nz/library/FreInSpec.pdf"
  destfile <- "InputFiles/originaldatabase_freshwater_invasive_species_in_new_zealand_niwa_2020.pdf"
  download.file(url, destfile, mode = "wb")
  pdf_subset(destfile, pages = 2, output = "FreInSpec_page2.pdf") #Looking at the PDF, we can see that for our database we did not find any

  #Depurate
  text <- pdf_text("FreInSpec_page2.pdf") #We open the pdf downloaded with the second page
  lines <- unlist(strsplit(text, "\n")) #We convert the text in ordered list
  pattern <- "(.*?)(\\d+)$"
  matches <- regmatches(lines, regexpr(pattern, lines)) # Extract titles and page numbers using regular expression
  matches <- matches[matches != ""]
  titles <- sub(pattern, "\\1", matches)
  pages <- as.integer(sub(pattern, "\\2", matches))
  content_table <- data.frame(Title = titles, Page = pages)
  names <- content_table$Title
  pattern <- "[A-Z][a-z]+(?: [a-z]+)*" # Regular expression to identify species names (May and min.)
  species <- unlist(lapply(names, function(x) {
    regmatches(x, gregexpr(pattern, x))[[1]]
  }))
  species <- species[species != ""]
  dataset <- as.data.frame(species)
  colnames(dataset)[1] <- "SpeciesName"
  write.xlsx (dataset, "./InputFiles/originaldatabase_freshwater_invasive_species_in_new_zealand_niwa_2020.xlsx")

  #Depurate
  dataset <- dataset
  species_list0 <- dataset$SpeciesName
  species_list <- name_backbone_checklist(species_list0)$canonicalName
  dataset_updated <- check_habitat(species_list, dataset)
  dataset_freshwater <- dataset_updated %>%
    filter(grepl("FRESHWATER", Habitat))
  dataset_freshwater <- dataset_freshwater %>%
    select(-Species)
  Invaded_Country <- "New Zealand"
  dataset_freshwater$Invaded_Country <- Invaded_Country

  #Save
  write.xlsx(dataset_freshwater, "InputFiles/freshwatersubset_freshwater_invasive_species_in_new_zealand_niwa_2020.xlsx")
}
