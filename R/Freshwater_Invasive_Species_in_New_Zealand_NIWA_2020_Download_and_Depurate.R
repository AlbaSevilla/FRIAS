Freshwater_Invasive_Species_in_New_Zealand_NIWA_2020_Download_and_Depurate <- function(){

  ##########################################################################################
  ################# DOWNLOAD ###############################################################
  ##########################################################################################
  url <- "https://webstatic.niwa.co.nz/library/FreInSpec.pdf"
  destfile <- "InputFiles/Step0_OriginalDatabase_FreshInvSpecNewZealand.pdf"
  download.file(url, destfile, mode = "wb")
  pdf_subset(destfile, pages = 2, output = "FreInSpec_page2.pdf") #Looking at the PDF, we can see that for our database we did not find any
  #relevant columns, so we are left with the list of species names, which is on the second page of the PDF. Furthermore, loading the full PDF takes a very long time.


  text <- pdf_text("FreInSpec_page2.pdf") #We open the pdf downloaded with the second page
  lines <- unlist(strsplit(text, "\n")) #We convert the text in ordered list

  pattern <- "(.*?)(\\d+)$"
  matches <- regmatches(lines, regexpr(pattern, lines)) # Extract titles and page numbers using regular expression
  matches <- matches[matches != ""]
  titles <- sub(pattern, "\\1", matches)
  pages <- as.integer(sub(pattern, "\\2", matches))
  content_table <- data.frame(Title = titles, Page = pages)
  nombres <- content_table$Title
  nombres

  pattern <- "[A-Z][a-z]+(?: [a-z]+)*" # Regular expression to identify species names (May and min.)
  especies <- unlist(lapply(nombres, function(x) {
    regmatches(x, gregexpr(pattern, x))[[1]]
  }))
  especies <- especies[especies != ""]


  dataset <- as.data.frame(especies)
  colnames(dataset)[1] <- "SpeciesName"

  write.xlsx (dataset, "./InputFiles/Step0_OriginalDatabase_FreshwaterInvasiveSpeciesofNewZealandNIWA.xlsx")

  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  #Obtain habitats
  dataset <- dataset
  especies_lista0 <- dataset$SpeciesName
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)

  ##############################################################
  ######### only FRESHWATER SPECIES ############################
  ##############################################################
  dataset_freshwater <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))

  dataset_freshwater <- dataset_freshwater %>%
    select(-Species)

  #Its a database of invasive Species in New Zealand so:
  Invaded_Country <- "New Zealand"
  dataset_freshwater$Invaded_Country <- Invaded_Country



  write.xlsx(dataset_freshwater, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Freshwater_Invasive_Species_in_New_Zealand_NIWA_2020.xlsx")
  cat("Archivo guardado correctamente: Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Freshwater_Invasive_Species_in_New_Zealand_NIWA_2020.xlsx")
}
