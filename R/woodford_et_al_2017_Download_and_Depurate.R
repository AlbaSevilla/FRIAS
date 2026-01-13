woodford_et_al_2017_Download_and_Depurate <- function(){
  #Depurate
  destfile <- "InputFiles/originaldatabase_woodford_et_al_2017.pdf"
  list1 <- pdf_text(destfile)[7:30]
  text1 <- list1
  list6 <- pdf_text(destfile)[37]
  text6 <- list6
  list7 <- pdf_text(destfile)[38:41]
  text7 <- list7
  list9 <- pdf_text(destfile)[43]
  text9 <- list9
  extract_scientific_name <- function(text) {
    text_collapse <- paste(text, collapse = " ")
    matches <- gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s\\([^\\)]+\\))?(?:\\s[a-zA-Z\\.]+)*", text_collapse, perl = TRUE)
    names <- regmatches(text_collapse, matches)[[1]]
    unique(trimws(names))
  }
  species1 <- extract_scientific_name(text1)
  species6 <- extract_scientific_name(text6)
  species7 <- extract_scientific_name(text7)
  species9 <- extract_scientific_name(text9)
  species <- c(species1, species6, species7, species9)
  dataset <- as.data.frame(species)
  write.xlsx(dataset, "Inputfiles/originaldatabase_woodford_et_al_2017.xlsx")

  dataset <- read_excel("Inputfiles/originaldatabase_woodford_et_al_2017.xlsx")
  dataset <- dataset
  names <- dataset$species
  nom_acep <- name_backbone_checklist(names)$canonicalName
  habitat_dat <- check_habitat(nom_acep, dataset)
  freshwater_species <- habitat_dat %>% filter(grepl("FRESHWATER", Habitat))
  freshwater_species$Invaded_country <- "South Africa"
  freshwater_species2 <- freshwater_species %>%
    mutate(across(everything(), ~str_replace_all(as.character(.), ";", ",")))
  freshwater_species2 <- freshwater_species2[,-1]

  #Save
  write.xlsx(freshwater_species2, "Inputfiles/freshwatersubset_woodford_et_al_2017.xlsx")
}
