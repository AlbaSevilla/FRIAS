bambaradeniya_et_al_2002_Download_and_Depurate <- function(){
  #Download
  url <- "https://zoosprint.org/index.php/zpj/article/view/6050/5467"
  destfile <- "InputFiles/originaldatabase_bambaradeniya_et_al_2002.pdf"
  download.file(url, destfile, mode = "wb")
  #Depurate
  text_pages <- pdf_text(destfile)[4:6]
  text <- text_pages
  species <- unlist(regmatches(text, gregexpr("\\b[A-Z][a-z]+\\s[a-z]+\\b", text)))
  dataset <- as.data.frame(species)
  colnames(dataset) <- "species"
  write.xlsx(dataset, "InputFiles/originaldatabase_bambaradeniya_et_al_2002.xlsx")
  names <- dataset$species
  nom_acep <- name_backbone_checklist(names)$canonicalName
  habitat_dat <- check_habitat(nom_acep, dataset)
  freshwater_species <- habitat_dat %>% filter(grepl("FRESHWATER", Habitat))
  freshwater_species$invaded_country <- "Sri Lanka"

  #Save
  write.xlsx(freshwater_species, "InputFiles/freshwatersubset_bambaradeniya_et_al_2002.xlsx")
}
