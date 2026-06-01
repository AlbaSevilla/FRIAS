cano_barbacil_et_al_2023_Download_and_Depurate <- function() {
  #Download
  url <- "https://ars.els-cdn.com/content/image/1-s2.0-S0301479723014846-mmc1.docx"
  destfile <- "InputFiles/originaldatabase_cano_barbacil_et_al_2023.docx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dat <- read_docx(destfile)
  content <- docx_summary(dat)
  paragraphs <- content %>%
    filter(content_type == "table cell") %>%
    select(text)
  species_list <- paragraphs %>%
    mutate(species = str_extract_all(text, "\\b[A-Z][a-z]+ [a-z]+\\b")) %>%
    pull(species) %>%
    unlist() %>%
    unique()

  dataset <- as.data.frame(species_list)
  names <- dataset$species_list
  acceptednames <- name_backbone_checklist(names)$canonicalName
  dat_hab <- check_habitat(acceptednames, dataset)
  dat_hab_noNA <- dat_hab %>% filter(!is.na(AcceptedNameGBIF))
  dat_frw <- dat_hab_noNA %>% filter(grepl("FRESHWATER", Habitat))
  names_dat <- dat_frw$AcceptedNameGBIF
  #taxonomy
  dat_frw$Kingdom <- name_backbone_checklist(names_dat)$kingdom
  dat_frw$Family <- name_backbone_checklist(names_dat)$family
  dat_frw$Order <- name_backbone_checklist(names_dat)$order
  dat_frw$Class <- name_backbone_checklist(names_dat)$class
  dat_frw$Phylum <- name_backbone_checklist(names_dat)$phylum

  #Save
  write.xlsx(dat_frw, "./InputFiles/freshwatersubset_cano_barbacil_et_al_2023.xlsx")
}

