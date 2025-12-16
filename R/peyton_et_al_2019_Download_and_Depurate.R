peyton_et_al_2019_Download_and_Depurate <- function() {
  #Download
  url <- "https://static-content.springer.com/esm/art%3A10.1007%2Fs10530-019-01961-7/MediaObjects/10530_2019_1961_MOESM3_ESM.xlsx"
  destfile <- "InputFiles/originaldatabase_peyton_et_al_2019.xlsx"
  download.file(url, destfile, mode = "wb")
  #Depurate
  dataset <- read.xlsx(destfile)
  colnames(dataset) <- dataset[1, ]
  dataset <- dataset[-1, ]
  dataset_fresh <- dataset %>%
    filter(`Group (Terrestrial animals; Freshwater animals; Plants; Marine)` %in% c("Freshwater animals", "Plants"))
  species_names <- dataset$Species
  accepted_names <- name_backbone_checklist(species_names)$canonicalName
  dataset_habitat <- check_habitat(accepted_names, dataset)
  dataset_fresh2 <- dataset_habitat %>% filter(grepl("FRESHWATER", Habitat))
  dataset_fresh1 <- dataset_fresh %>% select(-`Group (Terrestrial animals; Freshwater animals; Plants; Marine)`)
  dataset_fresh2 <- dataset_fresh2 %>% select(-`Group (Terrestrial animals; Freshwater animals; Plants; Marine)`, -AcceptedNameGBIF, -Habitat)
  merged <- rbind(dataset_fresh1, dataset_fresh2)
  merged$Habitat <- "Freshwater"
  impact_cols <- c(
    "1. Competition",
    "2. Predation ",
    "3. Hybridization",
    "4. Disease transmission",
    "5. Parasitism",
    "6. Poisoning /toxicity",
    "7. Bio-fouling",
    "8. Grazing / herbivory / browsing",
    "9. Interactions with other IAS"
  )
  for (col in impact_cols) {
    clean_name <- sub("^[0-9]+\\. ", "", col)
    merged[[col]][merged[[col]] == "X"] <- clean_name
  }
  merged2 <- merged %>%
    mutate(
      ImpactMechanism = apply(select(., all_of(impact_cols)), 1, function(x) {
        paste(na.omit(x[x != ""]), collapse = "; ")
      })
    )
  merged2$RecipientCountry <- "Cyprus"
  merged2 <- merged2 %>%
    separate_rows(`Native distribution (see codes)`, sep = ",") %>%
    mutate(`Native distribution (see codes)` = trimws(`Native distribution (see codes)`))
  merged2$`Native distribution (see codes)` <- merged2$`Native distribution (see codes)` %>%
    str_replace_all("\\bAfr\\b", "Africa") %>%
    str_replace_all("\\bEur\\b", "Europe") %>%
    str_replace_all("\\bAs\\b", "Asia") %>%
    str_replace_all("\\bSAm\\b", "South America") %>%
    str_replace_all("\\bNAm\\b", "North America") %>%
    str_replace_all("\\bAus\\b", "Australia") %>%
    str_replace_all("\\bAT\\b", "South East Asia (Tropical Asia)")
  names <- merged2$Species
  merged2$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName
  #Save
  write.xlsx(merged2, "InputFiles/freshwatersubset_peyton_et_al_2019.xlsx")
}
