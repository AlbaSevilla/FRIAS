StandardizeHabitat <- function() {
  # Correspondence habitat table
  equivs <- read_excel("TablesToStandardize/Table S2.xlsx", sheet = "6. Habitat", col_names = TRUE) %>%
    mutate(
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategorieshabitat = tolower(trimws(StandardizedCategorieshabitat))
    ) %>%
    separate_rows(OriginalCategories, sep = ";") %>%
    mutate(OriginalCategories = tolower(trimws(OriginalCategories)))

  # File
  dat <- read_excel("OutputFiles/Intermediate/step11_standardizedmechanism_masterlist.xlsx") %>%
    mutate(Habitat = str_split(Habitat, ",")) %>%
    unnest(Habitat) %>%
    mutate(Habitat = Habitat %>%
             str_trim() %>%
             tolower() %>%
             gsub("[_/\\-]", "", .)
    ) %>%
    mutate(Habitat = tolower(trimws(Habitat)))

  # Apply standardization
  if ("Habitat" %in% colnames(dat) && any(!is.na(dat$Habitat) & dat$Habitat != "")) {
    matches <- match(dat$Habitat, equivs$OriginalCategories)
    replacements <- equivs$StandardizedCategorieshabitat[matches]

    # Save non-matching values before replacing
    no_match <- dat %>% filter(is.na(replacements))
    write_xlsx(no_match, "OutputFiles/Check/NA_Habitat_masterlist.xlsx")

    # Replace matched values
    dat$Habitat[!is.na(replacements)] <- replacements[!is.na(replacements)]

    # Replace non-matched with NA
    dat$Habitat[is.na(replacements)] <- NA
  }

  # Merge and remove duplicates
  source(file.path("R", "noduplicates.r"))
  MasterlistStandardized <- noduplicates(dat, "AcceptedNameGBIF")

  MasterlistStandardized2 <- MasterlistStandardized %>%
    rowwise() %>%
    mutate(Habitat = paste(
      sort(unique(str_trim(unlist(str_split(Habitat, ";|,"))))),
      collapse = "; "
    )) %>%
    ungroup()

  # Save
  write_xlsx(MasterlistStandardized2, "OutputFiles/Intermediate/step12_standardizedhabitat_masterlist.xlsx")
  write_csv(MasterlistStandardized2, "OutputFiles/Intermediate/step12_standardizedhabitat_masterlist.csv")
}
