StandardizeHabitat <- function() {
  # Correspondence habitat table
  equivs <- read_excel("TablesToStandardize/Table S2.xlsx", sheet = "6. Habitat", col_names = TRUE) %>%
    mutate(
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategorieshabitat = trimws(StandardizedCategorieshabitat)
    ) %>%
    separate_rows(OriginalCategories, sep = ";") %>%
    mutate(
      OriginalCategories = trimws(OriginalCategories)
    )

  # Leer datos desde archivo
  dat <- read_excel("OutputFiles/Intermediate/step10_standardizedmechanism_masterlist.xlsx") %>%
    mutate(Habitat = str_split(Habitat, ",")) %>%
    unnest(Habitat) %>%
    mutate(Habitat = Habitat %>%
             str_trim() %>%
             tolower() %>%
             gsub("[_/\\-]", "", .)
    )

  # Apply standardization
  if ("Habitat" %in% colnames(dat) && any(!is.na(dat$Habitat) & dat$Habitat != "")) {
    matches <- match(dat$Habitat, equivs$OriginalCategories)
    replacements <- equivs$StandardizedCategorieshabitat[matches]
    dat$Habitat[!is.na(replacements)] <- replacements[!is.na(replacements)]
  }

  #Merge and remove duplicates
  source(file.path("R", "noduplicates.r"))
  MasterlistStandardized <- noduplicates(dat, "AcceptedNameGBIF")

  #Save
  write_xlsx(MasterlistStandardized, "OutputFiles/Intermediate/step11_standardizedhabitat_masterlist.xlsx")
  write_csv(MasterlistStandardized, "OutputFiles/Intermediate/step11_standardizedhabitat_masterlist.csv")
}
