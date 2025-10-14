StandardizeMechanism <- function() {
  #Correspondence table
  equivs <- read_excel("TablesToStandardize/standardization_tables.xlsx", sheet = "mechanisms_table", col_names = TRUE) %>%
    mutate(
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategoriesMechanisms = trimws(StandardizedCategoriesMechanisms)
    )

  #Masterlist
  dat <- read_excel("OutputFiles/Intermediate/Step9_StandardLocationNames_MasterList.xlsx") %>%
    mutate(Mechanisms = str_split(Mechanisms, ",")) %>%
    unnest(Mechanisms) %>%
    mutate(Mechanisms = Mechanisms %>%
             str_trim() %>%
             tolower() %>%
             gsub("[_/\\-]", "", .)
    )

  # Apply correspondence and standardization
  if ("Mechanisms" %in% colnames(dat) && any(!is.na(dat$Mechanisms) & dat$Mechanisms != "")) {
    matches <- match(dat$Mechanisms, equivs$OriginalCategories)
    replacements <- equivs$StandardizedCategoriesMechanisms[matches]
    dat$Mechanisms[!is.na(replacements)] <- replacements[!is.na(replacements)]
  }

  # Remove duplicates rows and merge
  source(file.path("R", "noduplicates.r"))
  MasterlistStandardized <- noduplicates(dat, "AcceptedNameGBIF")

  #Save
  write_xlsx(MasterlistStandardized, "OutputFiles/Intermediate/Step10_StandardizedMechanism_Masterlist.xlsx")

}
