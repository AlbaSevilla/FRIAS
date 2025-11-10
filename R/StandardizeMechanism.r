StandardizeMechanism <- function() {
  #Correspondence table
  equivs <- read_excel("TablesToStandardize/Table S2.xlsx", sheet = "9.2 EICAT Mechanisms", col_names = TRUE) %>%
    mutate(
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategoriesMechanisms = trimws(StandardizedCategoriesMechanisms)
    )

  #Masterlist
  dat <- read_csv("OutputFiles/Intermediate/step9_standardlocationnames_masterlist.csv") %>%
    mutate(Mechanisms = str_split(Mechanisms, ",")) %>%
    unnest(Mechanisms) %>%
    mutate(Mechanisms = Mechanisms %>%
             str_trim() %>%
             tolower() %>%
             gsub("[_/\\-]", "", .)
    )
  names(dat)
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
  write_xlsx(MasterlistStandardized, "OutputFiles/Intermediate/step10_standardizedmechanism_masterlist.xlsx")
  write_csv(MasterlistStandardized, "OutputFiles/Intermediate/step10_standardizedmechanism_masterlist.csv")
}
