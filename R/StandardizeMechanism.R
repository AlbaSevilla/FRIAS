StandardizeMechanism <- function() {

  equivs <- read_excel("TablesToStandardize/Table S2.xlsx",
                       sheet = "9.2 EICAT Mechanisms",
                       col_names = TRUE) %>%
    mutate(
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategoriesMechanisms = trimws(StandardizedCategoriesMechanisms)
    ) %>%
    separate_rows(OriginalCategories, sep = ";") %>%
    mutate(OriginalCategories = trimws(OriginalCategories))

  dat <- read_csv("OutputFiles/Intermediate/step10_standardlocationnames_masterlist.csv") %>%
    mutate(Mechanisms = str_split(Mechanisms, ",|;")) %>%
    unnest(Mechanisms) %>%
    mutate(
      Mechanisms = Mechanisms %>%
        str_trim() %>%
        tolower() %>%
        gsub("[_/\\-]", "", .)
    )%>%
    mutate(
      Mechanisms = tolower(trimws(Mechanisms))
      )

  matches <- match(dat$Mechanisms, equivs$OriginalCategories)
  replacements <- equivs$StandardizedCategoriesMechanisms[matches]

  dat$Mechanisms[!is.na(replacements)] <- replacements[!is.na(replacements)]

  no_estandarizados <- dat %>% filter(is.na(replacements))

  write_xlsx(no_estandarizados,
             "OutputFiles/Check/NA_Mechanisms_masterlist.xlsx")

  MasterlistStandardized <- noduplicates(dat, "AcceptedNameGBIF")

  write_xlsx(MasterlistStandardized,
             "OutputFiles/Intermediate/step11_standardizedmechanism_masterlist.xlsx")

  write_csv(MasterlistStandardized,
            "OutputFiles/Intermediate/step11_standardizedmechanism_masterlist.csv")
}
