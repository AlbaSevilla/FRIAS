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
    separate_rows(Mechanisms, sep=",|;") %>%
    mutate(across(Mechanisms, ~ str_replace_all(., "/", ";"))) %>%
    separate_rows(Mechanisms, sep=",|;") %>%
    mutate(Mechanisms = tolower(trimws(Mechanisms)))

  matches <- match(dat$Mechanisms, equivs$OriginalCategories)
  replacements <- equivs$StandardizedCategoriesMechanisms[matches]

  dat_std <- dat %>%
    left_join(equivs, by = c("Mechanisms" = "OriginalCategories")) %>%
    mutate(
      Mechanisms = StandardizedCategoriesMechanisms  # solo conserva equivalencias, NA si no hay match
    ) %>%
    select(-StandardizedCategoriesMechanisms)

  #write_xlsx(no_estandarizados,"OutputFiles/Check/NA_Mechanisms_masterlist.xlsx")

  MasterlistStandardized <- noduplicates(dat_std, "AcceptedNameGBIF")

  write_xlsx(MasterlistStandardized,
             "OutputFiles/Intermediate/step11_standardizedmechanism_masterlist.xlsx")

  write_csv(MasterlistStandardized,
            "OutputFiles/Intermediate/step11_standardizedmechanism_masterlist.csv")
}
