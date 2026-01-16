StandardizeEICATImpact <- function() {

  #Equivs table
  equivs <- read_excel("TablesToStandardize/standardization_tables.xlsx", sheet = "eicat_impact_table", col_names = TRUE) %>%
    mutate(
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategoriesEICAT = trimws(StandardizedCategoriesEICAT)
    )

  #File
  dat <- read_excel("OutputFiles/Intermediate/Step3_StandardLocationNames_MasterList.xlsx")

  dat <- dat %>%
    mutate(EICATImpact = str_split(EICATImpact, ",")) %>%
    unnest(EICATImpact) %>%
    mutate(
      EICATImpact = tolower(str_trim(EICATImpact)),
      EICATImpact = gsub("[-_/]", "", EICATImpact)
    )
  unique(dat$EICATImpact)

  if ("EICATImpact" %in% colnames(dat) && any(!is.na(dat$EICATImpact) & dat$EICATImpact != "")) {
    match_idx <- match(dat$EICATImpact, equivs$OriginalCategories)
    correct_categories <- equivs$StandardizedCategoriesEICATImpact[match_idx]
   dat$EICATImpact[!is.na(correct_categories)] <- correct_categories[!is.na(correct_categories)]
  }

  source(file.path("R", "noduplicates.r"))
  noduplicados <- noduplicates(dat, "OriginalNameDB")

  write_xlsx(noduplicados, "OutputFiles/Intermediate/Step4_StandardizedEICATImpact_Masterlist.xlsx")

}
