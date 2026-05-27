StandardizePathways <- function(){
  #Correspondence table
  equivs <- read_excel("TablesToStandardize/Table S2.xlsx", sheet = "5. Pathways", col_names = TRUE) %>%
    mutate(
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategoriespathway = trimws(StandardizedCategoriespathway)
    )

  #Masterlist
  dat <- read_csv("OutputFiles/Intermediate/step12_standardizedhabitat_masterlist.csv") %>%
    separate_rows(pathway, sep=";|,") %>%
    mutate(pathway = trimws(tolower(pathway)))


  #Apply standardization
  if ("pathway" %in% colnames(dat) && any(!is.na(dat$pathway) & dat$pathway != "")) {
    dat$pathway <- tolower(trimws(dat$pathway))
    dat$pathway[dat$pathway == "na"] <- ""
    dat$pathway[is.na(dat$pathway)] <- ""
    equivs$OriginalCategories <- tolower(trimws(equivs$OriginalCategories))
    matches <- match(dat$pathway, equivs$OriginalCategories)
    replacements <- equivs$StandardizedCategoriespathway[matches]
    dat$pathway[!is.na(replacements)] <- replacements[!is.na(replacements)]
    dat$pathway[is.na(replacements)] <- ""
  }

  #Remove and merge duplicates
  MasterlistStandardized <- noduplicates(dat, "AcceptedNameGBIF")


  #Save
  write_xlsx(MasterlistStandardized, "OutputFiles/Intermediate/step13_standardizedpathway_masterlist.xlsx")
  write_csv(MasterlistStandardized, "OutputFiles/Intermediate/step13_standardizedpathway_masterlist.csv")
}
