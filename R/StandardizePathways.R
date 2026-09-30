StandardizePathways <- function(){
  #Correspondence table
  equivs <- read_excel("TablesToStandardize/Auxiliary Table 2.xlsx", sheet = "4. Pathways", col_names = TRUE) %>%
    mutate(
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategoriespathway = trimws(StandardizedCategoriespathway)
    )

  #Masterlist
  dat <- read_csv("OutputFiles/Intermediate/step13_standardizedhabitat_masterlist.csv") %>%
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
  write_xlsx(MasterlistStandardized, "OutputFiles/Intermediate/step14_standardizedpathway_masterlist.xlsx")
  write_csv(MasterlistStandardized, "OutputFiles/Intermediate/step14_standardizedpathway_masterlist.csv")
}
