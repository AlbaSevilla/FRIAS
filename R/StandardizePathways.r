StandardizePathways <- function(){
  #Correspondence table
  equivs <- read_excel("TablesToStandardize/Table S2.xlsx", sheet = "5. Pathways", col_names = TRUE) %>%
    mutate(
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategoriesPathways = trimws(StandardizedCategoriesPathways)
    )

  #Masterlist
  dat <- read_csv("OutputFiles/Intermediate/step11_standardizedhabitat_masterlist.csv") %>%
    mutate(Pathways = str_split(Pathway, ";")) %>%
    unnest(Pathways) %>%
    mutate(Pathways = Pathways %>%
             str_trim() %>%
             sub(":.*", "", .) %>%
             str_squish() %>%
             tolower() %>%
             gsub("[^a-z]", "", .)
    )

  #Apply standardization
  if ("Pathways" %in% colnames(dat) && any(!is.na(dat$Pathways) & dat$Pathways != "")) {
    dat$Pathways <- tolower(trimws(dat$Pathways))
    dat$Pathways[dat$Pathways == "na"] <- ""
    dat$Pathways[is.na(dat$Pathways)] <- ""
    equivs$OriginalCategories <- tolower(trimws(equivs$OriginalCategories))
    matches <- match(dat$Pathways, equivs$OriginalCategories)
    replacements <- equivs$StandardizedCategoriesPathways[matches]
    dat$Pathways[!is.na(replacements)] <- replacements[!is.na(replacements)]
    dat$Pathways[is.na(replacements)] <- ""
  }

  #Remove and merge duplicates
  source(file.path("R", "noduplicates.r"))
  MasterlistStandardized <- noduplicates(dat, "AcceptedNameGBIF")

  #Save
  write_xlsx(MasterlistStandardized, "OutputFiles/Intermediate/step12_standardizedpathway_masterlist.xlsx")
  write_csv(MasterlistStandardized, "OutputFiles/Intermediate/step12_standardizedpathway_masterlist.csv")
}
