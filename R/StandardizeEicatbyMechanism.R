StandardizeEicatbyMechanism <- function() {
  #Equivalences table
  equivs_eicat <- read_excel("TablesToStandardize/Standardization_EICATImpact.xlsx", sheet = 1, col_names = TRUE) %>%
    mutate(
      FileName = FileName2,
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategoriesEICAT = trimws(StandardizedCategoriesEICAT)
    )

  equivs_mec <- read_excel("TablesToStandardize/Standardization_Mechanisms.xlsx", sheet = 1, col_names = TRUE) %>%
    mutate(
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategoriesMechanisms = trimws(StandardizedCategoriesMechanisms)
    )

  #Files
  databases <- unique(equivs_eicat$FileName)
  databases <- databases[!is.na(databases)]


  for (database in databases) {
    dat <- read_csv2(database)
    if (!"EICAT_by_Mechanism" %in% names(dat)) next
    dat <- dat %>%
      mutate(EICAT_by_Mechanism = str_split(EICAT_by_Mechanism, ",")) %>%
      unnest(EICAT_by_Mechanism) %>%
      mutate(EICAT_by_Mechanism = tolower(str_trim(EICAT_by_Mechanism)))
    dat$EICAT_by_Mechanism <- gsub(" ","",dat$EICAT_by_Mechanism)
    dat <- dat %>%
      mutate(EICAT_by_Mechanism = ifelse(
        grepl("-", EICAT_by_Mechanism), EICAT_by_Mechanism, NA_character_
      )) %>%
      separate(
        EICAT_by_Mechanism,
        into = c("eicat", "mechanism"),
        sep = "-",
        fill = "right",
        remove = TRUE
      ) %>%
      mutate(
        eicat = str_trim(eicat),
        mechanism = str_trim(mechanism)
      )

    # Standardize EICAT
    equivs_filtered_eicat <- equivs_eicat %>% filter(FileName == database)
    dat$eicat <- tolower(trimws(dat$eicat))
    match_idx <- match(dat$eicat, equivs_filtered_eicat$OriginalCategories)
    correct_category <- equivs_filtered_eicat$StandardizedCategoriesEICAT[match_idx]
    dat$eicat[!is.na(correct_category)] <- correct_category[!is.na(correct_category)]

    # Standardize mechanism
    dat$mechanism <- tolower(trimws(dat$mechanism))
    dat$mechanism <- gsub("[_/\\-]", "", dat$mechanism)
    match_idx <- match(dat$mechanism, equivs_mec$OriginalCategories)
    correct_category <- equivs_mec$StandardizedCategoriesMechanisms[match_idx]
    dat$mechanism[!is.na(correct_category)] <- correct_category[!is.na(correct_category)]

    dat <- dat %>%
      unite("EICAT_by_Mechanism", eicat, mechanism, sep = " - ", remove = FALSE) %>%
      select(-eicat, -mechanism)

    #No duplicates
    source(file.path("R","noduplicates.r"))
    dat <- noduplicates(dat, "OriginalNameDB")

    dat$EICAT_by_Mechanism <- gsub("NA - NA", "NA", dat$EICAT_by_Mechanism)

    # Save result
    out_name <- paste0("OutputFiles/Intermediate/", tools::file_path_sans_ext(basename(database)), ".csv")
    write_csv2(dat, out_name)
  }
}
