StandardizeEstablishmentMeans <- function() {

  # equivalence table
  equivs <- read_excel("TablesToStandardize/standardization_tables.xlsx", sheet = "establishmentmeans_table", col_names = TRUE) %>%
    mutate(
      OriginalCategories = str_squish(str_replace_all(tolower(str_trim(OriginalCategories)), "[_/]", "")),
      StandardizedCategoriesEstablishmentMeans = str_squish(str_replace_all(tolower(str_trim(StandardizedCategoriesEstablishmentMeans)), "[_/]", ""))
    ) %>%
    separate_rows(OriginalCategories, sep=";") %>%
    mutate(
      OriginalCategories = trimws(OriginalCategories)
    )

  # masterlist
  dat <- read_excel("OutputFiles/Intermediate/Step12_Standardizedpathway_Masterlist.xlsx") %>%
    mutate(EstablishmentMeans = str_split(EstablishmentMeans, ",")) %>%
    unnest(EstablishmentMeans) %>%
    mutate(EstablishmentMeans = str_squish(str_replace_all(tolower(str_trim(EstablishmentMeans)), "[_/]", "")))

  # Assign standardized categories of correspondence table
  if ("EstablishmentMeans" %in% colnames(dat) &&
      any(!is.na(dat$EstablishmentMeans) & dat$EstablishmentMeans != "")) {
    matches <- match(dat$EstablishmentMeans, equivs$OriginalCategories)
    replacement <- equivs$StandardizedCategoriesEstablishmentMeans[matches]
    dat$EstablishmentMeans <- replacement
  }

  # Replace "na" to NA
  cols <- setdiff(names(dat), "Source_Data")
  dat[cols] <- lapply(dat[cols], function(x) {
    x[x == "" | x == "na"] <- NA
    x
  })

  # Overlap rows to avoid duplicates.
  source(file.path("R", "noduplicates.r"))
  MasterlistStandardized <- noduplicates(dat, "AcceptedNameGBIF")

  #Save Final File
  write_xlsx(MasterlistStandardized, "OutputFiles/Intermediate/Step13_StandardizedEstablishmentMeans_Masterlist.xlsx")
}
