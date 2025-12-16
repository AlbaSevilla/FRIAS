StandardizeEstablishmentMeans <- function() {

  # equivalence table
  equivs <- read_excel("TablesToStandardize/Table S2.xlsx", sheet = "4. Establishment means", col_names = TRUE) %>%
    mutate(
      OriginalCategories = str_squish(str_replace_all(tolower(str_trim(OriginalCategories)), "[_/]", "")),
      StandardizedCategoriesEstablishmentMeans = str_squish(str_replace_all(tolower(str_trim(StandardizedCategoriesEstablishmentMeans)), "[_/]", "")),
      ) %>%
    separate_rows(OriginalCategories, sep=";") %>%
    mutate(
      OriginalCategories = trimws(OriginalCategories)
    )
  equivs
  # masterlist
  dat <- read_csv("OutputFiles/Intermediate/step13_standardizedpathway_masterlist.csv") %>%
    mutate(establishmentMeans = str_split(establishmentMeans, ",")) %>%
    unnest(establishmentMeans) %>%
    mutate(establishmentMeans = str_squish(str_replace_all(tolower(str_trim(establishmentMeans)), "[_/]", "")))

  # Assign standardized categories of correspondence table
  if ("establishmentMeans" %in% colnames(dat) &&
      any(!is.na(dat$establishmentMeans) & dat$establishmentMeans != "")) {

    matches <- match(dat$establishmentMeans, equivs$OriginalCategories)
    replacement <- equivs$StandardizedCategoriesEstablishmentMeans[matches]
    dat$establishmentMeans <- replacement
    }

  # Replace "na" to NA
  cols <- setdiff(names(dat), "Source_Data")
  dat[cols] <- lapply(dat[cols], function(x) {
    x[x == "" | x == "na"] <- NA
    x
  })

  #Replace NA to 'Introduced' since this workflow compiles 'Introduced species'
  dat$establishmentMeans[is.na(dat$establishmentMeans)] <- "Introduced"

  # Overlap rows to avoid duplicates.
  source(file.path("R", "noduplicates.r"))
  MasterlistStandardized <- noduplicates(dat, "AcceptedNameGBIF")

  #Save Final File
  write_xlsx(MasterlistStandardized, "OutputFiles/Intermediate/step14_standardizedestablishmentmeans_masterlist.xlsx")
  write_csv(MasterlistStandardized, "OutputFiles/Intermediate/step14_standardizedestablishmentmeans_masterlist.csv")
}
