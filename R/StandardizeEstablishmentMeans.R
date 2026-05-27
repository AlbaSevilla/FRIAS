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
    x <- as.character(x)
    # Reemplazamos "NA" o "na" dentro del string
    x <- str_replace_all(x, "\\bNA\\b|\\bna\\b", "")
    # Quitamos comas duplicadas o al inicio/final
    x <- str_replace_all(x, "^,\\s*|\\s*,\\s*|\\s*,$", ",")  # ajusta comas
    x <- str_replace_all(x, ",+", ",")  # comas consecutivas
    x <- str_trim(x, side = "both")     # quitar espacios al inicio y final
    x <- str_replace_all(x, "^,|,$", "")  # quitar coma inicial o final sobrante
    x[x == ""] <- NA                     # si queda vacío, NA
    return(x)
  })

  #Replace NA to 'Introduced' since this workflow compiles 'Introduced species'
  dat$establishmentMeans[is.na(dat$establishmentMeans)] <- "Alien"

  #If species has a the same country on native range and recipient range, this species
  #is classified also with 'native-alien' apart than the initial category (e.g introduced)
  dat <- dat %>%
    separate_rows(establishmentMeans, sep = ";|,") %>%
    separate_rows(NativeRange, sep = ";|,") %>%
    separate_rows(RecipientRange, sep = ";|,") %>%
    mutate(
      NativeRange = trimws(tolower(NativeRange)),
      RecipientRange = trimws(tolower(RecipientRange))
    ) %>%
    mutate(
      establishmentMeans = case_when(
        NativeRange == RecipientRange ~ "native-alien",
        TRUE ~ establishmentMeans
      )
    )


  # Overlap rows to avoid duplicates.
  MasterlistStandardized <- noduplicates(dat, "AcceptedNameGBIF")

  #eliminate duplicates
  MasterlistStandardized$establishmentMeans_clean <- sapply(
    MasterlistStandardized$establishmentMeans,
    function(x) {
      x |>
        tolower() |>
        strsplit(",") |>
        unlist() |>
        trimws() |>
        unique() |>
        sort() |>
        paste(collapse = ", ")
    }
  )
  MasterlistStandardized$establishmentMeans <-
    MasterlistStandardized$establishmentMeans_clean


  #Save Final File
  write_xlsx(MasterlistStandardized, "OutputFiles/Intermediate/step14_standardizedestablishmentmeans_masterlist.xlsx")
  write_csv(MasterlistStandardized, "OutputFiles/Intermediate/step14_standardizedestablishmentmeans_masterlist.csv")
}
