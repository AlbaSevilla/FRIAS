kumar_et_al_2025_Download_and_Depurate <- function() {
  #Download
  url <- "https://neobiota.pensoft.net/article/146421/download/suppl/33/"
  destfile <- "InputFiles/originaldatabase_kumar_et_al_2025.xlsx"
  download.file(url, destfile, mode = "wb")

  #DEpurate
  dat <- read.xlsx(destfile)
  names(dat) <- gsub("[:.-]", " ", names(dat))
  head(dat)
  dat <- dat %>%
    separate_rows(`Non Native Species`, sep = ",") %>%
    mutate(
      `Non Native Species` = trimws(tolower(`Non Native Species`)),
      `Non Native Species` = stringr::str_to_sentence(`Non Native Species`)
    ) %>%
    filter(!is.na(`Non Native Species`)) %>%
    filter(`Non Native Species` != "") %>%
    mutate(`Non Native Species` = trimws(tolower(`Non Native Species`)))


  NonNativeFishesIndia_noduplicates <- noduplicates(dat, "Non Native Species")
  NonNativeFishesIndia_noduplicates$Habitat <- "Freshwater"
  NonNativeFishesIndia_noduplicates$RecipientCountry <- "India"

  names <- NonNativeFishesIndia_noduplicates$`Non Native Species`
  NonNativeFishesIndia_noduplicates$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName
  NonNativeFishesIndia_noduplicates <- NonNativeFishesIndia_noduplicates %>%
    filter(!is.na(`Non Native Species`)) %>%
    filter(`Non Native Species` != "")
  NonNativeFishesIndia_noduplicates <- noduplicates(NonNativeFishesIndia_noduplicates, "Non Native Species")
  NonNativeFishesIndia_noduplicates <- NonNativeFishesIndia_noduplicates %>%
    mutate(
      `Non Native Species` =
        str_replace_all(`Non Native Species`, "[\r\n]+", " ") %>%
        str_squish()
    )
  #Save
  write.xlsx(NonNativeFishesIndia_noduplicates, "./InputFiles/freshwatersubset_kumar_et_al_2025.xlsx")
}
