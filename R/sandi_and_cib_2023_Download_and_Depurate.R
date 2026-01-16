sandi_and_cib_2023_Download_and_Depurate <- function() {
  #Download
  url <- "https://zenodo.org/records/14937470/files/A_checklist_of_alien_taxa_for_South%20Africa_v20250520.xlsx?download=1"
  destfile <- "InputFiles/originaldatabase_sandi_and_cib_2023.xlsx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dat <- read_excel("InputFiles/originaldatabase_sandi_and_cib_2023.xlsx")
  sandi_and_cib_2023_freshwater <- dat %>%
    filter(taxonRank %in% c("sp.")) %>%
    filter(isNative %in% c("FALSE")) %>%
    filter(IntroductionStatus %in% c("presentAsAlienNotNaturalised",
                                     "Invasive",
                                     "Invasive:NativeAlienPopulations",
                                     "PresentAsAlienNotNaturalised"))
   sandi_and_cib_2023_noduplicates <- noduplicates(sandi_and_cib_2023_freshwater, "scientificName")
   nrow(sandi_and_cib_2023_noduplicates)
   dataset <- sandi_and_cib_2023_freshwater
   names <- dataset$scientificName
   names_acep <- character(length(names))
   for (i in seq_along(names)) {
     names_acep[i] <- name_backbone_checklist(names[i])$canonicalName
     Sys.sleep(0.02)
   }
   dat_act <- check_habitat(names_acep, dataset)
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh$Invaded_country <- "South Africa"
  dataset <- dat_fresh %>%
    mutate(scientificName = str_extract(scientificName, "^\\S+\\s+\\S+"))

  #Save
  write.xlsx(dataset, "InputFiles/freshwatersubset_sandi_and_cib_2023.xlsx")
}
