dyer_et_al_2016_Download_and_Depurate <- function(){
  #Save
  url <- "https://figshare.com/ndownloader/files/6940181"
  destfile <- "InputFiles/originaldatabase_dyer_et_al_2016.csv"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dat <- read.csv("InputFiles/originaldatabase_dyer_et_al_2016.csv", sep=",") #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.
  colnames(dat)[colnames(dat) == "CountryName"] <- "InvadedCountry"
  colnames(dat)[colnames(dat) == "Binomial"] <- "ScientificName"
  dyer_et_al_2016_noduplicates <- noduplicates(dat, column_name_species="ScientificName")
  dyer_et_al_2016_noduplicates2 <- dyer_et_al_2016_noduplicates
  dyer_et_al_2016_noduplicates2$IntroducedDateGrouped <- as.numeric(dyer_et_al_2016_noduplicates2$IntroducedDateGrouped)
  dyer_et_al_2016_noduplicates2 <- OldestDate(dyer_et_al_2016_noduplicates2, "IntroducedDateGrouped")
  dataset <- dyer_et_al_2016_noduplicates2
  names <- dyer_et_al_2016_noduplicates2$ScientificName
  accepted_names <- name_backbone_checklist(names)$canonicalName
  dataset_act <- check_habitat(accepted_names, dataset)
  dataset_fresh <- dataset_act %>%
    filter(grepl("FRESHWATER", Habitat))
  dataset_fresh <- dataset_fresh %>%
    select(-Reference, -Notes, -TaxonomicNotes)

  #Save
  write.xlsx(dataset_fresh, "./InputFiles/freshwatersubset_dyer_et_al_2016.xlsx")
}
