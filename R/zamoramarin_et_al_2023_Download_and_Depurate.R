zamoramarin_et_al_2023_Download_and_Depurate <- function(){
  #Download
  url <- "https://neobiota.pensoft.net/article/105994/download/suppl/31/"
  destfile <- "InputFiles/originaldatabase_zamoramarin_et_al_2023.xlsx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dat <- read.xlsx("InputFiles/originaldatabase_zamoramarin_et_al_2023.xlsx", sheet="Sps_list")
  dat <- dat %>% filter(Status != "Cryptogenic")
  dataset <- dat
  native_replacements <- c(
    "Eur" = "Europe",
    "Afr" = "Africa",
    "As"  = "Asia-temperate",
    "At"  = "Asia-tropical",
    "Aus" = "Australasia",
    "Pac" = "Pacific",
    "NAm" = "North America",
    "SAm" = "South America",
    "Ant" = "Antarctica",
    "Sam" = "SAm",  # correcciones de abreviaturas inconsistentes
    "Nam" = "NAm"
  )
  dataset <- dataset %>%
    mutate(
      Native.Range = str_replace_all(Native.Range, native_replacements)
    )
  pathway_replacements <- c(
    "1" = "Escape",
    "2" = "Release",
    "3" = "Contaminant",
    "4" = "Stowaway",
    "5" = "Corridor",
    "6" = "Unaided",
    "7" = "Unknown"
  )
  dataset <- dataset %>%
    mutate(
      Pathways = str_replace_all(Pathways, pathway_replacements)
    )


  names <- dataset$Scientific.Name
  accepted_names <- name_backbone_checklist(names)$canonicalName
  dat_hab <- check_habitat(accepted_names, dataset)
  dat_frw <- dat_hab %>% filter(grepl("FRESHWATER", Habitat))
  dat_frw$RecipientCountry <- "Spain; Portugal"
  dat_frw <- dat_frw %>%
    select(Scientific.Name, everything())
  #save
  write.xlsx(dat_frw, "InputFiles/freshwatersubset_zamoramarin_et_al_2023.xlsx")
}
