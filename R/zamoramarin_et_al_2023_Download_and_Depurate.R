zamoramarin_et_al_2023_Download_and_Depurate <- function(){
  #Download
  url <- "https://neobiota.pensoft.net/article/105994/download/suppl/31/"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dat <- read.xlsx("InputFiles/originaldatabase_zamoramarin_et_al_2023.xlsx", sheet="Sps_list")
  dat <- dat %>% filter(Status != "Cryptogenic")
  dataset <- dat
  names <- dataset$Scientific.Name
  accepted_names <- name_backbone_checklist(names)$canonicalName
  dat_hab <- check_habitat(accepted_names, dataset)
  dat_frw <- dat_hab %>% filter(grepl("FRESHWATER", Habitat))
  dat_frw$RecipientCountry <- "Spain; Portugal"

  #save
  write.xlsx(dat_frw, "InputFiles/freshwatersubset_zamoramarin_et_al_2023.xlsx")
}
