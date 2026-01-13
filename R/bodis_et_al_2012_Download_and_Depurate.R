bodis_et_al_2012_Download_and_Depurate <- function(){
  #Download
  url <- "https://zenodo.org/records/5736021/files/source.pdf?download=1"
  destfile <- "InputFiles/originaldatabase_bodis_et_al_2012.pdf"
  download.file(url, destfile, mode = "wb")
 #generamos csv a mano, no se hace scrapping bien

  #Depurate
  dat <- read.csv("InputFiles/originaldatabase_bodis_et_al_2012.csv")
  dat
  dat$Species <- gsub('"', '', dat$Species)
  dataset <- dat
  names <- dataset$Species
  accep_names <- name_backbone_checklist(names)$canonicalName
  hab_dat <- check_habitat(accep_names, dataset)
  dat_frw <- hab_dat %>% filter(grepl("FRESHWATER", Habitat))
  dat_frw$RecipientRange <- "Hungary"
  dat_frw$establishmentMeans <- "invasive"

  #save
  write.xlsx(dat_frw, "InputFiles/freshwatersubset_bodis_et_al_2012.xlsx")
}
