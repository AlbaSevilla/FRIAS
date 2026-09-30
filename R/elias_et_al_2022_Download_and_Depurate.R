elias_et_al_2022_Download_and_Depurate <- function(){
  #Download
  url <- "https://neotropical.pensoft.net/article/80062/download/suppl/31/"
  destfile <- "InputFiles/originaldatabase_elias_et_al_2022.xlsx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dat <- read_excel("InputFiles/originaldatabase_elias_et_al_2022.xlsx", col_names = FALSE)
  dat <- dat[-c(1),]
  colnames(dat) <- dat[1,]
  dat <- dat[-c(1),]
  dat$ScientificName <- paste(dat$Genus, dat$Species)
  dat$Invaded_country <- "Guatemala"
  dat_noduplicates <- noduplicates(dat, "ScientificName")
  dat_noduplicates
  dat_noduplicates$Habitat_Database <- "Freshwater"
  dat_fresh <- dat_noduplicates %>% select("ScientificName", everything())
  dat_fresh$Year
  source(file.path("R", "OldestDate.r"))
  dat_fresh2 <- OldestDate(dat_fresh,"Year")
  names <- dat_fresh2$ScientificName
  dat_fresh2$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName

  #Save
  write_xlsx(dat_fresh2, "InputFiles/freshwatersubset_elias_et_al_2022.xlsx")
}
