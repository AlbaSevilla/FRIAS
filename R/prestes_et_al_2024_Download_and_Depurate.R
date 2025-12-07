prestes_et_al_2024_Download_and_Depurate <- function(){
  #Depurate
  destfile <- "InputFiles/originaldatabase_prestes_et_al_2024.xlsx"
  dat <- read.xlsx(destfile)
  dat <- dat[,-c(1:6)]
  dat <- dat %>%
    separate_rows(Species.name, sep=",") %>%
    mutate(Species.name = trimws(Species.name))
  dat$Species.name <- iconv(dat$Species.name, from = "UTF-8", to = "ASCII//TRANSLIT")
  dat_nodup <- noduplicates(dat, "Species.name")

  #save
  write.xlsx(dat_nodup, "InputFiles/freshwatersubset_prestes_et_al_2024.xlsx")
}
