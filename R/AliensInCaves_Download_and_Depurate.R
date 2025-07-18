AliensInCaves_Download_and_Depurate <- function(){
  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  #A MANO
  #https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fbrv.12933&file=brv12933-sup-0003-TableS1.xlsx


  #Depurar
  dat <- read_excel("InputFiles/Step0_OriginalDatabase_AliensInCaves.xlsx", col_names = TRUE)
  names(dat)

  #Noduplicados
  source(file.path("R","noduplicates.r"))
  dat_noduplicados <- noduplicates(dat, "Species")

  #Habitat
  #Freshwater
  dat_fresh <- dat_noduplicados %>% filter(grepl("freshwater", Domain))


  write.xlsx(dat_fresh, "InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_AliensInCaves.xlsx")

}
