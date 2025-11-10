nicolosi_et_al_2023_Download_and_Depurate <- function(){
  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  #A MANO
  #https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fbrv.12933&file=brv12933-sup-0003-TableS1.xlsx


  #Depurar
  dat <- read_excel("InputFiles/originaldatabase_nicolosi_et_al_2023.xlsx", col_names = TRUE)
  colnames(dat) <- dat[2,]
  #Noduplicados
  source(file.path("R","noduplicates.r"))
  dat_noduplicados <- noduplicates(dat, "Species")

  #Habitat
  #Freshwater
  source(file.path("R", "check_habitat.r"))
  nombres <- dat_noduplicados$Species
  nombres_aceptados <- name_backbone_checklist(nombres)$canonicalName
  dat_habitat <- check_habitat(nombres_aceptados, dat_noduplicados)

  #Quedarnos con los freshwater
  dat_fresh <- dat_habitat %>% filter(grepl("FRESHWATER", Habitat))

  #SAVE
  write.xlsx(dat_fresh, "InputFiles/freshwatersubset_nicolosi_et_al_2023.xlsx")
}
