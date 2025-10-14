Schmidt_Kloiber_et_Al_2025_Download_and_Depurate <- function(){
  #############################################
  ########### PARA DESCARGAR ##################
  #############################################

  #obtenido de la web https://www.freshwaterecology.info/fwe_neobiota.php PERO
  #copiando y pegando de la web, a mano, el html es con javascript


  ##############################################
  # DEPURACIÓN #################################

  #PECES
  Schmidt_Kloiber_et_Al_2025AlienSpeciesandRegions <- read_excel("Inputfiles/Freshwaterecology.info_AlienFishandRegions.xlsx")
  names(Schmidt_Kloiber_et_Al_2025AlienSpeciesandRegions)[names(Schmidt_Kloiber_et_Al_2025AlienSpeciesandRegions) == "alien in catchment region(s)"] <- "Invaded_regions"
  Schmidt_Kloiber_et_Al_2025AlienSpeciesandRegions$Status <- "Alien"
  head(Schmidt_Kloiber_et_Al_2025AlienSpeciesandRegions)


  #Para macro invertebrados no funciona la web

  #MACROFITOS
  Schmidt_Kloiber_et_Al_2025AlienMacrophytes <- read_excel("Inputfiles/Freshwaterecology.info_AlienMacrophytes.xlsx")
  Schmidt_Kloiber_et_Al_2025AlienMacrophytes$Status <- "Alien"
  Schmidt_Kloiber_et_Al_2025AlienMacrophytes$Invaded_regions <- ""
  head(Schmidt_Kloiber_et_Al_2025AlienMacrophytes)

  #UNIMOS DATASETS :
  DATASET_Schmidt_Kloiber_et_Al_2025 <- rbind(Schmidt_Kloiber_et_Al_2025AlienSpeciesandRegions,Schmidt_Kloiber_et_Al_2025AlienMacrophytes)
  DATASET_Schmidt_Kloiber_et_Al_2025


  write.xlsx(DATASET_Schmidt_Kloiber_et_Al_2025, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Schmidt_Kloiber_et_Al_2025.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_Schmidt_Kloiber_et_Al_2025.xlsx", "\n")
}


