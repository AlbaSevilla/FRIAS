FreshwaterEcologyInfo_Download_and_Depurate <- function(){
  #############################################
  ########### PARA DESCARGAR ##################
  #############################################

  #obtenido de la web https://www.freshwaterecology.info/fwe_neobiota.php PERO
  #copiando y pegando de la web, a mano, el html es con javascript


  ##############################################
  # DEPURACIÓN #################################

  #PECES
  Freshwaterecology_info_AlienSpeciesandRegions <- read_excel("Inputfiles/Freshwaterecology.info_AlienFishandRegions.xlsx")
  names(Freshwaterecology_info_AlienSpeciesandRegions)[names(Freshwaterecology_info_AlienSpeciesandRegions) == "alien in catchment region(s)"] <- "Invaded_regions"
  Freshwaterecology_info_AlienSpeciesandRegions$Status <- "Alien"
  head(Freshwaterecology_info_AlienSpeciesandRegions)


  #Para macro invertebrados no funciona la web

  #MACROFITOS
  Freshwaterecology_info_AlienMacrophytes <- read_excel("Inputfiles/Freshwaterecology.info_AlienMacrophytes.xlsx")
  Freshwaterecology_info_AlienMacrophytes$Status <- "Alien"
  Freshwaterecology_info_AlienMacrophytes$Invaded_regions <- ""
  head(Freshwaterecology_info_AlienMacrophytes)

  #UNIMOS DATASETS :
  DATASET_Freshwaterecology_info <- rbind(Freshwaterecology_info_AlienSpeciesandRegions,Freshwaterecology_info_AlienMacrophytes)
  DATASET_Freshwaterecology_info


  write.xlsx(DATASET_Freshwaterecology_info, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_FRESHWATERECOLOGYINFO.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_FRESHWATERECOLOGYINFO.xlsx", "\n")
}


