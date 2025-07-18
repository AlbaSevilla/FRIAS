InvasiveMammalsinEurope_Download_and_Depurate <- function() {
  ###############################################
  ########### TO DOWNLOAD  ######################
  ###############################################
  # de la web https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fmam.12277&file=mam12277-sup-0004-AppendixS4.xlsx


  ################################################################
  ########### PARA DEPURAR FIRST RECORDS #########################
  ################################################################
  dat <- read.xlsx(file.path("Inputfiles","Step0_OriginalDatabase_InvasiveMammalsinEurope.xlsx"),sheet=2)
  names(dat)

  dat <- dat %>%
    filter(Present_Status == "alien")


  #NO DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  dat_sinduplicados <- noduplicates(dat, column_name_species = "Species")


  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  #Obtain habitats
  dataset <- dat_sinduplicados
  especies_lista0 <- dataset$Species
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)


  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))

  write.xlsx(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_InvasiveMammalsinEurope.xlsx")
  cat("Archivo descargado correctamente: /Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_InvasiveMammalsinEurope.xlsx")
  #write.csv2(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_FirstRecords.csv")
}
