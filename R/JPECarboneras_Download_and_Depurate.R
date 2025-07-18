JPECarboneras_Download_and_Depurate <- function(){
  #################################
  ########## DOWNLOAD  ############
  #################################

  #a mano, https://datadryad.org/downloads/file_stream/34467

  #https://datadryad.org/dataset/doi:10.5061/dryad.5fm00

  #################################
  ########## DEPURAR ##############
  #################################
  dataset_Carboneras <- read.xlsx(file.path("Inputfiles","JPECarbonerasST2.xlsx"),sheet = "EU IAS prioritised list")

  #No duplicados
  source(file.path("R", "noduplicates.r"))
  dataset_Carboneras <- noduplicates(dataset_Carboneras, "Species.name")

  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))
  dataset <- dataset_Carboneras
  especies_lista0 <- dataset$Species.name
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)
  dataset_actualizado2 <- dataset_actualizado %>%
    select(-Species)

  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado2 %>%
    filter(grepl("FRESHWATER", Habitat))

  write.xlsx(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_JPECarboneras.xlsx")
  cat("Archivo descargado correctamente: /Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_JPECarboneras.xlsx")
}
