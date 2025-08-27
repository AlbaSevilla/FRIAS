Crayfish_Download_and_Depurate <- function(){
  ##########################################
  #### DOWNLOAD FILE #######################
  ##########################################
  url <- "https://dfzljdn9uc3pi.cloudfront.net/2024/18229/1/Supplementary_Table_1.xlsx"
  destfile <- "InputFiles/Step0_OriginalDatabase_Crayfish.xlsx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")
  cat("Archivo de base de datos de Crayfish descargado correctamente")

  ##########################################
  ##### depurar ############################
  ##########################################
  ruta_inputfiles <- file.path("Inputfiles")
  dat <- read.xlsx("Inputfiles/Step0_OriginalDatabase_Crayfish.xlsx")#Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.

  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))
  #Obtain habitats
  dataset <- dat
  especies_lista0 <- dataset$Crayfish.scientific.name
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  DAT_actualizado <- check_habitat(especies_lista, dataset)

  ##############################################################
  ######### only FRESHWATER SPECIES         ####################
  ##############################################################
  DAT_actualizado2 <- DAT_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))

  DAT_actualizado3 <- DAT_actualizado2 %>%
    select(-Species)


  write.xlsx (DAT_actualizado3, "./InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_CRAYFISH.xlsx")
  cat("El archivo se descargó correctamente: InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_CRAYFISH.xlsx")

}
