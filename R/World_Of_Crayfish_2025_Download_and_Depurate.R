World_Of_Crayfish_2025_Download_and_Depurate <- function() {

  ###############################################################
  ########### PARA DESCARGAR WORLD OF CRAYFISH ##################
  ###############################################################
  #descargada manual, la envió belinda


  ################################################################
  ########### PARA DEPURAR FIRST RECORDS #########################
  ################################################################
  dat <- read.xlsx(file.path("Inputfiles","Step0_OriginalDatabase_World_Of_Crayfish_2025.xlsx"))
  names(dat)
  dat <- dat[,-20]

  #NO DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  dat2_sinduplicados <- noduplicates(dat, "Crayfish_scientific_name")

  Habitat <- "Freshwater"
  dat2_sinduplicados$Habitat <- Habitat
  names(dat2_sinduplicados)
  seleccionar_columnas <- c("Crayfish_scientific_name", "Status", "Year_of_record")

  dat3_sinduplicados <- dat2_sinduplicados[,seleccionar_columnas]

  ##########################################################
  #PARA OBTENER LA FECHA MÁS ANTIGUA #######################
  ##########################################################
  source(file.path("R", "OldestDate.r"))
  dat3 <- OldestDate(dat3_sinduplicados,"Year_of_record")

  dat4 <- as.data.frame(dat3)

  dat4$Habitat_database <- "Freshwater"


  write.xlsx(dat4, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_World_Of_Crayfish_2025.xlsx")
  cat("Archivo descargado correctamente: /Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_World_Of_Crayfish_2025.xlsx")
  #write.csv2(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_FirstRecords.csv")
}
