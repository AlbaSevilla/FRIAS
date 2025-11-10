world_of_crayfish_2025_Download_and_Depurate.xlsx <- function() {

  ###############################################################
  ########### PARA DESCARGAR WORLD OF CRAYFISH ##################
  ###############################################################
  #descargada manual, la envió belinda

  ################################################################
  ########### PARA DEPURAR               #########################
  ################################################################
  dat <- read.xlsx(file.path("Inputfiles","originaldatabase_world_of_crayfish_2025.xlsx"))
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
  write.xlsx(dat4, "./Inputfiles/freshwatersubset_world_of_crayfish_2025.xlsx")
}
