Chen_et_All_2023_Megafauna_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################

  #a mano https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Ffwb.14073&file=fwb14073-sup-0001-Supinfo.doc

  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  dataset <- read_excel("InputFiles/Step0_OriginalDatabase_ChenetAllMegafauna2023.xlsx",
                                                              col_names = TRUE)

  dataset <- as.data.frame(dataset)

  #No duplicados
  source(file.path("R", "noduplicates.r"))
  dataset_sinduplicados <- noduplicates(dataset, "Binomial name")


  dataset_sinduplicados$Habitat_Database <- "Freshwater"
  colnames(dataset_sinduplicados) <- gsub(" ", "_", colnames(dataset_sinduplicados))


  write.xlsx(dataset_sinduplicados, "InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_ChenetAll2023.xlsx")
}
