Zenni_et_Al_2024_Download_and_Depurate <- function(){
  ####################################################################################
  ################### PARA DESCARGAR LA BASE DE DATOS ################################
  ####################################################################################
  url <- "https://static-content.springer.com/esm/art%3A10.1007%2Fs10530-024-03302-9/MediaObjects/10530_2024_3302_MOESM3_ESM.xlsx"
  destfile <- "InputFiles/Step0_OriginalDatabase_Zenni_et_Al_2024.xlsx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")
  cat("Archivo ./InputFiles/Step0_OriginalDatabase_Zenni_et_Al_2024.xlsx descargado correctamente")


  ####################################################################################
  #################### PARA DEPURAR LA BASE DE DATOS #################################
  ####################################################################################
  datos <- read.xlsx(file.path("Inputfiles","Step0_OriginalDatabase_Zenni_et_Al_2024.xlsx"),sheet=3)
  head(datos)
  names(datos)

  data_subset_Zenni_et_Al_20242 <- datos %>%
    filter(Habitat %in% c("Terrestrial, Freshwater", "Freshwater")) %>%
    filter(Origin %in% c("Non-native in Brazil", "Hybrid with no native range"))

  invaded_country <- "Brazil"
  data_subset_Zenni_et_Al_20242$Invaded_Country <- invaded_country

  #Eliminar duplicados
  source(file.path("R","noduplicates.r"))
  Zenni_et_Al_2024_sinduplicados <- noduplicates(data_subset_Zenni_et_Al_20242, "Species.name")

  write.xlsx (Zenni_et_Al_2024_sinduplicados, "./InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Zenni_et_Al_2024.xlsx")
  cat("El archivo se descargó correctamente: InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Zenni_et_Al_2024.xlsx")

}
