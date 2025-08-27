SupplementaryFilesHorizonScanningJuly2025_Download_and_Depurate <- function(){
  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://zenodo.org/api/records/16077384/files-archive"
  destfile <- "InputFiles/Step0_OriginalDatabase_HorizonScanningEuropeJuly2025.zip"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  # Descomprimir el archivo y renombrar los archivos para un uso mas facil
  unzip("InputFiles/Step0_OriginalDatabase_HorizonScanningEuropeJuly2025.zip", exdir = "InputFiles")
  file.rename("InputFiles/Supplementary_Material1.xlsx", "InputFiles/Step0_OriginalDatabase_HorizonScanningEuropeJuly2025SuppMat1.xlsx")
  file.rename("InputFiles/Supplementary_Material2.xlsx", "InputFiles/Step0_OriginalDatabase_HorizonScanningEuropeJuly2025SuppMat2.xlsx")


  #######################################################
  ########## DEPURAR DATOS ##############################
  #######################################################
  #HORIZON SCANNING EU 1
  EUHorizonScanning1 <- read_excel("InputFiles/Step0_OriginalDatabase_HorizonScanningEuropeJuly2025SuppMat1.xlsx",sheet = "List widely spread")
  EUHorizonScanning1 <- EUHorizonScanning1[-c(1:4),]
  colnames(EUHorizonScanning1) <- EUHorizonScanning1[1,]
  EUHorizonScanning1 <- EUHorizonScanning1[-1,]
  nombres_EUHorizonScanning1 <- EUHorizonScanning1$`Scientific Name`
  nombres_EUHorizonScanning1 <- name_backbone_checklist(nombres_EUHorizonScanning1)$canonicalName
  nombres_EUHorizonScanning1
  #Which are freshwater
  source(file.path("R", "check_habitat.r"))
  EUHorizonScanning1_frw <- check_habitat(nombres_EUHorizonScanning1, EUHorizonScanning1)
  EUHorizonScanning1_frw <- as.data.frame(EUHorizonScanning1_frw)
  #Save freshwaters
  EUHorizonScanning1_FRESHWATER <- EUHorizonScanning1_frw %>%
    filter(grepl("FRESHWATER", Habitat))

  #The column N grid cells/country refers to the areas of Europe occupied by the species = Invasive Range. We will remove the (3), (2), etc.  write_xlsx(EUHorizonScanning1_FRESHWATER, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_HorizonScanningEuropeJuly2025v1.xlsx")
  EUHorizonScanning1_FRESHWATER <- EUHorizonScanning1_FRESHWATER %>%
    mutate(`N grid cells/country` = gsub("\\([0-9]+\\)", "", `N grid cells/country`),
           `N grid cells/country` = gsub("\\s+,", ",", `N grid cells/country`),
           `N grid cells/country` = trimws(`N grid cells/country`))

  write_xlsx(EUHorizonScanning1_FRESHWATER, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_HorizonScanningEuropeJuly2025v1.xlsx")

  #HORIZON SCANNING EU 2
  EUHorizonScanning2 <- read_excel("InputFiles/Step0_OriginalDatabase_HorizonScanningEuropeJuly2025SuppMat2.xlsx")
  EUHorizonScanning2
  nombres_EUHorizonScanning2 <- EUHorizonScanning2$`Scientific name`
  nombres_EUHorizonScanning2 <- name_backbone_checklist(nombres_EUHorizonScanning2)$canonicalName
  #Which are freshwater
  EUHorizonScanning2_frw <- check_habitat(nombres_EUHorizonScanning2, EUHorizonScanning2)
  #Save freshwater
  EUHorizonScanning2_FRESHWATER <- EUHorizonScanning2_frw %>%
    filter(grepl("FRESHWATER", Habitat))
  names(EUHorizonScanning2_FRESHWATER)
  # Seleccionar solo las columnas de mecanismos
  mech_cols <- c(
    "1. Competition",
    "2. Predation",
    "3. Hybridisation",
    "4. Transmission of disease",
    "5. Parasitism",
    "6. Poisoning/toxicity",
    "7. Biofouling or other direct physical disturbance",
    "8. Grazing/herbivory/browsing",
    "9. Chemical, physical, structural impact on ecosystem",
    "10. Indirect impacts through interactions with other species"
  )


  EUHorizonScanning2_FRESHWATER2 <- EUHorizonScanning2_FRESHWATER %>%
    rowwise() %>%
    mutate(Mechanisms = paste(
      # quitamos los números con gsub()
      gsub("^[0-9]+\\. ", "", mech_cols[which(c_across(all_of(mech_cols)) == "X")]),
      collapse = ", "
    )) %>%
    ungroup()

  write_xlsx(EUHorizonScanning2_FRESHWATER2, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_HorizonScanningEuropeJuly2025v2.xlsx")

  cat("Step0_OriginalDatabaseFreshwaterNODUPLICATES_HorizonScanningEuropeJuly2025v1.xlsx and Step0_OriginalDatabaseFreshwaterNODUPLICATES_HorizonScanningEuropeJuly2025v2.xlsx Downloaded")
}
