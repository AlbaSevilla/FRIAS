GloNAF_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  #en esta web : https://idata.idiv.de/MMM/ShowMultimediaData/getFile?path=Datasets%255c257%255c257_2_GLONAF.zip

  directorio_original <- getwd()

  setwd("Inputfiles")
  ufile = list.files(pattern = '.zip$')
  ufile
  zip_list("Step0_OriginalDatabase_GloNAF.zip")
  unzip("Step0_OriginalDatabase_GloNAF.zip")

  setwd(directorio_original)

  ###############
  #MOVEMOS LOS ARCHIVOS A LA CARPETA INPUTFILES
  ###############
  origen <- "InputFiles/GLONAF/List_GloNAF_vanKleunenetal2018Ecology.csv"
  destino <- "Inputfiles/List_GloNAF_vanKleunenetal2018Ecology.csv"
  if (!dir.exists("ProcessedFiles")) {
    dir.create("ProcessedFiles")
  }
  file.rename(from = origen, to = destino)



  origen <- "InputFiles/GLONAF/Reference_GloNAF_vanKleunenetal2018Ecology.csv"
  destino <- "Inputfiles/Reference_GloNAF_vanKleunenetal2018Ecology.csv"
  if (!dir.exists("ProcessedFiles")) {
    dir.create("ProcessedFiles")
  }
  file.rename(from = origen, to = destino)

  origen <- "InputFiles/GLONAF/Region_GloNAF_vanKleunenetal2018Ecology.csv"
  destino <- "Inputfiles/Region_GloNAF_vanKleunenetal2018Ecology.csv"
  if (!dir.exists("ProcessedFiles")) {
    dir.create("ProcessedFiles")
  }
  file.rename(from = origen, to = destino)

  origen <- "InputFiles/GLONAF/Region_GloNAF_vanKleunenetal2018Ecology.csv"
  destino <- "Inputfiles/Region_GloNAF_vanKleunenetal2018Ecology.csv"
  if (!dir.exists("ProcessedFiles")) {
    dir.create("ProcessedFiles")
  }
  file.rename(from = origen, to = destino)

  origen <- "InputFiles/GLONAF/Taxon_x_List_GloNAF_vanKleunenetal2018Ecology.csv"
  destino <- "Inputfiles/Taxon_x_List_GloNAF_vanKleunenetal2018Ecology.csv"
  if (!dir.exists("ProcessedFiles")) {
    dir.create("ProcessedFiles")
  }
  file.rename(from = origen, to = destino)



  obtener_archivos_por_tipo <- function(directorio) {
    archivos <- list.files(directorio, full.names = FALSE)
    extensiones <- tools::file_ext(archivos)
    extensiones[extensiones == ""] <- "sin_extension"
    archivos_por_tipo <- split(archivos, extensiones)
    return(archivos_por_tipo)
  }
  obtener_archivos_por_tipo("Inputfiles")


  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  dataset_taxon <- read.xlsx(file.path("Inputfiles","Taxon_GloNAF_alien.xlsx"))
  dataset_region <- read.csv(file.path("Inputfiles","Region_GloNAF_vanKleunenetal2018Ecology.csv"), sep=",")

  # Unir los dos datasets por la columna 'region_id'
  dataset_mix <- merge(dataset_taxon, dataset_region[, c("region_id", "country")],
                         by = "region_id", all.x = TRUE)

  #No duplicados
  source(file.path("R", "noduplicates.r"))
  dataset_mix3 <- noduplicates(dataset_mix, "standardized_name")
  dataset_mix4 <- as.data.frame(dataset_mix3)

  write.xlsx(dataset_mix4, "./Inputfiles/Step0_OriginalDatabase_GloNAF.xlsx")



  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  #Obtain habitats
  dataset <- dataset_mix4
  especies_lista0 <- dataset$standardized_name
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)

  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))


  write.xlsx(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_GloNAF.xlsx")
  cat("Archivo descargado correctamente: /Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_GloNAF.xlsx")
}
