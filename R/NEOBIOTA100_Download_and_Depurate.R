NEOBIOTA100_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://neobiota.pensoft.net/article/96282/download/suppl/33/"
  destfile <- "InputFiles/Step0_OriginalDatabase_NEOBIOTA100.xlsx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")




  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  ruta_inputfiles <- file.path("Inputfiles")
  dat <- read.xlsx("Inputfiles/Step0_OriginalDatabase_NEOBIOTA100.xlsx")#Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.
  dat <- dat[,-1]
  colnames(dat) <- c("Species name", "Native Range", "Introduction year",
                     "Species occurrence records", "Invasive range", "Year of creation")
  # Eliminar la primera fila que contiene los nombres de las columnas
  dat <- dat[-c(1:2), ]
  head(dat)

  #CUÁLES SON FRESHWATER?
  #SIN DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  NEOBIOTA100_subset_sinduplicados <- noduplicates(dat, column_name_species = "Species name")

  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  #Obtain habitats
  dataset <- NEOBIOTA100_subset_sinduplicados
  especies_lista0 <- dataset$`Species name`
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)

  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))

  dataset <- dataset_freshwater
  dataset <- dataset[,c(1,2,8,9)]
  names(dataset)[1] <- "Species_Name"
  names(dataset)[2] <- "Native_Range"
  dataset$Invaded_region <- "Northern Eurasia"

  write.xlsx(dataset, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_NEOBIOTA100.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_NEOBIOTA100.xlsx", "\n")
}

