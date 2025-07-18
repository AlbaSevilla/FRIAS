USGS_Download_and_Depurate <- function() {

  ##########################################################
  ######### PARA DESCARGAR LA BASE DE DATOS USGS ##########
  ##########################################################
  #Lo descargamos de la web https://nas.er.usgs.gov/queries/SpeciesList.aspx
  #No lo hacemos automáticamente porque en el pie de la página pone que:
  #Data are not available for download from the web site. Please contact NAS staff for a custom query.



  ##########################################################
  ########## PARA DEPURAR USGS ############################
  ##########################################################
  ruta_inputfiles <- file.path("Inputfiles")
  data_name_USGS <- list.files(path = ruta_inputfiles, pattern = "Step0_OriginalDatabase_USGS.csv", full.names = TRUE)
  dat <- read.csv(data_name_USGS, sep=",") #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.


  #NOS QUEDAMOS CON LOS REGISTROS FRESHWATER
  USGS_freshwater_01 <- dat %>%
    filter(Native.Habitat %in% c("Freshwater","Freshwater-Marine","Marine-Freshwater",
                                 "Brackish-Freshwater","Freshwater-Brackish"))

  origin <- unique(USGS_freshwater_01$Species.Origin)

  USGS_freshwater_02 <- USGS_freshwater_01 %>%
    filter(Species.Origin %in% c("Exotic","Unknown","Exotic Hybrid"))


  USGS_freshwater_03 <- USGS_freshwater_02 %>%
    select(-Common.Name)

  nrow(USGS_freshwater_03)

  #ELIMINAMOS DUPLICADOS
  source(file.path("R","noduplicates.r"))
  USGS_sinduplicados <- noduplicates(USGS_freshwater_03, "Scientific.Name")
  names(USGS_sinduplicados)[names(USGS_sinduplicados) == "Scientific.Name"] <- "Scientific_Name"
  names(USGS_sinduplicados)[names(USGS_sinduplicados) == "Species.Origin"] <- "Species_Origin"
  names(USGS_sinduplicados)[names(USGS_sinduplicados) == "Native.Habitat"] <- "Native_Habitat"

  # Mover la columna 'Species' al inicio
  USGS_sinduplicados <- USGS_sinduplicados[, c("Scientific_Name", setdiff(names(USGS_sinduplicados), "Scientific_Name"))]

  write.xlsx(USGS_sinduplicados, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_USGS.xlsx")
  cat("Archivo descargado correctamente: Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_USGS.xlsx")
  #write.csv2(USGS_sinduplicados, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_USGS.csv")

}
