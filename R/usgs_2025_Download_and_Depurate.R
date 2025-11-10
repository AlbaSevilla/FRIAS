usgs_2025_Download_and_Depurate <- function() {

  ##########################################################
  ######### PARA DESCARGAR LA BASE DE DATOS usgs_2025 ##########
  ##########################################################
  #Lo descargamos de la web https://nas.er.usgs.gov/queries/SpeciesList.aspx
  #No lo hacemos automáticamente porque en el pie de la página pone que:
  #Data are not available for download from the web site. Please contact NAS staff for a custom query.



  ##########################################################
  ########## PARA DEPURAR usgs_2025 ############################
  ##########################################################
  ruta_inputfiles <- file.path("Inputfiles")
  data_name_usgs_2025 <- list.files(path = ruta_inputfiles, pattern = "originaldatabase_usgs_2025.csv", full.names = TRUE)
  dat <- read.csv(data_name_usgs_2025, sep=",") #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.


  #NOS QUEDAMOS CON LOS REGISTROS FRESHWATER
  usgs_2025_freshwater_01 <- dat %>%
    filter(Native.Habitat %in% c("Freshwater","Freshwater-Marine","Marine-Freshwater",
                                 "Brackish-Freshwater","Freshwater-Brackish"))

  origin <- unique(usgs_2025_freshwater_01$Species.Origin)

  usgs_2025_freshwater_02 <- usgs_2025_freshwater_01 %>%
    filter(Species.Origin %in% c("Exotic","Unknown","Exotic Hybrid"))


  usgs_2025_freshwater_03 <- usgs_2025_freshwater_02 %>%
    select(-Common.Name)

  nrow(usgs_2025_freshwater_03)

  #ELIMINAMOS DUPLICADOS
  source(file.path("R","noduplicates.r"))
  usgs_2025_sinduplicados <- noduplicates(usgs_2025_freshwater_03, "Scientific.Name")
  names(usgs_2025_sinduplicados)[names(usgs_2025_sinduplicados) == "Scientific.Name"] <- "Scientific_Name"
  names(usgs_2025_sinduplicados)[names(usgs_2025_sinduplicados) == "Species.Origin"] <- "Species_Origin"
  names(usgs_2025_sinduplicados)[names(usgs_2025_sinduplicados) == "Native.Habitat"] <- "Native_Habitat"

  # Mover la columna 'Species' al inicio
  usgs_2025_sinduplicados <- usgs_2025_sinduplicados[, c("Scientific_Name", setdiff(names(usgs_2025_sinduplicados), "Scientific_Name"))]

  write.xlsx(usgs_2025_sinduplicados, "./Inputfiles/freshwatersubset_usgs_2025.xlsx")
}
