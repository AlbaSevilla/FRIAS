ArtsdataBankenNoruega_Download_and_Depurate <- function(){
  #DOWNLOAD
  #in web https://lister.artsdatabanken.no/fremmedartslista/2023?TaxonRank=AssessedAtSameRank

  #DEPURATE:
  dat <- read_excel("InputFiles/Step0_OriginalDatabase_ArtsdataBankenNoruega.xlsx")
  names(dat)
#
#   #Nos quedamos con las columnas:
#   selected_columns <- c("Ekspertkomité","Vitenskapelig navn")
#   dat2 <- dat[,selected_columns]
#
#   names(dat2)[names(dat2) == "Ekspertkomité"] <- "Group"
#   names(dat2)[names(dat2) == "Vitenskapelig navn"] <- "Nombre_Cientifico"

  #NO DUPLICADOS:
  source(file.path("R","noduplicates.r"))
  dat_noduplicates <- noduplicates(dat, "Nombre_Cientifico")

  #Son invasoras en Noruega:
  dat_noduplicates$invaded_country <- "Norway"

  write.xlsx(dat_noduplicates, "InputFiles/Step0_OriginalDatabase_ArtsDataBankenNoruega.xlsx")
  #######################################################
  ############ habitat ##################################
  #######################################################
  source(file.path("R", "check_habitat.r"))
  dataset <- dat_noduplicates
  nombres <- dataset$Nombre_Cientifico
  nombres_aceptados <- name_backbone_checklist(nombres)$canonicalName
  dataset_actualizado <- check_habitat(nombres_aceptados, dataset)

  #Obtenemos freshwater
  dat_fresh <- dataset_actualizado %>% filter(grepl("FRESHWATER", Habitat))

  write.xlsx(dat_fresh, "InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_ArtsDataBankenNoruega.xlsx")
  cat("Archivo descargado: Step0_OriginalDatabaseFreshwaterNODUPLICATES_ArtsDataBankenNoruega.xlsx")

}
