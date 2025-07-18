EliasEtAll_Download_and_Depurate <- function(){
  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://neotropical.pensoft.net/article/80062/download/suppl/31/"
  destfile <- "InputFiles/Step0_OriginalDatabase_EliasEtAll.xlsx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  ##############################################
  ########### DEPURAR ##########################
  ##############################################
  dat <- read_excel("Inputfiles/Step0_OriginalDatabase_EliasEtAll.xlsx", col_names = FALSE)
  dat <- dat[-c(1),]
  colnames(dat) <- dat[1,]
  dat <- dat[-c(1),]
  dat$ScientificName <- paste(dat$Genus, dat$Species)
  dat$Invaded_country <- "Guatemala"
  #NO DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  dat_noduplicates <- noduplicates(dat, "ScientificName")
  dat_noduplicates

  #Obtener Habitat
  #Ya sabemos que son freshwater de base.
  dat_noduplicates$Habitat_Database <- "Freshwater"
  dat_fresh <- dat_noduplicates %>% select("ScientificName", everything())
  dat_fresh$Year

  ##########################################################
  #PARA OBTENER LA FECHA MÁS ANTIGUA #######################
  ##########################################################
  # Crear una función para extraer la fecha más antigua desde la cadena de texto
  source(file.path("R", "OldestDate.r"))
  dat_fresh2 <- OldestDate(dat_fresh,"Year")
  dat_fresh2$OldestDate

  write_xlsx(dat_fresh2, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_EliasEtAll.xlsx")

}
