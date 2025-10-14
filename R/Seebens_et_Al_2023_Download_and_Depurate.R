Seebens_et_Al_2023_Download_and_Depurate <- function() {

  ###############################################################
  ########### TO DOWNLOAD Seebens_et_Al_2023    #######################
  ###############################################################
  url <- "https://zenodo.org/records/10039630/files/GlobalAlienSpeciesFirstRecordDatabase_v3.1_freedata.xlsx?download=1"
  destfile <- "InputFiles/Step0_OriginalDatabase_Seebens_et_Al_2023.xlsx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }

  #no tiene pathways para hacer lista
  download.file(url, destfile, mode = "wb")
  cat("Archivo descargado correctamente:", destfile, "\n")




  ################################################################
  ########### PARA DEPURAR FIRST RECORDS #########################
  ################################################################
  dat <- read.xlsx(file.path("Inputfiles","Step0_OriginalDatabase_Seebens_et_Al_2023.xlsx"),sheet=2)

  ##ELIMINAR LOS REGISTROS CON FECHAS MENORES A 0
  Seebens_et_Al_2023_01 <- dat[dat$FirstRecord >= 0, ]


  #NOS QUEDAMOS CON LAS ALIENS
  Seebens_et_Al_2023_01 <- Seebens_et_Al_2023_01 %>% filter(PresentStatus == "alien" |
                                                  PresentStatus == "Established invasive")

  #NOS QUEDAMOS CON LOS QUE DICE QUE SON FRESHWATER LA BD
  Seebens_et_Al_2023_01 <- Seebens_et_Al_2023_01 %>%
    mutate(
      Habitat_freshwater = as.numeric(Habitat_freshwater),
      Habitat_terrestrial = as.numeric(Habitat_terrestrial),
      Habitat_marine = as.numeric(Habitat_marine)
    )
  Seebens_et_Al_2023_01 <- Seebens_et_Al_2023_01 %>%
    filter(Habitat_freshwater == 1 |
             (Habitat_freshwater == 0 & Habitat_terrestrial == 0 & Habitat_marine == 0) |
             (Habitat_freshwater == "NA" & Habitat_terrestrial == "NA" & Habitat_marine == "NA") |
             (Habitat_freshwater == 0 & Habitat_terrestrial == "NA" & Habitat_marine == "NA") |
             (Habitat_freshwater == "NA" & Habitat_terrestrial == 0 & Habitat_marine == "NA") |
             (Habitat_freshwater == "NA" & Habitat_terrestrial == "NA" & Habitat_marine == 0) |
             (Habitat_freshwater == "NA" & Habitat_terrestrial == 0 & Habitat_marine == 0) |
             (Habitat_freshwater == 0 & Habitat_terrestrial == "NA" & Habitat_marine == 0) |
             (Habitat_freshwater == 0 & Habitat_terrestrial == 0 & Habitat_marine == "NA"))


  #NO DUPLICATES
  Seebens_et_Al_2023_01[] <- lapply(Seebens_et_Al_2023_01, as.character)
  source(file.path("R","noduplicates.r"))
  Seebens_et_Al_2023_01 <- noduplicates(Seebens_et_Al_2023_01, "TaxonName")


  ##########################################################
  #PARA OBTENER LA FECHA MÁS ANTIGUA #######################
  ##########################################################
  source(file.path("R", "OldestDate.r"))
  Seebens_et_Al_2023_02 <- OldestDate(Seebens_et_Al_2023_01,"FirstRecord")

  colnames(Seebens_et_Al_2023_02)[colnames(Seebens_et_Al_2023_02) == "LifeForm"] <- "Group"


  ###########################################################################
  #########  OBTENEMOS LAS FRESHWATER                              ##########
  ###########################################################################
  #En la nueva versión, vienen marcadas con columnas el habitat. Miramos
  source(file.path("R", "check_habitat.r"))
  dataset <- Seebens_et_Al_2023_02
  nombres <- dataset$TaxonName
  nombres_acep <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(nombres_acep, dataset)

  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dat_act %>%
    filter(grepl("FRESHWATER", Habitat))

  write.xlsx(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Seebens_et_Al_2023.xlsx")
  cat("Archivo descargado correctamente: /Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Seebens_et_Al_2023.xlsx")
  #write.csv2(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Seebens_et_Al_2023.csv")
}
