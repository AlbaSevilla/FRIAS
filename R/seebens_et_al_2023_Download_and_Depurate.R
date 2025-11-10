seebens_et_al_2023_Download_and_Depurate <- function() {

  ###############################################################
  ########### TO DOWNLOAD seebens_et_al_2023    #######################
  ###############################################################
  url <- "https://zenodo.org/records/10039630/files/GlobalAlienSpeciesFirstRecordDatabase_v3.1_freedata.xlsx?download=1"
  destfile <- "InputFiles/originaldatabase_seebens_et_al_2023.xlsx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }

  #no tiene pathways para hacer lista
  download.file(url, destfile, mode = "wb")


  ################################################################
  ########### PARA DEPURAR FIRST RECORDS #########################
  ################################################################
  dat <- read.xlsx(file.path("Inputfiles","originaldatabase_seebens_et_al_2023.xlsx"),sheet=2)
  dim(dat)

  ##ELIMINAR LOS REGISTROS CON FECHAS MENORES A 0
  seebens_et_al_2023_01 <- dat[dat$FirstRecord >= 0, ]
  dim(seebens_et_al_2023_01)

  #NOS QUEDAMOS CON LAS ALIENS
  seebens_et_al_2023_01 <- seebens_et_al_2023_01 %>% filter(PresentStatus == "alien" |
                                                  PresentStatus == "Established invasive")
  dim(seebens_et_al_2023_01)

  #NOS QUEDAMOS CON LOS QUE DICE QUE SON FRESHWATER LA BD
  seebens_et_al_2023_01 <- seebens_et_al_2023_01 %>%
    mutate(
      Habitat_freshwater = as.numeric(Habitat_freshwater),
      Habitat_terrestrial = as.numeric(Habitat_terrestrial),
      Habitat_marine = as.numeric(Habitat_marine)
    ) %>%
    filter(Habitat_freshwater == 1 |
             (Habitat_freshwater == 0 & Habitat_terrestrial == 0 & Habitat_marine == 0) |
             (Habitat_freshwater == "NA" & Habitat_terrestrial == "NA" & Habitat_marine == "NA") |
             (Habitat_freshwater == 0 & Habitat_terrestrial == "NA" & Habitat_marine == "NA") |
             (Habitat_freshwater == "NA" & Habitat_terrestrial == 0 & Habitat_marine == "NA") |
             (Habitat_freshwater == "NA" & Habitat_terrestrial == "NA" & Habitat_marine == 0) |
             (Habitat_freshwater == "NA" & Habitat_terrestrial == 0 & Habitat_marine == 0) |
             (Habitat_freshwater == 0 & Habitat_terrestrial == "NA" & Habitat_marine == 0) |
             (Habitat_freshwater == 0 & Habitat_terrestrial == 0 & Habitat_marine == "NA"))
  dim(seebens_et_al_2023_01)

  #NO DUPLICATES
  seebens_et_al_2023_01[] <- lapply(seebens_et_al_2023_01, as.character)
  source(file.path("R","noduplicates.r"))
  seebens_et_al_2023_01 <- noduplicates(seebens_et_al_2023_01, "TaxonName")
  dim(seebens_et_al_2023_01)

  ##########################################################
  #PARA OBTENER LA FECHA MÁS ANTIGUA #######################
  ##########################################################
  source(file.path("R", "OldestDate.r"))
  seebens_et_al_2023_02 <- OldestDate(seebens_et_al_2023_01,"FirstRecord")
  colnames(seebens_et_al_2023_02)[colnames(seebens_et_al_2023_02) == "LifeForm"] <- "Group"


  ###########################################################################
  #########  OBTENEMOS LAS FRESHWATER                              ##########
  ###########################################################################
  #En la nueva versión, vienen marcadas con columnas el habitat. Miramos
  source(file.path("R", "check_habitat.r"))
  dataset <- seebens_et_al_2023_02
  nombres <- dataset$TaxonName
  nombres_acep <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(nombres_acep, dataset)

  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dat_act %>%
    filter(grepl("FRESHWATER", Habitat))

  write.xlsx(dataset_freshwater, "./Inputfiles/freshwatersubset_seebens_et_al_2023.xlsx")
}
