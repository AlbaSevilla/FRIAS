BiologicalInvasionsPlankton_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://ars.els-cdn.com/content/image/1-s2.0-S004313542200865X-mmc1.csv"
  destfile <- "InputFiles/Step0_OriginalDatabase_BiologicalInvasionsPlankton.csv"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")


  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  dat <- read.csv2("InputFiles/Step0_OriginalDatabase_BiologicalInvasionsPlankton.csv") #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.
  names(dat)
  dat$habitat <- factor(dat$habitat)
  # [1] "BRACKISH" *                              "BRACKISH|MARINE"  *
  # [3] "FRESHWATER"         *                    "FRESHWATER|BRACKISH" *
  # [5] "FRESHWATER|BRACKISH|MARINE"        *     "FRESHWATER|MARINE"   *
  # [7] "HOST"                                   "MARINE"
  # [9] "TERRESTRIAL"                            "TERRESTRIAL|BRACKISH"   *
  # [11] "TERRESTRIAL|BRACKISH|MARINE"     *       "TERRESTRIAL|FRESHWATER"     *
  # [13] "TERRESTRIAL|FRESHWATER|BRACKISH"   *     "TERRESTRIAL|FRESHWATER|BRACKISH|MARINE"*
  # [15] "TERRESTRIAL|FRESHWATER|MARINE"    *      "TERRESTRIAL|MARINE"
  dat$establishmentMeans <- factor(dat$establishmentMeans)
  #[1] "ALIEN"*                 "CRYPTOGENIC|UNCERTAIN" "NATIVE|ALIEN"*
  dat$taxonRank <- factor(dat$taxonRank)
  #[1] "FORM"       "GENUS"      "SPECIES"*    "SUBSPECIES" "VARIETY"
  dat$isHybrid <- factor(dat$isHybrid)

  #OBTENER EL FILTRADO QUE QUEREMOS DE ESPECIES, ESTABLISHMENTMEANS Y TAXONRANK
  BiologicalInvasionsPlankton_freshwater <- dat %>%
    filter(habitat %in% c("FRESHWATER",
                          "FRESHWATER|BRACKISH", "FRESHWATER|BRACKISH|MARINE",
                          "FRESHWATER|MARINE", #"TERRESTRIAL|FRESHWATER",
                          "TERRESTRIAL|FRESHWATER|MARINE",
                          "TERRESTRIAL|FRESHWATER|BRACKISH|MARINE",
                          "TERRESTRIAL|FRESHWATER|BRACKISH")) %>%
    filter(establishmentMeans %in% c("ALIEN", "NATIVE|ALIEN")) %>%
    filter(taxonRank %in% c("SPECIES")) %>%
    filter(isHybrid %in% c("FALSE"))


  ##### NOS QUEDAMOS CON LAS COLUMNAS DE INTERÉS
  columnas_a_quedarse <- c("species","kingdom","order","family","taxonRank","country","habitat","establishmentMeans")

  BiologicalInvasionsPlankton_freshwater_subset_BiologicalInvasionsPlankton <- BiologicalInvasionsPlankton_freshwater %>%
    select(all_of(columnas_a_quedarse))

  #NO DUPLICADOS
  source(file.path("R","noduplicates.r"))
  BiologicalInvasionsPlankton_sinduplicados <- noduplicates(BiologicalInvasionsPlankton_freshwater_subset_BiologicalInvasionsPlankton, column_name_species = "species")
  nrow(BiologicalInvasionsPlankton_sinduplicados)

  #Los que son freshwater para GBIF
  # source(file.path("R", "check_habitat.r"))
  # dataset <- BiologicalInvasionsPlankton_sinduplicados
  # nombres <- dataset$species
  # nombres_acep <- name_backbone_checklist(nombres)$canonicalName
  # dat_act <- check_habitat(nombres_acep, dataset)
  # dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  # nrow(dat_fresh)


  write.xlsx(BiologicalInvasionsPlankton_sinduplicados, "./InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_BiologicalInvasionsPlankton.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_BiologicalInvasionsPlankton.xlsx", "\n")
  # write.csv2(BiologicalInvasionsPlankton_sinduplicados, "./InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_BiologicalInvasionsPlankton.csv")
  # cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_BiologicalInvasionsPlankton.csv", "\n")
  #
}
