griis_2022_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://zenodo.org/records/6348164/files/GRIIS%20-%20Country%20Compendium%20V1_0.csv?download=1"
  destfile <- "InputFiles/originaldatabase_griis_2022.csv"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")


  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  ruta_inputfiles <- file.path("Inputfiles")
  data_name_griis_2022 <- list.files(path = ruta_inputfiles, pattern = "originaldatabase_griis_2022.csv", full.names = TRUE)
  dat <- read.csv(data_name_griis_2022) #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.

  dat$habitat <- factor(dat$habitat)
  # [1] "BRACKISH" *                              "BRACKISH|MARINE"  *
  # [3] "FRESHWATER"         *                    "FRESHWATER|BRACKISH" *
  # [5] "FRESHWATER|BRACKISH|MARINE"        *     "FRESHWATER|MARINE"   *
  # [7] "HOST"                                   "MARINE"
  # [9] "TERRESTRIAL"                            "TERRESTRIAL|BRACKISH"   *
  # [11] "TERRESTRIAL|BRACKISH|MARINE"     *       "TERRESTRIAL|FRESHWATER"     *
  # [13] "TERRESTRIAL|FRESHWATER|BRACKISH"   *     "TERRESTRIAL|FRESHWATER|BRACKISH|MARINE"*
  # [15] "TERRESTRIAL|FRESHWATER|MARINE"    *      "TERRESTRIAL|MARINE"
  # Convertir factor a carácter, hacer reemplazo y volver a factor (si quieres)
  dat$establishmentMeans <- as.character(dat$establishmentMeans)
  dat$establishmentMeans[dat$establishmentMeans == "NATIVE|ALIEN"] <-
    "Native and Introduced in different areas but same country"

  # Opcional: volver a factor si lo necesitas
  dat$establishmentMeans <- as.factor(dat$establishmentMeans)
  unique(dat$establishmentMeans)
  #[1] "ALIEN"*                 "CRYPTOGENIC|UNCERTAIN" "NATIVE|ALIEN"*
  dat$taxonRank <- factor(dat$taxonRank)
  #[1] "FORM"       "GENUS"      "SPECIES"*    "SUBSPECIES" "VARIETY"
  dat$isHybrid <- factor(dat$isHybrid)

  #OBTENER EL FILTRADO QUE QUEREMOS DE ESPECIES, ESTABLISHMENTMEANS Y TAXONRANK
  griis_2022_freshwater <- dat %>%
    filter(habitat %in% c("FRESHWATER",
                          "FRESHWATER|BRACKISH", "FRESHWATER|BRACKISH|MARINE",
                          "FRESHWATER|MARINE", #"TERRESTRIAL|FRESHWATER",
                          "TERRESTRIAL|FRESHWATER|MARINE",
                          "TERRESTRIAL|FRESHWATER|BRACKISH|MARINE",
                          "TERRESTRIAL|FRESHWATER|BRACKISH")) %>%
    filter(establishmentMeans %in% c("ALIEN", "Native and Introduced in different areas but same country")) %>%
    filter(taxonRank %in% c("SPECIES")) %>%
    filter(isHybrid %in% c("FALSE"))


    #NO DUPLICADOS
  source(file.path("R","noduplicates.r"))
  griis_2022_sinduplicados <- noduplicates(griis_2022_freshwater, column_name_species = "species")
  nrow(griis_2022_sinduplicados)

  write.xlsx(griis_2022_sinduplicados, "./Inputfiles/freshwatersubset_griis_2022.xlsx")
}
