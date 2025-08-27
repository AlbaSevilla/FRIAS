SouthAfrica2022_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://zenodo.org/records/8217197/files/SANBI_and_CIB_(2023)_SBISA_in_2022_Appendix_2_The_species_list.xlsx?download=1"
  destfile <- "InputFiles/Step0_OriginalDatabase_SouthAfrica2022.xlsx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")


  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  dat <- read_excel("Inputfiles/Step0_OriginalDatabase_SouthAfrica2022.xlsx")
  names(dat)

  #OBTENER EL FILTRADO QUE QUEREMOS DE ESPECIES, ESTABLISHMENTMEANS Y TAXONRANK
  SouthAfrica2022_freshwater <- dat %>%
    filter(taxonRank %in% c("sp.")) %>%
    filter(isNative %in% c("FALSE")) %>%
    filter(IntroductionStatus %in% c("presentAsAlienNotNaturalised",
                                     "Invasive",
                                     "Invasive:NativeAlienPopulations",
                                     "PresentAsAlienNotNaturalised"))


  #NO DUPLICADOS
  # source(file.path("R","noduplicates.r"))
  # SouthAfrica2022_sinduplicados <- noduplicates(SouthAfrica2022_freshwater, "scientificName")
  # nrow(SouthAfrica2022_sinduplicados)
  #

  #NO TIENE DUBPLICADOS YA DE BASE


  #Los que son freshwater para GBIF
  source(file.path("R", "check_habitat.r"))
  dataset <- SouthAfrica2022_freshwater
  nombres <- dataset$scientificName
  nombres_acep <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(nombres_acep, dataset)

  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))

  dat_fresh$Invaded_country <- "South Africa"

  dataset <- dat_fresh %>%
    mutate(scientificName = str_extract(scientificName, "^\\S+\\s+\\S+"))

  write.xlsx(dataset, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_SouthAfrica2022.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_SouthAfrica2022.xlsx", "\n")

  # write.csv2(SouthAfrica2022_sinduplicados, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_SouthAfrica2022.csv")
  # cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_SouthAfrica2022.csv", "\n")
  #
}
