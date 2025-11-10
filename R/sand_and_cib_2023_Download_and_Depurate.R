sand_and_cib_2023_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://zenodo.org/records/14937470/files/A_checklist_of_alien_taxa_for_South%20Africa_v20250520.xlsx?download=1"
  destfile <- "InputFiles/originaldatabase_sand_and_cib_2023.xlsx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")


  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  dat <- read_excel("InputFiles/originaldatabase_sand_and_cib_2023.xlsx")
  names(dat)

  #OBTENER EL FILTRADO QUE QUEREMOS DE ESPECIES, ESTABLISHMENTMEANS Y TAXONRANK
  sand_and_cib_2023_freshwater <- dat %>%
    filter(taxonRank %in% c("sp.")) %>%
    filter(isNative %in% c("FALSE")) %>%
    filter(IntroductionStatus %in% c("presentAsAlienNotNaturalised",
                                     "Invasive",
                                     "Invasive:NativeAlienPopulations",
                                     "PresentAsAlienNotNaturalised"))


  #NO DUPLICADOS
   source(file.path("R","noduplicates.r"))
   sand_and_cib_2023_sinduplicados <- noduplicates(sand_and_cib_2023_freshwater, "scientificName")
   nrow(sand_and_cib_2023_sinduplicados)
  #

  #NO TIENE DUBPLICADOS YA DE BASE


  #Los que son freshwater para GBIF
   source(file.path("R", "check_habitat.r"))
   dataset <- sand_and_cib_2023_freshwater
   nombres <- dataset$scientificName
   nombres_acep <- character(length(nombres))
   for (i in seq_along(nombres)) {
     nombres_acep[i] <- name_backbone_checklist(nombres[i])$canonicalName
     Sys.sleep(0.02)
   }
   dat_act <- check_habitat(nombres_acep, dataset)

  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh$Invaded_country <- "South Africa"
  dataset <- dat_fresh %>%
    mutate(scientificName = str_extract(scientificName, "^\\S+\\s+\\S+"))

  write.xlsx(dataset, "InputFiles/freshwatersubset_sand_and_cib_2023.xlsx")
}
