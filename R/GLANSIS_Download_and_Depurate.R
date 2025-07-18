GLANSIS_Download_and_Depurate <- function(){

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://nas.er.usgs.gov/queries/greatLakes/SpeciesList.aspx?SpeciesCategory=3&Group=&HUCNumber=&Genus=&Species=&ComName=&status=0&pathway=0&Sortby=1"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[2]]
  write.xlsx(res3, "Inputfiles/Step0_OriginalDatabase_GLANSIS.xlsx")

  #########################################################
  ################# DEPURAR ###############################
  #########################################################
  ruta_inputfiles <- file.path("Inputfiles")
  data_name <- list.files(path = ruta_inputfiles, pattern = "Step0_OriginalDatabase_GLANSIS.xlsx", full.names = TRUE)
  dat <- read.xlsx(data_name) #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.
  dat <- dat[,-1]
  dat

  #NO DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  dat_noduplicados <- noduplicates(dat, "Scientific.Name(click.for.species.profile)")
  dat_noduplicados
  names(dat_noduplicados)[names(dat_noduplicados) ==
          "Scientific.Name(click.for.species.profile)"] <- "ScientificName"
  names(dat_noduplicados)[names(dat_noduplicados) ==
                            "Year.First.Collected"] <- "OldestDate"

  #Obtener habitat
  source(file.path("R", "check_habitat.r"))
  dataset <- dat_noduplicados
  especies <- dataset$ScientificName
  nombres_acept <- name_backbone_checklist(especies)$canonicalName
  dat_act <- check_habitat(nombres_acept, dataset)


  #Freshwater
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh
  names(dat_fresh) <- gsub("\\.", "_", names(dat_fresh))

  write.xlsx(dat_fresh, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_GLANSIS.xlsx")
}

