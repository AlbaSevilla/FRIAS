BiosecurityNewZealand_Download_and_Depurate <- function(){
  #DESCARGAR
  #A mano en https://pierpestregister.mpi.govt.nz/pests-of-concern/?scientificName=&organismType=&freeSearch=
  #selecionando opcion 'excel'


  #DEPURAR
  dat <- read_excel("Inputfiles/Step0_OriginalDatabase_BiosecurityNewZealand.xlsx")
  colnames(dat) <- gsub(" ", "_", colnames(dat))
  names(dat)
  dat$Invaded_country <- "New Zealand"


  #Vamos a quitarnos los virus, microorganismos
  unique(dat$Organism_type)
  tipos_a_eliminar <- c(
    "Virus", "Bacterium", "Fungus", "Viroid", "Protista",
    "Chromista", "Prion", "Protozoan", "Archaea"
  )
  dat <- dat[!(dat$Organism_type %in% tipos_a_eliminar), ]


  #No duplicados:
  source(file.path("R","noduplicates.r"))
  dat_noduplicados <- noduplicates(dat, "Scientific_name(s)")

  #Habitat
  source(file.path("R","check_habitat.r"))
  dataset <- dat_noduplicados
  nombres <- dataset$`Scientific_name(s)`
  nombres_acep <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(nombres_acep, dataset)

  #Freshwater?
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh <- dat_fresh[, c("Species", setdiff(names(dat_fresh), "Species"))]

  #Guardamos
  write.xlsx(dat_fresh, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_BiosecurityNewZealand.xlsx")


}
