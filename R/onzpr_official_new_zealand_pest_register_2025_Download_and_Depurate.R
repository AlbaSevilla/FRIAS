onzpr_official_new_zealand_pest_register_2025_Download_and_Depurate <- function(){
  #DESCARGAR
  #A mano en https://pierpestregister.mpi.govt.nz/pests-of-concern/?scientificName=&organismType=&freeSearch=
  #selecionando opcion 'excel'

  dat <- read_excel("Inputfiles/originaldatabase_onzpr_official_new_zealand_pest_register_2025.xlsx")
  colnames(dat) <- gsub(" ", "_", colnames(dat))
  names(dat)
  dat$Invaded_country <- "New Zealand"


  eliminate_categories <- c(
    "Virus", "Bacterium", "Fungus", "Viroid", "Protista",
    "Chromista", "Prion", "Protozoan", "Archaea"
  )
  dat <- dat[!(dat$Organism_type %in% eliminate_categories), ]


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

  write.xlsx(dat_fresh, "Inputfiles/freshwatersubset_onzpr_official_new_zealand_pest_register_2025.xlsx")
}
