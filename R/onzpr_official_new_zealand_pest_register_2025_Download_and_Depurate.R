onzpr_official_new_zealand_pest_register_2025_Download_and_Depurate <- function(){
  #Depurate
  dat <- read_excel("InputFiles/originaldatabase_onzpr_official_new_zealand_pest_register_2025.xlsx")
  colnames(dat) <- gsub(" ", "_", colnames(dat))
  dat$Invaded_country <- "New Zealand"
  eliminate_categories <- c(
    "Virus", "Bacterium", "Fungus", "Viroid", "Protista",
    "Chromista", "Prion", "Protozoan", "Archaea"
  )
  dat <- dat[!(dat$Organism_type %in% eliminate_categories), ]
  dat_noduplicates <- noduplicates(dat, "Scientific_name(s)")
  dataset <- dat_noduplicates
  names <- dataset$`Scientific_name(s)`
  accepted_names <- name_backbone_checklist(names)$canonicalName
  dat_act <- check_habitat(accepted_names, dataset)
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh <- dat_fresh[, c("Species", setdiff(names(dat_fresh), "Species"))]

  #Save
  write.xlsx(dat_fresh, "InputFiles/freshwatersubset_onzpr_official_new_zealand_pest_register_2025.xlsx")
}
