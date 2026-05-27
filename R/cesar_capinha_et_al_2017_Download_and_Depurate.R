cesar_capinha_et_al_2017_Download_and_Depurate <- function() {
  #Depurate
  ruta_InputFiles <- file.path("InputFiles")
  dat0 <- read.xlsx("InputFiles/originaldatabase_cesar_capinha_et_al_2017.xlsx")
  species_filtered <- dat0 %>%
    filter(`1_Cesar` == "1")
  species_filtered_freshwater <- species_filtered$Species
  dat <- read.xlsx("InputFiles/originaldatabase_cesar_capinha_et_al_2012_v2.xlsx", sheet=2) # Aquí estamos indicando que sustraiga los excel de las bases de datos iniciales de la carpeta 'InputFiles'.
  dat <- dat[dat$Species %in% species_filtered_freshwater, ]
  dataset_noduplicates <- dat %>%
    dplyr::group_by(Species) %>%
    dplyr::summarise(across(everything(), ~ {
      texto <- as.character(.)
      vals <- unlist(str_split(texto, "[,;|]+|\\s{2,}"))
      vals <- str_trim(vals)
      vals <- vals[vals != ""]
      vals <- unique(vals)
      paste(vals, collapse = ", ")
    }), .groups = "drop")
  dataset_freshwater <- dataset_noduplicates
  dataset_freshwater$Habitat <- "Freshwater"
  dataset_freshwater <- dataset_freshwater[, c("Species", setdiff(names(dataset_freshwater), "Species"))]
  names <- dataset_freshwater$Species
  dataset_freshwater$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName

  #Save
  write.xlsx(dataset_freshwater, "./InputFiles/freshwatersubset_cesar_capinha_et_al_2017.xlsx")
}
