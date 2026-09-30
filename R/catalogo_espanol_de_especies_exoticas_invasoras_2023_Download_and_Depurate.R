catalogo_espanol_de_especies_exoticas_invasoras_2023_Download_and_Depurate <- function() {
  #Download
  url <- "https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/conservacion-de-especies/Tabla%20CEEEIcompleta.xlsx"
  destfile <- "InputFiles/originaldatabase_catalogo_espanol_de_especies_exoticas_invasoras_2023.xlsx"
  download.file(url, destfile, mode = "wb")
  data <- read_excel("InputFiles/originaldatabase_catalogo_espanol_de_especies_exoticas_invasoras_2023.xlsx")  # Cambia esta ruta por la ubicación de tu archivo
  write.csv2(data, "InputFiles/originaldatabase_catalogo_espanol_de_especies_exoticas_invasoras_2023.csv", row.names = FALSE)

  #Depurate
  dat <- read.csv("InputFiles/originaldatabase_catalogo_espanol_de_especies_exoticas_invasoras_2023.csv",sep=";")
  colnames(dat)[colnames(dat) == "Grupo.taxonómico"] <- "Group"
  colnames(dat)[colnames(dat) == "Rango.taxonómico"] <- "Rango_taxonomico"
  colnames(dat)[colnames(dat) == "Nombre.científico.CEEEI..BOE"] <- "Nombre_cientifico_CEEEI"
  colnames(dat)[colnames(dat) == "Nombre.científico.actualizado"] <- "Nombre_cientifico_actualizado"
  colnames(dat)[colnames(dat) == "Ámbito.de.aplicación"] <- "Ambito_de_aplicacion"
  colnames(dat)[colnames(dat) == "Nombre.común"] <- "Nombre_comun"
  colnames(dat)[colnames(dat) == "Situación.anterior"] <- "Situacion_anterior"
  catalogo_espanol_de_especies_exoticas_invasoras_2023_rangoespecie <- dat %>%
    filter(Rango_taxonomico %in% c("especie")) %>%
    select(Group, Nombre_cientifico_CEEEI)
  invasive_country <- c("Spain")
  catalogo_espanol_de_especies_exoticas_invasoras_2023_rangoespecie$invasive_country <- invasive_country
  catalogo_espanol_de_especies_exoticas_invasoras_2023_subset_noduplicates <- catalogo_espanol_de_especies_exoticas_invasoras_2023_rangoespecie %>%
    dplyr::group_by(Nombre_cientifico_CEEEI) %>%
    dplyr::summarise(across(everything(), ~ {
      text <- as.character(.)
      text <- str_remove_all(text, "https://doi[^[:space:]]+")
      vals <- unlist(str_split(text, "[,;|]+|\\s{2,}"))
      vals <- str_trim(vals)
      vals <- vals[vals != ""]
      vals <- unique(vals)
      paste(vals, collapse = ", ")
    }), .groups = "drop")

  dataset <- catalogo_espanol_de_especies_exoticas_invasoras_2023_subset_noduplicates
  species_list0 <- dataset$Nombre_cientifico_CEEEI
  species_list <- name_backbone_checklist(species_list0)$canonicalName
  dataset_updated <- check_habitat(species_list, dataset)
  dataset_freshwater <- dataset_updated %>%
    filter(grepl("FRESHWATER", Habitat))
  dataset_freshwater <- dataset_freshwater[, c("Species", setdiff(names(dataset_freshwater), "Species"))]

  #Save
  write.xlsx(dataset_freshwater, "./InputFiles/freshwatersubset_catalogo_espanol_de_especies_exoticas_invasoras_2023.xlsx")
}

