lüskow_et_al_2023_Download_and_Depurate <- function() {
  ################################################################################
  ######## OBTENCIÓN DE LA BASE DE DATOS A TRAVÉS DE ZENODO ######################
  ################################################################################
#
#   url <- "https://download.pangaea.de/dataset/962186/files/L%C3%BCskow_et_al_jellyfish_literature.xlsx"

  datos <- read.xlsx(file.path("InputFiles","originaldatabase_lüskow_et_al_2023.xlsx"),sep=";")
  # Agregar la columna 'SpeciesName' con condiciones
  datos <- datos %>%
    mutate(SpeciesName = case_when(
      Genus == "Craspedacusta" ~ "Craspedacusta sowerbii",
      Genus == "Astrohydra" ~ "Astrohydra japonica",
      Genus == "Limnocnida" ~ "Limnocnida indica",
      TRUE ~ NA_character_  # Para los que no cumplen ninguna condición
    ))

  datos <- datos %>% select(SpeciesName, everything())%>%
    select(SpeciesName)
  datos <- unique(datos)
  datos$Habitat <- "Freshwater"

  # Guardar el dataframe en un archivo Excel
  write.xlsx(datos,file.path("InputFiles", paste("freshwatersubset_lüskow_et_al_2023.xlsx")), rowNames = FALSE)
}
