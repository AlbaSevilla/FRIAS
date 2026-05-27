lüskow_et_al_2023_Download_and_Depurate <- function() {
  #Depurate
  data <- read.xlsx(file.path("InputFiles","originaldatabase_lüskow_et_al_2023.xlsx"), sep=";")
  data <- data %>%
    mutate(SpeciesName = case_when(
      Genus == "Craspedacusta" ~ "Craspedacusta sowerbii",
      Genus == "Astrohydra" ~ "Astrohydra japonica",
      Genus == "Limnocnida" ~ "Limnocnida indica",
      TRUE ~ NA_character_
    ))
  data <- data %>% select(SpeciesName, everything()) %>%
    select(SpeciesName, Continent)
  data <- unique(data)
  data$Habitat <- "Freshwater"
  names <- data$SpeciesName
  data$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName
  #Save
  write.xlsx(data, file.path("InputFiles", "freshwatersubset_lüskow_et_al_2023.xlsx"), rowNames = FALSE)
}
