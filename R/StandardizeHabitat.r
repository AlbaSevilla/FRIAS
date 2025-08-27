StandardizeHabitat <- function() {

  ###############################################
  ######## STANDARDIZE Habitat   #############
  ###############################################

  # Leer archivo de equivalencias
  equivs <- read_excel("TablesToStandardize/Standardization_Habitat.xlsx", sheet = 1, col_names = TRUE) %>%
    mutate(
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategorieshabitat = trimws(StandardizedCategorieshabitat)
    )

  # Leer datos desde archivo
  dat <- read_excel("OutputFiles/Intermediate/Step10_StandardizedMechanism_Masterlist.xlsx")

  # Separar mecanismos múltiples, limpiar
  dat <- dat %>%
    mutate(Habitat = str_split(Habitat, ",")) %>%
    unnest(Habitat) %>%
    mutate(
      Habitat = tolower(str_trim(Habitat)))

  dat$Habitat <- gsub("[_/\\-]", "", dat$Habitat)

  # Verificar existencia de columna y datos válidos
  if ("Habitat" %in% colnames(dat) && any(!is.na(dat$Habitat) & dat$Habitat != "")) {

    # Hacer match con equivalencias
    match_idx <- match(dat$Habitat, equivs$OriginalCategories)
    reemplazos <- equivs$StandardizedCategorieshabitat[match_idx]

    # Reemplazar solo donde hay match
    dat$Habitat[!is.na(reemplazos)] <- reemplazos[!is.na(reemplazos)]
  }

  source(file.path("R", "noduplicates.r"))
  noduplicados <- noduplicates(dat, "AcceptedNameGBIF")

  write_xlsx(noduplicados, "OutputFiles/Intermediate/Step11_StandardizedHabitat_Masterlist.xlsx")
}
