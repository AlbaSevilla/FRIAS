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
  dat <- read_excel("OutputFiles/Intermediate/Step5_StandardizedMechanism_Masterlist.xlsx")

  # Separar mecanismos múltiples, limpiar
  dat <- dat %>%
    mutate(habitat = str_split(habitat, ";")) %>%
    unnest(habitat) %>%
    mutate(
      habitat = tolower(str_trim(habitat)))

  dat$habitat <- gsub("[_/\\-]", "", dat$habitat)

  # Verificar existencia de columna y datos válidos
  if ("habitat" %in% colnames(dat) && any(!is.na(dat$habitat) & dat$habitat != "")) {

    # Hacer match con equivalencias
    match_idx <- match(dat$habitat, equivs$OriginalCategories)
    reemplazos <- equivs$StandardizedCategorieshabitat[match_idx]

    # Reemplazar solo donde hay match
    dat$habitat[!is.na(reemplazos)] <- reemplazos[!is.na(reemplazos)]
  }

  source(file.path("R", "colapsar_por_AcceptedNameGBIF.r"))
  noduplicados <- colapsar_por_AcceptedNameGBIF(dat)

  write_xlsx(noduplicados, "OutputFiles/Intermediate/Step6_StandardizedHabitat_Masterlist.xlsx")
}
