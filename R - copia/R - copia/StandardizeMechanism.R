StandardizeMechanism <- function() {

  ###############################################
  ######## STANDARDIZE MECHANISMS   #############
  ###############################################

  # Leer archivo de equivalencias
  equivs <- read_excel("TablesToStandardize/Standardization_Mechanisms.xlsx", sheet = 1, col_names = TRUE) %>%
    mutate(
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategoriesMechanisms = trimws(StandardizedCategoriesMechanisms)
    )

  # Leer datos desde archivo
  dat <- read_excel("OutputFiles/Intermediate/Step4_StandardLocationNames_MasterList.xlsx")
  names(dat)

  # Separar mecanismos múltiples, limpiar
  dat <- dat %>%
    mutate(Mechanism = str_split(Mechanism, ",")) %>%
    unnest(Mechanism) %>%
    mutate(
      Mechanism = tolower(str_trim(Mechanism)))

  dat$Mechanism <- gsub("[_/\\-]", "", dat$Mechanism)

  # Verificar existencia de columna y datos válidos
  if ("Mechanism" %in% colnames(dat) && any(!is.na(dat$Mechanism) & dat$Mechanism != "")) {

    # Hacer match con equivalencias
    match_idx <- match(dat$Mechanism, equivs$OriginalCategories)
    reemplazos <- equivs$StandardizedCategoriesMechanisms[match_idx]

    # Reemplazar solo donde hay match
    dat$Mechanism[!is.na(reemplazos)] <- reemplazos[!is.na(reemplazos)]
  }

  source(file.path("R", "colapsar_por_AcceptedNameGBIF.r"))
  noduplicados <- colapsar_por_AcceptedNameGBIF(dat)

  write_xlsx(noduplicados, "OutputFiles/Intermediate/Step5_StandardizedMechanism_Masterlist.xlsx")

}
