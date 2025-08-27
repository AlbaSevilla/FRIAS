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
  dat <- read_excel("OutputFiles/Intermediate/Step9_StandardLocationNames_MasterList.xlsx")
  names(dat)

  # Separar mecanismos múltiples, limpiar
  dat <- dat %>%
    mutate(Mechanisms = str_split(Mechanisms, ",")) %>%
    unnest(Mechanisms) %>%
    mutate(
      Mechanisms = tolower(str_trim(Mechanisms)))

  dat$Mechanisms <- gsub("[_/\\-]", "", dat$Mechanisms)

  # Verificar existencia de columna y datos válidos
  if ("Mechanisms" %in% colnames(dat) && any(!is.na(dat$Mechanisms) & dat$Mechanisms != "")) {

    # Hacer match con equivalencias
    match_idx <- match(dat$Mechanisms, equivs$OriginalCategories)
    reemplazos <- equivs$StandardizedCategoriesMechanisms[match_idx]

    # Reemplazar solo donde hay match
    dat$Mechanisms[!is.na(reemplazos)] <- reemplazos[!is.na(reemplazos)]
  }

  source(file.path("R", "noduplicates.r"))
  noduplicados <- noduplicates(dat, "AcceptedNameGBIF")

  write_xlsx(noduplicados, "OutputFiles/Intermediate/Step10_StandardizedMechanism_Masterlist.xlsx")

}
