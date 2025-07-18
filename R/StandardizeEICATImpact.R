StandardizeEICATImpact <- function() {

  ###############################################
  ######## STANDARDIZE EICATImpact   ############
  ###############################################

  # Leer archivo de equivalencias
  equivs <- read_excel("TablesToStandardize/Standardization_EICATImpact.xlsx", sheet = 1, col_names = TRUE) %>%
    mutate(
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategoriesEICAT = trimws(StandardizedCategoriesEICAT)
    )

  # Leer datos desde archivo
  dat <- read_excel("OutputFiles/Intermediate/Step3_StandardLocationNames_MasterList.xlsx")

  # Separar mecanismos múltiples, limpiar
  dat <- dat %>%
    mutate(EICATImpact = str_split(EICATImpact, ",")) %>%
    unnest(EICATImpact) %>%
    mutate(
      EICATImpact = tolower(str_trim(EICATImpact)),
      EICATImpact = gsub("[-_/]", "", EICATImpact)  # Eliminar caracteres especiales pero mantener espacios
    )
  unique(dat$EICATImpact)

  # Verificar existencia de columna y datos válidos
  if ("EICATImpact" %in% colnames(dat) && any(!is.na(dat$EICATImpact) & dat$EICATImpact != "")) {

    # Hacer match con equivalencias
    match_idx <- match(dat$EICATImpact, equivs$OriginalCategories)
    reemplazos <- equivs$StandardizedCategoriesEICATImpact[match_idx]

    # Reemplazar solo donde hay match
    dat$EICATImpact[!is.na(reemplazos)] <- reemplazos[!is.na(reemplazos)]
  }

  source(file.path("R", "noduplicates.r"))
  noduplicados <- noduplicates(dat, "OriginalNameDB")

  write_xlsx(noduplicados, "OutputFiles/Intermediate/Step4_StandardizedEICATImpact_Masterlist.xlsx")

}
