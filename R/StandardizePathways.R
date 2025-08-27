StandardizePathways <- function(){

  # Pathways_levels: esta tabla no da tantos problemas. El único detalle es lo que
  # comentamos el viernes: uno de los niveles que sacaste de LIFE INVASAQUA incluye
  # varios pathways para una misma especie, separados por un punto. Antes de estandarizar
  # los niveles con la tabla, habría que reemplazar los “.” por “;”, como dijiste.
  #
  # He revisado que todos los niveles útiles estén recogidos en la tabla. Por tanto,
  # puedes eliminar el resto del texto (entre “;”) que no tenga correspondencia. Y creo
  # que ya lo hiciste para otros campos de la Master List, pero… ¿podrías quitar también
  # las redundancias? Por ejemplo, si para una especie aparece
  # “escape from confinement; escape from confinement: agriculture”,
  # dejar solo el segundo, que es más específico y aporta más información.
  #

  ###############################################
  ######## STANDARDIZE Pathways   #############
  ###############################################

  # Leer archivo de equivalencias
  equivs <- read_excel("TablesToStandardize/Standardization_Pathways.xlsx", sheet = 1, col_names = TRUE) %>%
    mutate(
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategoriesPathways = trimws(StandardizedCategoriesPathways)
    )

  # Leer datos desde archivo
  dat <- read_excel("OutputFiles/Intermediate/Step11_StandardizedHabitat_Masterlist.xlsx")
  names(dat)

  # Separar mecanismos múltiples, limpiar
  dat <- dat %>%
    mutate(Pathways = str_split(Pathways, ";")) %>%
    unnest(Pathways) %>%
    mutate(
      Pathways = tolower(str_trim(Pathways)))

  dat$Pathways <- gsub("\\s+", " ", trimws(sub(":.*", "", dat$Pathways)))
  dat$Pathways <- tolower(dat$Pathways)  # Asegura minúsculas
  dat$Pathways <- gsub("[^a-z]", "", dat$Pathways)  # Deja solo letras

  if ("Pathways" %in% colnames(dat) && any(!is.na(dat$Pathways) & dat$Pathways != "")) {

    # Limpiar Pathways: convertir a minúsculas, quitar espacios, quitar "NA"
    dat$Pathways <- tolower(trimws(dat$Pathways))
    dat$Pathways[dat$Pathways == "na"] <- ""
    dat$Pathways[is.na(dat$Pathways)] <- ""

    # Limpiar equivalencias
    equivs$OriginalCategories <- tolower(trimws(equivs$OriginalCategories))

    # Hacer match con OriginalCategories
    match_idx <- match(dat$Pathways, equivs$OriginalCategories)
    reemplazos <- equivs$StandardizedCategoriesPathways[match_idx]

    # Reemplazar donde hay match
    dat$Pathways[!is.na(reemplazos)] <- reemplazos[!is.na(reemplazos)]

    # Dejar vacío si no hay coincidencia
    dat$Pathways[is.na(reemplazos)] <- ""
  }

  source(file.path("R", "noduplicates.r"))
  noduplicados <- noduplicates(dat, "AcceptedNameGBIF")

  write_xlsx(noduplicados, "OutputFiles/Intermediate/Step12_Standardizedpathway_Masterlist.xlsx")

}
