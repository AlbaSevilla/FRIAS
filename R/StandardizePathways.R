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
  dat <- read_excel("OutputFiles/Intermediate/Step6_StandardizedHabitat_Masterlist.xlsx")
  names(dat)

  # Separar mecanismos múltiples, limpiar
  dat <- dat %>%
    mutate(pathway = str_split(pathway, ";")) %>%
    unnest(pathway) %>%
    mutate(
      pathway = tolower(str_trim(pathway)))

  dat$pathway <- gsub("\\s+", " ", trimws(sub(":.*", "", dat$pathway)))
  dat$pathway <- tolower(dat$pathway)  # Asegura minúsculas
  dat$pathway <- gsub("[^a-z]", "", dat$pathway)  # Deja solo letras

  View(dat)
  unique(dat$pathway)


  if ("pathway" %in% colnames(dat) && any(!is.na(dat$pathway) & dat$pathway != "")) {

    # Limpiar pathway: convertir a minúsculas, quitar espacios, quitar "NA"
    dat$pathway <- tolower(trimws(dat$pathway))
    dat$pathway[dat$pathway == "na"] <- ""
    dat$pathway[is.na(dat$pathway)] <- ""

    # Limpiar equivalencias
    equivs$OriginalCategories <- tolower(trimws(equivs$OriginalCategories))

    # Hacer match con OriginalCategories
    match_idx <- match(dat$pathway, equivs$OriginalCategories)
    reemplazos <- equivs$StandardizedCategoriesPathways[match_idx]

    # Reemplazar donde hay match
    dat$pathway[!is.na(reemplazos)] <- reemplazos[!is.na(reemplazos)]

    # Dejar vacío si no hay coincidencia
    dat$pathway[is.na(reemplazos)] <- ""
  }

  source(file.path("R", "colapsar_por_AcceptedNameGBIF.r"))
  noduplicados <- colapsar_por_AcceptedNameGBIF(dat)

  write_xlsx(noduplicados, "OutputFiles/Intermediate/Step7_Standardizedpathway_Masterlist.xlsx")

}
