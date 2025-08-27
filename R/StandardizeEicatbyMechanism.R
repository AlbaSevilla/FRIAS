StandardizeEicatbyMechanism <- function() {
  # Leer equivalencias una sola vez
  equivs_eicat <- read_excel("TablesToStandardize/Standardization_EICATImpact.xlsx", sheet = 1, col_names = TRUE) %>%
    mutate(
      FileName = FileName2, # Cambia a FileName si tu columna se llama así
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategoriesEICAT = trimws(StandardizedCategoriesEICAT)
    )

  equivs_mec <- read_excel("TablesToStandardize/Standardization_Mechanisms.xlsx", sheet = 1, col_names = TRUE) %>%
    mutate(
      OriginalCategories = tolower(trimws(OriginalCategories)),
      StandardizedCategoriesMechanisms = trimws(StandardizedCategoriesMechanisms)
    )

  # Archivos únicos a procesar
  databases <- unique(equivs_eicat$FileName)
  databases <- databases[!is.na(databases)]


  for (database in databases) {
    # Leer el archivo de datos
    dat <- read_csv2(database)
    # Verifica que la columna exista
    if (!"EICAT_by_Mechanism" %in% names(dat)) next

    # 1. Separar por coma y limpiar
    dat <- dat %>%
      mutate(EICAT_by_Mechanism = str_split(EICAT_by_Mechanism, ",")) %>%
      unnest(EICAT_by_Mechanism) %>%
      mutate(EICAT_by_Mechanism = tolower(str_trim(EICAT_by_Mechanism)))
    dat$EICAT_by_Mechanism <- gsub(" ","",dat$EICAT_by_Mechanism)

    # 2. Separar por " - " en dos columnas
    dat <- dat %>%
      mutate(EICAT_by_Mechanism = ifelse(
        grepl("-", EICAT_by_Mechanism), EICAT_by_Mechanism, NA_character_
      )) %>%
      separate(
        EICAT_by_Mechanism,
        into = c("eicat", "mecanismo"),
        sep = "-",
        fill = "right",
        remove = TRUE
      ) %>%
      mutate(
        eicat = str_trim(eicat),
        mecanismo = str_trim(mecanismo)
      )

    # --- Estandarizar EICAT ---
    equivs_filtered_eicat <- equivs_eicat %>% filter(FileName == database)
    dat$eicat <- tolower(trimws(dat$eicat))
    match_idx <- match(dat$eicat, equivs_filtered_eicat$OriginalCategories)
    reemplazos <- equivs_filtered_eicat$StandardizedCategoriesEICAT[match_idx]
    dat$eicat[!is.na(reemplazos)] <- reemplazos[!is.na(reemplazos)]

    # --- Estandarizar mecanismo ---
    dat$mecanismo <- tolower(trimws(dat$mecanismo))
    dat$mecanismo <- gsub("[_/\\-]", "", dat$mecanismo)
    match_idx <- match(dat$mecanismo, equivs_mec$OriginalCategories)
    reemplazos <- equivs_mec$StandardizedCategoriesMechanisms[match_idx]
    dat$mecanismo[!is.na(reemplazos)] <- reemplazos[!is.na(reemplazos)]

    # 3. Volver a unir las columnas estandarizadas
    dat <- dat %>%
      unite("EICAT_by_Mechanism", eicat, mecanismo, sep = " - ", remove = FALSE) %>%
      select(-eicat, -mecanismo)

    #Noduplicados
    source(file.path("R","noduplicates.r"))
    dat <- noduplicates(dat, "OriginalNameDB")

    dat$EICAT_by_Mechanism <- gsub("NA - NA", "NA", dat$EICAT_by_Mechanism)

    # Guardar el resultado
    out_name <- paste0("OutputFiles/Intermediate/", tools::file_path_sans_ext(basename(database)), ".csv")
    write_csv2(dat, out_name)
    cat("Guardado:", out_name, "\n")
  }
}
