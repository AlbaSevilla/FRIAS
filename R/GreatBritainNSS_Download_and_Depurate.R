GreatBritainNSS_Download_and_Depurate <- function(){
  #DESCARGAR
  html_texto <- readLines("https://www.nonnativespecies.org/non-native-species/id-sheets")

  #Guardamos el html
  writeLines(html_texto, "Inputfiles/html_GreatBritainNSS_Download_and_Depurate.html")

  #Lo parseamos
  #significa leer y estructurar el contenido de una página web (escrita en HTML) en una forma que R o cualquier lenguaje de programación pueda entender y manipular fácilmente.
  html_parseado <- read_html(paste(html_texto, collapse = "\n"))

  # Extraer especies
  especies <- html_parseado %>%
    html_elements("td > em") %>%
    html_text() %>%
    trimws()

  # Extraer enlaces PDF
  enlaces_relativos <- html_parseado %>%
    html_elements("a") %>%
    html_attr("href") %>%
    na.omit() %>%
    .[grepl("^/assets/Uploads/.*\\.pdf$", .)]  # Solo enlaces que empiezan con /assets/Uploads/ y terminan en .pdf

  #Urls especies
  base_url <- "https://www.nonnativespecies.org"
  enlaces_completos <- paste0(base_url, enlaces_relativos)


  resultados <- data.frame(
    Species = character(),
    Native_to = character(),
    Habitat = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(enlaces_completos)) {
    especie <- especies[i]
    url <- enlaces_completos[i]
    numero <- i
    total <- length(enlaces_completos)
    cat("Procesando",numero,"/",total," : ", especie, "\n")

    texto <- tryCatch(pdf_text(url), error = function(e) return(NA))
    if (all(is.na(texto))) next  # <- CORREGIDO

    lineas <- unlist(strsplit(texto, "\n"))

    linea_native <- grep("Native to:", lineas, value = TRUE)
    native_to <- if (length(linea_native)) sub(".*Native to:\\s*", "", linea_native[1]) else NA

    linea_habitat <- grep("Habitat:", lineas, value = TRUE)
    habitat <- if (length(linea_habitat)) sub(".*Habitat:\\s*", "", linea_habitat[1]) else NA

    resultados <- rbind(resultados, data.frame(
      Species = especie,
      Native_to = native_to,
      Habitat = habitat,
      stringsAsFactors = FALSE
    ))
  }

  resultados$Invaded_country <- "Great Britain"
  resultados$Habitat_Database <- resultados$Habitat
  #Guardamos
  write.xlsx(resultados, "Inputfiles/Step0_OriginalDatabase_GreatBritainNSS.xlsx")


  #######################################
  ############# HABITAT #################
  #######################################
  source(file.path("R", "check_habitat.r"))
  dataset <- resultados
  nombres <- dataset$Species
  nom_acep <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(nom_acep, dataset)
  #Freshwater?
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))

  #Guardamos
  write.xlsx(dat_fresh, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_GreatBritainNSS.xlsx")


}
