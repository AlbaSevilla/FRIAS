Global_Compendium_of_Weeds_2025_Download_and_Depurate <- function(){
  url <- "http://www.hear.org/gcw/scientificnames/allscinames.htm"
  res <- read_html(url)
  res
  # Extraer los nombres de especie
  especies <- res %>%
    html_elements("a[href*='/species/']") %>%  # busca enlaces que contienen "/species/"
    html_text()                                # extrae el texto (nombre científico)
  especies

  especies <- gsub("\r\n","",especies)
  especies2 <- as.data.frame(especies)
  especies2

  # Paso 1: Limpiar los nombres
  especies2$url_name <- tolower(gsub(" ", "_", especies2$especies))       # pasar a minúsculas y cambiar espacio por "_"

  # Paso 2: Crear URLs completas
  base_url <- "http://www.hear.org/gcw/species/"
  especies2$enlace <- paste0(base_url, especies2$url_name, "/")

  # Ver los primeros resultados
  head(especies2)

  ####################################################
  ########## obtener habitat ########################
  ####################################################
  source(file.path("R", "check_habitat.r"))
  dataset <- especies2
  nombres <- dataset$especies
  nomb_acep <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(nomb_acep, dataset)
  write.xlsx(dat_act, "Inputfiles/Step0_originalDatabase_Global_Compendium_of_Weeds_2025.xlsx")

  ############# FRESHWATER
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))


  ###############################################################################
  ########## OBTENER RANGO NATIVO ###############################################
  ###############################################################################
  # url <- "http://www.hear.org/gcw/species/abrus_precatorius/"
  # res <- read_html(url)
  # origen <- res %>%
  #   html_element(xpath = "//b[contains(., 'Origin')]/following-sibling::text()[1]") %>%
  #   html_text(trim = TRUE)
  # origen
  # rango_nativo <- as.data.frame(origen)
  # names(rango_nativo)[names(rango_nativo) == "origen"] <- "Rango_Nativo"
  # rango_nativo

  # library(readxl)
  # library(rvest)
  # library(pbapply)
  # library(parallel)
  # library(stringr)
  # library(openxlsx)

  # Leer archivo y seleccionar subset
  especies2 <- read_excel("Inputfiles/Step0_originalDatabaseFreshwaterNODUPLICATES_Global_Compendium_of_Weeds_2025.xlsx")

  # Configurar clúster paralelo
  n_cores <- detectCores() - 1  # Deja 1 núcleo libre
  cl <- makeCluster(n_cores)

  # Exportar variables al clúster
  clusterExport(cl, varlist = c("especies2"), envir = environment())
  clusterEvalQ(cl, {
    library(rvest)
    library(stringr)
  })

  # Función que extrae ambos campos de una vez
  extraer_info <- function(i) {
    nombre <- especies2$especies[i]
    url <- especies2$enlace[i]
    tryCatch({
      res <- read_html(url)

      rango_nativo <- res %>%
        html_element(xpath = "//b[contains(., 'Origin')]/following-sibling::text()[1]") %>%
        html_text(trim = TRUE)

      status <- res %>%
        html_element(xpath = "//b[contains(., 'Status(es)')]/following::font[1]/b[1]") %>%
        html_text(trim = TRUE)
      return(c(rango_nativo, status))
    }, error = function(e) {
      return(c(NA, NA))
    })
  }

  # Aplicar en paralelo
  total_especies <- nrow(especies2)
  resultados <- pbapply::pblapply(seq_len(total_especies), extraer_info, cl = cl)

  # Detener clúster
  stopCluster(cl)

  # Convertir resultados a dataframe
  resultados_df <- do.call(rbind, resultados)
  colnames(resultados_df) <- c("Rango_Nativo", "Status")

  # Añadir al dataframe original
  especies2$Rango_Nativo <- str_squish(as.character(resultados_df[, "Rango_Nativo"]))
  especies2$Status <- str_squish(as.character(resultados_df[, "Status"]))

  # Limpiar valores con "[no info]"
  especies2$Rango_Nativo[grepl("\\[no info\\]", especies2$Rango_Nativo)] <- ""
  especies2$Status[grepl("\\[no info\\]", especies2$Status)] <- ""

  AlienSpecies <- especies2 %>%
    filter(grepl("introduced|naturalised",Status, ignore.case = TRUE))

  dataset <- AlienSpecies %>%
    mutate(
      Rango_Nativo = str_extract_all(Rango_Nativo, "\\b[A-Z][a-z]+\\b"),
      Rango_Nativo = lapply(Rango_Nativo, function(x) unique(x)),
      Rango_Nativo = sapply(Rango_Nativo, function(x) paste(x, collapse = ", "))
    )
  source(file.path("R", "ConservarLocalizaciones.r"))
  dataset$Rango_Nativo <- sapply(dataset$Rango_Nativo, ConservarLocalizaciones)
  dataset$Rango_Nativo



  write.xlsx(dataset, "Inputfiles/Step0_OriginalDatabaseAlienFreshwaterNODUPLICATES_Global_Compendium_of_Weeds_2025.xlsx")

}
