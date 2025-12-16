global_compendium_of_weeds_Download_and_Depurate <- function(){
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


  ####################################################
  ########## obtener habitat ########################
  ####################################################
  source(file.path("R", "check_habitat.r"))
  dataset <- especies2
  nombres <- dataset$especies
  nomb_acep <- character(length(nombres))

  for (i in seq_along(nombres)) {
    cat("Processing", i, "of", length(nombres), "→", nombres[i], "...\n")
    result <- try(name_backbone_checklist(nombres[i]), silent = TRUE)
    if (inherits(result, "try-error") || is.null(result$canonicalName)) {
      nomb_acep[i] <- NA
    } else {
      nomb_acep[i] <- result$canonicalName
      cat(result$canonicalName)
    }

    Sys.sleep(0.02)  # pausa de 20 ms
  }
  dat_act <- check_habitat(nomb_acep, dataset)
  write.xlsx(dat_act, "InputFiles/originaldatabase_global_compendium_of_weeds.xlsx")

  ############# FRESHWATER
  dat_act <- read.xlsx("InputFiles/originaldatabase_global_compendium_of_weeds.xlsx")
  dim(dat_act)
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dim(dat_fresh)

  ###############################################################################
  ########## OBTENER RANGO NATIVO ###############################################
  ###############################################################################
  especies2 <- dat_fresh
  n_cores <- detectCores() - 1  # Deja 1 núcleo libre
  cl <- makeCluster(n_cores)
  clusterExport(cl, varlist = c("especies2"), envir = environment())
  clusterEvalQ(cl, {
    library(rvest)
    library(stringr)
  })
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
  total_especies <- nrow(especies2)
  resultados <- pbapply::pblapply(seq_len(total_especies), extraer_info, cl = cl)
  stopCluster(cl)
  resultados_df <- do.call(rbind, resultados)
  colnames(resultados_df) <- c("Rango_Nativo", "Status")
  especies2$Rango_Nativo <- str_squish(as.character(resultados_df[, "Rango_Nativo"]))
  especies2$Status <- str_squish(as.character(resultados_df[, "Status"]))
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
  source(file.path("R", "saveLocations.r"))
  dataset$Rango_Nativo <- sapply(dataset$Rango_Nativo, saveLocations)
  dataset$Rango_Nativo
  write.xlsx(dataset, "InputFiles/freshwatersubset_global_compendium_of_weeds.xlsx")
}
