Weeds_Of_Australia_Database_2025_Download_and_Depurate <- function(){

  # 1. Obtener páginas válidas
  obtain_valid_urls <- function(base_url = "https://weeds.org.au/weeds-profiles/page/", quiet = FALSE) {
    n <- 1
    urls_weedsaustralia <- c()

    repeat {
      url <- paste0(base_url, n, "/")
      response <- try(GET(url), silent = TRUE)

      if (inherits(response, "try-error") || status_code(response) != 200) {
        if (!quiet) cat("Error o página no accesible:", url, "\n")
        break
      }

      html <- read_html(response)
      mensaje_no_dataset <- html %>%
        html_nodes("h2.no-filter") %>%
        html_text(trim = TRUE)

      if (any(grepl("no matching filter results", mensaje_no_dataset, ignore.case = TRUE))) {
        if (!quiet) cat("Página vacía:", url, "\n")
        break
      } else {
        if (!quiet) cat("Página válida:", url, "\n")
        urls_weedsaustralia <- c(urls_weedsaustralia, url)
        n <- n + 1
      }
    }

    return(urls_weedsaustralia)
  }

  # 2. Extraer <p>Scientific names</p> de cada página
  scientificnames <- function(urls, quiet = FALSE) {
    dataset <- data.frame(pagina = integer(), nombre = character(), stringsAsFactors = FALSE)

    for (i in seq_along(urls)) {
      url <- urls[i]
      if (!quiet) cat("Extrayendo de:", url, "\n")

      response <- try(GET(url), silent = TRUE)
      if (inherits(response, "try-error") || status_code(response) != 200) next

      html <- read_html(response)

      # Buscar todos los <p>
      nombres <- html %>%
        html_nodes("p") %>%
        html_text(trim = TRUE)

      # Filtrar nombres científicos (opcional: más fino con regex)
      nombres_filtrados <- nombres[nchar(nombres) > 3 & grepl("^[A-Z][a-z]+ [a-z]+", nombres)]

      if (length(nombres_filtrados) > 0) {
        dataset <- bind_rows(dataset,
                                data.frame(pagina = i,
                                           nombre = nombres_filtrados,
                                           stringsAsFactors = FALSE))
      }
    }

    return(dataset)
  }


  urls_weedsaustralia <- obtain_valid_urls()
  dataset_web <- scientificnames(urls_weedsaustralia)

  write.xlsx(dataset_web, "Inputfiles/Step0_OriginalDatabase_Weeds_Of_Australia_Database_2025.xlsx")

  ###########################################
  ######### DEPURAR #########################
  dataset_web$Invaded_country <- "Australia"

  #Habitat?
  source(file.path("R", "check_habitat.r"))
  dataset <- dataset_web
  nombres <- dataset$nombre
  acep_name <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(acep_name, dataset)

  #freshwater
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh <- dat_fresh[, c("Species", setdiff(names(dat_fresh), "Species"))]

  write.xlsx(dat_fresh, "Inputfiles/Step0_OriginalDatabaseFreshwwaterNODUPLICATES_Weeds_Of_Australia_Database_2025.xlsx")

}
