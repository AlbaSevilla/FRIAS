glonaf_2025_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################

  #TAXON X REGION
  url <- "https://zenodo.org/records/17105725/files/glonaf_TxR.csv?download=1"
  destfile <- "InputFiles/originaldatabase_glonaf_2025_TXR.csv"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  #REGION
  url <- "https://zenodo.org/records/17105725/files/glonaf_region.csv?download=1"
  destfile <- "InputFiles/originaldatabase_glonaf_2025_R.csv"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")


  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  # Leer dataset
  dataset_taxon <- read.csv("InputFiles/originaldatabase_glonaf_2025_TXR.csv")
  result <- tibble(
    wcvp12_accepted_plant_name_id = integer(),
    taxa_accepted = character(),
    region = character()
  )
  total <- nrow(dataset_taxon)
  for(i in 1:total){
    row <- dataset_taxon[i, ]
    regions <- names(row)[which(row == 1)]
    regions_str <- paste(regions, collapse = ", ")
    result <- bind_rows(result, tibble(
      wcvp12_accepted_plant_name_id = row$wcvp12_accepted_plant_name_id,
      taxa_accepted = row$taxa_accepted,
      region = regions_str
    ))
    cat("Processing register", i, "/", total, "\n")
  }
  head(result)


  dataset_region <- read.csv("InputFiles/originaldatabase_glonaf_2025_R.csv")
  head(dataset_region)

  # Unir los dos datasets
  region_map <- dataset_region %>%
    select(code, name) %>%
    deframe()
  dataset_mix <- result %>%
    rowwise() %>%
    mutate(
      region = {
        codigos <- str_split(region, ",\\s*")[[1]]
        nombres <- sapply(codigos, function(x) region_map[x] %||% x)
        paste(nombres, collapse = ", ")
      }
    ) %>%
    ungroup()
  head(dataset_mix)


  #No duplicados
  source(file.path("R", "noduplicates.r"))
  dataset_mix3 <- noduplicates(dataset_mix, "taxa_accepted")
  dataset_mix4 <- as.data.frame(dataset_mix3)

  write.xlsx(dataset_mix4, "./InputFiles/originaldatabase_glonaf_2025.xlsx")


  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  #Obtain habitats
  dataset <- dataset_mix4
  especies_lista0 <- dataset$taxa_accepted
  resultados <- character(length(especies_lista0))
  for (i in seq_along(especies_lista0)) {
    especie <- especies_lista0[i]
    tryCatch({
      res <- name_backbone_checklist(especie)
      resultados[i] <- res$canonicalName
    }, error = function(e) {
      message("Species not found ", especie, ": ", e$message)
      resultados[i] <- NA
    })

    Sys.sleep(0.5)
    }
  especies_lista <- resultados
  dataset_actualizado <- check_habitat(especies_lista, dataset)

  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))

  write.xlsx(dataset_freshwater, "./InputFiles/freshwatersubset_glonaf_2025.xlsx")
}
