glonaf_2025_Download_and_Depurate <- function() {
  #Download
  url <- "https://zenodo.org/records/17105725/files/glonaf_TxR.csv?download=1"
  destfile <- "InputFiles/originaldatabase_glonaf_2025_TXR.csv"
  download.file(url, destfile, mode = "wb")
  url <- "https://zenodo.org/records/17105725/files/glonaf_region.csv?download=1"
  destfile <- "InputFiles/originaldatabase_glonaf_2025_R.csv"
  download.file(url, destfile, mode = "wb")

  #Depurate
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
  dataset_region <- read.csv("InputFiles/originaldatabase_glonaf_2025_R.csv")
  region_map <- dataset_region %>% select(code, name) %>% deframe()
  dataset <- result %>%
    rowwise() %>%
    mutate(
      region = {
        codes <- str_split(region, ",\\s*")[[1]]
        names <- sapply(codes, function(x) region_map[x] %||% x)
        paste(names, collapse = ", ")
      }
    ) %>%
    ungroup()
  dataset <- noduplicates(dataset, "taxa_accepted") %>% as.data.frame()
  write.xlsx(dataset, "./InputFiles/originaldatabase_glonaf_2025.xlsx")

  #Depurate
  species_list <- dataset$taxa_accepted
  accepted_names <- character(length(species_list))
  for (i in seq_along(species_list)) {
    species <- species_list[i]
    tryCatch({
      res <- name_backbone_checklist(species)
      accepted_names[i] <- res$canonicalName
    }, error = function(e) {
      message("Species not found ", species, ": ", e$message)
      accepted_names[i] <- NA
    })
    Sys.sleep(0.5)
  }
  dataset <- check_habitat(accepted_names, dataset)
  dataset <- dataset %>% filter(grepl("FRESHWATER", Habitat))

  #Save
  write.xlsx(dataset, "./InputFiles/freshwatersubset_glonaf_2025.xlsx")
}
