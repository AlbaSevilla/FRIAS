NISIC_Download_and_Depurate <- function(){
  # Función para extraer y procesar los datos de una página web dada por su URL
  #PÁGINA 1
  url<- "https://www.nobanis.org/search-alien-species/?SpeciesQuery=&PageSize=1000&Page=1"
  html <- read_html(url)
  elementos <- html %>%
    html_nodes(xpath = '//*[@id="speciesSearchResult"]/div[2]/div/div') %>%
    html_text() %>%
    trimws()
  parse_species_data <- function(species_info) {
    lines <- strsplit(species_info, "\r\n")[[1]]
    lines <- lines[lines != ""]
    species_data <- c(NA, NA, NA, NA, NA, NA)
    if (length(lines) >= 1) species_data[1] <- lines[1]  # Nombre de la especie
    if (length(lines) >= 2) species_data[2] <- lines[2]  # Nombres comunes
    if (length(lines) >= 3) species_data[3] <- lines[3]  # Sinónimos
    if (length(lines) >= 4) species_data[4] <- lines[4]  # Grupo
    if (length(lines) >= 5) species_data[5] <- lines[5]  # Familia
    if (length(lines) >= 6) species_data[6] <- lines[6]  # Hábitats
    return(species_data)
  }
  species_data_list <- lapply(elementos, parse_species_data)
  species_df <- do.call(rbind, species_data_list)
  colnames(species_df) <- c("Species", "Common names", "Synonymes", "Group", "Family", "Habitats")
  species_df <- as.data.frame(species_df, stringsAsFactors = FALSE)
  species_df1 <- species_df %>%
    mutate(across(everything(), ~ gsub("^\\s*Family:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Common names:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Synonymes:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Group:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Family:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Habitats:\\s*", "", trimws(.))))
  head(species_df1)

  #PÁGINA 2
  url<- "https://www.nobanis.org/search-alien-species/?SpeciesQuery=&PageSize=1000&Page=2"
  html <- read_html(url)
  elementos <- html %>%
    html_nodes(xpath = '//*[@id="speciesSearchResult"]/div[2]/div/div') %>%
    html_text() %>%
    trimws()
  parse_species_data <- function(species_info) {
    lines <- strsplit(species_info, "\r\n")[[1]]
    lines <- lines[lines != ""]
    species_data <- c(NA, NA, NA, NA, NA, NA)
    if (length(lines) >= 1) species_data[1] <- lines[1]  # Nombre de la especie
    if (length(lines) >= 2) species_data[2] <- lines[2]  # Nombres comunes
    if (length(lines) >= 3) species_data[3] <- lines[3]  # Sinónimos
    if (length(lines) >= 4) species_data[4] <- lines[4]  # Grupo
    if (length(lines) >= 5) species_data[5] <- lines[5]  # Familia
    if (length(lines) >= 6) species_data[6] <- lines[6]  # Hábitats
    return(species_data)
  }
  species_data_list <- lapply(elementos, parse_species_data)
  species_df <- do.call(rbind, species_data_list)
  colnames(species_df) <- c("Species", "Common names", "Synonymes", "Group", "Family", "Habitats")
  species_df <- as.data.frame(species_df, stringsAsFactors = FALSE)
  species_df2 <- species_df %>%
    mutate(across(everything(), ~ gsub("^\\s*Family:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Common names:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Synonymes:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Group:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Family:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Habitats:\\s*", "", trimws(.))))
  head(species_df2)

  #PÁGINA 3
  url<- "https://www.nobanis.org/search-alien-species/?SpeciesQuery=&PageSize=1000&Page=3"
  html <- read_html(url)
  elementos <- html %>%
    html_nodes(xpath = '//*[@id="speciesSearchResult"]/div[2]/div/div') %>%
    html_text() %>%
    trimws()
  parse_species_data <- function(species_info) {
    lines <- strsplit(species_info, "\r\n")[[1]]
    lines <- lines[lines != ""]
    species_data <- c(NA, NA, NA, NA, NA, NA)
    if (length(lines) >= 1) species_data[1] <- lines[1]  # Nombre de la especie
    if (length(lines) >= 2) species_data[2] <- lines[2]  # Nombres comunes
    if (length(lines) >= 3) species_data[3] <- lines[3]  # Sinónimos
    if (length(lines) >= 4) species_data[4] <- lines[4]  # Grupo
    if (length(lines) >= 5) species_data[5] <- lines[5]  # Familia
    if (length(lines) >= 6) species_data[6] <- lines[6]  # Hábitats
    return(species_data)
  }
  species_data_list <- lapply(elementos, parse_species_data)
  species_df <- do.call(rbind, species_data_list)
  colnames(species_df) <- c("Species", "Common names", "Synonymes", "Group", "Family", "Habitats")
  species_df <- as.data.frame(species_df, stringsAsFactors = FALSE)
  species_df3 <- species_df %>%
    mutate(across(everything(), ~ gsub("^\\s*Family:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Common names:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Synonymes:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Group:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Family:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Habitats:\\s*", "", trimws(.))))
  head(species_df3)

  #PÁGINA 4
  url<- "https://www.nobanis.org/search-alien-species/?SpeciesQuery=&PageSize=1000&Page=4"
  html <- read_html(url)
  elementos <- html %>%
    html_nodes(xpath = '//*[@id="speciesSearchResult"]/div[2]/div/div') %>%
    html_text() %>%
    trimws()
  parse_species_data <- function(species_info) {
    lines <- strsplit(species_info, "\r\n")[[1]]
    lines <- lines[lines != ""]
    species_data <- c(NA, NA, NA, NA, NA, NA)
    if (length(lines) >= 1) species_data[1] <- lines[1]  # Nombre de la especie
    if (length(lines) >= 2) species_data[2] <- lines[2]  # Nombres comunes
    if (length(lines) >= 3) species_data[3] <- lines[3]  # Sinónimos
    if (length(lines) >= 4) species_data[4] <- lines[4]  # Grupo
    if (length(lines) >= 5) species_data[5] <- lines[5]  # Familia
    if (length(lines) >= 6) species_data[6] <- lines[6]  # Hábitats
    return(species_data)
  }
  species_data_list <- lapply(elementos, parse_species_data)
  species_df <- do.call(rbind, species_data_list)
  colnames(species_df) <- c("Species", "Common names", "Synonymes", "Group", "Family", "Habitats")
  species_df <- as.data.frame(species_df, stringsAsFactors = FALSE)
  species_df4 <- species_df %>%
    mutate(across(everything(), ~ gsub("^\\s*Family:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Common names:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Synonymes:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Group:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Family:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Habitats:\\s*", "", trimws(.))))
  head(species_df4)

  #PÁGINA 5
  url<- "https://www.nobanis.org/search-alien-species/?SpeciesQuery=&PageSize=1000&Page=5"
  html <- read_html(url)
  elementos <- html %>%
    html_nodes(xpath = '//*[@id="speciesSearchResult"]/div[2]/div/div') %>%
    html_text() %>%
    trimws()
  parse_species_data <- function(species_info) {
    lines <- strsplit(species_info, "\r\n")[[1]]
    lines <- lines[lines != ""]
    species_data <- c(NA, NA, NA, NA, NA, NA)
    if (length(lines) >= 1) species_data[1] <- lines[1]  # Nombre de la especie
    if (length(lines) >= 2) species_data[2] <- lines[2]  # Nombres comunes
    if (length(lines) >= 3) species_data[3] <- lines[3]  # Sinónimos
    if (length(lines) >= 4) species_data[4] <- lines[4]  # Grupo
    if (length(lines) >= 5) species_data[5] <- lines[5]  # Familia
    if (length(lines) >= 6) species_data[6] <- lines[6]  # Hábitats
    return(species_data)
  }
  species_data_list <- lapply(elementos, parse_species_data)
  species_df <- do.call(rbind, species_data_list)
  colnames(species_df) <- c("Species", "Common names", "Synonymes", "Group", "Family", "Habitats")
  species_df <- as.data.frame(species_df, stringsAsFactors = FALSE)
  species_df5 <- species_df %>%
    mutate(across(everything(), ~ gsub("^\\s*Family:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Common names:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Synonymes:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Group:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Family:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Habitats:\\s*", "", trimws(.))))
  head(species_df5)

  #PÁGINA 6
  url<- "https://www.nobanis.org/search-alien-species/?SpeciesQuery=&PageSize=1000&Page=6"
  html <- read_html(url)
  elementos <- html %>%
    html_nodes(xpath = '//*[@id="speciesSearchResult"]/div[2]/div/div') %>%
    html_text() %>%
    trimws()
  parse_species_data <- function(species_info) {
    lines <- strsplit(species_info, "\r\n")[[1]]
    lines <- lines[lines != ""]
    species_data <- c(NA, NA, NA, NA, NA, NA)
    if (length(lines) >= 1) species_data[1] <- lines[1]  # Nombre de la especie
    if (length(lines) >= 2) species_data[2] <- lines[2]  # Nombres comunes
    if (length(lines) >= 3) species_data[3] <- lines[3]  # Sinónimos
    if (length(lines) >= 4) species_data[4] <- lines[4]  # Grupo
    if (length(lines) >= 5) species_data[5] <- lines[5]  # Familia
    if (length(lines) >= 6) species_data[6] <- lines[6]  # Hábitats
    return(species_data)
  }
  species_data_list <- lapply(elementos, parse_species_data)
  species_df <- do.call(rbind, species_data_list)
  colnames(species_df) <- c("Species", "Common names", "Synonymes", "Group", "Family", "Habitats")
  species_df <- as.data.frame(species_df, stringsAsFactors = FALSE)
  species_df6 <- species_df %>%
    mutate(across(everything(), ~ gsub("^\\s*Family:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Common names:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Synonymes:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Group:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Family:\\s*", "", trimws(.)))) %>%
    mutate(across(everything(), ~ gsub("^\\s*Habitats:\\s*", "", trimws(.))))
  head(species_df6)


  #url<- "https://www.nobanis.org/search-alien-species/?SpeciesQuery=&PageSize=1000&Page=7"
  #html <- read_html(url)
  #elementos <- html %>%
  #  html_nodes(xpath = '//*[@id="speciesSearchResult"]/div[2]/div/div[1]') %>%
  #  html_text() %>%
  #  trimws()
  #elementos
  #elementos <- html %>%
  #  html_nodes(xpath = '//*[@id="speciesSearchResult"]/div[2]/div/div[1]/h3/a') %>%
  #  html_attr("href")%>%
  #  trimws()
  #elementos


  DATASET_NOBANIS_SINSUBSET <- rbind(species_df1,species_df2,species_df3,species_df4,species_df5,species_df6)
  #View(DATASET_NOBANIS_SINSUBSET)
  #names(DATASET_NOBANIS_SINSUBSET)


  ###########################################################################
  #########  OBTENEMOS LAS FRESHWATER                              ##########
  ###########################################################################
  source(file.path("R", "check_habitat.r"))


  dataset <- DATASET_NOBANIS_SINSUBSET
  especies <- dataset$Species
  NISIC_actualizado1 <- check_habitat(especies, dataset)

  dataset_freshwater <- NISIC_actualizado1 %>%
    filter(grepl("FRESHWATER", Habitat))

  NISIC_freshwater <- dataset_freshwater

  write.xlsx(NISIC_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_NISIC.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_NISIC.xlsx", "\n")
  write.csv2(NISIC_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_NISIC.csv")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_NISIC.csv", "\n")

}
