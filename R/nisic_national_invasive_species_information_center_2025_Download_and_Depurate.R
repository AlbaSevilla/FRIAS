nisic_national_invasive_species_information_center_2025_Download_and_Depurate <- function(){

  html <- read_html("https://www.invasivespeciesinfo.gov/species-profiles-list")
  tabla_de_especies <- html |> html_elements(".table-responsive") |> html_table() #Extraemos los datos de la web con webscrapping
  datos_tabla <- as.data.frame(tabla_de_especies)
  nombres_comunes <- datos_tabla$Common.Name
  nombre_cientifico <- datos_tabla$Scientific.Name
  nombre_cientifico2 <- gsub(" ", "_", nombre_cientifico)
  nombre_cientifico3 <- sub("^([^_]+_[^_]+).*", "\\1", nombre_cientifico2)
  nombre_cientifico4 <- gsub("_", " ", nombre_cientifico3)
  nisic_national_invasive_species_information_center_2025_names <- nombre_cientifico4 #Nombres científicos de las especies


  #########################################################
  #######   Enlaces a cada especie, como los saco  ########
  #########################################################
  link_nodes <- html %>% html_nodes("a")
  links <- html %>%
    html_nodes("a") %>%
    html_attr("href")
  # Filtrar los enlaces que siguen el patrón "/terrestrial/plants/", "/aquatic/", etc.
  species_links <- links[grepl("^/(terrestrial|aquatic)/[a-z-]+/[a-z-]+$", links)]

  ######################################################################
  ########## EXTRAER INFORMACIÓN DE LAS ESPECIES #######################
  ######################################################################
  extract_species_info <- function(species_url) {
    html_url <- read_html(species_url)
    field_label_divs <- html_url %>% # Extraemos los elementos <div> con la clase "field--label"
      html_nodes("div.field--label")
    field_label_text <- field_label_divs %>%
      html_text()

    info1 <- field_label_text[1:6]

    field_item_divs <- html_url %>% # Extraemos los elementos <div> con la clase "field--item"
      html_nodes("div.field--item")

    field_item_text <- field_item_divs %>% # Extraemos el texto de los divs seleccionados
      html_text()

    info2 <- field_item_text[1:6]

    # Crear un data.frame, con info1 como nombres de las columnas y info2 como valores
    species_info <- data.frame(t(info2))  # Transponer info2 para que cada valor esté en su columna correspondiente
    colnames(species_info) <- info1  # Asignar los nombres de las columnas a info1

    return(species_info)
  }

  total_species <- length(species_links)
  species_data <- list()
  for (i in 1:total_species) {
    link <- species_links[i]

    species_url <- paste0("https://www.invasivespeciesinfo.gov", link)
    species_name <- gsub("^.*/(terrestrial|aquatic)/[a-z-]+/([a-z-]+)$", "\\2", link)
    cat("Cargando información de la especie ", i, " de ", total_species, ": ", species_name, "\n", sep = "")
    species_data[[i]] <- extract_species_info(species_url)
  }

  df_species <- bind_rows(species_data)
  df_species2 <- df_species[,c(1:6)]
  datos_tabla <- as.data.frame(df_species2)

  #datos_tabla
  nombre_cientifico <- datos_tabla$`Scientific Name`
  nombre_cientifico2 <- gsub("\\s+", " ", nombre_cientifico)
  nombre_cientifico3 <- gsub("[ ,.-]", "_", nombre_cientifico2)
  nombre_cientifico4 <- gsub("_+", "_", nombre_cientifico3)


  # Eliminar todo después del segundo "_"
  nombre_cientifico5 <- sub("^([^_]+_[^_]+).*", "\\1", nombre_cientifico4)
  nombre_cientifico6 <- gsub("_", " ", nombre_cientifico5)

  species <- nombre_cientifico6
  dataset_final <- cbind(species, df_species2)
  nisic_national_invasive_species_information_center_2025_allspecies <- dataset_final %>%
    select(-`Common Name`)

  # Simulamos una columna como la tuya (aquí deberías usar nisic_national_invasive_species_information_center_2025_allspecies$`Native To`)
  native_clean <- nisic_national_invasive_species_information_center_2025_allspecies$`Native To` %>%
    # Quitamos referencias (entre paréntesis), saltos de línea, y puntos
    str_replace_all("\\(.*?\\)", "") %>%
    str_replace_all("\\n", "") %>%
    str_replace_all("\\.", "") %>%
    str_trim()

  # Nos quedamos con frases que contengan posibles países/regiones
  # Eliminamos frases como "First identified in", "Origin unknown", etc.
  native_clean <- native_clean %>%
    str_replace_all("(?i)first (discovered|identified|noted).*?in ", "") %>%
    str_replace_all("(?i)originally from ", "") %>%
    str_replace_all("(?i)native to ", "") %>%
    str_replace_all("(?i)believed to have originated in ", "") %>%
    str_replace_all("(?i)origin unknown.*", "") %>%
    str_replace_all("(?i)unknown.*", "") %>%
    str_trim()

  # Ahora puedes actualizar tu dataframe
  nisic_national_invasive_species_information_center_2025_allspecies$`Native_Countries_Only` <- native_clean

  names(nisic_national_invasive_species_information_center_2025_allspecies)
  names(nisic_national_invasive_species_information_center_2025_allspecies) <- gsub(" ", "_", names(nisic_national_invasive_species_information_center_2025_allspecies))
  names(nisic_national_invasive_species_information_center_2025_allspecies) <- gsub("\\.", "", names(nisic_national_invasive_species_information_center_2025_allspecies))

  # Renombrar la columna "Native_Countries_Only" a "Native_Range"
  names(nisic_national_invasive_species_information_center_2025_allspecies)[names(nisic_national_invasive_species_information_center_2025_allspecies) == "Native_Countries_Only"] <- "Native_Range"

  #NOS QUEDAMOS CON LOS AÑOS DE INTRODUCCIÓN EN US
  # Función para eliminar el texto entre paréntesis
  remove_text_in_parentheses <- function(text) {
    return(str_replace_all(text, "\\(.*?\\)", ""))
  }

  # Función para extraer todas las fechas y separarlas por coma
  extract_all_years <- function(text) {
    # Eliminar el texto entre paréntesis primero
    text_cleaned <- remove_text_in_parentheses(text)

    # Buscar todos los números de 4 dígitos (años) en el texto limpio
    years <- str_extract_all(text_cleaned, "\\d{4}")

    # Si hay años, unirlos con coma; si no, devolver NA
    if (length(years[[1]]) > 0) {
      return(paste(years[[1]], collapse = ", "))
    } else {
      return(NA)
    }
  }

  # Aplicar la función a la columna "Date of U.S. Introduction"
  nisic_national_invasive_species_information_center_2025_allspecies$Date_of_US_Introduction <- sapply(nisic_national_invasive_species_information_center_2025_allspecies$Date_of_US_Introduction, extract_all_years)


  write.xlsx(nisic_national_invasive_species_information_center_2025_allspecies, "./Inputfiles/originaldatabase_nisic_national_invasive_species_information_center_2025.xlsx")
  write.csv2(nisic_national_invasive_species_information_center_2025_allspecies, "./Inputfiles/originaldatabase_nisic_national_invasive_species_information_center_2025.csv")

  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  #Obtain habitats
  dataset <- nisic_national_invasive_species_information_center_2025_allspecies
  especies_lista0 <- dataset$species
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)

  dataset_actualizado2 <- dataset_actualizado %>%
    select(-Species)

  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado2 %>%
    filter(grepl("FRESHWATER", Habitat))


  #######
  #OBTENER FECHA MAS ANTIGUA
  #######
  source(file.path("R", "OldestDate.r"))
  dataset_freshwater <- OldestDate(dataset_freshwater,"Date_of_US_Introduction")


  DATASET <- dataset_freshwater %>%
    mutate(
      Native_Range = str_extract_all(Native_Range, "\\b[A-Z][a-z]+\\b"),
      Native_Range = lapply(Native_Range, function(x) unique(x)),
      Native_Range = sapply(Native_Range, function(x) paste(x, collapse = ", "))
    )

  dataset <- DATASET
  dataset$Native_Range <- sapply(dataset$Native_Range, ConservarLocalizaciones)
  dataset$Native_Range


  write.xlsx(dataset, "./Inputfiles/freshwatersubset_nisic_national_invasive_species_information_center_2025.xlsx")
}


