FishBaseSeaLife_Download_and_Depurate <- function() {
  #LA OBTENCIÓN DE LA BASE DE DATOS DE FISHBASE SEALIFE no se aplica a través de un repositorio de zenodo, por lo que obtenemos los datos desde R. Para ello:

  ##########################################################################################################
  # Obtener el listado de todos los peces disponibles en FishBase y SeaLifeBase en su versión más reciente #
  ##########################################################################################################
  fishbase_dataset <- species_names(server = c("fishbase", "sealifebase"), version = "latest")

  ##########################################################################################################
  # Convertir el tibble en un dataframe ####################################################################
  ##########################################################################################################
  dataframe_species_dataset <- as.data.frame(fishbase_dataset)
  write.xlsx(dataframe_species_dataset, file.path("InputFiles", "Step0_OriginalDatabase_FishBaseSeaLife.xlsx"))

  ##########################################################################################################
  # Obtener los nombres científicos de los peces ###########################################################
  ##########################################################################################################
  nombres_delos_peces <- dataframe_species_dataset$Species

  ##########################################################################################################
  # Obtener el Status, Salinity y EcosystemType en base al nombre científico ###############################
  ##########################################################################################################
  informacion_peces <- ecosystem(species_list = nombres_delos_peces)
  informacion_peces$Status <- factor(informacion_peces$Status)
  informacion_peces <- informacion_peces %>%
    filter(Status == "introduced")
  # endemic             error        extirpated        introduced* misidentification            native            Native   not established      questionable             stray
  # 17161               143                88              1562               655            138504               198                12              1767               244
  informacion_peces$Salinity <- factor(informacion_peces$Salinity)
  unique(informacion_peces$Salinity)
  informacion_peces <- informacion_peces %>% filter(Salinity == "freshwater")
  # brackish* freshwater*  saltwater
  # 1421      53198     103905
  informacion_peces$EcosystemType <- factor(informacion_peces$EcosystemType)
  unique(informacion_peces$EcosystemType)
  informacion_peces <- informacion_peces %>%
    filter(EcosystemType == "River (basin)" | EcosystemType == "Lake" | EcosystemType == "Zoogreographic realm")
  # Lagoon*                Lake*       River (basin)*        sea/bay/gulf        Sea/Bay/Gulf            Seamount Zoogeographic realm
  # 712                4065               30221                  26              102639                1777               19124


  #######################################################################
  #######   OBTENCIÓN DE LOS PECES de interés               #############
  #######################################################################
  subset_peces_introducidos <- informacion_peces

  # Unir la base de datos de FishBase con la información sobre los peces introducidos por 'SpecCode'
  dataset_conjunto <- fishbase_dataset %>%
    full_join(subset_peces_introducidos, by = "SpecCode") %>%
    filter(!is.na(Status))

  columnas_a_quedarse <- c("Species.x","Family","Order","Status","Salinity","EcosystemType","EcosystemName")

  data_subset_fishbase <- dataset_conjunto %>%
    select(all_of(columnas_a_quedarse))

  #NO DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  FishBase_sinduplicados <- noduplicates(data_subset_fishbase,column_name_species = "Species.x")

  write.xlsx(FishBase_sinduplicados, file.path("InputFiles", "Step0_OriginalDatabaseFreshwater_FishBaseSeaLife.xlsx"))

  FishBase_sinduplicados <- read_excel("Inputfiles/Step0_OriginalDatabaseFreshwater_FishBaseSeaLife.xlsx")
  #obtener rango nativo y año
  especies <- unique(FishBase_sinduplicados$Species.x)

  intro_df <- map_dfr(especies, function(sp) {
    tbl <- introductions(sp)
    # Comprobar que hay datos y las columnas existen
    if (nrow(tbl) > 0 && any(c("From", "RangeMin", "Year") %in% names(tbl))) {
      # Fusionar RangeMin y Year en una sola columna Year_numérica
      tbl <- tbl %>%
        mutate(
          Year_fusion = ifelse(!is.na(RangeMin), RangeMin, as.numeric(stringr::str_extract(Year, "\\d{4}")))
        ) %>%
        filter(!is.na(Year_fusion))
      if (nrow(tbl) > 0) {
        idx_min <- which.min(tbl$Year_fusion)
        rango_nativo <- tbl$From[idx_min]
        year_nativo <- tbl$Year_fusion[idx_min]
        # Si el país es "Unknown", ponerlo como NA
        if (!is.na(rango_nativo) && rango_nativo == "Unknown") rango_nativo <- NA_character_
        tibble(Species.x = sp, Rango_nativo = rango_nativo, Year_Nativo = year_nativo)
      } else {
        tibble(Species.x = sp, Rango_nativo = NA_character_, Year_Nativo = NA_real_)
      }
    } else {
      tibble(Species.x = sp, Rango_nativo = NA_character_, Year_Nativo = NA_real_)
    }
  })

  FishBase_sinduplicados <- FishBase_sinduplicados %>%
    left_join(intro_df, by = "Species.x")

  especies <- FishBase_sinduplicados$Species.x

  ecosystem_species <- map_dfr(especies, function(sp) {
    tbl <- ecosystem(sp)

    # Verificar que tbl tenga datos y columnas necesarias
    if (nrow(tbl) > 0 && all(c("EcosystemName", "Location", "Status") %in% names(tbl))) {

      # Filtrar por registros "native"
      tbl <- tbl %>% filter(grepl("native", Status, ignore.case = TRUE))

      # Si hay datos después del filtrado
      if (nrow(tbl) > 0) {
        tbl <- tbl %>%
          mutate(Native_Range = paste(EcosystemName, Location, sep = "; ")) %>%
          mutate(Native_Range = ifelse(is.na(Native_Range) | Native_Range == "NA", NA_character_, Native_Range))

        # Devolver tibble con múltiples rangos nativos por especie si existen
        return(tibble(Species.x = sp, Native_Range = tbl$Native_Range))
      }
    }

    # Si no hay datos o no se cumple alguna condición
    tibble(Species.x = sp, Native_Range = NA_character_)
  })

  #sinduplicados
  source(file.path("R", "noduplicates.r"))
  fishbase_ecosystemnoduplicates <- noduplicates(ecosystem_species, "Species.x")


  FishBase_sinduplicados2 <- FishBase_sinduplicados %>%
    left_join(fishbase_ecosystemnoduplicates, by = "Species.x")

  View(FishBase_sinduplicados2)
  FishBase_sinduplicados3 <- FishBase_sinduplicados2 %>%
    mutate(Native_Range = paste(EcosystemName, Native_Range, sep=";"))

  FishBase_sinduplicados3$Native_Range <- gsub("NA;", "", FishBase_sinduplicados3$Native_Range)
  FishBase_sinduplicados3$Native_Range <- gsub("NA,", "", FishBase_sinduplicados3$Native_Range)
  FishBase_sinduplicados3$Native_Range <- gsub(";NA", "", FishBase_sinduplicados3$Native_Range)
  FishBase_sinduplicados3$Native_Range <- gsub(",NA", "", FishBase_sinduplicados3$Native_Range)
  FishBase_sinduplicados3$Native_Range <- gsub(",", ";", FishBase_sinduplicados3$Native_Range)

  FishBase_sinduplicados4 <- FishBase_sinduplicados3 %>%
    select(-EcosystemName, -EcosystemType)


  write.xlsx(FishBase_sinduplicados4, file.path("InputFiles", "Step0_OriginalDatabaseFreshwaterNODUPLICATES_FishBaseSeaLife.xlsx"))

  cat("Archivo descargado correctamente: InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_FishBaseSeaLife.xlsx")
  #write.csv2(FishBase_sinduplicados, file.path("InputFiles", "Step0_OriginalDatabaseFreshwaterNODUPLICATES_FishBaseSeaLife.csv"))
}

