fishbase_2025_Download_and_Depurate <- function() {
  #Download
  fishbase_dataset <- species_names(server = c("fishbase", "sealifebase"), version = "latest")
  dataframe_species_dataset <- as.data.frame(fishbase_dataset)
  write.xlsx(dataframe_species_dataset, file.path("InputFiles", "originaldatabase_fishbase_2025.xlsx"))
  fish_names <- dataframe_species_dataset$Species
  fish_information <- ecosystem(species_list = fish_names)
  fish_information$Status <- factor(fish_information$Status)
  fish_information <- fish_information %>%
    filter(Status == "introduced")
  fish_information$Salinity <- factor(fish_information$Salinity)
  fish_information <- fish_information %>% filter(Salinity == "freshwater")
  fish_information$EcosystemType <- factor(fish_information$EcosystemType)
  fish_information <- fish_information %>%
    filter(EcosystemType == "River (basin)" | EcosystemType == "Lake" | EcosystemType == "Zoogreographic realm")
  fish_subset <- fish_information
  merged_dataset_fish <- fishbase_dataset %>%
    full_join(fish_subset, by = "SpecCode") %>%
    filter(!is.na(Status))
  selected_columns <- c("SpecCode","Species.x","Family","Order","Class","Status","Salinity","EcosystemType","EcosystemName")
  data_subset_fishbase <- merged_dataset_fish %>%
    select(all_of(selected_columns))
  dataset_noduplicates <- noduplicates(data_subset_fishbase,column_name_species = "Species.x")
  write.xlsx(dataset_noduplicates, file.path("InputFiles", "originaldatabase_fishbase_2025.xlsx"))

  #Depurate
  dataset_noduplicates <- read_excel(
    "InputFiles/originaldatabase_fishbase_2025.xlsx"
  )
  especies <- unique(dataset_noduplicates$Species.x)
  intro_df <- map_dfr(especies, function(sp) {

    tbl <- introductions(sp)

    if (
      nrow(tbl) > 0 &&
      all(c("From", "TO", "RangeMin", "Year", "Estabwild") %in% names(tbl))
    ) {

      tbl %>%
        mutate(
          # Si NO está establecido, TO = NA
          TO = if_else(
            grepl("not established", Estabwild, ignore.case = TRUE),
            NA_character_,
            TO
          ),

          # Año:
          # primero RangeMin; si no existe, extraer año de Year
          Year_fusion = ifelse(
            !is.na(RangeMin),
            RangeMin,
            as.numeric(str_extract(Year, "\\d{4}"))
          ),

          # Unknown -> NA
          From = if_else(
            From == "Unknown",
            NA_character_,
            From
          ),

          TO = if_else(
            TO == "Unknown",
            NA_character_,
            TO
          ),

          # Añadimos la especie
          Species.x = sp
        ) %>%
        select(
          Species.x,
          native_range = From,
          native_year = Year_fusion,
          RecipientRange = TO,
          Estabwild
        )

    } else {

      tibble(
        Species.x = sp,
        native_range = NA_character_,
        native_year = NA_real_,
        RecipientRange = NA_character_,
        Estabwild = NA_character_
      )
    }
  })


  # ============================================================
  # 3. Añadir información de introductions() a la base
  # ============================================================

  dataset_noduplicates <- dataset_noduplicates %>%
    left_join(
      intro_df,
      by = "Species.x"
    )

  especies <- dataset_noduplicates$Species.x

  res <- ecosystem(especies)

  NativeRange <- res %>%
    filter(Status == "native") %>%
    select(Location, SpecCode)

  NativeRange <- noduplicates(
    NativeRange,
    "SpecCode"
  )

  NativeRange <- NativeRange %>%
    mutate(
      NativeRange_ecosystem = Location
    ) %>%
    select(-Location)

  NativeRange$SpecCode <- as.character(
    NativeRange$SpecCode
  )

  InvadedRange2 <- res %>%
    filter(Status == "introduced") %>%
    select(Location, SpecCode)

  InvadedRange2 <- noduplicates(
    InvadedRange2,
    "SpecCode"
  )

  InvadedRange2 <- InvadedRange2 %>%
    mutate(
      InvadedRange2 = Location
    ) %>%
    select(-Location)

  InvadedRange2$SpecCode <- as.character(
    InvadedRange2$SpecCode
  )

  dataset_noduplicates2 <- dataset_noduplicates %>%
    left_join(
      NativeRange,
      by = "SpecCode"
    ) %>%
    left_join(
      InvadedRange2,
      by = "SpecCode"
    )

  dataset_noduplicates3 <- dataset_noduplicates2 %>%
    select(
      Species.x,
      SpecCode,
      Family,
      Order,
      Class,
      Status,
      Salinity,
      native_range,
      NativeRange_ecosystem,
      native_year,
      RecipientRange,
      EcosystemName,
      InvadedRange2
    ) %>%
    mutate(

      NativeRange = paste(
        native_range,
        NativeRange_ecosystem,
        sep = ";"
      ),

      IntroducedRange = paste(
        RecipientRange,
        EcosystemName,
        InvadedRange2,
        sep = ";"
      )
    ) %>%

    select(
      -native_range,
      -NativeRange_ecosystem,
      -RecipientRange,
      -EcosystemName,
      -InvadedRange2
    )

  dataset_noduplicates3 <- dataset_noduplicates3 %>%
    mutate(

      NativeRange = gsub(
        "(^|;)NA(?=;|$)",
        "",
        NativeRange,
        perl = TRUE
      ),

      IntroducedRange = gsub(
        "(^|;)NA(?=;|$)",
        "",
        IntroducedRange,
        perl = TRUE
      ),

      NativeRange = gsub(
        ";+",
        ";",
        NativeRange
      ),

      IntroducedRange = gsub(
        ";+",
        ";",
        IntroducedRange
      ),

      NativeRange = gsub(
        "^;|;$",
        "",
        NativeRange
      ),

      IntroducedRange = gsub(
        "^;|;$",
        "",
        IntroducedRange
      ),

      # Cambiar comas por ;
      NativeRange = gsub(
        ",",
        ";",
        NativeRange
      ),

      IntroducedRange = gsub(
        ",",
        ";",
        IntroducedRange
      )
    )

  dataset_noduplicates3 <- dataset_noduplicates3 %>%
    rowwise() %>%
    mutate(
      NativeRange = paste(
        unique(
          trimws(
            unlist(
              strsplit(
                NativeRange,
                ";"
              )
            )
          )
        ),
        collapse = ";"
      ),

      IntroducedRange = paste(
        unique(
          trimws(
            unlist(
              strsplit(
                IntroducedRange,
                ";"
              )
            )
          )
        ),
        collapse = ";"
      )
    ) %>%
    ungroup()

  dataset_noduplicates3$Name <- paste(
    dataset_noduplicates3$Species.x
  )

  dataset_noduplicates3 <- noduplicates(dataset_noduplicates3, "Species.x")

  #Save
  write.xlsx(dataset_noduplicates3, "InputFiles/freshwatersubset_fishbase_2025.xlsx")
}

