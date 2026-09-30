GBIF_NativeRange <- function() {

  FRIAS_masterlist <- read_excel(
    "OutputFiles/Intermediate/step9_additionalNativeRangeSInAS_masterlist.xlsx"
  )

  species <- FRIAS_masterlist$AcceptedNameGBIF
  #species <- species[1:10]
  species_datasets <- list()

  result <- map_dfr(seq_along(species), function(i) {
    species_name <- species[i]
    message("Searching species ",i,"/",length(species),": ",species_name)
    x <- tryCatch(

      suppressMessages(
        suppressWarnings(
          occ_search(
            scientificName = species_name,
            facet = "establishmentMeans",
            limit = 100
          )
        )
      ),

      error = function(e) {

        message(
          "Error with ",
          species_name,
          "Skipping that species since no information about native range was found on GBIF"
        )

        return(NULL)
      }
    )

    Sys.sleep(0.05)

    # ------------------------------------------------------------
    # If occ_search() has failed
    # ------------------------------------------------------------

    if (is.null(x)) {

      species_datasets[[species_name]] <<- NULL

      return(
        tibble(
          especie = species_name,
          facet = NA_character_,
          count = NA_integer_
        )
      )
    }

    # ------------------------------------------------------------
    # If occ_search() returns NULL in data
    # ------------------------------------------------------------

    if (is.null(x$data)) {

      message(
        "  No data for ",
        species_name,
        " → leaving NA"
      )

      species_datasets[[species_name]] <<- NULL

      return(
        tibble(
          especie = species_name,
          facet = NA_character_,
          count = NA_integer_
        )
      )
    }

    # ------------------------------------------------------------
    # Save species data
    # ------------------------------------------------------------

    species_datasets[[species_name]] <<-
      tryCatch(

        x$data %>%
          mutate(species = species_name),

        error = function(e) {

          message(
            "  ERROR processing data for ",
            species_name,
            " → leaving NA"
          )

          NULL
        }
      )

    # ------------------------------------------------------------
    # If there was an error processing x$data
    # ------------------------------------------------------------

    if (is.null(species_datasets[[species_name]])) {

      return(
        tibble(
          especie = species_name,
          facet = NA_character_,
          count = NA_integer_
        )
      )
    }

    # ------------------------------------------------------------
    # If there are no facets
    # ------------------------------------------------------------

    if (
      is.null(x$facets) ||
      length(x$facets) == 0 ||
      is.null(x$facets[[1]])
    ) {

      return(
        tibble(
          especie = species_name,
          facet = NA_character_,
          count = NA_integer_
        )
      )
    }

    # ------------------------------------------------------------
    # Normal result
    # ------------------------------------------------------------

    tibble(
      especie = species_name,
      facet = x$facets[[1]]$name,
      count = x$facets[[1]]$count
    )
  })

  native_species <- result %>%
    filter(facet == "native") %>%
    pull(especie)

  native_datasets <- species_datasets[
    names(species_datasets) %in% native_species
  ]

  native_data <- bind_rows(native_datasets)

  native_data <- native_data %>%
    select(
      species,
      continent,
      stateProvince
    )

  source(file.path("R", "noduplicates.R"))

  final_data <- noduplicates(
    native_data,
    "species"
  )

  final_data <- final_data %>%
    mutate(
      continent = paste0(
        toupper(
          substr(
            tolower(trimws(continent)),
            1,
            1
          )
        ),
        substr(
          tolower(trimws(continent)),
          2,
          nchar(trimws(continent))
        )
      ),
      stateProvince = tolower(trimws(stateProvince))
    )

  final_data <- final_data %>%
    mutate(
      NativeRegionGBIF = paste0(
        continent,
        ",",
        stateProvince
      )
    ) %>%
    select(
      -continent,
      -stateProvince
    )

  # Add GBIF native range and merge it with the existing NativeRange
  FRIAS_masterlist2 <- FRIAS_masterlist %>%
    left_join(
      final_data %>%
        select(
          species,
          NativeRegionGBIF
        ),
      by = c(
        "AcceptedNameGBIF" = "species"
      )
    ) %>%
    mutate(
      NativeRange = case_when(
        !is.na(NativeRange) & !is.na(NativeRegionGBIF) ~
          paste(NativeRange, NativeRegionGBIF, sep = ","),
        is.na(NativeRange) & !is.na(NativeRegionGBIF) ~
          NativeRegionGBIF,
        TRUE ~
          NativeRange
      )
    ) %>%
    select(
      -NativeRegionGBIF
    )

  # Save the updated masterlist
  write_xlsx(
    FRIAS_masterlist2,
    "OutputFiles/Intermediate/step10_additionalNativeRangeGBIF_masterlist.xlsx"
  )
}
