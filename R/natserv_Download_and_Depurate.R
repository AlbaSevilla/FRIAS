natserv_Download_and_Depurate <- function(){
    get_species_info <- function(uid, species_name) {
      message("Obtaining info of : ", species_name, " | UID: ", uid)
      x <- tryCatch(ns_altid(uid = uid),
                    error = function(e) return(NULL))
      if (is.null(x)) {
        return(tibble(
          species        = species_name,
          uniqueId       = uid,
          food           = NA,
          country        = NA,
          state          = NA,
          terr_habitat   = NA,
          palus_habitat  = NA,
          lacu_habitat   = NA,
          river_habitat  = NA,
          est_habitat    = NA,
          marine_habitat = NA,
          sub_habitat    = NA
        ))
      }
      # -------------------------------
      # FOOD
      food <- tryCatch(
        x$animalCharacteristics$animalFoodHabits$foodHabits$foodHabitsDescEn,
        error = function(e) NA
      )
      # -------------------------------
      # COUNTRY
      countries <- tryCatch(
        x$elementNationals$nation$nameEn,
        error = function(e) NA
      )
      # -------------------------------
      # STATES
      states <- tryCatch(
        map_chr(x$elementNationals$elementSubnationals,
                ~ .x$subnation$nameEn),
        error = function(e) NA
      )
      extract_habitat <- function(dataset, field) {
        tryCatch({
          if (is.null(dataset) || length(dataset) == 0) return(NA)
          if (is.data.frame(dataset)) {
            if (field %in% names(dataset)) {
              return(paste(unique(dataset[[field]]), collapse = "; "))
            }
          }
          if (is.list(dataset)) {
            vals <- suppressWarnings(map_chr(dataset, ~ .x[[field]]))
            vals <- vals[!is.na(vals)]
            if (length(vals) == 0) return(NA)
            return(paste(unique(vals), collapse = "; "))
          }
          NA
        }, error = function(e) NA)
      }
      terr_habitat <- extract_habitat(
        x$speciesCharacteristics$speciesTerrestrialHabitats,
        "terrestrialHabitat.terrestrialHabitatDescEn"
      )
      palus_habitat <- extract_habitat(
        x$speciesCharacteristics$speciesPalustrineHabitats$palustrineHabitat,
        "palustrineHabitatDescEn"
      )
      lacu_habitat <- extract_habitat(
        x$speciesCharacteristics$speciesLacustrineHabitats$lacustrineHabitat,
        "lacustrineHabitatDescEn"
      )
      river_habitat <- extract_habitat(
        x$speciesCharacteristics$speciesRiverineHabitats$riverineHabitat,
        "riverineHabitatDescEn"
      )
      est_habitat <- extract_habitat(
        x$speciesCharacteristics$speciesEstuarineHabitats$estuarineHabitat,
        "estuarineHabitatDescEn"
      )
      marine_habitat <- extract_habitat(
        x$speciesCharacteristics$speciesMarineHabitats$marineHabitat,
        "marineHabitatDescEn"
      )
      sub_habitat <- extract_habitat(
        x$speciesCharacteristics$speciesSubterraneanHabitats$subterraneanHabitat,
        "subterraneanHabitatDescEn"
      )
      tibble(
        species        = species_name,
        uniqueId       = uid,
        food           = paste(food, collapse = "; "),
        country        = countries,
        state          = states,
        terr_habitat   = terr_habitat,
        palus_habitat  = palus_habitat,
        lacu_habitat   = lacu_habitat,
        river_habitat  = river_habitat,
        est_habitat    = est_habitat,
        marine_habitat = marine_habitat,
        sub_habitat    = sub_habitat
      )
    }
    get_all_species_info <- function(dataset, pause = 0.01) {
      total <- nrow(dataset)
      results <- lapply(seq_len(total), function(i) {
        sp  <- dataset$`Scientific Name`[i]
        uid <- dataset$uid[i]
        message("Processing species ", i, "/", total, ": ", sp)
        Sys.sleep(pause)
        get_species_info(uid, sp)
      })
      bind_rows(results)
    }
    species_vec <- read.xlsx("InputFiles/originaldatabase_NatServ_2025.xlsx")
    colnames(species_vec) <- species_vec[1,]
    species_vec <- species_vec[-1,]
    col_quedar <- c("Scientific Name", "Species Group (Broad)",
                    "Species Group (Fine)", "Distribution",
                    "View on NatureServe Explorer")
    species_vec <- species_vec[, col_quedar]
    species_vec$uid <- sub(".*(/ELEMENT_GLOBAL[^/]+).*", "\\1", species_vec$`View on NatureServe Explorer`)
    species_vec$uid <- sub("^/", "", species_vec$uid)
    final_dataset <- get_all_species_info(species_vec, pause = 0.01)
    final_dataset2 <- final_dataset %>%
      rowwise() %>%
      mutate(
        Habitat = paste(
          na.omit(c_across(c(
            terr_habitat, palus_habitat, lacu_habitat,
            river_habitat, est_habitat, marine_habitat,
            sub_habitat
          ))),
          collapse = ", "
        )
      ) %>%
      ungroup() %>%
      select(
        -terr_habitat,
        -palus_habitat,
        -lacu_habitat,
        -river_habitat,
        -est_habitat,
        -marine_habitat,
        -sub_habitat
      ) %>%
      mutate(Habitat_natserv = Habitat) %>%
      select(-Habitat)
  dataset <- final_dataset2
  nombres <- dataset$species
  dat_hab <- check_habitat(nombres, dataset)
  dat_frw <- dat_hab %>%
    filter(grepl("FRESHWATER", Habitat))

  #Save
  write_xlsx(dat_frw, "InputFiles/freshwatersubset_natserv_2025.xlsx")
}
