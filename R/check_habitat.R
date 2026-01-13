check_habitat <- function(species, dataset) {
  dataset$Species <- species
  dataset$AcceptedNameGBIF <- NA_character_
  dataset$Habitat <- NA_character_
  for (i in seq_along(species)) {
    specie <- trimws(species[i])
    message("Processing species ", i, " de ", length(species), ": ", specie)

    if (is.na(specie) || specie == "") next

    idx <- which(dataset$Species == specie)

    try({
      res <- name_lookup(query = specie, rank = "species", habitat = "freshwater")$data
      res <- res[res$canonicalName == specie, ]

      if (nrow(res) > 0) {
        dataset$AcceptedNameGBIF[idx] <- specie
        hab <- if (length(res$habitats) > 0) paste(res$habitats, collapse = ",") else NA_character_
        if (!is.na(hab)) dataset$Habitat[idx] <- hab

      } else {
        res2 <- name_lookup(query = specie, rank = "species", habitat = "terrestrial")$data
        res2 <- res2[res2$canonicalName == specie, ]

        if (nrow(res2) > 0) {
          dataset$AcceptedNameGBIF[idx] <- specie
          hab2 <- if (length(res2$habitats) > 0) paste(res2$habitats, collapse = ",") else NA_character_
          if (!is.na(hab2)) dataset$Habitat[idx] <- hab2

        } else {
          res3 <- name_lookup(query = specie, rank = "species", habitat = "marine")$data
          res3 <- res3[res3$canonicalName == specie, ]

          if (nrow(res3) > 0) {
            dataset$AcceptedNameGBIF[idx] <- specie
            hab3 <- if (length(res3$habitats) > 0) paste(res3$habitats, collapse = ",") else NA_character_
            if (!is.na(hab3)) dataset$Habitat[idx] <- hab3
          }
        }
      }
    }, silent = TRUE)
  }
  dataset$Habitat <- sapply(dataset$Habitat, function(x) {
    if (is.na(x)) return(NA_character_)
    parts <- unlist(strsplit(x, ","))
    parts <- parts[parts != "NA"]
    if (length(parts) == 0) {
      return(NA_character_)
    } else {
      return(paste(parts, collapse = ","))
    }
  })
  dataset$Habitat <- sapply(dataset$Habitat, function(x) {
    if (is.na(x)) return(NA_character_)
    parts <- unlist(strsplit(x, ","))
    parts_unique <- unique(trimws(parts))
    paste(parts_unique, collapse = ",")
  })
  return(dataset)
}
