Masterlist <- read_excel("OutputFiles/Intermediate/Step2_ObtainTaxonNAS_MasterList.xlsx")

check_habitat <- function(species, dataset) {
  dataset$Species <- species
  if (!("AcceptedNameGBIF" %in% names(dataset))) {
    dataset <- dataset %>% mutate(AcceptedNameGBIF = NA_character_)
  }
  if (!("Habitat" %in% names(dataset))) {
    dataset <- dataset %>% mutate(Habitat = NA_character_)
  }

  pb <- txtProgressBar(min = 0, max = length(species), style = 3, char = "=")

  for (i in seq_along(species)) {
    specie <- trimws(species[i])

    if (is.na(specie) || specie == "") {
      setTxtProgressBar(pb, i)
      next
    }

    message("Procesando especie ", i, "/", length(species), ": ", specie)

    tryCatch({
      tabla_resultados <- name_lookup(query = specie, rank = "species", habitat = "freshwater")$data

      if (nrow(tabla_resultados) > 0 && any(tabla_resultados$canonicalName == specie)) {
        dataset$AcceptedNameGBIF[dataset$Species == specie] <- specie
        dataset$Habitat[dataset$Species == specie] <- "Freshwater"
        message("  -> ", specie, " identificada como especie de agua dulce (Freshwater).")
      } else {
        tabla_resultados_no_freshwater <- name_lookup(query = specie, rank = "species")$data
        if (nrow(tabla_resultados_no_freshwater) > 0 && any(tabla_resultados_no_freshwater$canonicalName == specie)) {
          dataset$AcceptedNameGBIF[dataset$Species == specie] <- specie
          dataset$Habitat[dataset$Species == specie] <- "other"
          message("  -> ", specie, " no es una especie de agua dulce.")
        } else {
          message("  -> Nombre no válido para: ", specie)
        }
      }
    }, error = function(e) {
      message("  -> Error con ", specie, ": ", conditionMessage(e))
    })

    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(dataset)
}


#Obtain habitats
dataset <- Masterlist
especies_lista <- dataset$AcceptedNameGBIF
dataset_actualizado <- check_habitat(especies_lista, dataset)


##############################################################
######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
##############################################################
dataset_freshwater <- dataset_actualizado %>%
  filter(Habitat == "Freshwater")
write.xlsx(dataset_freshwater, "./OutputFiles/Intermediate/FreshwaterHabitatwithFunctionMasterList.xlsx")

dataset_nofreshwater <- dataset_actualizado %>%
  filter(Habitat == "other")
write.xlsx(dataset_nofreshwater, "./OutputFiles/Intermediate/No_FreshwaterHabitatwithFunctionMasterList.xlsx")


#######################################################################
### OBTENEMOS LOS HABITATS DE LAS ESPECIES QUE NOS HAN SALIDO OTHER ###
#######################################################################
library(readxl)
library(rgbif)

# Leer datos
HABITAT_other <- read_excel("OutputFiles/Intermediate/No_FreshwaterHabitatwithFunctionMasterList.xlsx")
nombres_aceptados <- HABITAT_other$AcceptedNameGBIF

# Crear vectores vacíos
nombres <- c()
habitats <- c()

# Bucle simple
for (i in seq_along(nombres_aceptados)) {
  cat("Procesando", i, "de", length(nombres_aceptados), "\n")

  res <- tryCatch(
    name_lookup(query = nombres_aceptados[i], rank = "species")$data,
    error = function(e) NULL
  )

  if (!is.null(res)) {
    habitat <- paste(unique(res$habitats), collapse = "; ")
  } else {
    habitat <- NA
  }

  nombres <- c(nombres, nombres_aceptados[i])
  habitats <- c(habitats, habitat)
}

# Crear dataframe final
resultado <- data.frame(Nombre = nombres, Habitat = habitats, stringsAsFactors = FALSE)

resultado$Habitat <- sapply(resultado$Habitat, function(x) {
  if (is.na(x)) return(NA)
  x <- gsub("NA[;,]?", "", x)       # Quitar "NA"
  x <- gsub("[,;]+", ";", x)        # Unificar separadores
  x <- gsub("^;|;$", "", x)          # Borrar ";" inicial/final
  x
})

resultado_limpio <- resultado %>%
  separate_rows(Habitat, sep = ";") %>%
  mutate(Habitat = trimws(Habitat)) %>%  # Quitar espacios
  filter(!is.na(Habitat) & Habitat != "") # Eliminar vacíos

unique(resultado_limpio$Habitat)

resultado2 <- resultado_limpio %>% filter(Habitat == "FRESHWATER")
View(resultado2)
tabla_resultados <- name_lookup(query = "Acipenser sturio", rank = "species", habitat = "freshwater")$data
tabla_resultados2 <- tabla_resultados %>% filter(canonicalName == "Acipenser sturio")

View(tabla_resultados2)
