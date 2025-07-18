#Vamos a comprobar que algunas especies de Pili et al salen en la MasterList:
library(readxl)
MasterList <- read_excel("Output/Intermediate/Step2_ObtainTaxonNAS_MasterList.xlsx")
names(MasterList)
Nombres_MasterList <- MasterList$AcceptedNameGBIF


Amphibios_Pili <- read.csv("D:/NextCloud/FRIASpackage_6/Inputfiles/Step0_ForecastingPotentialInvadersArmanN.Pili.csv")
names(Amphibios_Pili)
#Nos quedamos con un subset de aquellos que tienen Aqu=1 porque son los acuáticos
Amphibios_Pili2 <- Amphibios_Pili %>%
  filter(Aqu==1)
Nombres_amfibios <- Amphibios_Pili2$scientificName_harmonise

# Coincidencias exactas
nombres_en_ambas <- Nombres_amfibios[Nombres_amfibios %in% Nombres_MasterList]
nombres_solo_en_pili <- Nombres_amfibios[!(Nombres_amfibios %in% Nombres_MasterList)]
nombres_solo_en_masterlist <- Nombres_MasterList[!(Nombres_MasterList %in% Nombres_amfibios)]

# Resultados
cat("✅ Coincidencias:", length(nombres_en_ambas), "\n")
cat("❌ Solo en Amphibios_Pili2:", length(nombres_solo_en_pili), "\n")

# Si querés ver los nombres:
print("Nombres que coinciden:")
print(nombres_en_ambas)

print("Nombres que solo están en Pili:")
print(nombres_solo_en_pili)




#Vamos a comprobar que algunas especies de Pili et al salen en la MasterList:
library(readxl)
MasterList <- read_excel("Output/Intermediate/Step2_ObtainTaxonNAS_MasterList.xlsx")
names(MasterList)
Nombres_MasterList <- MasterList$AcceptedNameGBIF

library(readxl)
Reptiles_Harmonished <- read_excel("C:/Users/53917798W/Downloads/Table_S2_1_raw.xlsx",
                                   sheet = "Reptiles_harmonised")
#Quitamos los que no tenga rango nativo
Reptiles_Harmonished <- Reptiles_Harmonished[Reptiles_Harmonished$geographicRange != "NA", ]

names(Reptiles_Harmonished)
#Nos quedamos con un subset de aquellos que tienen Aqu=1 porque son los acuáticos
Nombres_amfibios <- Reptiles_Harmonished$scientificName_harmonise

# Coincidencias exactas
nombres_en_ambas <- Nombres_amfibios[Nombres_amfibios %in% Nombres_MasterList]
nombres_solo_en_pili <- Nombres_amfibios[!(Nombres_amfibios %in% Nombres_MasterList)]
nombres_solo_en_masterlist <- Nombres_MasterList[!(Nombres_MasterList %in% Nombres_amfibios)]

# Resultados
cat("✅ Coincidencias:", length(nombres_en_ambas), "\n")
cat("❌ Solo en Amphibios_Pili2:", length(nombres_solo_en_pili), "\n")

# Si querés ver los nombres:
print("Nombres que coinciden:")
print(nombres_en_ambas)

print("Nombres que solo están en amphibios_Pili2:")
print(nombres_solo_en_pili)









library(furrr)
library(rgbif)
library(dplyr)

especies_freshwater <- function(especies, dataset) {
  # Asegurarse de que las columnas necesarias existan
  if (!("AcceptedNameGBIF" %in% names(dataset))) {
    dataset <- dataset %>% mutate(AcceptedNameGBIF = NA_character_)
  }
  if (!("Habitat" %in% names(dataset))) {
    dataset <- dataset %>% mutate(Habitat = NA_character_)
  }
  if (!("Valid" %in% names(dataset))) {
    dataset <- dataset %>% mutate(Valid = NA_character_)
  }

  # Estandarizar los nombres de especies en el dataset
  dataset$scientificName_harmonise <- tolower(gsub("\\s+", " ", trimws(dataset$scientificName_harmonise)))

  for (i in seq_along(especies)) {
    especie <- tolower(gsub("\\s+", " ", trimws(especies[i])))  # Limpiar y estandarizar el nombre

    if (is.na(especie) || especie == "") {
      cat("Progreso: Especie", i, "de", length(especies), "es NA o vacía, saltando...\n")
      next
    }

    cat("Progreso: Especie", i, "de", length(especies), "analizando", especie, "\n")

    tryCatch({
      resultado <- name_lookup(query = especie, rank = "species", habitat = "freshwater")$data

      fila <- which(dataset$scientificName_harmonise == especie)

      if (length(fila) == 1) {
        if (nrow(resultado) > 0) {
          if (tolower(resultado$canonicalName[1]) == especie) {
            cat("✅", especie, "es Freshwater según GBIF\n")
            dataset$AcceptedNameGBIF[fila] <- resultado$canonicalName[1]
            dataset$Habitat[fila] <- "Freshwater"
            dataset$Valid[fila] <- "Sí"
          } else {
            cat("❌", especie, "NO coincide exactamente (nombre aceptado distinto)\n")
            dataset$AcceptedNameGBIF[fila] <- resultado$canonicalName[1]
            dataset$Habitat[fila] <- "other"
            dataset$Valid[fila] <- "No"
          }
        } else {
          # No es freshwater, buscar igual el nombre aceptado
          resultado_no_freshwater <- name_lookup(query = especie, rank = "species")$data

          if (nrow(resultado_no_freshwater) > 0) {
            cat("❌", especie, "NO es Freshwater según GBIF\n")
            dataset$AcceptedNameGBIF[fila] <- resultado_no_freshwater$canonicalName[1]
            dataset$Habitat[fila] <- "other"
            dataset$Valid[fila] <- "Sí"
          } else {
            cat("⚠️ No se encontró ningún resultado para", especie, "\n")
          }
        }
      } else {
        cat("⚠️ Fila no encontrada o múltiples para", especie, "\n")
      }
    }, error = function(e) {
      cat("❌ Error al procesar", especie, ":", conditionMessage(e), "\n")
    })
  }

  return(dataset)
}

# Ejecución
dataset <- Reptiles_Harmonished
especies_lista <- dataset$scientificName_harmonise
dataset_actualizado <- especies_freshwater(especies_lista, dataset)

View(dataset_actualizado)

dataset_actualizado %>% filter(Habitat=="Freshwater")
