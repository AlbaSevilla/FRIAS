#GRAFICO DE SECTORES FINAL
FRIAS_SpeciesList_MasterList <- read_excel("OutputFiles/Final/FRIAS_SpeciesList_MasterList.xlsx")
FRIAS_SpeciesList_MasterList <- FRIAS_SpeciesList_MasterList %>%
  mutate(Source_Date = str_split(Source_Date, ",")) %>%
  unnest(Source_Date) %>%
  mutate(Source_Date = trimws(Source_Date))

# Vector de nombres estándar
TablesToStandarize <- read_excel("TablesToStandardize/DatabasesInformation.xlsx")
names(TablesToStandarize)
nombres_estandar <- TablesToStandarize$Column_SourceDate

# Función para encontrar el nombre más parecido
buscar_mas_parecido <- function(nombre) {
  distancias <- stringdist::stringdist(nombre, nombres_estandar, method = "jw") # método Jaro-Winkler
  nombres_estandar[which.min(distancias)]
}

# Aplicar la función a cada valor de Data_Source
FRIAS_SpeciesList_MasterList <- FRIAS_SpeciesList_MasterList %>%
  mutate(Source_Date = sapply(Source_Date, buscar_mas_parecido))
unique(FRIAS_SpeciesList_MasterList$Source_Date)
conteo <- FRIAS_SpeciesList_MasterList %>%
  dplyr::count(Source_Date) %>%
  arrange(desc(n))
conteo
View(conteo)
sum(conteo$n)
