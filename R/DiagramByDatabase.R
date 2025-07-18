MasterList <- read_excel("OutputFiles/Intermediate/Step2_MasterList.xlsx")
head(MasterList)

#CORRECIÓN NOMBRES DATASETS
MasterList <- MasterList %>%
  mutate(Source_Date = str_split(Source_Date, ";")) %>%      # Separar por ";"
  unnest(Source_Date)
head(MasterList$Source_Date)
TablesToStandarize <- read_excel("TablesToStandardize/DatabasesInformation.xlsx")
nombres_estandar <- TablesToStandarize$Column_SourceDate
#los nombres de las bbdd no salen correctamente, los cambiamos asi:
buscar_mas_parecido <- function(nombre) {
  distancias <- stringdist::stringdist(nombre, nombres_estandar, method = "jw") # método Jaro-Winkler
  nombres_estandar[which.min(distancias)]
}

# Aplicar la función a cada valor de Data_Source
MasterList <- MasterList %>%
  mutate(Source_Date = sapply(Source_Date, buscar_mas_parecido))
head(MasterList$Source_Date)


names(MasterList)
expanded <- MasterList %>%
  mutate(Source_Date = strsplit(as.character(Source_Date), ";")) %>%
  unnest(Source_Date) %>%
  mutate(Source_Date = str_trim(Source_Date)) # Quitamos espacios
unique(expanded$Source_Date)

# Calcular el conteo de cada Kingdom
df_pie <- expanded %>%
  dplyr::count(Source_Date) %>%
  dplyr::arrange(desc(n))
View(df_pie)

df_pie10 <- df_pie[c(1:5,10,28),]
df_pie10 <- df_pie10[-2,]

# Crear el gráfico de sectores con los números dentro de cada sector
ggplot(df_pie10, aes(x = "", y = n, fill = Source_Date)) +
  geom_col(width = 1, color = "white") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), color = "white", size = 8) +
  coord_polar(theta = "y") +
  theme_void()+
  labs(title = "Dataset's number alien species")
