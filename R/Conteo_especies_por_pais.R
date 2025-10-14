library(readxl)
MasterList <- read_excel("Output/Intermediate/Step2_ObtainTaxonNAS_MasterList.xlsx")
head(MasterList)
names(MasterList)
#Nos quedamos con el nombre de las especies aceptado y la lista de los paises invadidos
columnas_a_quedarse <- c("AcceptedNameGBIF", "invaded_country_list")

MasterList_v2 <- MasterList %>%
  select(all_of(columnas_a_quedarse))

MasterList_v2$invaded_country_list <- gsub(",", ";", MasterList_v2$invaded_country_list)

MasterList_v3 <- MasterList_v2 %>%
  separate_rows(invaded_country_list, sep = ";")

MasterList_v3

# Contar el número de especies distintas por país
species_count <- MasterList_v3 %>%
  distinct(AcceptedNameGBIF, invaded_country_list) %>%
  count(invaded_country_list, name = "species_count")


species_count <- species_count %>%
  arrange(desc(species_count))

# Ordenar el dataset por la columna 'invaded_country_list' alfabéticamente
species_count <- species_count %>%
  arrange(invaded_country_list)

# Ver los primeros registros para comprobar
View(species_count)

#species_count <- species_count[1:30,]


library(ggplot2)
library(dplyr)

# Suponiendo que tienes el data frame species_count
ggplot_plot <- ggplot(species_count, aes(x = reorder(invaded_country_list, species_count), y = species_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Invierte los ejes para que los países estén en Y
  labs(title = "Número de especies invasoras por país",
       x = "País",
       y = "Número de especies invasoras") +
  theme_minimal()

ggplot_plot


library(ggplot2)

ggplot_plot <- ggplot(species_count, aes(x = reorder(invaded_country_list, species_count), y = species_count)) +
  geom_bar(stat = "identity", fill = "#96CDCD", color = "black", width = 0.7) +
  geom_text(aes(label = species_count), vjust = -0.5, color = "black", size = 3.5) +
  coord_flip() +
  labs(title = "ESPECIES INVASORAS",
       subtitle = "Número total de especies invasoras en diferentes países",
       caption = "Fuente: MasterList",
       x = "País",
       y = "Número de especies invasoras") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10, face = "italic")
  ) +
  scale_y_continuous(labels = scales::comma)
print(ggplot_plot)



#################################################
MasterList <- read_excel("Output/Intermediate/Step2_ObtainTaxonNAS_MasterList.xlsx")
head(MasterList)
names(MasterList)
#Nos quedamos con el nombre de las especies aceptado y la lista de los paises invadidos
columnas_a_quedarse <- c("AcceptedNameGBIF", "InvadedCountriesISO3_List")

MasterList_v2 <- MasterList %>%
  select(all_of(columnas_a_quedarse))

MasterList_v2$InvadedCountriesISO3_List <- trimws(MasterList_v2$InvadedCountriesISO3_List)
MasterList_v2$InvadedCountriesISO3_List <- gsub("^\\s+|\\s+$", "", MasterList_v2$InvadedCountriesISO3_List)
MasterList_v2$InvadedCountriesISO3_List <- gsub(",", ";", MasterList_v2$InvadedCountriesISO3_List)

MasterList_v3 <- MasterList_v2 %>%
  separate_rows(InvadedCountriesISO3_List, sep = ";")
MasterList_v3$InvadedCountriesISO3_List <- trimws(MasterList_v3$InvadedCountriesISO3_List)
MasterList_v3$InvadedCountriesISO3_List <- gsub("^\\s+|\\s+$", "", MasterList_v3$InvadedCountriesISO3_List)
MasterList_v3$InvadedCountriesISO3_List <- gsub(",", ";", MasterList_v3$InvadedCountriesISO3_List)

# Contar el número de especies distintas por país
species_count <- MasterList_v3 %>%
  distinct(AcceptedNameGBIF, InvadedCountriesISO3_List) %>%
  count(InvadedCountriesISO3_List, name = "species_count")


species_count <- species_count %>%
  arrange(desc(species_count))

View(species_count)
species_count <- species_count[1:30,]


library(ggplot2)
library(dplyr)

# Suponiendo que tienes el data frame species_count
ggplot_plot <- ggplot(species_count, aes(x = reorder(InvadedCountriesISO3_List, species_count), y = species_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Invierte los ejes para que los países estén en Y
  labs(title = "Número de especies invasoras por país",
       x = "País",
       y = "Número de especies invasoras") +
  theme_minimal()

ggplot_plot


library(ggplot2)

ggplot_plot <- ggplot(species_count, aes(x = reorder(InvadedCountriesISO3_List, species_count), y = species_count)) +
  geom_bar(stat = "identity", fill = "#96CDCD", color = "black", width = 0.7) +
  geom_text(aes(label = species_count), vjust = -0.5, color = "black", size = 3.5) +
  coord_flip() +
  labs(title = "ESPECIES INVASORAS",
       subtitle = "Número total de especies invasoras en diferentes países",
       caption = "Fuente: MasterList",
       x = "País",
       y = "Número de especies invasoras") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10, face = "italic")
  ) +
  scale_y_continuous(labels = scales::comma)
print(ggplot_plot)
