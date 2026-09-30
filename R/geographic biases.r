library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)

counts_db_countries <- read_excel("/home/albasevilla/datos/Nextcloud2/AlbaSevilla2025/FRIASpackage_188/METHODS IN ECOLOGY AND EVOLUTION/counts_db_countries.xlsx") %>%
  separate_rows(`Continental Scope`, sep=";|,") %>%
  mutate(`Continental Scope` = trimws(`Continental Scope`))

counts_db_countries <- counts_db_countries %>%
  select(FRIAS_name, `Continental Scope`)

counts_db_countries <- counts_db_countries %>%
  group_by(`Continental Scope`) %>%
  summarise(n_databases = n(), .groups = "drop") %>%
  arrange(desc(n_databases))


counts_db_countries

library(ggplot2)
library(dplyr)

# Usando tus nombres exactos de columna
counts_db_countries <- data.frame(
  `Continental Scope` = c("Global", "Europe", "America", "Asia", "Africa", "Oceania"),
  n_databases = c(28, 26, 15, 13, 8, 4),
  check.names = FALSE # Para mantener el espacio en el nombre
)

counts_db_countries$percent_databases <- counts_db_countries$n_databases/sum(counts_db_countries$n_databases)
counts_db_countries$percent_databases <- counts_db_countries$percent_databases * 100
counts_db_countries$percent_databases <- round(counts_db_countries$percent_databases, 3)

ggplot(counts_db_countries, aes(x = reorder(`Continental Scope`, n_databases), y = n_databases)) +
  geom_segment(aes(xend = `Continental Scope`, yend = 0), color = "#666666", size = 1) +
  geom_point(size = 8, color = "#a63d40") + # Rojo característico de FRIAS
  geom_text(aes(label = n_databases), color = "white", size = 3, fontface = "bold") +
  coord_flip() +
  labs(title = "Sources by Continental Scope",
       subtitle = "High concentration of sources in the Global North",
       x = NULL, y = "Number of Databases") +
  theme_minimal()


# Gráfico circular
ggplot(counts_db_countries, aes(x = reorder(`Continental Scope`, n_databases), y = n_databases, fill = `Continental Scope`)) +
  geom_bar(stat = "identity", alpha = 0.8, show.legend = FALSE) +
  ylim(-10, 35) + # Espacio para el centro del círculo
  coord_polar(start = 0) +
  geom_text(aes(label = n_databases), position = position_stack(vjust = 0.5)) +
  scale_fill_viridis_d(option = "mako") +
  theme_void() +
  labs(title = "Data Sources Scope")

p <- ggplot(counts_db_countries, aes(x = reorder(`Continental Scope`, n_databases), y = n_databases, fill = `Continental Scope`)) +
  geom_bar(stat = "identity", alpha = 0.8, show.legend = FALSE) +
  # Aumentamos el límite superior de y (radius) para que las etiquetas no se corten
  ylim(-15, 45) +
  coord_polar(start = 0) +
  # 1. El número de bases de datos (dentro de la barra)
  geom_text(aes(label = n_databases),
            position = position_stack(vjust = 0.5),
            color = "black", fontface = "bold") +
  # 2. El nombre del continente (justo encima/fuera de la barra)
  geom_text(aes(y = n_databases + 6, label = `Continental Scope`),
            size = 4, fontface = "bold") +
  scale_fill_viridis_d(option = "mako") +
  theme_void() +
  labs(title = "Data Sources Scope")


p


ggsave(
  filename = "Figure5a.png",
  plot = p,
  path = "/home/albasevilla/datos/Nextcloud2/AlbaSevilla2025/FRIASpackage_188/METHODS IN ECOLOGY AND EVOLUTION",
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

ggsave(
  filename = "Figure5a.svg",
  plot = p,
  path = "/home/albasevilla/datos/Nextcloud2/AlbaSevilla2025/FRIASpackage_188/METHODS IN ECOLOGY AND EVOLUTION",
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

ggsave(
  filename = "Figure5a.tiff",
  plot = p,
  path = "/home/albasevilla/datos/Nextcloud2/AlbaSevilla2025/FRIASpackage_188/METHODS IN ECOLOGY AND EVOLUTION",
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)
