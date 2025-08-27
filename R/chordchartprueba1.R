library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(circlize)
library(viridis)

# 1. Cargar datos (ajusta la ruta a tu archivo)
datos <- readxl::read_excel("C:/Users/ruben/Desktop/FRIAS_SpeciesList_MasterList.xlsx")
datos <- datos[c(1:10),]

# 2. Separar la columna 'Source_Data' en listas
datos <- datos %>%
  mutate(Source_Data = strsplit(Source_Data, ",\\s*"))  # separa por coma y espacio opcional

# 3. Crear lista de datasets únicos
all_sources <- unique(unlist(datos$Source_Data))

# 4. Función para obtener especies únicas por dataset
get_species_per_source <- function(source_name, df) {
  df %>% 
    filter(sapply(Source_Data, function(x) source_name %in% x)) %>%
    pull(AcceptedNameGBIF) %>%
    unique()
}

# 5. Crear lista de especies por dataset
species_list <- lapply(all_sources, get_species_per_source, df = datos)
names(species_list) <- all_sources

# 6. Crear matriz de intersección de especies compartidas entre datasets
n <- length(all_sources)
intersections <- matrix(0, nrow = n, ncol = n, dimnames = list(all_sources, all_sources))

for (i in seq_len(n)) {
  for (j in seq_len(n)) {
    intersections[i,j] <- length(intersect(species_list[[i]], species_list[[j]]))
  }
}

# 7. Preparar datos para chordDiagram
df <- as.data.frame(intersections)

# Evitar conflicto si existe columna "from"
if ("from" %in% colnames(df)) {
  colnames(df)[colnames(df) == "from"] <- "from_old"
}

data_long <- df %>%
  rownames_to_column(var = "from") %>%
  pivot_longer(cols = -from, names_to = "to", values_to = "value") %>%
  filter(value > 0)

# 8. Parámetros del gráfico
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

# Paleta de colores
mycolor <- viridis(length(all_sources), alpha = 1, begin = 0, end = 1, option = "D")
set.seed(123)  # para reproducibilidad
mycolor <- sample(mycolor)

# 9. Dibujar chordDiagram
chordDiagram(
  x = data_long,
  grid.col = mycolor,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"),
  diffHeight = -0.04,
  annotationTrack = "grid",
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.largest.ontop = TRUE
)

# 10. Añadir texto y ejes
circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {
    xlim <- get.cell.meta.data("xlim")
    sector.index <- get.cell.meta.data("sector.index")
    
    # Nombre de sectores
    circos.text(
      x = mean(xlim),
      y = 3.2,
      labels = sector.index,
      facing = "bending",
      cex = 0.8
    )
    
    # Graduación de eje
    circos.axis(
      h = "top",
      major.at = seq(from = 0, to = xlim[2], by = ifelse(xlim[2] > 10, 2, 1)),
      minor.ticks = 1,
      major.tick.percentage = 0.5,
      labels.niceFacing = FALSE
    )
  }
)

# 2. Separar 'Source_Data' en listas
datos <- datos %>%
  mutate(Source_Data = strsplit(Source_Data, ",\\s*"))

# 3. Contar frecuencia de aparición de cada dataset
all_sources <- unlist(datos$Source_Data)
source_freq <- table(all_sources)
top10_sources <- names(sort(source_freq, decreasing = TRUE))[1:5]

# 4. Filtrar especies solo para esos 10 datasets
datos_top10 <- datos %>%
  filter(sapply(Source_Data, function(x) any(x %in% top10_sources)))

# 5. Crear lista de especies para cada uno de los top 10 datasets
get_species_per_source <- function(source_name, df) {
  df %>% 
    filter(sapply(Source_Data, function(x) source_name %in% x)) %>%
    pull(AcceptedNameGBIF) %>%
    unique()
}

species_list <- lapply(top10_sources, get_species_per_source, df = datos_top10)
names(species_list) <- top10_sources

# 6. Matriz de intersección para los top 10
n <- length(top10_sources)
intersections <- matrix(0, nrow = n, ncol = n, dimnames = list(top10_sources, top10_sources))

for (i in seq_len(n)) {
  for (j in seq_len(n)) {
    intersections[i, j] <- length(intersect(species_list[[i]], species_list[[j]]))
  }
}

# 7. Preparar datos para chordDiagram
df <- as.data.frame(intersections)
if ("from" %in% colnames(df)) {
  colnames(df)[colnames(df) == "from"] <- "from_old"
}

data_long <- df %>%
  rownames_to_column(var = "from") %>%
  pivot_longer(cols = -from, names_to = "to", values_to = "value") %>%
  filter(value > 0)

# 8. Parámetros del gráfico
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

# Paleta de colores para los 10 datasets
mycolor <- viridis(n, alpha = 1, begin = 0, end = 1, option = "D")
set.seed(123)
mycolor <- sample(mycolor)

# 9. Crear chordDiagram
chordDiagram(
  x = data_long,
  grid.col = mycolor,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"),
  diffHeight = -0.04,
  annotationTrack = "grid",
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.largest.ontop = TRUE
)

# 10. Añadir texto y ejes
circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {
    xlim <- get.cell.meta.data("xlim")
    sector.index <- get.cell.meta.data("sector.index")
    
    circos.text(
      x = mean(xlim),
      y = 3.2,
      labels = sector.index,
      facing = "bending",
      cex = 0.8
    )
    
    circos.axis(
      h = "top",
      major.at = seq(from = 0, to = xlim[2], by = ifelse(xlim[2] > 10, 2, 1)),
      minor.ticks = 1,
      major.tick.percentage = 0.5,
      labels.niceFacing = FALSE
    )
  }
)
