library(dplyr)
library(treemapify)
library(ggplot2)

get_selected_folder_sizes <- function(base_folder) {
  folders <- c("InputFiles", "Metadata", "R", "OutputFiles",
               "TablesToStandardize", "FinalFiles", "Figures")

  df <- data.frame(folder = character(), size = numeric(), stringsAsFactors = FALSE)

  for (f in folders) {
    folder_path <- file.path(base_folder, f)

    if (dir.exists(folder_path)) {
      if (f == "OutputFiles") {
        subfolders <- c("Check", "Intermediate")
        for (sf in subfolders) {
          subfolder_path <- file.path(folder_path, sf)
          if (dir.exists(subfolder_path)) {
            files <- list.files(subfolder_path, recursive = TRUE, full.names = TRUE)
            files <- files[file.info(files)$isdir == FALSE]
            size <- sum(file.info(files)$size)
            df <- rbind(df, data.frame(folder = paste(f, sf, sep = "/"), size = size))
          }
        }
      } else {
        files <- list.files(folder_path, recursive = TRUE, full.names = TRUE)
        files <- files[file.info(files)$isdir == FALSE]
        size <- sum(file.info(files)$size)
        df <- rbind(df, data.frame(folder = f, size = size))
      }
    }
  }

  return(df)
}

# Carpeta base
base_folder <- "/Users/ruben/Desktop/Instituto Pirenaico de Ecología/FRIAS_ casa/FRIASpackage_150"
df_sizes <- get_selected_folder_sizes(base_folder)

# Transformación para que carpetas pequeñas se vean
# Usamos log(1 + size) para mantener proporción pero hacer visibles las pequeñas
df_sizes <- df_sizes %>%
  mutate(size_vis = log1p(size))

# Treemap
p <- ggplot(df_sizes, aes(area = size_vis, fill = folder, label = folder)) +
  geom_treemap() +
  geom_treemap_text(
    colour = "Black",
    place = "centre",
    size = 24   # Ajusta este valor según necesites
  ) +
  theme(legend.position = "none")  # Quita la leyenda

p

ggsave(
  filename = "Figures/SizeFoldersGraphs.svg",
  plot = p,
  width = 12,
  height = 8,
  dpi = 300
)
ggsave(
  filename = "Figures/SizeFoldersGraphs.png",
  plot = p,
  width = 12,
  height = 8,
  dpi = 300
)
