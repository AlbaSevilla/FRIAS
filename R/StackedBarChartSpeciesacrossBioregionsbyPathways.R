StackedBarChartSpeciesacrossBioregionsbyPathways <- function(){
  masterlist <- read_excel("FinalFiles/FRIAS_SpeciesList_Masterlist.xlsx")

  names(masterlist)
  # Copia de las columnas relevantes
  df <- masterlist[, c("OriginalNameDB", "InvadedBioregions", "Pathways")]

  # Eliminar filas con NA en cualquiera de las dos variables
  df <- df[!is.na(df$InvadedBioregions) & !is.na(df$Pathways), ]

  # Separar InvadedBioregions por coma
  df$InvadedBioregions <- strsplit(as.character(df$InvadedBioregions), ",\\s*")

  # Separar Pathways por ; o coma
  df$Pathways <- strsplit(as.character(df$Pathways), ";|,")

  # Expandir filas para todas las combinaciones
  expanded <- do.call(rbind, lapply(seq_len(nrow(df)), function(i) {
    expand.grid(
      OriginalNameDB = df$OriginalNameDB[i],
      InvadedBioregions = df$InvadedBioregions[[i]],
      Pathways = df$Pathways[[i]],
      stringsAsFactors = FALSE
    )
  }))

  # Quitar espacios extra
  expanded$InvadedBioregions <- trimws(expanded$InvadedBioregions)
  expanded$Pathways <- trimws(expanded$Pathways)

  # Contar número de especies distintas por combinación
  res <- aggregate(
    OriginalNameDB ~ InvadedBioregions + Pathways,
    data = expanded,
    FUN = function(x) length(unique(x))
  )

  # Renombrar columna de conteo
  names(res)[3] <- "n_species"

  masterlist_long <- res
  # Extraemos colores de la paleta NEJM
  colors <- paletteer_d("ggsci::default_nejm")

  ggplot(masterlist_long, aes(x = n_species, y = reorder(InvadedBioregions, n_species), fill = Pathways)) +
    geom_col() +
    scale_fill_manual(values = colors) +
    labs(
      title = "Number of Species by introduction Pathways across invaded bioregions",
      x = "Number of Species",
      y = "Invaded Bioregion",
      fill = "Pathways"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")


  p <-   ggplot(masterlist_long, aes(x = n_species, y = reorder(InvadedBioregions, n_species), fill = Pathways)) +
    geom_col() +
    scale_fill_manual(values = colors) +
    labs(
      title = "Number of Species by introduction Pathways across invaded bioregions",
      x = "Number of Species",
      y = "Invaded Bioregion",
      fill = "Pathways"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Crear la carpeta si no existe
  if (!dir.exists("Figures")) dir.create("graficos")

  # Guardar el gráfico
  ggsave(
    filename = "Figures/StackedBarChart.png",  # ruta y nombre del archivo
    plot = p,                                       # el gráfico que definiste
    width = 10, height = 6, dpi = 300               # tamaño y resolución
  )


}
