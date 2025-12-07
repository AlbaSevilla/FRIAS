Type_download <- function(){
  TypeofData_Databases <- read.xlsx("TablesToStandardize/Harmonization_Databases.xlsx",
                                    sheet="Databases") %>%
                        select(Column_SourceData, Source, How_downloaded)
  table(TypeofData_Databases$Source)
  unique(TypeofData_Databases$`Source`)
  unique(TypeofData_Databases$`How_downloaded`)
  matriz_relaciones <- table(TypeofData_Databases$`Source`,TypeofData_Databases$`How_downloaded`)
  matriz_relaciones

  data <- table(TypeofData_Databases$Source)
  lab <- paste0(round(data/sum(data) * 100, 2), "%")
  cols <- hcl.colors(length(data), "Dynamic")
  pie3D(
    data,
    col = cols,
    labels = lab,
    explode = 0.1,
    main = "Percentage of different Data Sources used",   # Título del gráfico
    labelcex = 1.3                               # Tamaño de las etiquetas
  )
  legend(
    "topright",
    legend = names(data),
    fill = cols,
    cex = 0.9,
    bty = "n",
    title = "Type"                     # Título de la leyenda
  )
  TypeofData_Databases <- TypeofData_Databases %>%
    rename(
      Type_of_Source = `Source`,
      How_downloaded = `How_downloaded`
    ) %>%
    mutate(
      Type_of_Source = factor(Type_of_Source),
      How_downloaded = factor(How_downloaded)
    )
  stackbar_examp <- ggplot(data = TypeofData_Databases) +
    geom_mosaic(aes(x = product(Type_of_Source, How_downloaded), fill = Type_of_Source),
                divider = c("vspine", "hbar")) +
    labs(x = "How_downloaded", y = "Source", title = "Stacked Bar Chart")
  counts <- table(TypeofData_Databases$Type_of_Source,
                   TypeofData_Databases$How_downloaded)
  counts <- as.data.frame(counts)
  total <- sum(counts$Freq)
  counts <- counts %>%
    mutate(total_percentage = Freq / total * 100)
  ggplot(counts, aes(x = Var1, y = Var2)) +
    geom_tile(aes(fill = total_percentage), color = "white") +
    geom_text(
      aes(label = paste0(round(total_percentage, 1), "%")),
      size = 6,
      color = "black"
    ) +
    scale_fill_distiller(palette = "Blues", direction = 1) +  # ← paleta tipo matriz de confusión
    labs(x = "Source", y = "How_downloaded", fill = "Percentage") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 13, face = "bold")
    )

  ggsave(filename = "Figures/Type_download1.png",
    width = 10,
    height = 7,
    dpi = 300
  )


  counts2 <- counts %>%
    rename(
      TypeOfSource = Var1,
      HowDownloaded = Var2
    ) %>%
    mutate(
      label = paste0(TypeOfSource, "\n", round(total_percentage, 1), "%")
    )

  pastel_colors <- c("#AEC6CF","#FFD1DC","#77DD77","#FDFD96",
                     "#FFB347","#CBAACB","#FF6961","#B0E0E6",
                     "#E0BBE4","#D5F4E6")

  n_colors <- length(unique(counts2$HowDownloaded))
  pal <- pastel_colors[1:n_colors]

  ggplot(counts2, aes(
    area = total_percentage,
    fill = HowDownloaded,
    subgroup = HowDownloaded,
    label = label
  )) +
    geom_treemap() +
    geom_treemap_subgroup_border(color = "white", size = 2) +
    geom_treemap_text(
      aes(label = label),
      place = "center",
      grow = TRUE,
      reflow = TRUE
    ) +
    scale_fill_manual(values = pal) +  # <- colores pastel manuales
    labs(
      title = "Treemap: Source vs How_downloaded",
      fill = "HowDownloaded"
    ) +
    theme_minimal()


  ggsave(
    filename = "Figures/Type_download2.png",
    width = 10,
    height = 7,
    dpi = 300
  )
}

