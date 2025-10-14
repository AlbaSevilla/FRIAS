TreeMapSourcesvsHowDownloaded <- function(){
  TypeofData_Databases <- read_excel("Metadata/TypeofData_Databases.xlsx")
  TypeofData_Databases
  colnames(TypeofData_Databases) <- TypeofData_Databases[4,]
  TypeofData_Databases <- TypeofData_Databases[-c(1:4),]
  table(TypeofData_Databases$Source)
  unique(TypeofData_Databases$`Source`)
  unique(TypeofData_Databases$`How downloaded`)
  matriz_relaciones <- table(TypeofData_Databases$`Source`,TypeofData_Databases$`How downloaded`)
  matriz_relaciones


  #Diagrama de sectores de las bases de datos consultadas
  library(plotrix)
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


  # Renombrar columnas
  TypeofData_Databases <- TypeofData_Databases %>%
    rename(
      Type_of_Source = `Source`,
      How_downloaded = `How downloaded`
    ) %>%
    mutate(
      Type_of_Source = factor(Type_of_Source),
      How_downloaded = factor(How_downloaded)
    )

  # Stacked Bar Chart
  stackbar_examp <- ggplot(data = TypeofData_Databases) +
    geom_mosaic(aes(x = product(Type_of_Source, How_downloaded), fill = Type_of_Source),
                divider = c("vspine", "hbar")) +
    labs(x = "How downloaded", y = "Source", title = "Stacked Bar Chart")

  stackbar_examp

  # Crear tabla de conteos
  conteos <- table(TypeofData_Databases$Type_of_Source,
                   TypeofData_Databases$How_downloaded)
  conteos <- as.data.frame(conteos)

  # Calcular porcentaje respecto al total
  total <- sum(conteos$Freq)
  conteos <- conteos %>%
    mutate(porcentaje_total = Freq / total * 100)

  ggplot(conteos, aes(x = Var1, y = Var2)) +
    geom_tile(aes(fill = porcentaje_total), color = "white") +
    geom_text(
      aes(label = paste0(round(porcentaje_total, 1), "%")),
      size = 6,        # <--- prueba con 6, 7, 8 o más
      color = "black"  # asegura contraste con el fondo
    ) +
    scale_fill_gradient(low = "yellow", high = "darkred") +
    labs(x = "Source", y = "How downloaded", fill = "Percentage") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1), # etiquetas de ejes más grandes
      axis.text.y = element_text(size = 12),                        # idem para Y
      axis.title = element_text(size = 14, face = "bold"),          # títulos más grandes
      legend.text = element_text(size = 14),                        # texto de la leyenda
      legend.title = element_text(size = 13, face = "bold")         # título de la leyenda
    )

  # Renombrar columnas
  conteos2 <- conteos %>%
    rename(
      TypeOfSource = Var1,
      HowDownloaded = Var2
    ) %>%
    mutate(
      label = paste0(TypeOfSource, "\n", round(porcentaje_total, 1), "%")
    )

  ggplot(conteos2, aes(
    area = porcentaje_total,
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
    scale_fill_manual(values = paletteer_d("tvthemes::Regular", n = length(unique(conteos2$HowDownloaded)))) +
    labs(
      title = "Treemap: Source vs How downloaded",
      fill = "HowDownloaded"
    ) +
    theme_minimal()

  # Guardar el gráfico en un objeto
  treemap_plot <- ggplot(conteos2, aes(
    area = porcentaje_total,
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
    scale_fill_manual(values = paletteer_d("tvthemes::Regular", n = length(unique(conteos2$HowDownloaded)))) +
    labs(
      title = "Treemap: Source vs How downloaded",
      fill = "HowDownloaded"
    ) +
    theme_minimal()


  # Guardar el gráfico en PNG
  ggsave(
    filename = "Figures/treemap_source_howdownloaded.png",
    plot = treemap_plot,   # usamos el objeto
    width = 10,
    height = 7,
    dpi = 300
  )
}

