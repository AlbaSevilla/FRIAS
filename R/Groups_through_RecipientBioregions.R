Groups_through_RecipientBioregions <- function(){
  MasterList <- read_csv("FinalFiles/(Table S3) FRIAS_masterlist.csv") %>%
    dplyr::select(
      AcceptedNameGBIF,
      ID_GBIF,
      Group,
      Kingdom,
      NativeRangeISO3,
      RecipientRangeISO3
    ) %>%
    mutate(
      Group = case_when(
        Group %in% c(
          "Fish",
          "Amphibian",
          "Bird",
          "Mammal",
          "Reptile"
        ) ~ "Vertebrates",

        Group %in% c(
          "Other invertebrate",
          "Crustacean",
          "Insect",
          "Mollusk",
          "Arachnid"
        ) ~ "Invertebrates",

        Group %in% c(
          "Vascular plant",
          "Bryophyte"
        ) ~ "Plants",

        Group %in% c(
          "Bacteria or protozoan",
          "Fungus",
          "Sar",
          "Alga"
        ) ~ "Other",

        TRUE ~ "Other"
      )
    )


  # ============================================================
  # Rango nativo
  # ============================================================

  dataset_native <- MasterList %>%
    mutate(
      Range = strsplit(NativeRangeISO3, ",|;")
    ) %>%
    unnest(Range) %>%
    mutate(
      Range = trimws(Range)
    ) %>%
    drop_na(Range)


  # ============================================================
  # Rango introducido
  # ============================================================

  dataset_Recipient <- MasterList %>%
    mutate(
      Range = strsplit(RecipientRangeISO3, ",|;")
    ) %>%
    unnest(Range) %>%
    mutate(
      Range = trimws(Range)
    ) %>%
    drop_na(Range)


  # ============================================================
  # Especies introducidas por GRUPO y CONTINENTE
  # ============================================================

  RecipientBioregions_group_counts <- dataset_Recipient %>%
    mutate(
      Continent = countrycode(
        Range,
        origin = "iso3c",
        destination = "continent"
      )
    ) %>%
    group_by(Group, Continent) %>%
    summarise(
      RecipientSpeciesCount = n_distinct(AcceptedNameGBIF),
      .groups = "drop"
    ) %>%
    arrange(
      Group,
      desc(RecipientSpeciesCount)
    )


  groups_colors <- c(
    "Vertebrates"   = "#B02A2A",
    "Plants"        = "#5A7330",
    "Other"         = "#E57200",
    "Invertebrates" = "#7873B5"
  )

  # Ordenar continentes por total de especies
  continent_order <- RecipientBioregions_group_counts %>%
    group_by(Continent) %>%
    summarise(
      TotalSpecies = sum(RecipientSpeciesCount, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(TotalSpecies)) %>%
    pull(Continent)

  # Aplicar el orden
  plot_data <- RecipientBioregions_group_counts %>%
    mutate(
      Continent = factor(
        Continent,
        levels = continent_order
      )
    )

  # Totales para las etiquetas
  totals <- plot_data %>%
    group_by(Continent) %>%
    summarise(
      TotalSpecies = sum(RecipientSpeciesCount, na.rm = TRUE),
      .groups = "drop"
    )

  # Gráfico horizontal
  p <- ggplot(
    plot_data,
    aes(
      x = Continent,
      y = RecipientSpeciesCount,
      fill = factor(Group)
    )
  ) +
    geom_col(
      color = "black",
      linewidth = 0.4
    ) +

    # Etiquetas con el total
    geom_text(
      data = totals,
      aes(
        x = Continent,
        y = TotalSpecies,
        label = TotalSpecies
      ),
      inherit.aes = FALSE,
      hjust = -0.2,
      size = 9,
      fontface = "bold"
    ) +

    scale_fill_manual(
      values = groups_colors,
      breaks = c(
        "Vertebrates",
        "Plants",
        "Other",
        "Invertebrates"
      ),
      name = "Group Category"
    ) +

    scale_y_continuous(
      expand = expansion(mult = c(0, 0.12))
    ) +

    coord_flip() +

    labs(
      x = "",
      y = "Number of unique species"
    ) +

    theme_minimal(base_size = 28) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      axis.line = element_line(
        color = "black",
        linewidth = 1
      ),

      axis.ticks = element_line(
        color = "black",
        linewidth = 1
      ),

      # Título del eje
      axis.title.x = element_text(
        size = 32,
        face = "bold"
      ),

      # Etiquetas de los continentes
      axis.text.y = element_text(
        size = 30,
        face = "bold"
      ),

      # Números del eje
      axis.text.x = element_text(
        size = 28,
        face = "bold"
      ),

      # Leyenda
      legend.position = "bottom",
      legend.direction = "horizontal",

      legend.title = element_text(
        size = 28,
        face = "bold"
      ),

      legend.text = element_text(
        size = 26
      ),

      legend.key.size = unit(
        1.4,
        "cm"
      )
    )

  p
  ggsave(
    filename = "Figures/Figure4a.png",
    plot = p,
    width = 12,
    height = 8,
    dpi = 300
  )

  ggsave(
    filename = "Figures/Figure4a.svg",
    plot = p,
    width = 12,
    height = 8,
    dpi = 300
  )

  ggsave(
    filename = "Figures/Figure4a.tiff",
    plot = p,
    width = 12,
    height = 8,
    dpi = 300
  )
}
