Resolved_missing_information <- function() {
  contar_gaps_df <- function(df, version_name) {
    gaps <- sapply(df, function(col) sum(is.na(col)))
    data.frame(
      columna = names(gaps),
      gaps = as.numeric(gaps),
      version = version_name
    )
  }

  masterlist_before <- read_csv("OutputFiles/Intermediate/step4_selectedfreshwatergbif_masterlist.csv")
  masterlist_after  <- read_csv("FinalFiles/FRIAS_masterlist.csv")

  names(masterlist_before)
  names(masterlist_after)

  selected_cols <- c("AcceptedNameGBIF", "ID_GBIF", "Kingdom", "Phylum", "Class", "Order", "Family", "FunctionalGroup",
                     "Group", "NativeCountry")
  masterlist_before <- masterlist_before %>%
    select(selected_cols)
  masterlist_after  <- masterlist_after  %>%
    select(selected_cols)

  gaps_before <- contar_gaps_df(masterlist_before, "Before")
  gaps_after  <- contar_gaps_df(masterlist_after, "After")

  gaps_combined <- bind_rows(gaps_before, gaps_after) %>%
    pivot_wider(names_from = version, values_from = gaps, values_fill = 0) %>%
    arrange(desc(Before)) %>%
    mutate(columna = factor(columna, levels = columna))  # factor ordenado por Before

  max_gaps_before <- max(gaps_combined$Before, na.rm = TRUE)
  gaps_combined <- gaps_combined %>%
    mutate(Before = ifelse(Before == 0, max_gaps_before, Before))

  gaps_long <- gaps_combined %>%
    pivot_longer(cols = c("Before", "After"), names_to = "version", values_to = "gaps") %>%
    mutate(
      version = factor(version, levels = c("Before", "After")),
      columna = factor(columna, levels = gaps_combined$columna)  # <-- orden correcto
    )

  orden_before <- gaps_long %>%
    filter(version == "Before") %>%
    arrange(desc(gaps)) %>%
    select(columna)

  gaps_long <- gaps_long %>%
    mutate(columna = factor(columna, levels = orden_before$columna)) %>%
    arrange(columna, desc(version == "Before"))

  max_y <- max(gaps_long$gaps, na.rm = TRUE)

  p1 <- ggplot(gaps_long, aes(x = columna, y = gaps, fill = version)) +
    geom_bar(stat = "identity", position = "identity", alpha = 1, color = "black") +
    scale_y_continuous(limits = c(0, max_y * 1.05)) +
    labs(
      title = "Comparison of Missing Values (NA) per Variable: Before vs. After",
      x = "Variable",
      y = "Number of Missing Values",
      fill = "Dataset Version"
    ) +
    theme_minimal() +
    coord_flip() +
    theme(axis.text.y = element_text(hjust = 1))

  p1

  ggsave("Figures/Resolved_missing_information_superposed_horizontal.png", plot = p1, width = 10, height = 8, dpi = 300)

  # Definir colores azul claro y azul oscuro
  blue_colors <- c("Before" = "#AED6F1", "After" = "#2E86C1")  # Ajusta según tus niveles de version

  p2 <- ggplot(gaps_long, aes(x = columna, y = gaps, fill = version)) +
    geom_bar(stat = "identity",
             position = position_dodge2(width = 0.9, preserve = "single"),
             alpha = 0.9,   # transparencia ligera
             color = "black",
             width = 0.7) +
    geom_text(aes(label = gaps),
              position = position_dodge2(width = 0.9, preserve = "single"),
              vjust = -0.5,  # etiqueta encima de la barra
              size = 3) +
    scale_fill_manual(values = blue_colors) +  # colores azules
    scale_y_continuous(limits = c(0, max_y * 1.1)) +
    labs(
      title = "Comparison of Missing Values (NA) per Variable: Before vs. After",
      x = "Variable",
      y = "Number of Missing Values",
      fill = "MasterList Version"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(hjust = 1),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )

  p2

  ggsave("Figures/Resolved_missing_information.png", plot = p2, width = 10, height = 8, dpi = 300)
}
