Species_coverage <- function(){
  MasterList1 <- read_csv("FinalFiles/FRIAS_masterlist.csv")

  MasterList2 <- MasterList1 %>%
      mutate(Source_Data = str_split(Source_Data, ",")) %>%
      unnest(Source_Data) %>%
      mutate(
        Source_Data = tolower(str_trim(Source_Data))) %>%
      mutate(Source_Data = str_split(Source_Data, ";")) %>%
      unnest(Source_Data) %>%
      mutate(
        Source_Data = tolower(str_trim(Source_Data)))

    source_counts <- MasterList2 %>%
      dplyr::group_by(Source_Data) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
      dplyr::bind_rows(
        tibble(
          Source_Data = "frias_2025",
          count = nrow(MasterList1)
        )
      ) %>%
      dplyr::arrange(desc(count))

    top10_sources <- source_counts %>%
      arrange(desc(count)) %>%
      slice_head(n = 10)

    emrld_colors <- as.character(paletteer::paletteer_c("grDevices::Emrld", 30))
    emrld_colors <- emrld_colors[10:30]
    top10_sources$Source_Data <- factor(top10_sources$Source_Data,
                                        levels = unique(top10_sources$Source_Data))

    n_colors <- length(unique(top10_sources$Source_Data))
    pal <- emrld_colors[1:n_colors]

    p <- ggplot(top10_sources, aes(x = reorder(Source_Data, -count), y = count, fill = Source_Data)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = pal) +  # Usamos los colores de Emrld manualmente
      geom_text(aes(label = count),
                vjust = -0.5, color = "black", size = 3.5) +
      labs(
        title = "Freshwater Introduced Species coverage: FRIAS Workflow compared with other databases",
        x = "Databases",
        y = "Number of Freshwater Introduced Species"
      ) +
      theme_classic(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(color = "#4B3621"),
        axis.title = element_text(face = "bold", color = "#4B3621")
      )

    p

    # Guardar el gráfico en la carpeta "Figures"
    ggsave(filename = "Figures/Species_coverage.png",
           plot = p,
           width = 10, height = 6, dpi = 300)
}
