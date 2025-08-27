FreshwaterAlienSpeciesCoverageComparative <- function(){
    MasterList1 <- read_excel("FinalFiles/FRIAS_SpeciesList_MasterList.xlsx")
    MasterList2 <- MasterList1 %>%
      mutate(Source_Data = str_split(Source_Data, ",")) %>%
      unnest(Source_Data) %>%
      mutate(
        Source_Data = tolower(str_trim(Source_Data)))
    MasterList2 <- MasterList2 %>%
      mutate(Source_Data = str_split(Source_Data, ";")) %>%
      unnest(Source_Data) %>%
      mutate(
        Source_Data = tolower(str_trim(Source_Data)))

    unique(MasterList2$Source_Data)
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

    View(source_counts)

    top10_sources <- source_counts %>%
      arrange(desc(count)) %>%
      slice_head(n = 10)

    ggplot(top10_sources, aes(x = reorder(Source_Data, -count), y = count, fill = Source_Data)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_manual(values = c("#8B4513", "#A0522D", "#D2B48C", "#CD853F", "#DEB887",
                                   "#F5DEB3", "#BC8F8F", "#C19A6B", "#EED5B7", "#A67B5B")) +
      geom_text(aes(label = count), vjust = -0.5, color = "black", size = 3.5) +
      labs(
        title = "Freshwater Alien Species Coverage: FRIAS Workflow Compared with Other Databases",
        x = "Databases",
        y = "Number of Freshwater Alien Species"
      ) +
      theme_classic(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(color = "#4B3621"),
        axis.title = element_text(face = "bold", color = "#4B3621")
      )




    # Crear el gráfico y guardarlo en una variable
    p <- ggplot(top10_sources, aes(x = reorder(Source_Data, -count), y = count, fill = Source_Data)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_manual(values = c("#8B4513", "#A0522D", "#D2B48C", "#CD853F", "#DEB887",
                                   "#F5DEB3", "#BC8F8F", "#C19A6B", "#EED5B7", "#A67B5B")) +
      geom_text(aes(label = count), vjust = -0.5, color = "black", size = 3.5) +
      labs(
        title = "Freshwater Alien Species Coverage: FRIAS Workflow Compared with Other Databases",
        x = "Databases",
        y = "Number of Freshwater Alien Species"
      ) +
      theme_classic(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(color = "#4B3621"),
        axis.title = element_text(face = "bold", color = "#4B3621")
      )

    # Guardar el gráfico en la carpeta "Figures"
    ggsave(filename = "Figures/FreshwaterAlienSpeciesCoverageComparative.png",
           plot = p,
           width = 10, height = 6, dpi = 300)
}
