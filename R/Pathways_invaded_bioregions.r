Pathways_invaded_bioregions <- function(){
  ruta <- "FinalFiles/FRIAS_masterlist.csv"

  MasterList <- read_csv(ruta) %>%
    select(AcceptedNameGBIF, ID_GBIF, Kingdom, Pathways, InvadedCountryISO3, InvadedBioregions)

  MasterList_expanded <- MasterList %>%
    mutate(Pathways = strsplit(Pathways, ",")) %>%
    unnest(Pathways) %>%
    mutate(Pathways = trimws(Pathways)) %>%
    mutate(InvadedBioregions = strsplit(InvadedBioregions, ",")) %>%
    unnest(InvadedBioregions) %>%
    mutate(InvadedBioregions = trimws(InvadedBioregions))

  counts <- MasterList_expanded %>%
    group_by(InvadedBioregions, Pathways) %>%
    dplyr::summarise(SpeciesCount = n_distinct(AcceptedNameGBIF), .groups = "drop")

  counts_pivot <- counts %>%
    tidyr::pivot_wider(
      names_from = Pathways,
      values_from = SpeciesCount,
      values_fill = 0
    )

  counts_pivot <- counts_pivot %>%
    mutate(Total = rowSums(across(where(is.numeric)))) %>%
    arrange(Total)

  counts_long <- counts_pivot %>%
    select(-Total) %>%
    pivot_longer(-InvadedBioregions, names_to = "Pathways", values_to = "SpeciesCount")

  bioregion_order <- counts_pivot$InvadedBioregions
  counts_long <- counts_long %>%
    mutate(InvadedBioregions = factor(InvadedBioregions, levels = bioregion_order))

  n_colors <- length(unique(counts_long$Pathways))
  colors <- colorRampPalette(brewer.pal(8, "Set2"))(n_colors)

  p <- ggplot(counts_long, aes(x = SpeciesCount, y = InvadedBioregions, fill = Pathways)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = colors) +
    labs(
      x = "Number of Introduced Species",
      y = "Invaded Bioregions",
      title = "Number of Introduced Species by Introduction Pathways across Invaded Bioregions",
      fill = "Pathways"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 14),
      plot.title = element_text(size = 20, face = "bold"),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 18)
    )

  print(p)

  # ==========================================
  # 8️⃣ GUARDAR COMO PNG
  # ==========================================
  ggsave(
    "Figures/introduced_species_by_pathways.png",
    plot = p,
    width = 17,
    height = 10,
    dpi = 300
  )
}
