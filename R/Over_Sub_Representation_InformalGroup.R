Over_Sub_Representation_InformalGroup <- function(){
  #Data
  MasterList <- read_csv("FinalFiles/(Table S3) FRIAS_masterlist.csv")
  table(MasterList$Group)

  taxa_db <- data.frame(
    Taxa = c(
      "Algae",
      "Amphibians",
      "Arachnids",
      "Bacteria and protozoans",
      "Birds",
      "Bryophytes",
      "Crustaceans",
      "Fishes",
      "Fungi",
      "Insects",
      "Mammals",
      "Molluscs",
      "Reptiles",
      "SAR",
      "Vascular plants"
    ),

    MasterList_Count = c(
      8, 77, 2, 14, 189, 3, 396, 1334, 10, 50, 12, 150, 55, 320, 532
    ),

    FW_Total = c(
      2732, 5089, 6149, 2254, 764, 83, 11990, 19180, 2000, 75874,
      127, 4998, 639, 4545, 2614
    ),

    Described_total = c(
      27299, 8918, 97085, 23862, 11185, 21925, 83263, 37288, 157648,
      1003469, 6819, 88244, 12502, 58069, 381913
    ),

    IUCN_evaluated = c(
      114, 8051, 1053, 0, 11185, 327, 3361, 29114, 1302, 13696,
      6036, 9502, 10368, 0, 76441
    )
  )

  taxa_db <- taxa_db %>%
    mutate(
      Taxa = as.character(Taxa),
      MasterList_Count = as.numeric(MasterList_Count),
      FW_Total = as.numeric(FW_Total)
    )

  # Si quieres aplicar también la limpieza de "*"
  taxa_db[] <- lapply(taxa_db, function(x) {
    if (is.character(x)) gsub("\\*", "", x) else x
  })
  attach(taxa_db)

  #Expected Proportions
  expected_prop <- FW_Total / sum(FW_Total)
  expected_counts <- expected_prop * sum(MasterList_Count)

  #Chi-square test
  chi_square_test <- chisq.test(x = MasterList_Count, p = expected_prop)
  chi_square_test

  #FinalTable
  table <- data.frame(
    Taxa,
    Observed = MasterList_Count,
    Expected = expected_counts,
    StdResidual = chi_square_test$stdres
  ) %>%
    mutate(
      Representation = case_when(
        StdResidual > 2 ~ "Overrepresented",
        StdResidual < -2 ~ "Underrepresented",
        TRUE ~ "Neutral"
      ),
      Color = case_when(
        Representation == "Overrepresented" ~ "#A50026FF",
        Representation == "Underrepresented" ~ "#74ADD1FF",
        TRUE ~ "grey80"
      )
    )

  table


  #Graph
  graph <- ggplot(
    table,
    aes(
      x = reorder(Taxa, StdResidual),
      y = StdResidual,
      fill = Representation
    )
  ) +
    geom_bar(stat = "identity", color = "black") +
    scale_fill_manual(
      values = c(
        "Overrepresented"  = "#A50026FF",
        "Underrepresented" = "#74ADD1FF",
        "Neutral"          = "grey80"
      ),
      name = "Representation"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
    geom_hline(yintercept = c(-2, 2), linetype = "dashed") +
    coord_flip() +
    labs(
      x = "",
      y = "Standardized residuals"
    ) +
    theme_minimal(base_size = 20) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      axis.text.x  = element_text(size = 18),
      axis.text.y  = element_text(size = 18),
      axis.line = element_line(color = "black", linewidth = 0.6),
      axis.ticks = element_line(color = "black", linewidth = 0.6),
      legend.position = "bottom"
    )
  graph

  ggsave("Figures/Figure5a.png", graph, width = 15, height = 6, dpi = 300)
  ggsave("Figures/Figure5a.svg", graph, width = 15, height = 6, dpi = 300)
  ggsave("Figures/Figure5a.tiff", graph, width = 15, height = 6, dpi = 300)

}
