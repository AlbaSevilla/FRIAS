Over_Sub_Representation_InformalGroup <- function(){

  #Data
  TableS5 <- read.xlsx("TablesToStandardize/Table S5.xlsx")
  Taxa <- TableS5$Taxa
  MasterList_Count <- TableS5$MasterList_Count
  FW_Total <- TableS5$FW_Total

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
    StdResidual = chi$stdres
  ) %>%
    mutate(
      Representation = case_when(
        StdResidual > 2 ~ "Overrepresented",
        StdResidual < -2 ~ "Underrepresented",
        TRUE ~ "Neutral"
      ),
      Color = case_when(
        Representation == "Overrepresented" ~ "lightpink",
        Representation == "Underrepresented" ~ "lightblue",
        TRUE ~ "grey80"
      )
    )

  table


  #Graph
  graph <- ggplot(tabla, aes(x = reorder(Taxa, StdResidual), y = StdResidual, fill = Color)) +
    geom_bar(stat = "identity", color = "black") +
    scale_fill_identity() +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
    geom_hline(yintercept = c(-2, 2), linetype = "dashed") +
    coord_flip() +
    labs(
      x="",
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
      axis.ticks = element_line(color = "black", linewidth = 0.6)
    )
  graph

  ggsave("Figures/Figure6.png", graph, width = 10, height = 6, dpi = 300)
  ggsave("Figures/Figure6.svg", graph, width = 10, height = 6, dpi = 300, compression = "lzw")
}
