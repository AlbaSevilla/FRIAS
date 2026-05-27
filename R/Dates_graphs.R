Dates_graphs <- function(){
  MasterList <- read_csv("FinalFiles/(Table S3) FRIAS_masterlist.csv")
  MasterList <- MasterList %>%
    mutate(EarliestReport = as.character(EarliestReport)) %>%
    separate_rows(EarliestReport, sep = ",|;") %>%
    mutate(EarliestReport = trimws(EarliestReport)) %>%
    distinct()

  database_pie <- MasterList %>%
    dplyr::count(EarliestReport) %>%
    dplyr::arrange(EarliestReport)
  database_pie <- database_pie %>%
    filter(grepl("^\\d{4}$", EarliestReport))
  database_pie$EarliestReport <- as.numeric(database_pie$EarliestReport)
  database_pie$Accum <- cumsum(database_pie$n)

  df <- database_pie %>% filter(EarliestReport >= 1500)
  upper_limit <- ceiling(max(df$n) / 10) * 10
  upper_x <- ceiling(max(df$EarliestReport) / 50) * 50

  theme_pub <- theme_minimal(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),

      # SOLO líneas verticales
      panel.grid.major.x = element_line(
        color = "grey85",
        linewidth = 0.3
      ),

      panel.grid.major.y = element_blank(),

      axis.line = element_line(
        color = "black",
        linewidth = 0.6
      ),

      axis.ticks = element_line(
        color = "black",
        linewidth = 0.5
      ),

      axis.ticks.length = unit(0.2, "cm"),

      axis.title = element_text(
        size = 17,
        face = "bold"
      ),

      axis.text = element_text(
        size = 14,
        color = "black"
      ),

      plot.title = element_text(
        size = 20,
        face = "bold",
        hjust = 0
      ),

      plot.subtitle = element_text(
        size = 14,
        color = "grey30"
      ),

      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),

      plot.margin = margin(15, 15, 15, 15)
    )

  # =========================================================
  # P1
  # =========================================================

  p1 <- ggplot(df, aes(x = EarliestReport, y = n)) +

    geom_point(
      size = 3.5,
      color = "#1f4e79",
      alpha = 0.9
    ) +

    scale_x_continuous(
      breaks = seq(1500, upper_x, by = 50),
      expand = expansion(mult = c(0.01, 0.03))
    ) +

    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 5),
      expand = expansion(mult = c(0, 0.05))
    ) +

    labs(
      x = "Earliest Report Year",
      y = "Number of newly recorded species"
    ) +

    theme_pub

  p1

  # =========================================================
  # P1_2
  # =========================================================

  df2 <- df[-1,]
  upper_limit2 <- max(df2$n) + 1
  upper_x2 <- max(df2$EarliestReport) + 50

  p1_2 <- ggplot(df2, aes(x = EarliestReport, y = n)) +

    geom_point(
      size = 3.5,
      color = "#1f4e79",
      alpha = 0.9
    ) +

    scale_x_continuous(
      breaks = seq(1500, upper_x2, by = 50),
      limits = c(1500, upper_x2),
      expand = expansion(mult = c(0.01, 0.02))
    ) +

    scale_y_continuous(
      breaks = seq(0, upper_limit2, by = 20),
      limits = c(0, upper_limit2),
      expand = expansion(mult = c(0, 0.05))
    ) +

    labs(
      title = "",
      x = "Earliest Report Year",
      y = "Number of newly recorded species"
    ) +

    theme_pub

  p1_2

  # =========================================================
  # P2
  # =========================================================

  p2 <- ggplot(
    database_pie %>% filter(EarliestReport >= 1500),
    aes(x = EarliestReport, y = Accum)
  ) +

    geom_point(
      size = 3,
      color = "#1f4e79",
      alpha = 0.9
    ) +

    scale_x_continuous(
      breaks = seq(1500, max(database_pie$EarliestReport), by = 50)
    ) +

    scale_y_continuous(
      breaks = seq(0, max(database_pie$Accum), by = 1000),
      expand = expansion(mult = c(0, 0.05))
    ) +

    labs(
      title = "",
      x = "Earliest Report Year",
      y = "Cumulative species count"
    ) +

    theme_pub

  p2

  ggsave(filename = "Figures/Figure4b.png",p1_2,
         width = 10, height = 6, dpi = 300)
  ggsave(filename = "Figures/Figure4b.svg",p1_2,
         width = 10, height = 6, dpi = 300)

  ggsave(filename = "Figures/Dates_graph2.png",p2,
         width = 10, height = 6, dpi = 300)
}

