Dates_graphs <- function(){
  MasterList <- read_csv("FinalFiles/FRIAS_masterlist.csv")
  MasterList$EarliestReport <- sapply(MasterList$EarliestReport, function(x) {
    if (is.na(x)) return(NA)
    x <- as.character(x)
    dates <- unlist(strsplit(x, ","))
    unique_dates <- unique(dates)
    paste(unique_dates, collapse = ",")
  })

  MasterList <- MasterList %>%
    mutate(EarliestReport = str_split(EarliestReport, ",")) %>%      # Separar por ";"
    unnest(EarliestReport)
  database_pie <- MasterList %>%
    dplyr::count(EarliestReport) %>%
    dplyr::arrange(EarliestReport)
  database_pie <- database_pie %>%
    filter(grepl("^\\d{4}$", EarliestReport))
  database_pie$EarliestReport <- as.numeric(database_pie$EarliestReport)
  database_pie$Accum <- cumsum(database_pie$n)
  par(mfrow = c(1, 2))

  df <- database_pie %>% filter(EarliestReport >= 1500)
  upper_limit <- ceiling(max(df$n) / 10) * 10
  upper_x <- ceiling(max(df$EarliestReport) / 50) * 50

  p1 <- ggplot(df, aes(x = EarliestReport, y = n)) +
    geom_point(size = 3) +
    scale_x_continuous(
      breaks = seq(1500, upper_x, by = 50)
    ) +
    scale_y_continuous(
      breaks = seq(0, upper_limit, by = 50)
    ) +
    labs(
      x = "First Record Year",
      y = "Number of newly recorded species"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 20, face = "bold"),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 18),

      # 🔹 Ejes visibles
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black")
    )

  p1

  df2 <- df[-1,]
  upper_limit2 <- max(df2$n) + 1  # límite Y dinámico, un poquito más del máximo
  upper_x2 <- max(df2$EarliestReport) + 50  # límite X dinámico

  p1_2 <- ggplot(df2, aes(x = EarliestReport, y = n)) +
    geom_point(size = 3) +
    scale_x_continuous(
      breaks = seq(1500, upper_x2, by = 50),
      limits = c(1500, upper_x2)
    ) +
    scale_y_continuous(
      breaks = seq(0, upper_limit2, by = 20),
      limits = c(0, upper_limit2)
    ) +
    labs(
      x = "First Record Year",
      y = "Number of newly recorded species"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 20, face = "bold"),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 18),
      axis.line = element_line(color = "black", linewidth = 0.7),   # 🔹 eje más gordito
      axis.ticks = element_line(color = "black", linewidth = 0.7)  # 🔹 ticks más gorditos
    )

  p1_2

  p2 <- ggplot(database_pie %>% filter(EarliestReport >= 1500), aes(x = EarliestReport, y = Accum)) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = seq(0, max(database_pie$EarliestReport), by = 50)) +
    scale_y_continuous(breaks = seq(0, max(database_pie$Accum), by = 1000)) +
    labs(x = "Year",
         y = "Accumulative Number of newly recorded species with that year in EarliestReport") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  p2

  ggsave(filename = "Figures/Figure4b.png",p1_2,
         width = 10, height = 6, dpi = 300)
  ggsave(filename = "Figures/Figure4b.svg",p1_2,
         width = 10, height = 6, dpi = 300)

  ggsave(filename = "Figures/Dates_graph2.png",p2,
         width = 10, height = 6, dpi = 300)
}

