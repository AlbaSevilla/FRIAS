Dates_graphs <- function(){

  MasterList <- read_csv("FinalFiles/FRIAS_masterlist.csv")
  MasterList$EarliestReport <- sapply(MasterList$EarliestReport, function(x) {
    if (is.na(x)) return(NA)
    fechas <- unlist(strsplit(x, ","))
    fechas_unicas <- unique(fechas)
    paste(fechas_unicas, collapse = ",")
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
  database_pie$SumaAcumulada <- cumsum(database_pie$n)


  par(mfrow = c(1, 2))  # Esto no afecta a ggplot, es para gráficos base

  p1 <- ggplot(database_pie %>% filter(EarliestReport >= 1500), aes(x = EarliestReport, y = n)) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = seq(0, max(database_pie$EarliestReport), by = 100)) +
    scale_y_continuous(breaks = seq(0, max(database_pie$n), by = 100)) +
    labs(title = "Growth of invasive freshwater species",
         x = "First Record Year",
         y = "Number of new species") +
    theme_minimal()

  p2 <- ggplot(database_pie %>% filter(EarliestReport >= 1500), aes(x = EarliestReport, y = SumaAcumulada)) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = seq(0, max(database_pie$EarliestReport), by = 50)) +
    scale_y_continuous(breaks = seq(0, max(database_pie$SumaAcumulada), by = 1000)) +
    labs(title = "Growth of invasive freshwater species",
         x = "Year",
         y = "Accumulative number of new species with that year in EarliestReport") +
    theme_minimal()

  print(p1)

  ggsave(filename = "Figures/Dates_graph1.png",
         width = 10, height = 6, dpi = 300)

  print(p2)

  ggsave(filename = "Figures/Dates_graph2.png",
         width = 10, height = 6, dpi = 300)
}
