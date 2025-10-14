GrowthOfInvasiveSpecies <- function(){
  data <- read_excel("FinalFiles/FRIAS_SpeciesList_Masterlist.xlsx")

  # Subset the FirstRecords column
  dates <- data.frame(FirstRecords = as.factor(data$FirstRecords))

  # Fix merged year dates by inserting a comma
  dates$FirstRecords <- gsub("(\\d{4})(?=\\d{4})", "\\1,", dates$FirstRecords, perl = TRUE)

  # Split comma-separated years into rows
  all_years <- dates %>%
    separate_rows(FirstRecords, sep = ",") %>%
    filter(!is.na(FirstRecords), FirstRecords != "NA", FirstRecords != "") %>%
    mutate(FirstRecords = suppressWarnings(as.numeric(FirstRecords))) %>%
    filter(!is.na(FirstRecords), FirstRecords >= 1500)

  year_counts <- all_years %>%
    dplyr::count(FirstRecords) %>%
    rename(Count = n) %>%
    dplyr::mutate(Accumulation = cumsum(Count)) %>%
    arrange(FirstRecords)


  # Scatter plot
  ggplot(year_counts, aes(x = as.numeric(FirstRecords), y = Count)) +
    geom_point(color = "grey30", size = 3) +
    scale_x_continuous(
      breaks = seq(1500, max(as.numeric(year_counts$FirstRecords)), by = 50)
    ) +
    labs(x = "Year",
         y = "First record rate") +
    theme_minimal() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_text(size=15),
          axis.title.y = element_text(size=15))

  p1 <- ggplot(year_counts, aes(x = as.numeric(FirstRecords), y = Count)) +
    geom_point(color = "grey30", size = 3) +
    scale_x_continuous(
      breaks = seq(1500, max(as.numeric(year_counts$FirstRecords)), by = 50)
    ) +
    labs(x = "Year",
         y = "First record rate") +
    theme_minimal() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_text(size=15),
          axis.title.y = element_text(size=15))
  #
  # ggplot(year_counts %>% filter(FirstRecords >= 1500), aes(x = FirstRecords, y = Count)) +
  #   geom_point(size = 2, alpha = 0.7) +
  #   labs(
  #     x = "Year",
  #     y = "First record rate"
  #   )

  # # Graficar con eje x cada 1000 Years
  # ggplot(year_counts %>% filter(FirstRecords>=1500),aes(x = FirstRecords, y = Accumulation)) +
  #   geom_line(color = "darkgreen", size = 1.2) +
  #   geom_point(size = 3) +
  #   scale_x_continuous(
  #     breaks = seq(0, max(year_counts$FirstRecords), by = 200)
  #   ) +
  #   labs(title = "Growth of invasive freshwater species",
  #        x = "First Record Year",
  #        y = "Accumulation number of species") +
  #   theme_minimal()

  # ggplot(year_counts %>% filter(FirstRecords >= 1500), aes(x = FirstRecords, y = Accumulation)) +
  #   geom_area(fill = "lightblue", alpha = 0.6) +
  #   geom_line(color = "darkgreen", size = 1.2) +
  #   geom_point(color = "black", size = 2) +
  #   scale_x_continuous(breaks = seq(1500, max(year_counts$FirstRecords), by = 50)) +
  #   scale_y_continuous(breaks = seq(0, max(year_counts$Accumulation), by = 500)) +
  #   labs(
  #     title = "Growth of invasive freshwater species",
  #     x = "Year",
  #     y = "Number of accumulated species"
  #   ) +
  #   theme_minimal(base_size = 14)
  #
  # p2 <- ggplot(year_counts %>% filter(FirstRecords >= 1500), aes(x = FirstRecords, y = Accumulation)) +
  #   geom_area(fill = "lightblue", alpha = 0.6) +
  #   geom_line(color = "darkgreen", size = 1.2) +
  #   geom_point(color = "black", size = 2) +
  #   scale_x_continuous(breaks = seq(1500, max(year_counts$FirstRecords), by = 50)) +
  #   scale_y_continuous(breaks = seq(0, max(year_counts$Accumulation), by = 500)) +
  #   labs(
  #     title = "Growth of invasive freshwater species",
  #     x = "Year",
  #     y = "Number of accumulated species"
  #   ) +
  #   theme_minimal(base_size = 14)

  #Save Figures
  ggsave("Figures/GrowthOfInvasiveSpecies.png", plot = p1, width = 8, height = 6, dpi = 300)
}
