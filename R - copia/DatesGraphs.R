DatesGraphs <- function(){

  MasterList <- read_excel("OutputFiles/Final/FRIAS_SpeciesList_Masterlist.xlsx")
  head(MasterList$DateList)

  MasterList$DateList <- sapply(MasterList$DateList, function(x) {
    if (is.na(x)) return(NA)
    fechas <- unlist(strsplit(x, ";"))
    fechas_unicas <- unique(fechas)
    paste(fechas_unicas, collapse = ";")
  })

  MasterList <- MasterList %>%
    mutate(DateList = str_split(DateList, ";")) %>%      # Separar por ";"
    unnest(DateList)

  df_pie <- MasterList %>%
    dplyr::count(DateList) %>%
    dplyr::arrange(DateList)

  df_pie <- df_pie %>%
    filter(grepl("^\\d{4}$", DateList))

  df_pie$DateList <- as.numeric(df_pie$DateList)
  df_pie$SumaAcumulada <- cumsum(df_pie$n)


  par(mfrow = c(1, 2))  # Esto no afecta a ggplot, es para gráficos base

  p1 <- ggplot(df_pie %>% filter(DateList >= 1500), aes(x = DateList, y = n)) +
    geom_line(color = "darkgreen", size = 1.2) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = seq(0, max(df_pie$DateList), by = 100)) +
    scale_y_continuous(breaks = seq(0, max(df_pie$n), by = 100)) +
    labs(title = "Growth of invasive freshwater species",
         x = "First Record Year",
         y = "Number of new species") +
    theme_minimal()

  p2 <- ggplot(df_pie %>% filter(DateList >= 1500), aes(x = DateList, y = SumaAcumulada)) +
    geom_line(color = "darkgreen", size = 1.2) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = seq(0, max(df_pie$DateList), by = 50)) +
    scale_y_continuous(breaks = seq(0, max(df_pie$SumaAcumulada), by = 1000)) +
    labs(title = "Growth of invasive freshwater species",
         x = "Year",
         y = "Accumulative number of new species with that year in DateList") +
    theme_minimal()

  print(p1)
  print(p2)

}
