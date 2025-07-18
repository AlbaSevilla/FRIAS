  GrowthOfInvasiveSpecies <- function(){
  #REPRESENTACIÓN DE LA ACUMULACIÓN DEL NUMERO
  #DE ESPECIES INVASORAS RESPECTO AL AUMENTO DE LOS
  #YearS

  # Cargar datos y convertir Year
  MasterList <- read_excel("OutputFiles/Intermediate/Step2_Masterlist.xlsx")
  MasterList <- MasterList[, c("AcceptedNameGBIF", "OldestDate")]

  especies <- MasterList %>%
    mutate(Year = as.numeric(OldestDate)) %>%
    filter(!is.na(Year))

  # Contar y acumular por Year (numérico)
  conteo_anual <- especies %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::arrange(Year) %>%
    dplyr::mutate(Accumulation = cumsum(n))

  View(conteo_anual)

  conteo_anual <- conteo_anual[-219,]

  # Graficar con eje x cada 1000 Years
  ggplot(conteo_anual, aes(x = Year, y = Accumulation)) +
    geom_line(color = "darkgreen", size = 1.2) +
    geom_point(size = 3) +
    scale_x_continuous(
      breaks = seq(0, max(conteo_anual$Year), by = 200)
    ) +
    labs(title = "Growth of invasive freshwater species",
         x = "First Record Year",
         y = "Accumulation number of species") +
    theme_minimal()


  ggplot(conteo_anual, aes(x = Year, y = Accumulation)) +
    geom_area(fill = "lightblue", alpha = 0.6) +
    geom_line(color = "darkgreen", size = 1.2) +
    geom_point(color = "black", size = 2) +
    scale_x_continuous(breaks = seq(0, max(conteo_anual$Year), by = 200)) +
    labs(title = "Growth of invasive freshwater species",
         x = "Year",
         y = "Number of accumulated species") +
    theme_minimal(base_size = 14)

  ggplot(conteo_anual %>% filter(Year >= 1500), aes(x = Year, y = Accumulation)) +
    geom_area(fill = "lightblue", alpha = 0.6) +
    geom_line(color = "darkgreen", size = 1.2) +
    geom_point(color = "black", size = 2) +
    scale_x_continuous(breaks = seq(1500, max(conteo_anual$Year), by = 50)) +
    scale_y_continuous(breaks = seq(0, max(conteo_anual$Accumulation), by = 100)) +
    labs(
      title = "Growth of invasive freshwater species",
      x = "Year",
      y = "Number of accumulated species"
    ) +
    theme_minimal(base_size = 14)


  #####################
  # VERSION INTERACTIVA
  # Crear tabla con nombres por Year (para tooltips)
  especies_por_Year <- especies %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(
      n = n(),
      nombres = paste(AcceptedNameGBIF, collapse = ", ")
    ) %>%
    dplyr::arrange(Year) %>%
    dplyr::mutate(Accumulation = cumsum(n))

  # Plot interactivo
  p <- ggplot(especies_por_Year, aes(x = Year, y = Accumulation,
                                    text = paste0("Year: ", Year,
                                                  "<br>Accumulation: ", Accumulation,
                                                  "<br>New Freshwater Species: ", n,
                                                  "<br>", nombres))) +
    geom_line(color = "darkgreen", size = 1.2) +
    geom_point(size = 2) +
    labs(title = "Growth of invasive freshwater species",
         x = "Year",
         y = "Accumulation number") +
    theme_minimal()

  ggplotly(p, tooltip = "text")

}
