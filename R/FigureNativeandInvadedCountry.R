FigureNativeandInvadedCountry <- function(){

  # Leer datos
  MasterList <- read_excel("FinalFiles/FRIAS_SpeciesList_MasterList.xlsx")
  MasterList <- MasterList %>%
    select(AcceptedNameGBIF, ID_GBIF, Kingdom, NativeCountryISO3, InvadedCountryISO3)

  # --- PROCESO PARA NATIVECOUNTRY ---
  expanded_native <- MasterList %>%
    separate_rows(NativeCountryISO3, sep = ",") %>%
    mutate(NativeCountryISO3 = str_trim(NativeCountryISO3)) %>%
    filter(!is.na(NativeCountryISO3) & NativeCountryISO3 != "NA") %>%
    mutate(country_iso3 = NativeCountryISO3) %>%
    filter(!is.na(country_iso3))

  native_counts <- as.data.frame(table(expanded_native$country_iso3))
  names(native_counts) <- c("country_iso3", "n_species")

  # --- PROCESO PARA INVADEDCOUNTRY ---
  expanded_invaded <- MasterList %>%
    separate_rows(InvadedCountryISO3, sep = ",") %>%
    mutate(InvadedCountryISO3 = str_trim(InvadedCountryISO3)) %>%
    filter(!is.na(InvadedCountryISO3) & InvadedCountryISO3 != "NA") %>%
    mutate(country_iso3 = InvadedCountryISO3) %>%
    filter(!is.na(country_iso3))

  invaded_counts <- as.data.frame(table(expanded_invaded$country_iso3))
  names(invaded_counts) <- c("country_iso3", "n_species")

  # Calcular el rango combinado de n_species para la escala de colores
  combined_counts <- bind_rows(native_counts, invaded_counts)
  min_val <- min(combined_counts$n_species, na.rm = TRUE)
  max_val <- max(combined_counts$n_species, na.rm = TRUE)

  # Cargar mapa mundial
  world <- ne_countries(scale = "medium", returnclass = "sf")

  # Unir datos con mapa
  world_native <- world %>%
    left_join(native_counts, by = c("iso_a3" = "country_iso3"))

  world_invaded <- world %>%
    left_join(invaded_counts, by = c("iso_a3" = "country_iso3"))

  # Crear carpeta "Figures" si no existe
  if(!dir.exists("Figures")) {
    dir.create("Figures")
  }

  # Paleta personalizada
  viridis_palette <- rev(paletteer_c("grDevices::Viridis", 30))

  # Gráfico NativeCountry
  p_native <- ggplot(data = world_native) +
    geom_sf(aes(fill = n_species)) +
    scale_fill_gradientn(
      colors = viridis_palette,
      na.value = "grey90",
      limits = c(min_val, max_val),
    ) +
    theme_minimal() +
    labs(
      fill = "Species Number",
      title = "Amount of Species by Native Country",
      caption = "FRIAS Masterlist 2025"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.title.position = "plot"
    )

  p_native

  ggsave(filename = "Figures/FigureNativeCountry.png", plot = p_native,
         width = 10, height = 7, units = "in", dpi = 300)

  # Gráfico InvadedCountry
  p_invaded <- ggplot(data = world_invaded) +
    geom_sf(aes(fill = n_species)) +
    scale_fill_gradientn(
      colors = viridis_palette,
      na.value = "grey95",
      limits = c(min_val, max_val)
    ) +
    theme_void() +
    labs(
      fill = "Species Number",
      title = "Amount of Species by Invaded Country",
      caption = "FRIAS Masterlist 2025"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.title.position = "plot"
    )

  p_invaded

  ggsave(filename = "Figures/FigureInvadedCountry.png", plot = p_invaded,
         width = 10, height = 7, units = "in", dpi = 300)
}
