FigureNativeandInvadedCountry <- function(){

  # Leer datos
  MasterList <- read_excel("OutputFiles/FinalMasterlists/FRIAS_SpeciesList_MasterList.xlsx")

  MasterList <- MasterList %>%
    select(AcceptedNameGBIF, ID_GBIF, Kingdom, NativeCountries_ISO3, InvadedCountriesISO3_List)

  # --- PROCESO PARA NATIVECOUNTRY ---
  expanded_native <- MasterList %>%
    separate_rows(NativeCountries_ISO3, sep = ",") %>%
    mutate(NativeCountries_ISO3 = str_trim(NativeCountries_ISO3)) %>%
    filter(!is.na(NativeCountries_ISO3) & NativeCountries_ISO3 != "NA") %>%
    mutate(country_iso3 = NativeCountries_ISO3) %>%
    filter(!is.na(country_iso3))

  native_counts <- expanded_native %>%
    group_by(country_iso3) %>%
    summarise(n_species = n(), .groups = "drop")

  # --- PROCESO PARA INVADEDCOUNTRY ---
  expanded_invaded <- MasterList %>%
    separate_rows(InvadedCountriesISO3_List, sep = ",") %>%
    mutate(InvadedCountriesISO3_List = str_trim(InvadedCountriesISO3_List)) %>%
    filter(!is.na(InvadedCountriesISO3_List) & InvadedCountriesISO3_List != "NA") %>%
    mutate(country_iso3 = InvadedCountriesISO3_List) %>%
    filter(!is.na(country_iso3))

  invaded_counts <- expanded_invaded %>%
    group_by(country_iso3) %>%
    summarise(n_species = n(), .groups = "drop")

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

  # Gráfico NativeCountry con escala fija
  p_native <- ggplot(data = world_native) +
    geom_sf(aes(fill = n_species)) +
    scale_fill_viridis_c(option = "A", direction = -1, na.value = "grey90",
                         limits = c(min_val, max_val)) +
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

  ggsave(filename = "Figures/FigureNativeCountry.png", plot = p_native,
         width = 10, height = 7, units = "in", dpi = 300)

  # Gráfico InvadedCountry con escala fija
  p_invaded <- ggplot(data = world_invaded) +
    geom_sf(aes(fill = n_species)) +
    scale_fill_viridis_c(option = "A", direction = -1, na.value = "grey90",
                         limits = c(min_val, max_val)) +
    theme_minimal() +
    labs(
      fill = "Species Number",
      title = "Amount of Species by Invaded Country",
      caption = "FRIAS Masterlist 2025"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.title.position = "plot"
    )

  ggsave(filename = "Figures/FigureInvadedCountry.png", plot = p_invaded,
         width = 10, height = 7, units = "in", dpi = 300)
}
