Native_to_Recipient_maps <- function() {

  ruta <- "FinalFiles/FRIAS_masterlist.csv"

  MasterList <- read_csv(ruta) %>%
    select(AcceptedNameGBIF, ID_GBIF, Group, Kingdom, NativeRangeISO3, RecipientRangeISO3)

  dataset_native <- MasterList %>%
    mutate(Range = strsplit(NativeRangeISO3, ",|;")) %>%
    unnest(Range) %>%
    mutate(Range = trimws(Range)) %>%
    drop_na(Range)

  dataset_Recipient <- MasterList %>%
    mutate(Range = strsplit(RecipientRangeISO3, ",|;")) %>%
    unnest(Range) %>%
    mutate(Range = trimws(Range)) %>%
    drop_na(Range)

  native_counts <- dataset_native %>%
    group_by(Range) %>%
    dplyr::summarise(NativeSpeciesCount = n_distinct(AcceptedNameGBIF))

  Recipient_counts <- dataset_Recipient %>%
    group_by(Range) %>%
    dplyr::summarise(RecipientSpeciesCount = n_distinct(AcceptedNameGBIF))

  # Leer shapefile mundial
  url <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
  tmpfile <- tempfile(fileext = ".geojson")
  GET(url, write_disk(tmpfile, overwrite = TRUE))
  world <- st_read(tmpfile, quiet = TRUE)

  if ("id" %in% names(world)) {
    world <- world %>% rename(iso_a3 = id)
  } else if ("ISO_A3" %in% names(world)) {
    world <- world %>% rename(iso_a3 = ISO_A3)
  }

  world_native <- left_join(world, native_counts, by = c("iso_a3" = "Range"))
  world_Recipient <- left_join(world, Recipient_counts, by = c("iso_a3" = "Range"))

  global_min <- 0
  global_max <- max(
    max(world_native$NativeSpeciesCount, na.rm = TRUE),
    max(world_Recipient$RecipientSpeciesCount, na.rm = TRUE)
  )
  limits_global <- c(global_min, global_max)

  plot_species_map <- function(world_sf, column, cmap, limits) {
    column_sym <- sym(column)

    ggplot() +
      geom_sf_pattern(
        data = dplyr::filter(world_sf, is.na(!!column_sym) | !!column_sym == 0),
        aes(geometry = geometry),
        fill = "white",
        color = "grey70",
        pattern = "stripe",
        pattern_fill = "grey70",
        pattern_density = 0.4,
        pattern_spacing = 0.03
      ) +
      geom_sf(
        data = dplyr::filter(world_sf, !is.na(!!column_sym) & !!column_sym > 0),
        aes(fill = !!column_sym, geometry = geometry),
        color = "grey70",
        linewidth = 0.2
      ) +
      scale_fill_viridis_c(
        option = cmap,
        direction = -1,
        name = "Density",
        limits = limits
      ) +
      coord_sf(crs = "+proj=moll") +
      theme_void() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
  }

  p1 <- plot_species_map(world_native, "NativeSpeciesCount", "viridis", limits_global)
  p2 <- plot_species_map(world_Recipient, "RecipientSpeciesCount", "viridis", limits_global)

  combined <- p1 / p2
  print(combined)

  ggsave("Figures/native_Recipient_maps_same_scale_pattern.png", combined, dpi = 300, width = 12, height = 14)
  ggsave("Figures/Figure4cd.tiff", combined, dpi = 300, width = 12, height = 14)

}
