Native_to_Recipient_maps <- function() {

  RdYlBu_palette <- rev(viridis(256))


  MasterList <- read_csv("FinalFiles/(Table S3) FRIAS_masterlist.csv") %>%
    dplyr::select(AcceptedNameGBIF, ID_GBIF, Group, Kingdom, NativeRangeISO3, RecipientRangeISO3)

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

  plot_species_map <- function(world_sf, column, palette, limits,
                               legend_title = "Density") {

    world_sf$zero_or_na <- ifelse(
      is.na(world_sf[[column]]) | world_sf[[column]] == 0,
      TRUE, FALSE
    )

    hatch_lines <- st_make_grid(world_sf, cellsize = 10) %>%
      st_as_sf() %>%
      mutate(id = row_number()) %>%
      st_cast("LINESTRING")

    ggplot() +
      geom_sf(
        data = world_sf %>% filter(!zero_or_na),
        aes(fill = !!sym(column), geometry = geometry),
        color = "black",
        linewidth = 0.15
      ) +
      geom_sf(
        data = world_sf %>% filter(zero_or_na),
        aes(geometry = geometry),
        fill = "white",
        color = "black",
        linewidth = 0.15
      ) +
      geom_sf(
        data = suppressWarnings(
          st_intersection(
            world_sf %>% filter(zero_or_na),
            hatch_lines
          )
        ),
        color = "black",
        linewidth = 0.2,
        alpha = 0.6
      ) +
      scale_fill_gradientn(
        colours = palette,
        limits = limits,
        na.value = "white",
        breaks = seq(0, limits[2], by = 200),
        name = legend_title
      ) +
      coord_sf(crs = "+proj=moll") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 25, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.key.height = unit(1.2, "cm"),
        legend.key.width = unit(3, "cm")
      )
  }



  p1 <- plot_species_map(
    world_native,
    "NativeSpeciesCount",
    RdYlBu_palette,
    limits_global,
    legend_title = "Density by native range"
  )

  p2 <- plot_species_map(
    world_Recipient,
    "RecipientSpeciesCount",
    RdYlBu_palette,
    limits_global,
    legend_title = "Density by recipient range"
  )

  p1
  p2

  # ----- SAVE -----
  ggsave("Figures/Figure4d.png",
         p1, dpi = 300, width = 12, height = 14)

  ggsave("Figures/Figure4d.svg",
         p1, dpi = 300, width = 12, height = 14)

  ggsave("Figures/Figure4c.png",
         p2, dpi = 300, width = 12, height = 14)

  ggsave("Figures/Figure4c.svg",
         p2, dpi = 300, width = 12, height = 14)

}

