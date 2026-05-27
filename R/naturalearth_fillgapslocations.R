naturalearth_fillgapslocations <- function(){
  # The categories that can be placed in type=--- are defined here:
  # https://www.naturalearthdata.com/downloads/10m-physical-vectors/

  sf::sf_use_s2(FALSE)

  download_layer <- function(tipo, label) {
    layer <- tryCatch(
      ne_download(scale = "large", type = tipo, category = "physical", returnclass = "sf"),
      error = function(e) {
        warning(paste("No se pudo descargar la layer:", tipo))
        return(NULL)
      }
    )
    if (!is.null(layer)) {
      layer$type <- label
    }
    return(layer)
  }

  # Download layer's water
  layers <- list(
    download_layer("coastline", "Coastline"),
    download_layer("rivers_lake_centerlines", "Lake Centerlines"),
    download_layer("lakes", "Lakes+Reservoirs"),
    download_layer("historic_lakes", "Historic Lakes"),
    download_layer("pluvial_lakes", "Pluvial Lakes"),
    download_layer("playas", "Beachs")
  )

  # Delete layers that could not be downloaded
  layers <- layers[!sapply(layers, is.null)]

  # Merge layers
  dataset_water <- bind_rows(layers)

  # Download Countries
  countries1 <- ne_countries(scale = "large", returnclass = "sf")
  countries1 <- countries1[,c("name", "iso_a2", "iso_a3", "sovereignt")]
  countries1 <- countries1 %>%
    select(name = name,
           iso2 = iso_a2,
           iso3 = iso_a3,
           sovereignt = sovereignt)

  countries2 <- ne_download(scale="medium", type = "populated_places")
  countries2 <- countries2[,c("NAME", "ISO_A2", "ADM0_A3", "SOV0NAME")]
  countries2 <- countries2 %>%
    select(name = NAME,
           iso2 = ISO_A2,
           iso3 = ADM0_A3,   # ojo: aquí ADM0_A3 ≠ ISO_A3 en todos los casos
           sovereignt = SOV0NAME)

  countries3 <- ne_download(type="sovereignty", category="cultural")
  countries3 <- countries3[,c("NAME_EN", "ISO_A2", "ISO_A3", "SOVEREIGNT")]
  countries3 <- countries3 %>%
    select(name = NAME_EN,
           iso2 = ISO_A2,
           iso3 = ISO_A3,
           sovereignt = SOVEREIGNT)

  countries <- rbind(countries1, countries2, countries3)

  # Ensure both objects have the same CRS
  dataset_water <- st_transform(dataset_water, st_crs(countries))

  # Spatial intersection by category
  water_with_country <- dataset_water |>
    group_split(type) |>
    lapply(function(layer) {
      st_join(layer, countries, join = st_intersects)
    }) |>
    bind_rows()

  water_with_country <- water_with_country |>
    select(name_en, type, sovereignt, iso2, iso3) %>%
    filter(!is.na(name_en))

  # No duplicates
  source(file.path("R", "noduplicates.r"))
  regiones_with_country <- noduplicates(water_with_country, "sovereignt")
  regiones_with_country <- regiones_with_country %>%
    mutate(name = name_en) %>%
    select(-name_en, -type)

  countries_final <- noduplicates(countries, "sovereignt")

  # Merge
  regiones_with_country <- regiones_with_country |> st_drop_geometry()
  countries_final <- countries_final |> st_drop_geometry()
  merge_final <- merge(regiones_with_country, countries_final, by="sovereignt")

  mergefinal_2 <- merge_final %>%
    select(-iso2.y, -iso3.y) %>%
    mutate(keywords = paste(name.x, name.y, sep=",")) %>%
    select(-name.x, -name.y) %>%
    mutate(
      Location = str_trim(tolower(sovereignt)),
      ISO3 = iso3.x,
      ISO2 = iso2.x) %>%
    select(-iso2.x, -iso3.x, -sovereignt) %>%
    mutate(
      ISO2 = str_split(ISO2, ",") %>% sapply(\(x) trimws(x[1])),
      ISO3 = str_split(ISO3, ",") %>% sapply(\(x) trimws(x[1]))
    ) %>%
    filter(
      !str_detect(ISO2, "\\d"),
      !str_detect(ISO3, "\\d")
    )

  return(mergefinal_2)
}

