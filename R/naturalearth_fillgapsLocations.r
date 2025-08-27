naturalearth_fillgapsLocations <- function(){
  # Las categorias que se pueden poner en el type=--- son las definidas aqui:
  #   https://www.naturalearthdata.com/downloads/10m-physical-vectors/

  #PARA AGUA
  #Nos quedamos con las categorias:
  #coastline
  #land
  #minor_islands
  #reefs
  #rivers_lake_centerlines
  #physical_labels
  #antartic_ice_shelves
  #glaciated_Areas
  #bathymetry
  #physical_building_blocks


  sf::sf_use_s2(FALSE)

  descargar_capa <- function(tipo, etiqueta) {
    capa <- tryCatch(
      ne_download(scale = "large", type = tipo, category = "physical", returnclass = "sf"),
      error = function(e) {
        warning(paste("No se pudo descargar la capa:", tipo))
        return(NULL)
      }
    )

    if (!is.null(capa)) {
      capa$type <- etiqueta
    }

    return(capa)
  }

  # Descargar capas de agua
  capas <- list(
    descargar_capa("coastline", "Coastline"),
    # descargar_capa("reefs", "Reefs"),
    # descargar_capa("ocean", "Ocean"),
    descargar_capa("rivers_lake_centerlines", "Lake Centerlines"),
    descargar_capa("lakes", "Lakes+Reservoirs"),
    descargar_capa("historic_lakes", "Historic Lakes"),
    descargar_capa("pluvial_lakes", "Pluvial Lakes"),
    descargar_capa("playas", "Beachs")
  )

  # Eliminar las capas que no se pudieron descargar
  capas <- capas[!sapply(capas, is.null)]

  # Unir todas las capas
  dataset_agua <- bind_rows(capas)

  # Descargar países
  paises1 <- ne_countries(scale = "large", returnclass = "sf")
  paises1 <- paises1[,c("name", "iso_a2", "iso_a3", "sovereignt")]
  paises1 <- paises1 %>%
    select(name = name,
           iso2 = iso_a2,
           iso3 = iso_a3,
           sovereignt = sovereignt)

  paises2 <- ne_download(scale="medium",type = "populated_places")
  paises2 <- paises2[,c("NAME", "ISO_A2", "ADM0_A3", "SOV0NAME")]
  paises2 <- paises2 %>%
    select(name = NAME,
           iso2 = ISO_A2,
           iso3 = ADM0_A3,   # ojo: aquí ADM0_A3 ≠ ISO_A3 en todos los casos
           sovereignt = SOV0NAME)

  paises3 <- ne_download(type="sovereignty",category="cultural")
  paises3 <- paises3[,c("NAME_EN", "ISO_A2", "ISO_A3", "SOVEREIGNT")]
  paises3 <- paises3 %>%
    select(name = NAME_EN,
           iso2 = ISO_A2,
           iso3 = ISO_A3,
           sovereignt = SOVEREIGNT)

  paises <- rbind(paises1, paises2, paises3)

  # Asegurar que ambos objetos tengan el mismo CRS
  dataset_agua <- st_transform(dataset_agua, st_crs(paises))

  # Intersección espacial por categoría
  agua_con_pais <- dataset_agua |>
    group_split(type) |>
    lapply(function(capa) {
      st_join(capa, paises, join = st_intersects)
    }) |>
    bind_rows()

  names(agua_con_pais)
  # Seleccionar columnas relevantes
  agua_con_pais <- agua_con_pais |>
    select(name_en, type, sovereignt, iso2, iso3) %>%
    filter(!is.na(name_en))

  #noduplicados
  source(file.path("R", "noduplicates.r"))
  regiones_con_pais <- noduplicates(agua_con_pais, "sovereignt")
  regiones_con_pais <- regiones_con_pais %>%
    mutate(name = name_en) %>%
    select(-name_en, -type)
  names(regiones_con_pais)
  paises_final <- noduplicates(paises, "sovereignt")
  names(paises_final)

  #unimos
  regiones_con_pais <- regiones_con_pais |> st_drop_geometry()
  paises_final <- paises_final |> st_drop_geometry()
  merge_final <- merge(regiones_con_pais, paises_final, by="sovereignt")

  # Guardar en Excel (sin geometría)
  write_xlsx(merge_final |> st_drop_geometry(), "TablesToStandardize/RNaturalEarthData_withCountries_byType.xlsx")
}
