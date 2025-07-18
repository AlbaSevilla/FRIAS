SankeyDiagramBioregions <- function(){
  MasterList <- read_excel("OutputFiles/Final/FRIAS_SpeciesList_MasterList.xlsx")

  # Seleccionamos columnas relevantes
  selected_columns <- c("NativeRangeBioregions", "InvadedBioregions")
  MasterList_selectedcolumns <- MasterList[,selected_columns]
  clean_na_from_list <- function(x) {
    # Separar por comas o punto y coma
    parts <- unlist(str_split(x, pattern = ";|,"))
    # Quitar espacios
    parts <- str_trim(parts)
    # Eliminar los "NA"
    parts <- parts[!parts %in% c("NA", "na", "")]
    # Si no quedan elementos, devolver NA
    if(length(parts) == 0) return(NA_character_)
    # Unir de nuevo con ", "
    paste(parts, collapse = ", ")
  }

  MasterList_selectedcolumns <- MasterList_selectedcolumns %>%
    mutate(
      NativeRangeBioregions = sapply(NativeRangeBioregions, clean_na_from_list),
      InvadedBioregions = sapply(InvadedBioregions, clean_na_from_list)
    )

  # Procesar regiones nativas
  MasterList_selectedcolumns <- MasterList_selectedcolumns %>%
    mutate(NativeRangeBioregions = str_split(NativeRangeBioregions, ";")) %>%
    unnest(NativeRangeBioregions) %>%
    mutate(NativeRangeBioregions = str_trim(tolower(NativeRangeBioregions))) %>%
    mutate(NativeRangeBioregions = str_split(NativeRangeBioregions, ", ")) %>%
    unnest(NativeRangeBioregions) %>%
    mutate(NativeRangeBioregions = str_trim(tolower(NativeRangeBioregions)))

  # Procesar regiones invadidas
  MasterList_selectedcolumns <- MasterList_selectedcolumns %>%
    mutate(InvadedBioregions = str_split(InvadedBioregions, ";")) %>%
    unnest(InvadedBioregions) %>%
    mutate(InvadedBioregions = str_trim(tolower(InvadedBioregions))) %>%
    mutate(InvadedBioregions = str_split(InvadedBioregions, ",")) %>%
    unnest(InvadedBioregions) %>%
    mutate(InvadedBioregions = str_trim(tolower(InvadedBioregions)))

  # Unificar nombres
  MasterList_selectedcolumns <- MasterList_selectedcolumns %>%
    mutate(NativeRangeBioregions = case_when(
      NativeRangeBioregions %in% c("americas", "america") ~ "america",
      NativeRangeBioregions %in% c("europa", "europe") ~ "europe",
      NativeRangeBioregions %in% c("NA", "") ~ NA_character_,
      TRUE ~ NativeRangeBioregions
    ),
    InvadedBioregions = case_when(
      InvadedBioregions %in% c("americas", "america") ~ "america",
      InvadedBioregions %in% c("europa", "europe") ~ "europe",
      InvadedBioregions %in% c("NA", "") ~ NA_character_,
      TRUE ~ InvadedBioregions
    ))
  MasterList_selectedcolumns <- MasterList_selectedcolumns %>%
    mutate(
      NativeRangeBioregions = ifelse(is.na(NativeRangeBioregions) | NativeRangeBioregions == "" | tolower(NativeRangeBioregions) == "na",
                                     "unknown", NativeRangeBioregions),
      InvadedBioregions = ifelse(is.na(InvadedBioregions) | InvadedBioregions == "" | tolower(InvadedBioregions) == "na",
                                 "unknown", InvadedBioregions)
    )

  # Crear tabla de conteos
  tabla <- table(MasterList_selectedcolumns$NativeRangeBioregions,
                 MasterList_selectedcolumns$InvadedBioregions)
  data <- as.data.frame(tabla)

  # CON ORIGEN NA (usa esta versión para la visualización)
  data_long <- data
  colnames(data_long) <- c("source", "target", "value")
  data_long$target <- paste(data_long$target, " ", sep="")  # para que nodos sean únicos

  # Nodos únicos
  nodes <- data.frame(name = unique(c(as.character(data_long$source), as.character(data_long$target))))

  # ID de cada nodo
  data_long$IDsource <- match(data_long$source, nodes$name) - 1
  data_long$IDtarget <- match(data_long$target, nodes$name) - 1

  # Tooltips personalizados
  data_long$tooltip <- paste0(data_long$source, " → ", data_long$target, ": ", data_long$value)

  # Totales por región como origen y destino
  data_long$source <- trimws(as.character(data_long$source))
  origen_totales <- aggregate(value ~ source, data = data_long, sum, na.rm = TRUE)
  origen_totales <- origen_totales[order(-origen_totales$value), ]
  names(origen_totales)[names(origen_totales) == "value"] <- "total_salida"

  # Limpiar espacios y convertir a character (por si acaso)
  data_long$target <- trimws(as.character(data_long$target))
  destino_totales <- aggregate(value ~ target, data = data_long, sum, na.rm = TRUE)
  destino_totales <- destino_totales[order(-destino_totales$value), ]
  names(destino_totales)[names(destino_totales) == "value"] <- "total_llegada"


  # Leyenda HTML con totales
  leyenda_totales <- tags$div(
    style = "text-align: center; color: #2C3E50; margin-bottom: 20px;",
    tags$h2("Sankey Diagram: Native Regions → Invaded Regions",
            style = "font-size: 24px; font-weight: bold;"),
    tags$div(
      style = "display: flex; justify-content: center; gap: 60px; font-size: 14px;",
      tags$div(
        tags$h4("Number of Species by Origin:"),
        tags$ul(
          lapply(1:nrow(origen_totales), function(i) {
            tags$li(paste(origen_totales$source[i], ":", origen_totales$total_salida[i]))
          })
        )
      ),
      tags$div(
        tags$h4("Number of Species by Destination:"),
        tags$ul(
          lapply(1:nrow(destino_totales), function(i) {
            tags$li(paste(destino_totales$target[i], ":", destino_totales$total_llegada[i]))
          })
        )
      )
    )
  )

  # Colores
  ColourScal <- 'd3.scaleOrdinal().range(["#FDE725FF","#B4DE2CFF","#6DCD59FF",
                                          "#35B779FF","#1F9E89FF","#26828EFF",
                                          "#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

  # Crear Sankey
  sankey <- sankeyNetwork(Links = data_long, Nodes = nodes,
                          Source = "IDsource", Target = "IDtarget",
                          Value = "value", NodeID = "name",
                          LinkGroup = "tooltip",
                          sinksRight = FALSE,
                          colourScale = ColourScal,
                          nodeWidth = 40, fontSize = 13, nodePadding = 20)

  # Mostrar leyenda + sankey
  browsable(
    tagList(
      tags$div(
        style = "display: flex; flex-direction: column; align-items: center;",
        leyenda_totales,
        sankey
      )
    )
  )
}
