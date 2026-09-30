Sankey_bioregions <- function(){
  MasterList <- read_csv("FinalFiles/FRIAS_masterlist.csv")
  selected_columns <- c("NativeBioregions", "InvadedBioregions")
  MasterList_selectedcolumns <- MasterList[,selected_columns]
  clean_na_from_list <- function(x) {
    parts <- unlist(str_split(x, pattern = ";|,"))
    parts <- str_trim(parts)
    parts <- parts[!parts %in% c("NA", "na", "")]
    if(length(parts) == 0) return(NA_character_)
    paste(parts, collapse = ", ")
  }
  MasterList_selectedcolumns <- MasterList_selectedcolumns %>%
    mutate(
      NativeBioregions = sapply(NativeBioregions, clean_na_from_list),
      InvadedBioregions = sapply(InvadedBioregions, clean_na_from_list)
    )
  MasterList_selectedcolumns <- MasterList_selectedcolumns %>%
    mutate(InvadedBioregions = str_split(InvadedBioregions, ";")) %>%
    unnest(InvadedBioregions) %>%
    mutate(InvadedBioregions = str_trim(tolower(InvadedBioregions))) %>%
    mutate(InvadedBioregions = str_split(InvadedBioregions, ",")) %>%
    unnest(InvadedBioregions) %>%
    mutate(InvadedBioregions = str_trim(tolower(InvadedBioregions)))
  MasterList_selectedcolumns <- MasterList_selectedcolumns %>%
    mutate(NativeBioregions = str_split(NativeBioregions, ";")) %>%
    unnest(NativeBioregions) %>%
    mutate(NativeBioregions = str_trim(tolower(NativeBioregions))) %>%
    mutate(NativeBioregions = str_split(NativeBioregions, ", ")) %>%
    unnest(NativeBioregions) %>%
    mutate(NativeBioregions = str_trim(tolower(NativeBioregions)))
  MasterList_selectedcolumns <- MasterList_selectedcolumns %>%
    mutate(NativeBioregions = case_when(
      NativeBioregions %in% c("americas", "america") ~ "america",
      NativeBioregions %in% c("europa", "europe") ~ "europe",
      NativeBioregions %in% c("NA", "") ~ NA_character_,
      TRUE ~ NativeBioregions
    ),
    InvadedBioregions = case_when(
      InvadedBioregions %in% c("americas", "america") ~ "america",
      InvadedBioregions %in% c("europa", "europe") ~ "europe",
      InvadedBioregions %in% c("NA", "") ~ NA_character_,
      TRUE ~ InvadedBioregions
    ))
  MasterList_selectedcolumns <- MasterList_selectedcolumns %>%
    mutate(
      NativeBioregions = ifelse(is.na(NativeBioregions) | NativeBioregions == "" | tolower(NativeBioregions) == "na",
                                     "unknown", NativeBioregions),
      InvadedBioregions = ifelse(is.na(InvadedBioregions) | InvadedBioregions == "" | tolower(InvadedBioregions) == "na",
                                 "unknown", InvadedBioregions)
    )
  table_data <- table(MasterList_selectedcolumns$NativeBioregions,
                 MasterList_selectedcolumns$InvadedBioregions)
  data <- as.data.frame(table_data)
  data_long <- data
  colnames(data_long) <- c("source", "target", "value")
  data_long$target <- paste(data_long$target, " ", sep="")  # para que nodos sean únicos
  nodes <- data.frame(name = unique(c(as.character(data_long$source), as.character(data_long$target))))
  data_long$IDsource <- match(data_long$source, nodes$name) - 1
  data_long$IDtarget <- match(data_long$target, nodes$name) - 1
  data_long$tooltip <- paste0(data_long$source, " → ", data_long$target, ": ", data_long$value)
  data_long$source <- trimws(as.character(data_long$source))
  total_origins <- aggregate(value ~ source, data = data_long, sum, na.rm = TRUE)
  total_origins <- total_origins[order(-total_origins$value), ]
  names(total_origins)[names(total_origins) == "value"] <- "total_salida"
  data_long$target <- trimws(as.character(data_long$target))
  total_recipients <- aggregate(value ~ target, data = data_long, sum, na.rm = TRUE)
  total_recipients <- total_recipients[order(-total_recipients$value), ]
  names(total_recipients)[names(total_recipients) == "value"] <- "total_llegada"
  legend_t <- tags$div(
    style = "text-align: center; color: #2C3E50; margin-bottom: 20px;",
    tags$h2("Sankey Diagram: Native Regions → Invaded Regions",
            style = "font-size: 24px; font-weight: bold;"),
    tags$div(
      style = "display: flex; justify-content: center; gap: 60px; font-size: 14px;",
      tags$div(
        tags$h4("Number of Species by Origin:"),
        tags$ul(
          lapply(1:nrow(total_origins), function(i) {
            tags$li(paste(total_origins$source[i], ":", total_origins$total_salida[i]))
          })
        )
      ),
      tags$div(
        tags$h4("Number of Species by Destination:"),
        tags$ul(
          lapply(1:nrow(total_recipients), function(i) {
            tags$li(paste(total_recipients$target[i], ":", total_recipients$total_llegada[i]))
          })
        )
      )
    )
  )
  ColourScal <- 'd3.scaleOrdinal().range(["#FDE725FF","#B4DE2CFF","#6DCD59FF",
                                          "#35B779FF","#1F9E89FF","#26828EFF",
                                          "#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
  sankey <- sankeyNetwork(Links = data_long, Nodes = nodes,
                          Source = "IDsource", Target = "IDtarget",
                          Value = "value", NodeID = "name",
                          LinkGroup = "tooltip",
                          sinksRight = FALSE,
                          colourScale = ColourScal,
                          nodeWidth = 40, fontSize = 13, nodePadding = 20)
  browsable(
    tagList(
      tags$h2("Sankey Diagram: Native → Invaded Regions", style = "text-align:center;"),
      sankey
    )
  )


  # -------------------------------------------
  # CONTINENTS --------------------------------
  # -------------------------------------------
  MasterList <- read_csv("FinalFiles/FRIAS_masterlist.csv")
  selected_columns <- c("NativeBioregions", "InvadedBioregions")
  MasterList_selectedcolumns <- MasterList[,selected_columns]
  clean_na_from_list <- function(x) {
    parts <- unlist(str_split(x, pattern = ";|,"))
    parts <- str_trim(parts)
    parts <- parts[!parts %in% c("NA", "na", "")]
    if(length(parts) == 0) return(NA_character_)
    paste(parts, collapse = ", ")
  }
  MasterList_selectedcolumns <- MasterList_selectedcolumns %>%
    mutate(
      NativeBioregions = sapply(NativeBioregions, clean_na_from_list),
      InvadedBioregions = sapply(InvadedBioregions, clean_na_from_list)
    )
  MasterList_selectedcolumns <- MasterList_selectedcolumns %>%
    mutate(InvadedBioregions = str_split(InvadedBioregions, ";")) %>%
    unnest(InvadedBioregions) %>%
    mutate(InvadedBioregions = str_trim(tolower(InvadedBioregions))) %>%
    mutate(InvadedBioregions = str_split(InvadedBioregions, ",")) %>%
    unnest(InvadedBioregions) %>%
    mutate(InvadedBioregions = str_trim(tolower(InvadedBioregions)))
  MasterList_selectedcolumns <- MasterList_selectedcolumns %>%
    mutate(NativeBioregions = str_split(NativeBioregions, ";")) %>%
    unnest(NativeBioregions) %>%
    mutate(NativeBioregions = str_trim(tolower(NativeBioregions))) %>%
    mutate(NativeBioregions = str_split(NativeBioregions, ", ")) %>%
    unnest(NativeBioregions) %>%
    mutate(NativeBioregions = str_trim(tolower(NativeBioregions)))
  region_to_continent <- c(
    "northern africa" = "Africa", "eastern africa" = "Africa", "middle africa" = "Africa",
    "southern africa" = "Africa", "western africa" = "Africa",
    "caribbean" = "Americas", "central america" = "Americas",
    "south america" = "Americas", "northern america" = "Americas",
    "central asia" = "Asia", "eastern asia" = "Asia", "south-eastern asia" = "Asia",
    "southern asia" = "Asia", "western asia" = "Asia",
    "eastern europe" = "Europe", "northern europe" = "Europe",
    "southern europe" = "Europe", "western europe" = "Europe",
    "australia and new zealand" = "Oceania", "melanesia" = "Oceania",
    "micronesia" = "Oceania", "polynesia" = "Oceania",
    "unknown" = "Unknown"
  )
  MasterList_selectedcolumns <- MasterList_selectedcolumns %>%
    mutate(
      NativeBioregions = region_to_continent[tolower(NativeBioregions)],
      InvadedBioregions = region_to_continent[tolower(InvadedBioregions)])
  MasterList_selectedcolumns <- MasterList_selectedcolumns %>%
    mutate(NativeBioregions = case_when(
      NativeBioregions %in% c("americas", "america") ~ "america",
      NativeBioregions %in% c("europa", "europe") ~ "europe",
      NativeBioregions %in% c("NA", "") ~ NA_character_,
      TRUE ~ NativeBioregions
    ),
    InvadedBioregions = case_when(
      InvadedBioregions %in% c("americas", "america") ~ "america",
      InvadedBioregions %in% c("europa", "europe") ~ "europe",
      InvadedBioregions %in% c("NA", "") ~ NA_character_,
      TRUE ~ InvadedBioregions
    ))
  MasterList_selectedcolumns <- MasterList_selectedcolumns %>%
    mutate(
      NativeBioregions = ifelse(is.na(NativeBioregions) | NativeBioregions == "" | tolower(NativeBioregions) == "na",
                                "unknown", NativeBioregions),
      InvadedBioregions = ifelse(is.na(InvadedBioregions) | InvadedBioregions == "" | tolower(InvadedBioregions) == "na",
                                 "unknown", InvadedBioregions)
    )
  table_data <- table(MasterList_selectedcolumns$NativeBioregions,
                 MasterList_selectedcolumns$InvadedBioregions)
  data <- as.data.frame(table_data)
  data_long <- data
  colnames(data_long) <- c("source", "target", "value")
  data_long$target <- paste(data_long$target, " ", sep="")  # para que nodos sean únicos
  nodes <- data.frame(name = unique(c(as.character(data_long$source), as.character(data_long$target))))
  data_long$IDsource <- match(data_long$source, nodes$name) - 1
  data_long$IDtarget <- match(data_long$target, nodes$name) - 1
  data_long$tooltip <- paste0(data_long$source, " → ", data_long$target, ": ", data_long$value)
  data_long$source <- trimws(as.character(data_long$source))
  total_origins <- aggregate(value ~ source, data = data_long, sum, na.rm = TRUE)
  total_origins <- total_origins[order(-total_origins$value), ]
  names(total_origins)[names(total_origins) == "value"] <- "total_salida"
  data_long$target <- trimws(as.character(data_long$target))
  total_recipients <- aggregate(value ~ target, data = data_long, sum, na.rm = TRUE)
  total_recipients <- total_recipients[order(-total_recipients$value), ]
  names(total_recipients)[names(total_recipients) == "value"] <- "total_llegada"
  legend_t <- tags$div(
    style = "text-align: center; color: #2C3E50; margin-bottom: 20px;",
    tags$h2("Sankey Diagram: Native Regions → Invaded Regions",
            style = "font-size: 24px; font-weight: bold;"),
    tags$div(
      style = "display: flex; justify-content: center; gap: 60px; font-size: 14px;",
      tags$div(
        tags$h4("Number of Species by Origin:"),
        tags$ul(
          lapply(1:nrow(total_origins), function(i) {
            tags$li(paste(total_origins$source[i], ":", total_origins$total_salida[i]))
          })
        )
      ),
      tags$div(
        tags$h4("Number of Species by Destination:"),
        tags$ul(
          lapply(1:nrow(total_recipients), function(i) {
            tags$li(paste(total_recipients$target[i], ":", total_recipients$total_llegada[i]))
          })
        )
      )
    )
  )
  ColourScal <- 'd3.scaleOrdinal().range(["#FDE725FF","#B4DE2CFF","#6DCD59FF",
                                          "#35B779FF","#1F9E89FF","#26828EFF",
                                          "#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
  sankey <- sankeyNetwork(Links = data_long, Nodes = nodes,
                          Source = "IDsource", Target = "IDtarget",
                          Value = "value", NodeID = "name",
                          LinkGroup = "tooltip",
                          sinksRight = FALSE,
                          colourScale = ColourScal,
                          nodeWidth = 40, fontSize = 13, nodePadding = 20)
  browsable(
    tagList(
      tags$h2("Sankey Diagram: Native → Invaded Regions", style = "text-align:center;"),
      sankey
    )
  )
}
