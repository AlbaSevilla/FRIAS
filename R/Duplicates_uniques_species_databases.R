Duplicates_uniques_species_databases <- function(){
  #1
  master_list <- read_csv("FinalFiles/(Table S3) FRIAS_masterlist.csv")
  master_expanded <- master_list %>%
    mutate(Source_Data = str_split(Source_Data, ",")) %>%
    unnest(Source_Data) %>%
    mutate(Source_Data = str_trim(str_to_lower(Source_Data))) %>%
    mutate(Source_Data = str_split(Source_Data, ";")) %>%
    unnest(Source_Data) %>%
    mutate(Source_Data = str_trim(str_to_lower(Source_Data)))

  #2
  species_db <- master_expanded %>%
    group_by(AcceptedNameGBIF, Source_Data) %>%
    dplyr::summarise(present = 1, .groups = "drop") %>%
    pivot_wider(names_from = Source_Data, values_from = present, values_fill = 0)

  species_matrix <- species_db %>% select(-AcceptedNameGBIF) %>% as.matrix()
  rownames(species_matrix) <- species_db$AcceptedNameGBIF

  #3
  unique_counts <- sapply(1:ncol(species_matrix), function(i) {
    sum(species_matrix[,i] == 1 & rowSums(species_matrix) == 1)
  })
  shared_counts <- sapply(1:ncol(species_matrix), function(i) {
    sum(species_matrix[,i] == 1 & rowSums(species_matrix) > 1)
  })

  dataset_counts_total <- data.frame(
    database = colnames(species_matrix),
    unique = unique_counts,
    shared = shared_counts
  ) %>%
    mutate(total = unique + shared) %>%
    arrange(desc(total))

  dataset_counts <- data.frame(
    database = colnames(species_matrix),
    unique = unique_counts,
    shared = shared_counts
  ) %>%
    mutate(total = unique + shared) %>%
    arrange(desc(total)) %>%
    slice_head(n = 10)

  #4
  ref <- read.xlsx("TablesToStandardize/Table S1.xlsx", sheet = "Databases")
  db_ref <- unique(c(ref$Column_SourceData, ref$FRIAS_name))
  db_ref <- db_ref[!is.na(db_ref)]

  original <- dataset_counts$database
  matched <- sapply(original, function(x) {
    dists <- stringdist(tolower(x), tolower(db_ref), method = "lv")
    db_ref[which.min(dists)]
  })

  dataset_counts$database <- matched

  #5
  dataset_counts$database <- ifelse(
    trimws(tolower(dataset_counts$database)) == "global_compendium_of_weeds_2025",
    "GWC 2025",
    dataset_counts$database
  )

  dataset_counts <- dataset_counts %>%
    arrange(desc(total)) %>%
    mutate(database = factor(database, levels = database))

  #6
  dataset_long <- pivot_longer(
    dataset_counts,
    cols = c("unique", "shared"),
    names_to = "type",
    values_to = "count"
  )

  # Calcular el total de especies por database
  dataset_long <- dataset_long %>%
    group_by(database) %>%
    mutate(
      total = sum(count),
      percentage = round(count / total * 100)
    ) %>%
    ungroup()

  # Crear etiqueta: n (x%)
  dataset_long <- dataset_long %>%
    group_by(database) %>%
    mutate(
      total = sum(count),
      percentage = round(count / total * 100),
      label = ifelse(
        count == 0,
        "",
        paste0(count, "\n(", percentage, "%)")
      )
    ) %>%
    ungroup()

  plot <- ggplot(dataset_long, aes(x = database, y = count, fill = type)) +
    geom_bar(stat = "identity") +

    # Números dentro de las barras
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      color = "black",
      size = 7
    ) +

    scale_fill_manual(
      values = c(
        "unique" = "lightgreen",
        "shared" = "seagreen"
      )
    ) +

    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      axis.line = element_line(color = "black", linewidth = 0.8),
      axis.ticks = element_line(color = "black", linewidth = 0.8),

      axis.title = element_text(size = 20, face = "bold"),
      axis.text = element_text(size = 18),

      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 18
      ),

      legend.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 18)
    ) +

    labs(
      x = "Database",
      y = "Number of Species",
      fill = "Type"
    )


  plot

  #Save
  ggsave("Figures/Figure3.png",plot,
         dpi = 300, width = 14, height = 12)
  ggsave("Figures/Figure3.svg",plot,
         dpi = 300, width = 14, height = 12)
  ggsave("Figures/Figure3.tiff",plot,
         dpi = 300, width = 14, height = 12)
}
