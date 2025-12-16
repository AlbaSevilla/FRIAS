Duplicates_uniques_species_databases <- function(){
  # ==============================
  # 1. Cargar y expandir master list
  # ==============================
  master_list <- read_excel("FinalFiles/FRIAS_masterlist.xlsx")
  master_expanded <- master_list %>%
    mutate(Source_Data = str_split(Source_Data, ",")) %>%
    unnest(Source_Data) %>%
    mutate(Source_Data = str_trim(str_to_lower(Source_Data))) %>%
    mutate(Source_Data = str_split(Source_Data, ";")) %>%
    unnest(Source_Data) %>%
    mutate(Source_Data = str_trim(str_to_lower(Source_Data)))

  # ==============================
  # 2. Crear matriz de presencia/ausencia
  # ==============================
  species_db <- master_expanded %>%
    group_by(AcceptedNameGBIF, Source_Data) %>%
    dplyr::summarise(present = 1, .groups = "drop") %>%
    pivot_wider(names_from = Source_Data, values_from = present, values_fill = 0)

  species_matrix <- species_db %>% select(-AcceptedNameGBIF) %>% as.matrix()
  rownames(species_matrix) <- species_db$AcceptedNameGBIF

  # ==============================
  # 3. Contar especies únicas y compartidas
  # ==============================
  unique_counts <- sapply(1:ncol(species_matrix), function(i) {
    sum(species_matrix[,i] == 1 & rowSums(species_matrix) == 1)
  })
  shared_counts <- sapply(1:ncol(species_matrix), function(i) {
    sum(species_matrix[,i] == 1 & rowSums(species_matrix) > 1)
  })

  dataset_counts <- data.frame(
    database = colnames(species_matrix),
    unique = unique_counts,
    shared = shared_counts
  ) %>%
    mutate(total = unique + shared) %>%
    arrange(desc(total)) %>%
    slice_head(n = 10)

  # ==============================
  # 4. Cargar referencia y hacer fuzzy matching
  # ==============================
  ref <- read.xlsx("TablesToStandardize/Table S1.xlsx", sheet = "Databases")
  db_ref <- unique(c(ref$Column_SourceData, ref$FRIAS_name))
  db_ref <- db_ref[!is.na(db_ref)]

  original <- dataset_counts$database
  matched <- sapply(original, function(x) {
    dists <- stringdist(tolower(x), tolower(db_ref), method = "lv")
    db_ref[which.min(dists)]
  })

  dataset_counts$database <- matched

  # ==============================
  # 5. Reemplazo específico GWC
  # ==============================
  dataset_counts$database <- ifelse(
    trimws(tolower(dataset_counts$database)) == "global_compendium_of_weeds_2025",
    "GWC 2025",
    dataset_counts$database
  )

  dataset_counts <- dataset_counts %>%
    arrange(desc(total)) %>%
    mutate(database = factor(database, levels = database))

  # ==============================
  # 6. Preparar datos para ggplot
  # ==============================
  dataset_long <- pivot_longer(dataset_counts, cols = c("unique","shared"),
                               names_to = "type", values_to = "count")

  # ==============================
  # 7. Crear gráfico
  # ==============================
  plot <- ggplot(dataset_long, aes(x = database, y = count, fill = type)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = count),
              position = position_stack(vjust = 0.5),
              color = "black", size = 4) +
    scale_fill_manual(values = c("unique" = "lightgreen", "shared" = "seagreen")) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.8),
      axis.ticks = element_line(color = "black", linewidth = 0.8),
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 16)
    ) +
    labs(x = "Database", y = "Density of Species", fill = "Type")

  plot


  #Save
  ggsave("Figures/Figure3.png",plot,
         dpi = 300, width = 10, height = 6)
  ggsave("Figures/Figure3.svg",plot,
         dpi = 300, width = 10, height = 6)
}
