Duplicates_uniques_species_databases <- function(){
  master_list <- read_excel("FinalFiles/FRIAS_masterlist.xlsx")
  master_expanded <- master_list %>%
    mutate(Source_Data = str_split(Source_Data, ",")) %>%
    unnest(Source_Data) %>%
    mutate(Source_Data = str_trim(str_to_lower(Source_Data))) %>%
    mutate(Source_Data = str_split(Source_Data, ";")) %>%
    unnest(Source_Data) %>%
    mutate(Source_Data = str_trim(str_to_lower(Source_Data)))
  species_db <- master_expanded %>%
    group_by(AcceptedNameGBIF, Source_Data) %>%
    dplyr::summarise(present = 1, .groups = "drop") %>%
    pivot_wider(names_from = Source_Data, values_from = present, values_fill = 0)
  species_matrix <- species_db %>% select(-AcceptedNameGBIF) %>% as.matrix()
  rownames(species_matrix) <- species_db$AcceptedNameGBIF
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
  )
  dataset_counts <- dataset_counts %>%
    mutate(total = unique + shared) %>%
    arrange(desc(total)) %>%
    slice_head(n = 10)
  dataset_counts <- dataset_counts %>%
    arrange(desc(total)) %>%
    mutate(database = factor(database, levels = database))  # mantener orden para ggplot
  dataset_long <- pivot_longer(dataset_counts, cols = c("unique","shared"), names_to = "type", values_to = "count")
  ggplot(dataset_long, aes(x = database, y = count, fill = type)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = count),
              position = position_stack(vjust = 0.5),  # centra el texto dentro de la barra
              color = "black", size = 3) +
    scale_fill_manual(values = c("unique" = "lightgreen", "shared" = "seagreen")) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(x = "Database", y = "Species", fill = "Type") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  #Save
  ggsave("Figures/UniqueVsSharedSpeciesByDatabaseTop10.png",
         dpi = 300, width = 10, height = 6)
  ggsave("Figures/Figure3.tiff",
         dpi = 300, width = 10, height = 6)
}
