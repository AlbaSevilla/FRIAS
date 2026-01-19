Groups_through_RecipientBioregions <- function(){
  dat <- read_csv("FinalFiles/FRIAS_masterlist.csv")

  counts <- dat %>%
    separate_rows(RecipientBioregions, sep = ";|,") %>%
    separate_rows(Group, sep = ";|,") %>%
    mutate(
      Group = tolower(trimws(Group)),
      RecipientBioregions = str_trim(RecipientBioregions)
    ) %>%
    group_by(RecipientBioregions, Group) %>%
    dplyr::summarise(
      SpeciesCount = n_distinct(AcceptedNameGBIF),
      .groups = "drop"
    ) %>%
    filter(Group != "")

  counts <- counts %>%
    mutate(
      Continent = case_when(
        str_detect(RecipientBioregions, regex("Africa", ignore_case = TRUE)) ~ "Africa",
        str_detect(RecipientBioregions, regex("Asia", ignore_case = TRUE)) ~ "Asia",
        str_detect(RecipientBioregions, regex("Europe", ignore_case = TRUE)) ~ "Europe",
        str_detect(RecipientBioregions, regex("America|Caribbean", ignore_case = TRUE)) ~ "Americas",
        str_detect(
          RecipientBioregions,
          regex("Oceania|Australia|New Zealand|Melanesia|Micronesia|Polynesia", ignore_case = TRUE)
        ) ~ "Oceania",
        TRUE ~ NA_character_
      ),
      Group5 = case_when(
        Group %in% c("fish", "amphibian", "bird", "mammal", "reptile") ~ "Vertebrates",
        Group %in% c("other invertebrate", "crustacean", "insect", "mollusk", "arachnid") ~ "Invertebrates",
        Group %in% c("vascular plant", "bryophyte") ~ "Plants",
        Group %in% c("bacteria or protozoan", "fungus", "sar", "alga") ~ "Other",
        TRUE ~ "Other"
      )
    )

  counts_complete <- counts %>%
    group_by(Continent, Group5) %>%
    dplyr::summarise(
      SpeciesCount = sum(SpeciesCount),
      .groups = "drop"
    )

  all_groups <- unique(counts_complete$Group5)
  all_continents <- unique(counts_complete$Continent)

  counts_complete <- expand.grid(
    Continent = all_continents,
    Group5 = all_groups,
    stringsAsFactors = FALSE
  ) %>%
    left_join(counts_complete, by = c("Continent", "Group5")) %>%
    mutate(SpeciesCount = ifelse(is.na(SpeciesCount), 0, SpeciesCount)) %>%
    filter(!is.na(Continent))

  ## ---- CALCULATE TOTAL PER CONTINENT AND FIX ORDER ----
  continent_totals <- counts_complete %>%
    group_by(Continent) %>%
    dplyr::summarise(Total = sum(SpeciesCount), .groups = "drop")

  counts_complete <- counts_complete %>%
    left_join(continent_totals, by = "Continent") %>%
    mutate(
      Continent = factor(
        Continent,
        levels = continent_totals %>%
          arrange(Total) %>%
          pull(Continent)
      )
    )

  p <- ggplot(
    counts_complete,
    aes(
      x = SpeciesCount,
      y = Continent,
      fill = Group5
    )
  ) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_viridis_d(option = "plasma") +
    labs(
      x = "Number of Invasive and Alien Species",
      y = "Continent",
      fill = "Group Category"
    ) +
    theme_minimal(base_size = 22) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      axis.text.x = element_text(size = 20, colour = "black"),
      axis.text.y = element_text(size = 20, colour = "black"),
      axis.title = element_text(size = 22, colour = "black"),
      axis.line = element_line(colour = "black")
    )

  print(p)

  ggsave(
    filename = "Figures/Figure4a.png",
    plot = p,
    width = 12,
    height = 8,
    dpi = 300
  )

  ggsave(
    filename = "Figures/Figure4a.svg",
    plot = p,
    width = 12,
    height = 8,
    dpi = 300
  )
}
