SuccessandFailRateofCheckHabitatFunction <- function(){
  table_rates <- tibble::tribble(
    ~Habitat, ~Success, ~Fail, ~Total, ~SuccessRate, ~FailRate,
    "Freshwater", 47, 1, 48, 0.979, 0.0208,
    "Freshwater, Marine", 3, 0, 3, 1, 0,
    "Freshwater, Marine, Terrestrial", 1, 0, 1, 1, 0,
    "Freshwater, Terrestrial", 6, 1, 7, 0.857, 0.143,
    "Marine", 3, 1, 4, 0.75, 0.25,
    "Terrestrial", 24, 3, 27, 0.889, 0.111,
    "Terrestrial, Marine", 1, 0, 1, 1, 0
  ) |>
    mutate(Habitat = fct_rev(fct_inorder(Habitat)))

  # ---- 1. Dot plot / Lollipop ----
  p1 <- ggplot(table_rates, aes(x = SuccessRate, y = Habitat)) +
    geom_segment(aes(x = 0, xend = SuccessRate, yend = Habitat),
                 color = "grey70") +
    geom_point(aes(size = Total), color = "#28A87D") +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_size(range = c(3, 10)) +
    labs(title = "Tasa de Fallo por hábitat (Lollipop)",
         x = "Tasa de Fallo", y = "Hábitat",
         size = "Tamaño muestra") +
    theme_minimal(base_size = 14)
  p1
  # ---- 2. Bubble chart ----
  p2 <- ggplot(table_rates, aes(x = Habitat, y = SuccessRate, size = Total, color = SuccessRate)) +
    geom_point(alpha = 0.7) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_size(range = c(3, 12)) +
    scale_color_gradient(low = "#EFAC00", high = "#28A87D") +
    coord_flip() +
    labs(title = "Tasa de Fallo por hábitat (Bubble chart)",
         y = "Tasa de Fallo", x = "Hábitat",
         size = "Muestra", color = "Tasa éxito") +
    theme_minimal(base_size = 14)
  p2
  # ---- Bubble chart con Habitat en X y Rate en Y ----
  p2 <- ggplot(table_rates, aes(x = Habitat, y = SuccessRate, size = Total, color = SuccessRate)) +
    geom_point(alpha = 1) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_size(range = c(3, 12)) +
    scale_color_gradient(low = "#C2A5CF", high = "#5AAE61") +
    labs(title = "Success and Fail rate of check_habitat() on GRIIS subset",
         y = "Success Rate", x = "Hábitat",
         size = "Size Sample", color = "Success Rate") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p2
}
