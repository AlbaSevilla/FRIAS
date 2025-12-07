Over_Sub_Representation_InformalGroup <- function(){
# -------------------------
# 1. Datos
# -------------------------
Taxa <- c(
  "Algae",
  "Amphibians",
  "Arachnids",
  "Bacteria and protozoans",
  "Birds",
  "Bryophytes",
  "Crustaceans",
  "Fishes",
  "Fungi",
  "Insects",
  "Mammals",
  "Molluscs",
  "Reptiles",
  "SAR",
  "Vascular plants"
)
masterlist <- read.xlsx("FinalFiles/FRIAS_masterlist.xlsx")
proportions <- table(masterlist$Group)
# Valores de MasterList_Count
MasterList_Count <- c(8, 92, 2, 15, 199, 4, 426, 1430, 10, 51, 15, 220, 52, 339, 574)

# Valores de FW_Total
FW_Total <- c(2732, 5089, 6149, 2254, 764, 83, 11990, 19138, 2000, 75874, 127, 4998, 639, 4545, 2614)

# -------------------------
# 2. Proporciones esperadas
# -------------------------
expected_prop <- FW_Total / sum(FW_Total)
expected_counts <- expected_prop * sum(MasterList_Count)

# -------------------------
# 3. Test chi-cuadrado
# -------------------------
chi <- chisq.test(x = MasterList_Count, p = expected_prop)
chi$p.value

# -------------------------
# 4. Crear tabla final con residuales estandarizados
# -------------------------
library(dplyr)

tabla <- data.frame(
  Taxa,
  Observed = MasterList_Count,
  Expected = expected_counts,
  StdResidual = chi$stdres
) %>%
  mutate(
    Representation = case_when(
      StdResidual > 2 ~ "Overrepresented",
      StdResidual < -2 ~ "Underrepresented",
      TRUE ~ "Neutral"
    ),
    Color = case_when(
      Representation == "Overrepresented" ~ "lightpink",
      Representation == "Underrepresented" ~ "lightblue",
      TRUE ~ "grey80"
    )
  )

tabla


# -------------------------
# 5. Gráfico con colores y sin cuadrícula
# -------------------------
library(ggplot2)
p <- ggplot(tabla, aes(x = reorder(Taxa, StdResidual), y = StdResidual, fill = Color)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_identity() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed") +
  coord_flip() +
  labs(
    x = "Taxonomic group",
    y = "Standardized residuals"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p
ggsave("Figures/Figure6.png", plot = p, width = 10, height = 6, dpi = 300)
ggsave("Figures/Figure6.tiff", plot = p, width = 10, height = 6, dpi = 300, compression = "lzw")



}
