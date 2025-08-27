########################################
###############Sources##################
########################################

########################################
########Custom "taxa" groups############
########################################

data <- read_excel("FinalFiles/FRIAS_SpeciesList_MasterList.xlsx")

# Define classification function
classify_group <- function(class, order, family) {
  if (!is.na(class)) {
    if (class %in% c("Magnoliopsida", "Liliopsida", "Polypodiopsida", "Pinopsida", "Lycopodiopsida")) return("Vascular plant")
    if (class %in% c("Bryopsida", "Marchantiopsida")) return("Bryophyte")
    if (class %in% c("Chlorophyceae", "Ulvophyceae", "Bacillariophyceae", "Charophyceae", "Florideophyceae", "Dinophyceae", "Prymnesiophyceae")) return("Algae")
    if (class %in% c("Sordariomycetes", "Dothideomycetes", "Leotiomycetes", "Lichinomycetes", "Rhizophydiomycetes")) return("Fungi")
    if (class == "Mammalia") return("Mammal")
    if (class == "Aves") return("Bird")
    if (class %in% c("Squamata", "Crocodylia", "Testudines")) return("Reptile")
    if (class == "Amphibia") return("Amphibian")
    if (class %in% c("Actinopterygii", "Dipneusti", "Petromyzonti", "Elasmobranchii")) return("Fish")
    if (class == "Insecta") return("Insect")
    if (class == "Arachnida") return("Arachnid")
    if (class %in% c("Malacostraca", "Branchiopoda", "Copepoda", "Ostracoda", "Maxillopoda")) return("Crustacean")
    if (class %in% c("Gastropoda", "Bivalvia")) return("Mollusk")
    return("Invertebrate")
  } else {
    if (!is.na(order)) {
      order <- tolower(order)
      if (grepl("cypriniformes|siluriformes|perciformes|salmoniformes|cichliformes|anabantiformes|eupercaria", order)) return("Fish")
    }
    if (!is.na(family)) {
      family <- tolower(family)
      if (grepl("cyprinidae|cichlidae|salmonidae|clariidae|centrarchidae|percidae", family)) return("Fish")
    }
    return("Not available")
  }
}

# Apply the classification
data$Group <- mapply(classify_group, data$Class, data$Order, data$Family)

# Count and filter non-empty groups
group_counts <- data %>%
  dplyr::count(Group) %>%
  filter(n > 0)

# Define tghe color palette
c15 <- c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
         "gold1", "skyblue2", "palegreen2", "#FDBF6F", "gray70",
         "maroon", "orchid1", "darkturquoise", "darkorange4", "brown")

# Plot the bar chart
ggplot(group_counts, aes(x = reorder(Group, -n), y = n, fill = Group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Biological Group",
       y = "Number of species") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c15)

########################################
###########Temporal trends##############
########################################

data <- read_excel("FinalFiles/FRIAS_SpeciesList_Masterlist.xlsx")

# Subset the FirstRecords column
dates <- data.frame(FirstRecords = as.factor(data$FirstRecords))

# Fix merged year dates by inserting a comma
dates$FirstRecords <- gsub("(\\d{4})(?=\\d{4})", "\\1,", dates$FirstRecords, perl = TRUE)

# Split comma-separated years into rows
all_years <- dates %>%
  separate_rows(FirstRecords, sep = ",") %>%
  filter(!is.na(FirstRecords), FirstRecords != "NA", FirstRecords != "") %>%
  mutate(FirstRecords = suppressWarnings(as.numeric(FirstRecords))) %>%
  filter(!is.na(FirstRecords), FirstRecords >= 1500)

year_counts <- all_years %>%
  dplyr::count(FirstRecords) %>%
  rename(Count = n) %>%
  arrange(FirstRecords)


# Scatter plot
ggplot(year_counts, aes(x = as.numeric(FirstRecords), y = Count)) +
  geom_point(color = "grey30", size = 3) +
  scale_x_continuous(
    breaks = seq(1500, max(as.numeric(year_counts$FirstRecords)), by = 50)
  ) +
  labs(x = "Year",
       y = "First record rate") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))
