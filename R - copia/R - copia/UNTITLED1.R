FRIAS_SpeciesList_MasterList <- read_excel("OutputFiles/Final/FRIAS_SpeciesList_MasterList.xlsx")

# Separar mecanismos múltiples, limpiar
FRIAS_SpeciesList_MasterList <- FRIAS_SpeciesList_MasterList %>%
  mutate(Group = str_split(Group, ",")) %>%
  unnest(Group) %>%
  mutate(
    Group = tolower(str_trim(Group)),
    )
unique(FRIAS_SpeciesList_MasterList$Group)

View(FRIAS_SpeciesList_MasterList %>% filter(Group == "est"))
