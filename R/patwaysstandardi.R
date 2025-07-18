Pathway_levels <- read_excel("TablesToStandardize/Pathway_levels.xlsx")


Pathway_levels <- Pathway_levels %>%
  group_by(Low_level)%>%
  summarise(across(everything(), ~ paste(unique(.), collapse = ";"), .names = "{.col}")) %>%
  arrange(High_level)

View(Pathway_levels)

write_xlsx(Pathway_levels, "TablesToStandardize/Pathway_levels2.xlsx")
