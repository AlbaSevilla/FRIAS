NoMicroorganism_MasterList <- function(){
  MasterList <- read_excel("OutputFiles/Intermediate/Step8_ObtainTaxonNAS_MasterList.xlsx")
  names(MasterList)
  MasterList <- MasterList %>%
    mutate(Kingdom = str_split(Kingdom, ";")) %>%
    unnest(Kingdom) %>%
    mutate(
      Kingdom = tolower(str_trim(Kingdom)))
  #Cambiamos metazoa por animales
  MasterList$Kingdom <- gsub("metazoa", "animalia", MasterList$Kingdom)

  table(MasterList$Kingdom)
  unique(MasterList$Kingdom)
  unique(MasterList$Phylum)
  unique(MasterList$Class)

  #Eliminamos los microorganismos
   # MasterList2 <- MasterList %>%
   #   filter(!grepl("bacteria", Kingdom))
   # MasterList3 <- MasterList2 %>%
   #   filter(!grepl("chromista", Kingdom))
   # MasterList4 <- MasterList3 %>%
   #   filter(!grepl("monera", Kingdom))
   # MasterList5 <- MasterList4 %>%
   #    filter(!grepl("riboviria", Kingdom))
   # MasterList7 <- MasterList5 %>%
   #   filter(!grepl("viruses", Kingdom))
   # MasterList8 <- MasterList7 %>%
   #   filter(!grepl("Flasuviricetes", Class))
   # MasterList9 <- MasterList8 %>%
   #   filter(!grepl("Microsporea", Class))
   # MasterList10 <- MasterList9 %>%
   #   filter(!grepl("Myxozoa", Kingdom))

  # Listas de valores a excluir por columna
  kingdom_exclude <- c(
    "bacteria", "chromista", "monera", "riboviria", "viruses", "Myxozoa",
    "Bacillati", "Euryarchaeota", "Methanobacteriati", "Monodnaviria",
    "Nanobdellati", "Orthornavirae", "Promethearchaeati", "Pseudomonadati",
    "Shotokuvirae", "Thermoproteati", "Thermotogati",
    "Apicomplexa", "Bacillariophyta", "Bacillota", "Cercozoa", "Chytridiomycota",
    "Ciliophora", "Discosea", "Euglenozoa", "Foraminifera", "Fornicata",
    "Microsporidia", "Miozoa", "Oomycota", "Pseudomonadota", "Tubulinea"
  )

  phylum_exclude <- c(
    "bacteria", "chromista", "monera", "riboviria", "viruses", "Myxozoa",
    "Bacillati", "Euryarchaeota", "Methanobacteriati", "Monodnaviria",
    "Nanobdellati", "Orthornavirae", "Promethearchaeati", "Pseudomonadati",
    "Shotokuvirae", "Thermoproteati", "Thermotogati",
    "Apicomplexa", "Bacillariophyta", "Bacillota", "Cercozoa", "Chytridiomycota",
    "Ciliophora", "Discosea", "Euglenozoa", "Foraminifera", "Fornicata",
    "Microsporidia", "Miozoa", "Oomycota", "Pseudomonadota", "Tubulinea"
  )

  class_exclude <- c(
    "Dinophyceae", "Flasuviricetes", "Glaucocystophyceae", "Saccharomycetes", "Copepoda",
    "bacteria", "chromista", "monera", "riboviria", "viruses", "Myxozoa",
    "Bacillati", "Euryarchaeota", "Methanobacteriati", "Monodnaviria",
    "Nanobdellati", "Orthornavirae", "Promethearchaeati", "Pseudomonadati",
    "Shotokuvirae", "Thermoproteati", "Thermotogati",
    "Apicomplexa", "Bacillariophyta", "Bacillota", "Cercozoa", "Chytridiomycota",
    "Ciliophora", "Discosea", "Euglenozoa", "Foraminifera", "Fornicata",
    "Microsporidia", "Miozoa", "Oomycota", "Pseudomonadota", "Tubulinea"
  )

  # Aplicar filtros
  MasterList_clean <- MasterList %>%
    filter(
      !tolower(Kingdom) %in% tolower(kingdom_exclude),
      !tolower(Phylum) %in% tolower(phylum_exclude),
      !tolower(Class) %in% tolower(class_exclude)
    )


  sort(unique(MasterList_clean$Kingdom))
  sort(unique(MasterList_clean$Phylum))
  sort(unique(MasterList_clean$Class))

  #noduplicados
  source(file.path("R", "colapsar_por_AcceptedNameGBIF.r"))
  noduplicates <- colapsar_por_AcceptedNameGBIF(MasterList_clean)

  write.xlsx(noduplicates, "OutputFiles/Intermediate/Step9_NoMicroorganism_MasterList.xlsx")
}
