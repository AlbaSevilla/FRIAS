NoMicroorganism_MasterList <- function(){
  MasterList <- read_excel("OutputFiles/Intermediate/Step6_StandardizedInformalGroup_Masterlist.xlsx")
  names(MasterList)
  MasterList <- MasterList %>%
    rename(Phylum = phylum) %>%
    # Explota cada columna de forma independiente
    separate_rows(Kingdom, sep = ";|,") %>%
    separate_rows(Phylum,  sep = ";|,") %>%
    separate_rows(Class,   sep = ";|,") %>%
    separate_rows(Order,   sep = ";|,") %>%
    separate_rows(Family,  sep = ";|,") %>%
    # Limpieza de espacios + pasar a minúsculas
    mutate(across(c(Kingdom, Phylum, Class, Order, Family),
                  ~ str_to_lower(str_trim(.))))


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
    "bacteria", "chromista", "monera", "riboviria",
    "viruses", "myxozoa", "bacillati", "euryarchaeota", "methanobacteriati",
    "monodnaviria", "nanobdellati", "orthornavirae", "promethearchaeati",
    "pseudomonadati","shotokuvirae", "thermoproteati", "thermotogati",
    "apicomplexa", "bacillariophyta", "bacillota", "cercozoa", "chytridiomycota",
    "ciliophora", "discosea", "euglenozoa", "foraminifera", "fornicata",
    "microsporidia", "miozoa", "oomycota", "pseudomonadota",
    "tubulinea", "bacillariophyceae")

  phylum_exclude <- c(
    "bacteria", "chromista", "monera", "riboviria",
    "viruses", "myxozoa", "bacillati", "euryarchaeota", "methanobacteriati",
    "monodnaviria", "nanobdellati", "orthornavirae", "promethearchaeati",
    "pseudomonadati","shotokuvirae", "thermoproteati", "thermotogati",
    "apicomplexa", "bacillariophyta", "bacillota", "cercozoa", "chytridiomycota",
    "ciliophora", "discosea", "euglenozoa", "foraminifera", "fornicata",
    "microsporidia", "miozoa", "oomycota", "pseudomonadota",
    "tubulinea", "bacillariophyceae")

  class_exclude <- c(
    "bacteria", "chromista", "monera", "riboviria",
    "viruses", "myxozoa", "bacillati", "euryarchaeota", "methanobacteriati",
    "monodnaviria", "nanobdellati", "orthornavirae", "promethearchaeati",
    "pseudomonadati","shotokuvirae", "thermoproteati", "thermotogati",
    "apicomplexa", "bacillariophyta", "bacillota", "cercozoa", "chytridiomycota",
    "ciliophora", "discosea", "euglenozoa", "foraminifera", "fornicata",
    "microsporidia", "miozoa", "oomycota", "pseudomonadota",
    "tubulinea", "bacillariophyceae")

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
  MasterList2 <- read_excel("OutputFiles/Intermediate/Step6_StandardizedInformalGroup_Masterlist.xlsx")

  dim(MasterList2)
  #noduplicados
  source(file.path("R", "noduplicates.r"))
  noduplicates <- noduplicates(MasterList_clean, "AcceptedNameGBIF")
  dim(noduplicates)

  #De las columnas Kingdom, Order, Class, Phylum y Family nos quedamos con el primer elemento de los separados por ,
  noduplicates2 <- noduplicates %>%
    mutate(across(c(Kingdom, Phylum, Class, Order, Family),
                  ~ str_split(., ";|,") %>%
                    sapply(function(x) str_to_lower(str_trim(x[1])))))
  dim(noduplicates2)

  write.xlsx(noduplicates2, "OutputFiles/Intermediate/Step7_NoMicroorganism_MasterList.xlsx")
}
