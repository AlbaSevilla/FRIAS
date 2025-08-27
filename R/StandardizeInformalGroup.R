StandardizeInformalGroup <- function() {

  # 1. Leer archivo de equivalencias
  equivs <- read_excel("TablesToStandardize/Standardization_InformalGroups.xlsx", sheet = 1, col_names = TRUE) %>%
    mutate(
      Informal_group = tolower(Informal_group),
      Levels = tolower(Levels),
      Keywords = str_to_lower(str_trim(Keywords))
    ) %>%
    mutate(Keywords = str_split(Keywords, ";")) %>%
    unnest(Keywords) %>%
    mutate(Keywords = str_trim(Keywords))

  # 2. Leer datos y preparar
  dat <- read_excel("OutputFiles/Intermediate/Step5_ObtainTaxonNAS_MasterList.xlsx") %>%
    rename(phylum = Phylum) %>%
    rename(kingdom = Kingdom) %>%
    rename(class = Class) %>%
    rename(order = Order) %>%
    rename(family = Family) %>%
    mutate(across(-c(AcceptedNameGBIF, OriginalNameDB), ~str_to_lower(as.character(.))))%>%
    mutate(Group = str_replace_all(Group, "[_/\\-]", "")) %>%
    mutate(Group = str_split(Group, ";")) %>%
    unnest(Group) %>%
    mutate(Group = str_trim(Group)) %>%
    rename(group = Group)

  # 3. Crear columna vacía para almacenar resultado
  dat$Informal_group <- NA_character_

  # 4. Obtener niveles a procesar (solo los que están en el dataframe)
  niveles <- unique(equivs$Levels)
  niveles <- niveles[niveles %in% colnames(dat)]

  # 5. Reemplazo por coincidencia
  for (nivel in niveles) {
    cat("Procesando nivel:", nivel, "\n")

    sub_equivs <- equivs %>% filter(Levels == nivel)

    dat <- dat %>%
      rowwise() %>%
      mutate(
        Informal_group = if_else(
          is.na(Informal_group) & get(nivel) %in% sub_equivs$Keywords,
          sub_equivs$Informal_group[match(get(nivel), sub_equivs$Keywords)],
          Informal_group
        )
      ) %>%
      ungroup()
  }

  #No duplicados
  source(file.path("R","colapsar_por_AcceptedNameGBIF.r"))
  noduplicados <- colapsar_por_AcceptedNameGBIF(dat)

  #Eliminamos la columna 'group' y nos quedamos con la de InformalGroup
  names(noduplicados)
  noduplicados <- noduplicados[,c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF","kingdom","phylum","class","order",
                                  "family","EstablishmentMeans","Pathways","FunctionalGroup", "group","Informal_group",
                                  "NativeRange","AffectedNativeSpecies","EICATImpact",
                                  "Mechanisms","EICAT_by_Mechanism","Habitat","FirstRecords","OldestDate",
                                  "InvadedRange","InvadedRangeISO3","Source_Data")]
  noduplicados <- noduplicados %>%
    rename(Group = Informal_group) %>%
    rename(Kingdom = kingdom)%>%
    rename(phylum = phylum)%>%
    rename(Class = class)%>%
    rename(Order = order)%>%
    rename(Family = family)

 write_xlsx(noduplicados, "OutputFiles/Intermediate/Step6_StandardizedInformalGroup_Masterlist.xlsx")
}
