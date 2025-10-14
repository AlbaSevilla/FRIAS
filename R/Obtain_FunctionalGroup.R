Obtain_FunctionalGroup <- function(){
  MasterList <- read.xlsx("OutputFiles/Intermediate/Step6_StandardizedInformalGroup_MasterList.xlsx") %>%
    separate_rows(FunctionalGroup, sep=";") %>%
    separate_rows(FunctionalGroup, sep=",")

  correct_categories <- read_excel(
    "TablesToStandardize/standardization_tables.xlsx",
    sheet = "functionalgroup_globi_table"
  ) %>%
    pull(`Functional Group`) %>%
    unique()

  MasterList <- MasterList %>%
    mutate(
      FunctionalGroup = ifelse(FunctionalGroup %in% correct_categories, FunctionalGroup, NA)
    )

  MasterList0 <- MasterList %>% filter(!is.na(FunctionalGroup))
  MasterList <- MasterList %>% filter(is.na(FunctionalGroup))

  ########################################
  ### 1 FISHBASE  ########################
  ########################################
  Fishbase_table <- read_excel(
    "TablesToStandardize/standardization_tables.xlsx",
    sheet = "functionalgroup_fishbase_table"
  ) %>%
    mutate(FoodTroph = str_replace_all(FoodTroph, ",", "."))

  # 2. Obtain species with missing information in Functional Group
  nas_species <- MasterList %>%
    filter(is.na(FunctionalGroup)) %>%
    pull(AcceptedNameGBIF)

  # 3. Download Functional Group information about these species in Fishbase
  dataset_functionalgroup <- fooditems(nas_species, fields = c("Species", "FoodI")) %>%
    left_join(ecology(nas_species, fields = c("Species", "FeedingType", "FoodTroph")), by = "Species") %>%
    select(Species, FeedingType, FoodI, FoodTroph)

  # 4. translation of text with symbols such as ‘>’ or ‘-’ into a real number range that the code can use for comparison
  parse_range <- function(range_str) {
    if (is.na(range_str)) return(c(NA_real_, NA_real_))
    if (str_detect(range_str, ">")) return(c(as.numeric(str_extract(range_str, "\\d+\\.\\d+")), Inf))
    if (str_detect(range_str, "-")) return(as.numeric(str_split(range_str, "-", simplify = TRUE)))
    c(NA_real_, NA_real_)
  }

  # 5. Associate FunctionalGroup in terms of FeedingType and/or FoodTroph
  dataset_functionalgroup <- dataset_functionalgroup %>%
    rowwise() %>%
    mutate(
      FunctionalGroupFishbase = {
        group <- NA_character_
        candidates <- if (!is.na(FeedingType)) filter(Fishbase_table, FeedingType == FeedingType) else Fishbase_table

        if (!is.na(FoodTroph)) {
          for (i in seq_len(nrow(candidates))) {
            r <- parse_range(candidates$FoodTroph[i])
            if (!any(is.na(r)) && FoodTroph >= r[1] && FoodTroph <= r[2]) {
              group <- candidates$Functional_Groups[i]
              break
            }
          }
        }
        if (is.na(group) && nrow(candidates) > 0) group <- candidates$Functional_Groups[1]
        group
      }
    ) %>%
    ungroup() %>%
    select(Species, FunctionalGroupFishbase)


  dataset_functionalgroup_summary <- dataset_functionalgroup %>%
    group_by(Species, FunctionalGroupFishbase) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    group_by(Species) %>%
    mutate(total = sum(n),
           perc = round(100 * n / total, 1),   # porcentaje correcto
           label = paste0(FunctionalGroupFishbase, " (", perc, "%)")) %>%
    dplyr::summarise(FG_summary = paste(label, collapse = ", "), .groups = "drop")

  # Result
  mergedatasets <- MasterList %>%
    left_join(dataset_functionalgroup_summary, by = c("AcceptedNameGBIF" = "Species")) %>%
    mutate(
      FunctionalGroup = paste(FunctionalGroup, FG_summary, sep = ";") %>%
        str_replace_all(";NA|NA;|NA", "")
    ) %>%
    select(-FG_summary)

  cols <- setdiff(names(mergedatasets), "Source_Data")
  mergedatasets[cols] <- lapply(mergedatasets[cols], function(x) replace(x, x %in% c("", "NA"), NA))


  FillGaps_Masterlist_Fishbase <- mergedatasets %>% filter(!is.na(FunctionalGroup)) #Los na solucionados con Fishbase

  MasterList2 <- mergedatasets %>% filter(is.na(FunctionalGroup)) #Los que todavia tienen na
  unique(MasterList2$FunctionalGroup)

  ##################################################
  ###  2 GLOBI #####################################
  ##################################################
  clean_tax <- function(x) {
    if (is.na(x)) return(NA_character_)
    x <- str_split(x, ",")[[1]] |> trimws() |> sort()
    paste(x, collapse = ", ")
  }

  mergedatasets2 <- MasterList2 %>%
    rename(Phylum = phylum)
  correspondence_table <- read_excel(
    "TablesToStandardize/standardization_tables.xlsx",
    sheet = "functionalgroup_globi_table"
  ) %>%
    mutate(across(
      c(Kingdom_Target, Kingdom_Source, Phylum_Source, Class_Source,
        Order_Source, Family_Source),
      ~ sapply(., clean_tax),
      .names = "{.col}_clean"
    )) %>%
    rename(Functional_Group = `Functional Group`)

  #Function to obtain functional group
  get_functional_group <- function(name) {
    cat("Processing:", name, "\n")

    tryCatch({
      taxonomy <- mergedatasets2 %>%
        filter(AcceptedNameGBIF == name) %>%
        distinct(Kingdom, Phylum, Class, Order, Family)

      for (col in c("Kingdom", "Phylum", "Class", "Order", "Family")) {
        value <- taxonomy[[col]]
        if (!is.null(value) && length(value) > 0 && !is.na(value)) {
          clean_value <- clean_tax(value)

          match_fg <- correspondence_table %>%
            filter(is.na(Interaction),
                   .data[[paste0(col, "_Source_clean")]] == clean_value) %>%
            pull(Functional_Group) %>%
            unique()

          if (length(match_fg) > 0 && !is.na(match_fg[1])) {
            return(match_fg[1])
          }
        }
      }
      interactions <- get_interactions(name) %>%
        filter(interaction_type %in% c(
          "eats", "preysOn", "parasiteOf", "endoparasiteOf",
          "ectoparasiteOf", "parasitoidOf", "pollinates",
          "pathogenOf", "rootparasiteOf", "hemiparasiteOf"
        )) %>%
        select(source_taxon_name, interaction_type, target_taxon_name)

      if (nrow(interactions) == 0) return(NA)

      interactions$Kingdom_target <- name_backbone_checklist(interactions$target_taxon_name)$kingdom

      result_join <- noduplicates(interactions, "interaction_type") %>%
        mutate(Kingdom_target_clean = sapply(Kingdom_target, clean_tax)) %>%
        left_join(correspondence_table,
                  by = c("interaction_type" = "Interaction",
                         "Kingdom_target_clean" = "Kingdom_Target_clean")) %>%
        mutate(Functional_Group = ifelse(is.na(Functional_Group) & interaction_type == "preysOn",
                                         "Predator", Functional_Group))

      groups <- result_join %>%
        filter(!is.na(Functional_Group)) %>%
        #distinct(Functional_Group) %>%
        pull(Functional_Group) %>%
        sort() %>%
        paste(collapse = "; ")

      if (groups == "") return(NA)
      groups
    },
    error = function(e) {
      message("Error and not obatined Functional Group to ", name, ": ", e$message)
      NA_character_
    })
  }
  for (i in seq_len(nrow(mergedatasets2))) {
    name <- mergedatasets2$AcceptedNameGBIF[i]
    already_has <- mergedatasets2$FunctionalGroup[i]

    if (is.na(already_has) || already_has == "") {
      cat(sprintf("Processing %d/%d: %s\n", i, nrow(mergedatasets2), name))
      mergedatasets2$FunctionalGroup[i] <- get_functional_group(name)
    } else {
      cat(sprintf("Skipping %d/%d (already has group): %s\n", i, nrow(mergedatasets2), name))
    }
  }

  FillGaps_Masterlist_Globi <- mergedatasets2 %>% filter(!is.na(FunctionalGroup)) #Estos son los na solucionados con GLOBI
  FillGaps_Masterlist_Globi

  MasterList3 <- mergedatasets2 %>% filter(is.na(FunctionalGroup)) #Estos son los que siguen teniendo NA

  #Save result
  names(MasterList0)
  MasterList0 <- MasterList0 %>%
    rename(Phylum=phylum)
  FillGaps_Masterlist_Fishbase <- FillGaps_Masterlist_Fishbase %>%
    rename(Phylum=phylum)
  mergedatasetsv1 <- rbind(MasterList0, FillGaps_Masterlist_Fishbase, FillGaps_Masterlist_Globi, MasterList3)
  unique(mergedatasets3$FunctionalGroup)

  write.xlsx(mergedatasetsv1, "OutputFiles/Intermediate/Step7.1_ObtainFuctionalGroup_Masterlist.xlsx")

  ######################################
  ### 3 TAXONOMY   #####################
  ######################################
  #mergedatasets2 <- read_excel("OutputFiles/Intermediate/Step7.1_ObtainFuctionalGroup_Masterlist.xlsx")
  taxonomy_table <- read_excel(
    "TablesToStandardize/standardization_tables.xlsx",
    sheet = "functionalgroup_taxonomy_table"
  )
  mergedatasets3 <- MasterList3 %>%
    separate_rows(FunctionalGroup, sep = ",") %>%
    separate_rows(Kingdom, sep = ",") %>%
    separate_rows(Phylum, sep = ",") %>%
    separate_rows(Class, sep = ",") %>%
    separate_rows(Order, sep = ",") %>%
    separate_rows(Family, sep = ",") %>%
    mutate(across(c(FunctionalGroup, Kingdom, Phylum, Class, Order, Family), trimws)) %>%
    mutate(across(c(Kingdom, Family, Order, Class, Phylum), tolower))

  taxonomy_table <- taxonomy_table %>%
    mutate(across(c(Kingdom, Family, Order, Class, Phylum, FunctionalGroup), tolower))
  print(taxonomy_table, n=30)

  #Function to hierarchically fill FunctionalGroup
  fill_functional_group <- function(df, taxonomy) {
    df %>%
      mutate(
        FG_kingdom = taxonomy$FunctionalGroup[match(Kingdom, taxonomy$Kingdom)],
        FG_family  = taxonomy$FunctionalGroup[match(Family,  taxonomy$Family)],
        FG_order   = taxonomy$FunctionalGroup[match(Order,   taxonomy$Order)],
        FG_class   = taxonomy$FunctionalGroup[match(Class,   taxonomy$Class)],
        FG_phylum  = taxonomy$FunctionalGroup[match(Phylum,  taxonomy$Phylum)],
        FunctionalGroup = coalesce(
          FunctionalGroup,
          FG_kingdom, FG_family, FG_order, FG_class, FG_phylum
        )
      ) %>%
      select(-FG_kingdom, -FG_family, -FG_order, -FG_class, -FG_phylum)
  }

  mergedatasets4 <- fill_functional_group(mergedatasets3, taxonomy_table)

  mergedatasets4 <- mergedatasets4%>%
    mutate(FunctionalGroup = tolower(FunctionalGroup))

  cols <- setdiff(names(mergedatasets4), "Source_Data")
  mergedatasets4[cols] <- lapply(mergedatasets4[cols], function(x) {
    x[x == "" | x == "NA"] <- NA
    x
  })

  #Validate categories functional groups
  valid_functional_groups <- unique(c(
    taxonomy_table$FunctionalGroup,
    correspondence_table$Functional_Group,
    Fishbase_table$Functional_Groups
  )) %>% na.omit()
  mergedatasets4 <- mergedatasets4 %>%
    mutate(FunctionalGroup = ifelse(
      FunctionalGroup %in% valid_functional_groups,
      FunctionalGroup,
      NA
    ))
  mergedatasets4 <- mergedatasets4 %>%
    mutate(
      FunctionalGroup = gsub("\\bNA\\b", "", FunctionalGroup),
      FunctionalGroup = gsub(",\\s*,", ",", FunctionalGroup),
      FunctionalGroup = gsub("^,\\s*|,\\s*$", "", FunctionalGroup),
      FunctionalGroup = trimws(FunctionalGroup),
      FunctionalGroup = na_if(FunctionalGroup, "")
    )

  FillGaps_Masterlist_taxonomy <- mergedatasets4 %>% filter(!is.na(FunctionalGroup)) #Estos son los NA resueltos con taxonomia
  MasterList_withNAFunctionalGroup <- mergedatasets4 %>% filter(is.na(FunctionalGroup)) #Estos son los que finalmente se quedan sin FG


  #unir MasterList0, FillGaps_Masterlist_Fishbase, FillGaps_Masterlist_Globi, FillGaps_Masterlist_taxonomy y MasterList_withNAFunctionalGroup
  allmasterlist <- rbind(MasterList0, FillGaps_Masterlist_Fishbase, FillGaps_Masterlist_Globi, FillGaps_Masterlist_taxonomy,MasterList_withNAFunctionalGroup)
  unique(allmasterlist$FunctionalGroup)


  #No duplicates
  source(file.path("R", "noduplicates.r"))
  allmasterlist <- noduplicates(allmasterlist, "AcceptedNameGBIF")
  allmasterlist <- allmasterlist %>%
    mutate(
      FunctionalGroup = gsub("\\bNA\\b", "", FunctionalGroup),
      FunctionalGroup = gsub(",\\s*,", ",", FunctionalGroup),
      FunctionalGroup = gsub("^,\\s*|,\\s*$", "", FunctionalGroup),
      FunctionalGroup = trimws(FunctionalGroup),
      FunctionalGroup = na_if(FunctionalGroup, ""),
      FunctionalGroup = tolower(FunctionalGroup),
      FunctionalGroup = gsub("\\s*\\(\\d+\\.?\\d*%\\)", "", FunctionalGroup)  # quitar "(100%)", "(50%)", etc.
    )

  unique(allmasterlist$FunctionalGroup)

  #Save result
  write.xlsx(allmasterlist, "OutputFiles/Intermediate/Step7_ObtainFuctionalGroup_Masterlist.xlsx")
}
