Obtain_FunctionalGroup <- function() {
  #This function follows this order to fill all empty rows (table S2):
    #1) With taxonomy ranges
    #2) Fishbase
      #2.1) fooditems function
      #2.2) diet_items function
    #3) Taxonommy ranges
    #4) GLoBI
    #5) Taxonomy ranges
    #6) Particular corrections
  MasterList <- read.csv("OutputFiles/Intermediate/step14_standardizedestablishmentmeans_masterlist.csv")
  MasterList$FunctionalGroup <- gsub("/", ";",MasterList$FunctionalGroup)
  MasterList$FunctionalGroup <- gsub("-", ";",MasterList$FunctionalGroup)
  MasterList <- MasterList %>%
    separate_rows(FunctionalGroup, sep=";|,") %>%
    mutate(FunctionalGroup = tolower(trimws(FunctionalGroup)))

  MasterList$FunctionalGroup <- gsub("ous$", "e", MasterList$FunctionalGroup)

  correct_categories<- read_excel(
    "TablesToStandardize/Table S2.xlsx",
    sheet = "3. Functional Feeding Group"
  ) %>%
    pull(`FunctionalGroup`) %>%
    unique() %>%
    tolower() %>%
    trimws()
  corrections <- read_excel(
    "TablesToStandardize/Table S2.xlsx",
    sheet = "3. Functional Feeding Group"
  ) %>%
    filter(FFG_Data_Source == "ParticularCorrections") %>%
    select(Orig_FG) %>%
    separate_rows(Orig_FG, sep = ";|,") %>%
    distinct() %>%
    mutate(Orig_FG = tolower(trimws(Orig_FG))) %>%
    pull(Orig_FG)

  #Obtain NAS from functional group
  MasterList_not_standardized <- MasterList %>%
    filter(!is.na(FunctionalGroup) & !FunctionalGroup %in% c(correct_categories, corrections))

  names(MasterList_not_standardized)
  MasterList_not_standardized_cols <- c("AcceptedNameGBIF", "OriginalNameDB",
                                        "Kingdom", "Order", "Family", "Phylum",
                                        "Class", "FunctionalGroup",
                                        "Habitat", "Source_Data")
  MasterList_not_standardized <- MasterList_not_standardized[,MasterList_not_standardized_cols]
  write.xlsx(MasterList_not_standardized, "OutputFiles/Check/NA_FunctionalGroup_masterlist.xlsx")
  MasterList <- MasterList %>%
    mutate(
      FunctionalGroup = ifelse(FunctionalGroup %in% correct_categories, FunctionalGroup, NA)
    )
  MasterList <- noduplicates(MasterList, "AcceptedNameGBIF")
  MasterList <- MasterList %>%
    mutate(
      across(
        -Source_Data,
        ~ {
          x <- trimws(.)
          x[x %in% c("NA", "na", "Na", "nA")] <- NA
          x
        }
      )
    )

  #Load standardization tables
  FG_table <- read_excel("TablesToStandardize/Table S2.xlsx", sheet = "3. Functional Feeding Group")

  taxonomy_table <- FG_table %>%
    filter(FFG_Data_Source == "taxonomy")

  Fishbase_table <- FG_table %>%
    filter(FFG_Data_Source == "fishbase")

  ParticularCorrections <- FG_table %>%
    filter(FFG_Data_Source == "ParticularCorrections") %>%
    select(Orig_FG, FunctionalGroup) %>%
    separate_rows(Orig_FG, sep=";|,") %>%
    mutate(across(everything(), ~(trimws(.))))

  correspondence_table <- read_excel("TablesToStandardize/Table S2.xlsx",
                                     sheet = "3. Functional Feeding Group") %>%
    filter(FFG_Data_Source == "globi") %>%
    mutate(across(c(Kingdom_Target, Kingdom_Source, Phylum_Source, Class_Source,
                    Order_Source, Family_Source), ~tolower(trimws(.)),
                  .names = "{.col}_clean"))

  #Create a function to fill functional group with taxonomy
  fill_taxonomy <- function(dataset){
    dataset %>%
      separate_rows(Kingdom:Family, sep=",") %>%
      mutate(across(Kingdom:Family, ~(trimws(.)))) %>%
      mutate(FG = coalesce(
        taxonomy_table$FunctionalGroup[match(Kingdom, taxonomy_table$Kingdom_Source)],
        taxonomy_table$FunctionalGroup[match(Phylum, taxonomy_table$Phylum_Source)],
        taxonomy_table$FunctionalGroup[match(Class, taxonomy_table$Class_Source)],
        taxonomy_table$FunctionalGroup[match(Order, taxonomy_table$Order_Source)],
        taxonomy_table$FunctionalGroup[match(Family, taxonomy_table$Family_Source)]
      ),
      FunctionalGroup = FG) %>%
      select(-FG)
  }

  #1. TAXONOMY RANGES
  na_rows <- MasterList %>% filter(is.na(FunctionalGroup))
  non_na_rows <- MasterList %>% filter(!is.na(FunctionalGroup))
  filled_na_rows <- fill_taxonomy(na_rows)
  filled_na_rows <- filled_na_rows %>%
    mutate(FunctionalGroup = tolower(FunctionalGroup))
  cols <- setdiff(names(filled_na_rows), "Source_Data")
  filled_na_rows[cols] <- lapply(filled_na_rows[cols], function(x) {
    x[x == "" | x == "NA"] <- NA
    x
  })
  taxonomy_0 <- bind_rows(non_na_rows, filled_na_rows)
  na_count <- sum(is.na(taxonomy_0$FunctionalGroup))
  non_na_count <- sum(!is.na(taxonomy_0$FunctionalGroup))


  #2. FISHBASE
                              # with fooditems function
  nas_species <- taxonomy_0 %>%
    filter(is.na(FunctionalGroup)) %>%
    pull(AcceptedNameGBIF)
  fishbase_0 <- fooditems(nas_species, fields = c("Species", "FoodI")) %>%
    left_join(ecology(nas_species, fields = c("Species", "FeedingType", "FoodTroph")), by = "Species") %>%
    select(Species, FeedingType, FoodI, FoodTroph)
  valid_levels <- unique(Fishbase_table$FeedingType)
  fishbase_0 <- fishbase_0 %>%
    mutate(
      FeedingType = ifelse(FeedingType %in% valid_levels, FeedingType, NA),
      FoodTroph = case_when(
        is.na(FoodTroph) ~ NA_character_,
        FoodTroph >= 2.0 & FoodTroph <= 2.19 ~ "2.0-2.19",
        FoodTroph >= 2.2 & FoodTroph <= 2.79 ~ "2.2-2.79",
        FoodTroph > 2.8 ~ ">2.8",
        TRUE ~ NA_character_
      )
    )
  fishbase_1 <- fishbase_0 %>% #Assign functional groups by fishbase according to priority
    rowwise() %>%
    mutate(
      FunctionalGroupFishbase = {
        fi <- FoodI
        ft <- FoodTroph
        type <- FeedingType
        fg <- NA_character_

        combs <- list(
          Fishbase_table %>% filter(FoodI == fi, FoodTroph == ft),
          Fishbase_table %>% filter(FoodI == fi, FeedingType == type),
          Fishbase_table %>% filter(FeedingType == type, FoodTroph == ft),
          Fishbase_table %>% filter(FoodTroph == ft),
          Fishbase_table %>% filter(FeedingType == type),
          Fishbase_table %>% filter(FoodI == fi)
        )
        for(m in combs){
          if(nrow(m) > 0){
            fg <- m$FunctionalGroup[1]
            break
          }
        }
        fg
      }
    ) %>%
    ungroup() %>%
    select(Species, FunctionalGroupFishbase) %>%
    noduplicates("Species") %>%
    mutate(across(everything(), ~ na_if(., "")))
  MasterList_filled <- taxonomy_0 %>%
    left_join(fishbase_1, by = c("AcceptedNameGBIF" = "Species")) %>%
    mutate(
      FunctionalGroup = ifelse(is.na(FunctionalGroup), FunctionalGroupFishbase, FunctionalGroup)
    ) %>%
    select(-FunctionalGroupFishbase)
  MasterList_filled <- MasterList_filled %>%
    mutate(
      across(
        -Source_Data,
        ~ {
          x <- trimws(.)
          x[x %in% c("NA", "na", "Na", "nA")] <- NA
          x
        }
      )
    )
                                        #with diet_items function
  nas_species <- MasterList_filled %>% filter(is.na(FunctionalGroup)) %>% pull(AcceptedNameGBIF)
  if(length(nas_species) > 0){
    fishbase_diet <- diet_items(nas_species, fields = "FoodI") %>%
      mutate(Species = rep(nas_species, length.out = n())) %>%
      select(Species, FoodI) %>%
      rowwise() %>%
      mutate(
        FunctionalGroupFishbase = {
          fi <- FoodI
          match_diet <- Fishbase_table %>% filter(FoodI == fi)
          if(nrow(match_diet) > 0) match_diet$FunctionalGroup[1] else NA_character_
        }
      ) %>%
      ungroup() %>%
      select(Species, FunctionalGroupFishbase) %>%
      noduplicates("Species") %>%
      mutate(across(everything(), ~ na_if(., "")))
    MasterList_filled <- MasterList_filled %>%
      left_join(fishbase_diet, by = c("AcceptedNameGBIF" = "Species")) %>%
      mutate(
        FunctionalGroup = ifelse(is.na(FunctionalGroup), FunctionalGroupFishbase, FunctionalGroup)
      ) %>%
      select(-FunctionalGroupFishbase)
  }
  cols <- setdiff(names(MasterList_filled), "Source_Data")
  MasterList_filled[cols] <- lapply(MasterList_filled[cols], function(x) replace(x, x %in% c("", "NA"), NA))
  MasterList_filled <- MasterList_filled %>%
    mutate(
      across(
        -Source_Data,
        ~ {
          x <- trimws(.)
          x[x %in% c("NA", "na", "Na", "nA")] <- NA
          x
        }
      )
    )
  na_count <- sum(is.na(MasterList_filled$FunctionalGroup))
  non_na_count <- sum(!is.na(MasterList_filled$FunctionalGroup))

  #3. TAXONOMY RANGES
  taxonomy_1 <- fill_taxonomy(MasterList_filled %>% filter(is.na(FunctionalGroup)))
  MasterList_filled2 <- bind_rows(
    MasterList_filled %>% filter(!is.na(FunctionalGroup)),
    taxonomy_1
  )
  cols <- setdiff(names(MasterList_filled2), "Source_Data")
  MasterList_filled2[cols] <- lapply(MasterList_filled2[cols], function(x){
    x <- trimws(x)
    x[x %in% c("NA","na","Na","nA","")] <- NA
    x
  })
  na_count <- sum(is.na(MasterList_filled2$FunctionalGroup))
  non_na_count <- sum(!is.na(MasterList_filled2$FunctionalGroup))


  #4. GLOBI
  capitalize_first_letter <- function(taxonomy_row) {
    taxonomy_row %>% mutate(across(everything(), ~ str_to_title(.)))
  }
  correspondence_table <- correspondence_table %>%
    mutate(Kingdom_Target_clean = trimws(Kingdom_Target))
  kingdom_options <- correspondence_table$Kingdom_Target %>%
    na.omit() %>%
    str_split(",") %>%
    unlist() %>%
    trimws() %>%
    unique()
  clean_target_path <- function(path, kingdom_options) {
    if(length(path) == 0 || all(is.na(path))) return(NA_character_)
    path <- ifelse(is.na(path), "", path)
    elements <- str_split(path, ",")[[1]] %>% trimws()
    valid <- elements[elements %in% kingdom_options]
    if(length(valid) == 0) return(NA_character_)
    paste(valid, collapse = ", ")
  }
  MasterList_filled2_NA <- MasterList_filled2 %>% filter(is.na(FunctionalGroup))
  MasterList_filled2_NA <- MasterList_filled2_NA %>%
    mutate(FunctionalGroup_Globi = NA_character_)
  for(i in seq_len(nrow(MasterList_filled2_NA))) {
    name <- MasterList_filled2_NA$AcceptedNameGBIF[i]
    total <- nrow(MasterList_filled2_NA)
    cat(sprintf("\nProcessing %d/%d: %s\n", i, total, name))

    # a) FIrst try with taxonomic hierarchy
    taxonomy <- MasterList_filled2_NA[i, ] %>%
      select(Kingdom, Phylum, Class, Order, Family) %>%
      capitalize_first_letter()
    fg_from_taxonomy <- NA_character_
    for(col in c("Kingdom","Phylum","Class","Order","Family")) {
      value <- taxonomy[[col]]
      if(!is.null(value) && length(value) > 0 && !is.na(value)) {
        clean_value <- trimws(value)
        match_fg <- correspondence_table %>%
          filter(is.na(Interaction), .data[[paste0(col, "_Source_clean")]] == clean_value) %>%
          pull(FunctionalGroup) %>% unique()
        if(length(match_fg) > 0 && !is.na(match_fg[1])) {
          fg_from_taxonomy <- match_fg[1]
          cat(sprintf("Assigned from taxonomy (%s = %s): %s\n", col, clean_value, fg_from_taxonomy))
          break
        }
      }
    }

    # b) SEcond, with GLOBI interactions
    fg_from_globi <- NA_character_
    interaction_found <- NA_character_
    if(is.na(fg_from_taxonomy)) {
      interaction_types <- c("eats","preysOn","parasiteOf","endoparasiteOf",
                             "ectoparasiteOf","parasitoidOf","pollinates",
                             "pathogenOf","rootparasiteOf","hemiparasiteOf")
      interactions_list <- lapply(interaction_types, function(inter_type){
        tryCatch(
          get_interactions(name, inter_type) %>%
            select(source_taxon_name, interaction_type, target_taxon_path),
          error = function(e) NULL
        )
      })
      interactions_all <- bind_rows(interactions_list)
      if(nrow(interactions_all) > 0) {
        interactions_all <- interactions_all %>%
          rowwise() %>%
          mutate(
            target_taxon_path = clean_target_path(target_taxon_path, kingdom_options),
            Kingdom_target_clean = target_taxon_path
          ) %>%
          ungroup()
        result_join <- interactions_all %>%
          left_join(correspondence_table,
                    by = c("interaction_type" = "Interaction",
                           "Kingdom_target_clean" = "Kingdom_Target_clean")) %>%
          mutate(FunctionalGroup = ifelse(
            is.na(FunctionalGroup) & interaction_type %in% c("preysOn","eats"),
            "Predator", FunctionalGroup
          ))
        groups <- result_join %>%
          filter(!is.na(FunctionalGroup)) %>%
          select(interaction_type, FunctionalGroup) %>%
          distinct()
        if(nrow(groups) > 0) {
          fg_from_globi <- paste(groups$FunctionalGroup, collapse = "; ")
          interaction_found <- paste(groups$interaction_type, collapse = "; ")
          cat(sprintf("Assigned from GLOBI interaction(s) (%s): %s\n", interaction_found, fg_from_globi))
        }
      }
    }
    final_fg <- ifelse(!is.na(fg_from_taxonomy), fg_from_taxonomy,
                       ifelse(!is.na(fg_from_globi), fg_from_globi, NA_character_))
    MasterList_filled2_NA$FunctionalGroup_Globi[i] <- final_fg
    cat(sprintf("Final FunctionalGroup for %s: %s\n", name, ifelse(is.na(final_fg),"NA",final_fg)))
  }
  MasterList_filled2_NA <- MasterList_filled2_NA %>%
    mutate(
      FunctionalGroup = paste(
        ifelse(is.na(FunctionalGroup), "", FunctionalGroup),
        ifelse(is.na(FunctionalGroup_Globi), "", FunctionalGroup_Globi),
        sep="; "
      ),
      FunctionalGroup = gsub("^; | ;$|^$","",FunctionalGroup),
      FunctionalGroup = gsub(";\\s*;","; ",FunctionalGroup),
      FunctionalGroup = trimws(FunctionalGroup),
      FunctionalGroup = na_if(FunctionalGroup,"")
    ) %>%
    select(-FunctionalGroup_Globi)
  MasterList_filled3 <- bind_rows(
    MasterList_filled2 %>% filter(!is.na(FunctionalGroup)),
    MasterList_filled2_NA
  )
  MasterList_NONA <- MasterList_filled3 %>% filter(!is.na(FunctionalGroup))
  MasterList_NA   <- MasterList_filled3 %>% filter(is.na(FunctionalGroup))


  # 5. TAXONOMY RANGES
  MasterList_filled3_NA <- MasterList_filled3 %>% filter(is.na(FunctionalGroup))
  taxonomy_final <- fill_taxonomy(MasterList_filled3_NA)
  MasterList_filled3 <- bind_rows(
    MasterList_filled3 %>% filter(!is.na(FunctionalGroup)),
    taxonomy_final
  )
  cols <- setdiff(names(MasterList_filled3), "Source_Data")
  MasterList_filled3[cols] <- lapply(MasterList_filled3[cols], function(x){
    x <- trimws(x)
    x[x %in% c("NA","na","Na","nA","")] <- NA
    x
  })


  #6. PARTICULAR CORRECTIONS
  final <- MasterList_filled3 %>%
    separate_rows(FunctionalGroup, sep=";|,") %>%
    mutate(FunctionalGroup = tolower(trimws(FunctionalGroup)))
  final[cols] <- lapply(final[cols], function(x){
    x <- trimws(x)
    x[x %in% c("NA","na","Na","nA","")] <- NA
    x
  })
  idx <- match(final$FunctionalGroup, ParticularCorrections$Orig_FG)
  final$FunctionalGroup[!is.na(idx)] <- ParticularCorrections$FunctionalGroup[idx[!is.na(idx)]]
  final <- noduplicates(final, "AcceptedNameGBIF")

  #SAVE FINAL
  write.xlsx(final, "OutputFiles/Intermediate/step15_obtainfunctionalgroup_masterlist.xlsx")
  write.csv(final, "OutputFiles/Intermediate/step15_obtainfunctionalgroup_masterlist.csv",
            row.names = FALSE)
}
