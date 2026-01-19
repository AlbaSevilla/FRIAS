Obtain_TaxonNAS <- function() {

  taxonomy_withGBIF <- read.xlsx("OutputFiles/Intermediate/step4_selectedfreshwatergbif_masterlist.xlsx")
  tax_cols <- c("Kingdom", "Order", "Family", "Class", "Phylum")
  for (col in tax_cols) {
    if (!col %in% colnames(taxonomy_withGBIF)) {
      taxonomy_withGBIF[[col]] <- NA_character_
    } else {
      taxonomy_withGBIF[[col]] <- trimws(taxonomy_withGBIF[[col]])
      taxonomy_withGBIF[[col]][taxonomy_withGBIF[[col]] == ""] <- NA
    }
  }
  taxonomy_withGBIF$AcceptedNameGBIF <- trimws(taxonomy_withGBIF$AcceptedNameGBIF)

  #GBIF
  print("Filling gaps in taxonomy with GBIF backbone")
  taxonomy_withGBIF_to_query <- taxonomy_withGBIF %>%
    filter(if_any(all_of(tax_cols), is.na)) %>%
    distinct(AcceptedNameGBIF) %>%
    pull(AcceptedNameGBIF)

  source(file.path("R", "taxonomy_query.R"))
  taxo_df <- list()
  if (length(taxonomy_withGBIF_to_query) > 0) {
    pb <- txtProgressBar(min = 0, max = length(taxonomy_withGBIF_to_query), style = 3)
    for (i in seq_along(taxonomy_withGBIF_to_query)) {
      taxo_df[[i]] <- taxonomy_query(taxonomy_withGBIF_to_query[i])
      if (interactive()) setTxtProgressBar(pb, i)
    }
    close(pb)
    taxo_df <- bind_rows(taxo_df)
  }
  for (col in tax_cols) {
    idx <- is.na(taxonomy_withGBIF[[col]]) & taxonomy_withGBIF$AcceptedNameGBIF %in% taxo_df$AcceptedNameGBIF
    taxonomy_withGBIF[[col]][idx] <- taxo_df[[col]][match(taxonomy_withGBIF$AcceptedNameGBIF[idx], taxo_df$AcceptedNameGBIF)]
  }

  taxonomy_withNCBI <- taxonomy_withGBIF
  write.xlsx(taxonomy_withGBIF, "OutputFiles/Intermediate/taxonomy_withGBIF.xlsx")

  #NCBI
  print("Filling gaps in taxonomy with NCBI")
  future::plan(future::multisession, workers = 2)
  progressr::handlers("progress")
  tax_cols <- c("Kingdom", "Order", "Family", "Class", "Phylum")
  for (col in tax_cols) {
    taxonomy_withNCBI[[col]][trimws(taxonomy_withNCBI[[col]]) == ""] <- NA
  }
  dataset_na <- taxonomy_withNCBI %>%
    filter(if_any(all_of(tax_cols), is.na))
  species_to_query <- unique(dataset_na$AcceptedNameGBIF)
  ncbi_query <- function(sp) {
    result <- tryCatch(
      tax_name(sci = sp, get = c("kingdom", "order", "family", "class", "phylum"), db = "ncbi"),
      error = function(e) return(data.frame(kingdom=NA, order=NA, family=NA, class=NA, phylum=NA))
    )
    if (is.data.frame(result) && nrow(result) > 0) {
      row <- result[1, ]
      row$species <- sp
      return(row)
    } else {
      return(data.frame(kingdom=NA, order=NA, family=NA, class=NA, phylum=NA, species=sp))
    }
  }
  progressr::with_progress({
    p <- progressr::progressor(along = species_to_query)
    results <- future_lapply(species_to_query, function(sp) {
      res <- ncbi_query(sp)
      p(sprintf("Processing: %s", sp))
      res
    })
  })
  taxonomy_df <- bind_rows(results)
  names(taxonomy_df)[names(taxonomy_df) == "species"] <- "AcceptedNameGBIF"
  names(taxonomy_df)[names(taxonomy_df) == "kingdom"] <- "Kingdom"
  names(taxonomy_df)[names(taxonomy_df) == "order"] <- "Order"
  names(taxonomy_df)[names(taxonomy_df) == "family"] <- "Family"
  names(taxonomy_df)[names(taxonomy_df) == "class"] <- "Class"
  names(taxonomy_df)[names(taxonomy_df) == "phylum"] <- "Phylum"
  for (col in tax_cols) {
    idx <- is.na(taxonomy_withNCBI[[col]]) & taxonomy_withNCBI$AcceptedNameGBIF %in% taxonomy_df$AcceptedNameGBIF
    taxonomy_withNCBI[[col]][idx] <- taxonomy_df[[col]][match(taxonomy_withNCBI$AcceptedNameGBIF[idx], taxonomy_df$AcceptedNameGBIF)]
  }
  dataset3 <- taxonomy_withNCBI %>% arrange(AcceptedNameGBIF)
  taxonomic_order <- c("Kingdom", "Phylum", "Class", "Order", "Family")
  other_columns <- setdiff(names(dataset3), taxonomic_order)
  dataset3 <- dataset3[, c(other_columns[1:2], taxonomic_order, other_columns[3:length(other_columns)])]
  write.xlsx(dataset3, "OutputFiles/Intermediate/taxonomy_withNCBI.xlsx")

  print("Filling gaps in taxonomy with Family level")

  taxonomy_withFamily <- dataset3
  tax_cols <- c("Kingdom", "Phylum", "Class", "Order", "Family")

  taxonomy_withFamily <- taxonomy_withFamily %>%
    group_by(Family) %>%
    mutate(across(all_of(tax_cols[tax_cols != "Family"]), ~ {
      # Rellenar los NA con el primer valor no NA dentro de la familia
      first_val <- na.omit(.x)[1]  # primer valor no NA
      if(!is.na(first_val)) {
        .x[is.na(.x)] <- first_val
      }
      .x
    })) %>%
    ungroup() %>%
    mutate(
      Order = sapply(str_split(Order, ";"), function(x) if(length(x) >= 2) x[2] else x[1]),
      Kingdom = gsub("Animalia;Metazoa", "Animalia", Kingdom)
    )
  write.xlsx(taxonomy_withFamily, "OutputFiles/Intermediate/taxonomy_withFamily.xlsx")

  print("Filling gaps in taxonomy with ITIS")
  sp <- taxonomy_withFamily %>%
    filter(is.na(Class) | is.na(Order)) %>%
    pull(AcceptedNameGBIF)

  taxonomy_itis <- lapply(sp, function(x) {
    res <- tryCatch({
      tax_name(sci = x, get = c("kingdom","phylum","class","order","family"), db="itis")
    }, error = function(e) NULL)
    if(is.null(res)) {
      res <- tryCatch({
        tax_name(sci = x, get = c("kingdom","phylum","class","order","family"), db="ncbi")
      }, error = function(e) NULL)
    }
    if(is.null(res)) {
      res <- data.frame(
        kingdom = NA,
        phylum  = NA,
        class   = NA,
        order   = NA,
        family  = NA,
        row.names = x
      )
    }
    return(res)
  })
  taxonomy_itis2 <- bind_rows(taxonomy_itis)
  taxonomy_itis2 <- taxonomy_itis2 %>%
    select(query, kingdom, phylum, class, order, family) %>%
    rename(AcceptedNameGBIF = query)
  write.xlsx(taxonomy_itis2, "OutputFiles/Intermediate/taxonomy_withITIS.xlsx")

  #masterlist final con los gaps de taxonomia corregidos
  taxonomy_withFamily<- read.xlsx("OutputFiles/Intermediate/taxonomy_withFamily.xlsx")
  taxonomy_itis2<- read.xlsx("OutputFiles/Intermediate/taxonomy_withITIS.xlsx")

  taxonomy_joined <- taxonomy_withFamily %>%
    left_join(
      taxonomy_itis2,
      by = "AcceptedNameGBIF"
    ) %>%
    mutate(
      Kingdom = coalesce(Kingdom, kingdom),
      Phylum  = coalesce(Phylum,  phylum),
      Class   = coalesce(Class,   class),
      Order   = coalesce(Order,   order),
      Family  = coalesce(Family,  family)
    ) %>%
    select(-kingdom, -phylum, -class, -order, -family)


  #Merge
  FinalDataset <- noduplicates(taxonomy_joined,"AcceptedNameGBIF")
  FinalDataset2 <- FinalDataset
  cols <- setdiff(names(FinalDataset2), "Source_Data")
  FinalDataset2[cols] <- lapply(FinalDataset2[cols], function(x) {
    x[x == "" | x == "NA"] <- NA
    x
  })

  taxonomy_cols <- c("Kingdom", "Phylum", "Class", "Order", "Family")

  FinalDataset3 <- FinalDataset2

  ##Some corrections
  FinalDataset3[ taxonomy_cols ] <- lapply(
    FinalDataset3[ taxonomy_cols ],
    function(x) sub(",.*", "", x)
  )
  FinalDataset3 <- FinalDataset3 %>%
    mutate(
      across(
        all_of(taxonomy_cols[taxonomy_cols != "Source_Data"]),
        ~ {
          x <- tolower(.x)
          substr(x, 1, 1) <- toupper(substr(x, 1, 1))
          x
        }
      )
    )
  FinalDataset3$Kingdom <- gsub("Metazoa", "", FinalDataset3$Kingdom)
  FinalDataset3$Kingdom <- gsub(",", "", FinalDataset3$Kingdom)
  FinalDataset3$Kingdom <- trimws(FinalDataset3$Kingdom)
  FinalDataset3$Class <- gsub("Actinopteri", "Actinopterygii", FinalDataset3$Class)
  FinalDataset3$Class <- gsub("Actinopterygios", "Actinopterygii", FinalDataset3$Class)
  FinalDataset3 <- FinalDataset3 %>%
    mutate(
      across(
        all_of(taxonomy_cols[taxonomy_cols != "Source_Data"]),
        ~ {
          x <- tolower(.x)
          substr(x, 1, 1) <- toupper(substr(x, 1, 1))
          x
        }
      )
    )
  FinalDataset3_NAS <- FinalDataset3 %>%
    filter(if_any(c(Kingdom, Class, Phylum, Order, Family), is.na))

  write.xlsx(FinalDataset3,
             file.path("OutputFiles", "Intermediate", "step5_obtaintaxonnas_masterlist.xlsx"),
             row.names = FALSE,
             col.names = TRUE,
             quote = FALSE)
  write.csv(FinalDataset3, "OutputFiles/Intermediate/step5_obtaintaxonnas_masterlist.csv")
  #SAve na cases
  write.xlsx(FinalDataset3_NAS,
             file.path("OutputFiles", "Check", "NA_Taxonomy_Masterlist.xlsx"),
             row.names = FALSE,
             col.names = TRUE,
             quote = FALSE)
  write.csv(FinalDataset3_NAS, "OutputFiles/Check/NA_Taxonomy_Masterlist.csv")
}
