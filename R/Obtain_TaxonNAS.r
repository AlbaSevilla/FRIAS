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

  source(file.path("R", "taxonomy_query.r"))
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


  #Taxonomy with family level
  print("Filling gaps in taxonomy with Family level")
  taxonomy_withFamily <- dataset3
  tax_cols <- c("Kingdom", "Phylum", "Class", "Order")

  taxonomy_withFamily <- taxonomy_withFamily %>%
    group_by(Family) %>%
    mutate(across(all_of(tax_cols[tax_cols != "Family"]), ~ {
      x <- .
      unique_vals <- unique(x[!is.na(x)])
      if(length(unique_vals) == 1) {
        x[is.na(x)] <- unique_vals
      }
      x
    })) %>%
    ungroup() %>%
    mutate(Order = sapply(str_split(Order, ";"), function(x) if(length(x) >= 2) x[2] else x[1]),
           Kingdom = gsub("Animalia;Metazoa", "Animalia", Kingdom))


  #ITIS
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
  names(taxonomy_itis2)

  taxonomy_withFamily
  names(taxonomy_withFamily)


  #masterlist final con los gaps de taxonomia corregidos
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
  source(file.path("R", "noduplicates.r"))
  FinalDataset <- noduplicates(taxonomy_joined,"AcceptedNameGBIF")
  cols <- setdiff(names(FinalDataset), "Source_Data")
  FinalDataset[cols] <- lapply(FinalDataset[cols], function(x) {
    x[x == "" | x == "NA"] <- NA
    x
  })

  taxonomy_cols <- c("Kingdom", "Phylum", "Class", "Order", "Family")
  FinalDataset <- FinalDataset %>%
    mutate(across(all_of(taxonomy_cols),
                  ~ sub(",.*", "", .))) %>%
    mutate(Kingdom = gsub("Metazoa", "Animalia", Kingdom)) %>%
    mutate(Class = gsub("actinopteri", "actinopterygii", Kingdom)) %>%
    mutate(Class = gsub("actinopterygios", "actinopterygii", Kingdom))


  write.xlsx(FinalDataset,
             file.path("OutputFiles", "Intermediate", "step5_obtaintaxonnas_masterlist.xlsx"),
             row.names = FALSE,
             col.names = TRUE,
             quote = FALSE)
  write.csv(FinalDataset, "OutputFiles/Intermediate/step5_obtaintaxonnas_masterlist.csv")
}
