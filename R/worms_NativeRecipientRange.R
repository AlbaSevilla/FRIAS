worrms_NativeRecipientRange <- function(){
  get_worms_info <- function(species_name) {
    rec <- try(wm_records_name(species_name), silent = TRUE)
    if (inherits(rec, "try-error") || length(rec) == 0) {
      return(data.frame(
        species = species_name,
        AphiaID = NA,
        stringsAsFactors = FALSE
      ))
    }
    AphiaID <- rec$AphiaID[1]
    return(data.frame(
      species = species_name,
      AphiaID = AphiaID,
      stringsAsFactors = FALSE
    ))
  }

  masterlist <- read.xlsx("OutputFiles/Intermediate/step7_additionalNativeRecipientRangeIUCN_masterlist.xlsx")
  species_vector <- masterlist$AcceptedNameGBIF

  worms_results <- vector("list", length(species_vector))
  total <- length(species_vector)

  for (i in seq_along(species_vector)) {
    species_list <- species_vector[i]
    cat("Processing species", i, "of", total, ":", species_list, "...\n")
    worms_results[[i]] <- get_worms_info(species_list)
  }

  worms_dataset <- bind_rows(worms_results)
  worms_dataset_noNA <- worms_dataset %>% filter(!is.na(AphiaID))
  worms_dataset_noNA_IDS <- worms_dataset_noNA$AphiaID


  distribution_species <- vector("list", length(worms_dataset_noNA_IDS))
  total2 <- length(worms_dataset_noNA_IDS)
  for (j in seq_along(worms_dataset_noNA_IDS)) {
    id <- worms_dataset_noNA_IDS[j]
    species_list <- worms_dataset_noNA$species[worms_dataset_noNA$AphiaID == id]
    cat("Downloading distribution", j, "of", total2,
        " | AphiaID:", id, "| Species:", species_list, "...\n")
    dist <- try(wm_distribution(id), silent = TRUE)
    if (inherits(dist, "try-error") || nrow(dist) == 0) {
      distribution_species[[j]] <- data.frame(AphiaID = id)
    } else {
      dist$AphiaID <- id
      distribution_species[[j]] <- dist
    }
  }

  distribution_dataset <- bind_rows(distribution_species)
  distribution_final <- distribution_dataset %>%
    left_join(worms_dataset_noNA, by = "AphiaID") %>%
    select(species, everything())

  #save
  dat_alien <- distribution_final %>% filter(establishmentMeans == "Alien")
  dat_noduplicates_alien <- noduplicates(dat_alien, "species")
  write.xlsx(dat_noduplicates_alien, "OutputFiles/Intermediate/worrms_RecipientRange_results.xlsx")

  dat_noduplicates_alien <- read.xlsx("OutputFiles/Intermediate/worrms_RecipientRange_results.xlsx")
  columnas_quedarse <- c("species", "higherGeography")
  dat_noduplicates_alien <- dat_noduplicates_alien[,columnas_quedarse]
  merged_results <- masterlist %>%
    left_join(dat_noduplicates_alien, by = c("AcceptedNameGBIF" = "species")) %>%
    mutate(NativeRange = paste(NativeRange, higherGeography, sep = "; ")) %>%
    select(-higherGeography)


  #save
  unique(distribution_final$establishmentMeans)
  dat_native <- distribution_final %>%  filter(establishmentMeans %in% c("Native",
                                                                         "Native - Endemic",
                                                                         "Native - Non-endemic"))
  dat_noduplicates_native <- noduplicates(dat_native, "species")
  write.xlsx(dat_noduplicates_native, "OutputFiles/Intermediate/worrms_NativeRange_results.xlsx")

  dat_noduplicates_native <- read.xlsx("OutputFiles/Intermediate/worrms_NativeRange_results.xlsx")
  columnas_quedarse <- c("species", "higherGeography")
  dat_noduplicates_native <- dat_noduplicates_native[,columnas_quedarse]

  merged_results2 <- merged_results %>%
    left_join(dat_noduplicates_native, by = c("AcceptedNameGBIF" = "species")) %>%
    mutate(NativeRange = paste(NativeRange, higherGeography, sep = "; ")) %>%
    select(-higherGeography)

  #Add citation
  today <- Sys.Date()
  formatted_date <- format(today, "%d %B %Y")  # ej: "02 December 2025"
  current_year <- format(today, "%Y")          # ej: "2025"
  worms_text <- paste0(
    "worms editorial board (", current_year, "). world register of marine species. ",
    "available from https://www.marinespecies.org at vliz - accessed ", formatted_date,
    "- doi:10.14284/170"
  )
  merged_results2 <- merged_results2 %>%
    mutate(Source_Data = if_else(
      AcceptedNameGBIF %in% dat_noduplicates_native$species |
        AcceptedNameGBIF %in% dat_noduplicates_alien$species,
      paste0(Source_Data, "; ", worms_text),
      Source_Data
    ))

  #save
  write.xlsx(merged_results2, "OutputFiles/Intermediate/step8_additionalNativeRecipientRangeWORRMS_masterlist.xlsx")
  write.xlsx(merged_results2, "OutputFiles/Intermediate/step8_additionalNativeRecipientRangeWORRMS_masterlist.csv")
}
