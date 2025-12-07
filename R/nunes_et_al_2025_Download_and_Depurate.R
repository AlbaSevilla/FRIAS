nunes_et_al_2025_Download_and_Depurate <- function() {
  #Download
  url <- "https://zenodo.org/records/16077384/files/Supplementary_Material1.xlsx?download=1"
  destfile <- "InputFiles/originaldatabase_nunes_et_al_2025_supl1.xlsx"
  download.file(url, destfile, mode = "wb")
  url <- "https://zenodo.org/records/16077384/files/Supplementary_Material2.xlsx?download=1"
  destfile <- "InputFiles/originaldatabase_nunes_et_al_2025_supl2.xlsx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dat <- read_excel("InputFiles/originaldatabase_nunes_et_al_2025_supl2.xlsx", sheet="Top 57 Species")
  names(dat) <- gsub("[:.]", "_", names(dat))
  names(dat) <- gsub(" ", "_", names(dat))
  names(dat) <- gsub("[1234567890]", "", names(dat))
  names(dat) <- gsub("__", "", names(dat))
  cols_mechanism <- c("Competition",	"Predation",	"Hybridisation",
                      "Transmission_of_disease",	"Parasitism",
                      "Poisoning/toxicity",	"Biofouling_or_other_direct_physical_disturbance",
                      "Grazing/herbivory/browsing",	"Chemical,_physical,_structural_impact_on_ecosystem",
                      "Indirect_impacts_through_interactions_with_other_species")
  dat2 <- dat %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Mechanism_Impact = paste(
      cols_mechanism[which(c_across(all_of(cols_mechanism)) == "X")],
      collapse = ", "
    )) %>%
    ungroup()
  dat2 <- dat2 %>%
    mutate(Mechanism_Impact = str_split(Mechanism_Impact, ",")) %>%
    unnest(Mechanism_Impact) %>%
    mutate(
      Mechanism_Impact = tolower(str_trim(Mechanism_Impact)))
  dat2 <- dat2 %>%
    mutate(EICAT_by_Mechanism = paste(Likelihood_of_impact_on_biodiversity, Mechanism_Impact, sep=" -"))
  colnames(dat2) <- gsub(" ", "_", colnames(dat2))
  dat <- dat2 %>%
    select(Thematic_Group, Scientific_name, Order, Family, Native_range_detailed_area,
           Invaded_range_detailed_area, Mechanism_Impact, Likelihood_of_impact_on_biodiversity, EICAT_by_Mechanism)
  nunes_et_al_2025_freshwater <- dat
  nunes_et_al_2025_freshwater <- noduplicates(nunes_et_al_2025_freshwater, "Scientific_name")
  dataset <- nunes_et_al_2025_freshwater
  species_list0 <- dataset$Scientific_name
  species_list <- name_backbone_checklist(species_list0)$canonicalName
  HORIZON_updated <- check_habitat(species_list, dataset)
  dataset_freshwater1 <- HORIZON_updated %>%
    filter(grepl("FRESHWATER", Habitat))
  dat2 <-  read_excel("InputFiles/originaldatabase_nunes_et_al_2025_supl1.xlsx",
                      sheet = "List no data")
  colnames(dat2) <- c("ID","Thematic_group","Scientific_Name",
                      "Kingdom",	"Phylum",	"Class"	,"Order",	"Family"	,
                      "Original_Source",	"Total_grid_cells_EU",	"N_grid_cells/country"	,
                      "Reason_for_exclusion")
  dat2 <- dat2[-c(1:3),]
  dat2_noduplicados <- noduplicates(dat2, "Scientific_Name")
  dataset <- dat2_noduplicados
  scientific <- dataset$Scientific_Name
  scientific_names <- name_backbone_checklist(scientific)$canonicalName
  habitat_dat <- check_habitat(scientific_names, dataset)
  dataset_freshwater2 <- habitat_dat %>% filter(grepl("FRESHWATER", Habitat))
  dat3 <-  read_excel("InputFiles/originaldatabase_nunes_et_al_2025_supl1.xlsx",
                      sheet = "List widely spread")
  dat3 <- dat3[-c(1:4),]
  colnames(dat3) <- dat3[1,]
  colnames(dat3) <- gsub(" ", "_", colnames(dat3))
  dat3_noduplicados <- noduplicates(dat3, "Scientific_Name")
  dataset <- dat3_noduplicados
  scientific <- dataset$Scientific_Name
  scientific_names <- name_backbone_checklist(scientific)$canonicalName
  habitat_dat <- check_habitat(scientific_names, dataset)
  dataset_freshwater3 <- habitat_dat %>% filter(grepl("FRESHWATER", Habitat))
  cols_ref <- names(dataset_freshwater1)
  add_missing_columns <- function(df, cols_ref) {
    missing_cols <- setdiff(cols_ref, names(df)) # columnas que faltan
    for (col in missing_cols) {
      df[[col]] <- ""  # agrega la columna con valores ""
    }
    # Reordenar columnas según cols_ref
    df <- df[, union(names(df), cols_ref)]
    df <- df[, cols_ref]
    return(df)
  }
  dataset_freshwater2 <- add_missing_columns(dataset_freshwater2, cols_ref)
  dataset_freshwater3 <- add_missing_columns(dataset_freshwater3, cols_ref)
  dataset_final <- rbind(dataset_freshwater1, dataset_freshwater2, dataset_freshwater3)
  dataset <- dataset_final
  dataset <- dataset[,c("Species", "AcceptedNameGBIF","Habitat", "Mechanism_Impact",
                        "Invaded_range_detailed_area", "Native_range_detailed_area", "Family", "Order",
                        "Thematic_Group","Likelihood_of_impact_on_biodiversity", "EICAT_by_Mechanism")]
  dataset <- dataset %>%
    separate_rows(Invaded_range_detailed_area, sep = ",") %>%
    mutate(Invaded_range_detailed_area = str_trim(Invaded_range_detailed_area)) %>%
    mutate(Invaded_range_detailed_area =
             str_extract(Invaded_range_detailed_area,
                         "[A-ZÁÉÍÓÚÑ][a-záéíóúñ]+(?: [A-ZÁÉÍÓÚÑ][a-záéíóúñ]+)*"
             ))
  dataset_final <- noduplicates(dataset, "Species")
  dataset_final <- dataset_final %>%
    mutate(
      Invaded_range_detailed_area = ifelse(
        is.na(Invaded_range_detailed_area),
        "Europe",
        Invaded_range_detailed_area
      )
    )

  #Save
  write.xlsx(dataset_final, "./InputFiles/freshwatersubset_nunes_et_al_2025.xlsx")
}
