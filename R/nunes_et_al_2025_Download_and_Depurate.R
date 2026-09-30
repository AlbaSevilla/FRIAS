nunes_et_al_2025_Download_and_Depurate <- function() {
  #Download
  url <- "https://zenodo.org/records/16077384/files/Supplementary_Material1.xlsx?download=1"
  destfile <- "InputFiles/originaldatabase_nunes_et_al_2025_supl1.xlsx"
  download.file(url, destfile, mode = "wb")
  url <- "https://zenodo.org/records/16077384/files/Supplementary_Material2.xlsx?download=1"
  destfile <- "InputFiles/originaldatabase_nunes_et_al_2025_supl2.xlsx"
  download.file(url, destfile, mode = "wb")

  #Depurate Top 57 Species
  dat_57species <- read_excel("InputFiles/originaldatabase_nunes_et_al_2025_supl2.xlsx", sheet="Top 57 Species")
  names(dat_57species) <- gsub("[:.]", "_", names(dat_57species))
  names(dat_57species) <- gsub(" ", "_", names(dat_57species))
  names(dat_57species) <- gsub("[1234567890]", "", names(dat_57species))
  names(dat_57species) <- gsub("__", "", names(dat_57species))
  cols_mechanism <- c("Competition",	"Predation",	"Hybridisation",
                      "Transmission_of_disease",	"Parasitism",
                      "Poisoning/toxicity",	"Biofouling_or_other_direct_physical_disturbance",
                      "Grazing/herbivory/browsing",	"Chemical,_physical,_structural_impact_on_ecosystem",
                      "Indirect_impacts_through_interactions_with_other_species")
  dat_57species <- dat_57species %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Mechanism_Impact = paste(
      cols_mechanism[which(c_across(all_of(cols_mechanism)) == "X")],
      collapse = ", "
    )) %>%
    ungroup()
  dat_57species$Mechanism_Impact <- gsub("_", " ", dat_57species$Mechanism_Impact)
  dat_57species$Mechanism_Impact <- gsub(",", "/", dat_57species$Mechanism_Impact)
  dat_57species <- dat_57species %>%
    mutate(Mechanism_Impact = str_split(Mechanism_Impact, ",")) %>%
    unnest(Mechanism_Impact) %>%
    mutate(
      Mechanism_Impact = tolower(str_trim(Mechanism_Impact)))
  dat_57species <- dat_57species %>%
    mutate(
      Likelihood_of_impact_on_biodiversity = c("0"="dd","1"="mc","2"="mn","3"="mo","4"="mr","5"="mv")[as.character(Likelihood_of_impact_on_biodiversity)]
    )
  dat_57species <- dat_57species %>%
    mutate(EICAT_by_Mechanism = paste(Likelihood_of_impact_on_biodiversity, Mechanism_Impact, sep="-"))
  colnames(dat_57species) <- gsub(" ", "_", colnames(dat_57species))
  dat_57species <- dat_57species %>%
    select(Thematic_Group, Scientific_name, Order, Family, Native_range_detailed_area,
           Invaded_range_detailed_area, Mechanism_Impact, Likelihood_of_impact_on_biodiversity, EICAT_by_Mechanism)
  dat_57species <- noduplicates(dat_57species, "Scientific_name")
  dat_57species_species_list0 <- dat_57species$Scientific_name
  dat_57species_species_list <- name_backbone_checklist(dat_57species_species_list0)$canonicalName
  dat_57species_habitat <- check_habitat(dat_57species_species_list, dat_57species)
  dat_57species_habitat_dataset_freshwater <- dat_57species_habitat %>%
    filter(grepl("FRESHWATER", Habitat))

  #Depurate High Risk Species
  dat_highrisk <- read_excel("InputFiles/originaldatabase_nunes_et_al_2025_supl2.xlsx", sheet="High Risk Species")
  names(dat_highrisk) <- gsub("[:.]", "_", names(dat_highrisk))
  names(dat_highrisk) <- gsub(" ", "_", names(dat_highrisk))
  names(dat_highrisk) <- gsub("[1234567890]", "", names(dat_highrisk))
  names(dat_highrisk) <- gsub("__", "", names(dat_highrisk))
  cols_mechanism <- c("Competition",	"Predation",	"Hybridisation",
                      "Transmission_of_disease",	"Parasitism",
                      "Poisoning/toxicity",	"Biofouling_or_other_direct_physical_disturbance",
                      "Grazing/herbivory/browsing",	"Chemical,_physical,_structural_impact_on_ecosystem",
                      "Indirect_impacts_through_interactions_with_other_species")
  dat_highrisk <- dat_highrisk %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Mechanism_Impact = paste(
      cols_mechanism[which(c_across(all_of(cols_mechanism)) == "X")],
      collapse = ", "
    )) %>%
    ungroup()
  dat_highrisk$Mechanism_Impact <- gsub("_", " ", dat_highrisk$Mechanism_Impact)
  dat_highrisk$Mechanism_Impact <- gsub(",", "/", dat_highrisk$Mechanism_Impact)
  dat_highrisk <- dat_highrisk %>%
    mutate(Mechanism_Impact = str_split(Mechanism_Impact, ",")) %>%
    unnest(Mechanism_Impact) %>%
    mutate(
      Mechanism_Impact = tolower(str_trim(Mechanism_Impact)))
  dat_highrisk <- dat_highrisk %>%
    mutate(
      Likelihood_of_impact_on_biodiversity = c("0"="dd","1"="mc","2"="mn","3"="mo","4"="mr","5"="mv")[as.character(Likelihood_of_impact_on_biodiversity)]
    )
  dat_highrisk <- dat_highrisk %>%
    mutate(EICAT_by_Mechanism = paste(Likelihood_of_impact_on_biodiversity, Mechanism_Impact, sep="-"))
  colnames(dat_highrisk) <- gsub(" ", "_", colnames(dat_highrisk))
  dat_highrisk <- dat_highrisk %>%
    select(Thematic_Group, Scientific_name, Order, Family, Native_range_detailed_area,
           Invaded_range_detailed_area, Mechanism_Impact, Likelihood_of_impact_on_biodiversity, EICAT_by_Mechanism)
  dat_highrisk <- noduplicates(dat_highrisk, "Scientific_name")
  dat_highrisk_species_list0 <- dat_highrisk$Scientific_name
  dat_highrisk_species_list <- name_backbone_checklist(dat_highrisk_species_list0)$canonicalName
  dat_highrisk_habitat <- check_habitat(dat_highrisk_species_list, dat_highrisk)
  dat_highrisk_habitat_dataset_freshwater <- dat_highrisk_habitat %>%
    filter(grepl("FRESHWATER", Habitat))

  #Depurate list widely spread
  dat_widelyspread <-  read_excel("InputFiles/originaldatabase_nunes_et_al_2025_supl1.xlsx",
                      sheet = "List widely spread")
  dat_widelyspread <- dat_widelyspread[-c(1:4),]
  colnames(dat_widelyspread) <- dat_widelyspread[1,]
  colnames(dat_widelyspread) <- gsub(" ", "_", colnames(dat_widelyspread))
  dat_widelyspread <- noduplicates(dat_widelyspread, "Scientific_Name")
  dat_widelyspread_specieslist0 <- dat_widelyspread$Scientific_Name
  dat_widelyspread_specieslist1 <- name_backbone_checklist(dat_widelyspread_specieslist0)$canonicalName
  dat_widelyspread_habitat <- check_habitat(dat_widelyspread_specieslist1, dat_widelyspread)
  dat_widelyspread_habitat_freshwater <- dat_widelyspread_habitat %>% filter(grepl("FRESHWATER", Habitat))
  cols_ref <- names(dat_57species_habitat_dataset_freshwater)
  add_missing_columns <- function(data, cols_ref) {
    missing_cols <- setdiff(cols_ref, names( data)) # columnas que faltan
    for (col in missing_cols) {
       data[[col]] <- ""
    }
     data <-  data[, union(names( data), cols_ref)]
     data <-  data[, cols_ref]
    return( data)
  }
  dat_highrisk_habitat_dataset_freshwater <- add_missing_columns(dat_highrisk_habitat_dataset_freshwater, cols_ref)
  dat_widelyspread_habitat_freshwater <- add_missing_columns(dat_widelyspread_habitat_freshwater, cols_ref)
  dataset_final <- rbind(dat_57species_habitat_dataset_freshwater, dat_highrisk_habitat_dataset_freshwater, dat_widelyspread_habitat_freshwater)
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

  #Save
  write.xlsx(dataset_final, "./InputFiles/freshwatersubset_nunes_et_al_2025.xlsx")
}
