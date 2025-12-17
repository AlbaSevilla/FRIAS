bailey_et_al_2021_Download_and_Depurate <- function(){
  #Depurate
  dat <- read_excel("InputFiles/originaldatabase_bailey_et_al_2021.xlsx", sheet=2)
  dat <- dat %>%
    filter(`Population Status`=="Established") %>%
    filter(`Recipient LME` == "Laurentian Great Lakes")
  dat <- dat %>%
    rename(Species_name = `Species  Name`)
  sum(count(unique(dat$Species_name))$freq)
  noduplicates <- noduplicates(dat, "Species_name")
  species <- noduplicates$Species_name
  accepted_names <- name_backbone_checklist(species)$canonicalName
  habitat_dat <- check_habitat(accepted_names, noduplicates)
  freshwater_dat <- habitat_dat %>%
    filter(grepl("FRESHWATER", Habitat))
  dat_2 <- noduplicates %>%
    mutate(
      pathways = paste(
        `Potential Pathway 1 \r\n(Ballast Water)`,
        `Potential Pathway 2 \r\n(Ship Fouling)`,
        `Potential Pathway 3`,
        `Potential Pathway 4`,
        sep = ","
      ),
      pathways = gsub("NA,", "", pathways),
      pathways = gsub(",NA", "", pathways)
    ) %>%
    select(-`Potential Pathway 1 \r\n(Ballast Water)`,
           -`Potential Pathway 2 \r\n(Ship Fouling)`,
           -`Potential Pathway 3`,
           -`Potential Pathway 4`) %>%
    rowwise() %>%
    mutate(
      Year_sorted = str_split(`Year of First Report`, ",")[[1]] %>%
        trimws() %>%
        as.numeric() %>%
        sort() %>%
        paste(collapse = ", "),
      OldestDate = min(as.numeric(str_split(`Year_sorted`, ",")[[1]]), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(`Year of First Report` = Year_sorted) %>%
    select(-Year_sorted) %>%
    arrange(OldestDate)
  dat_2$InvadedCountry <- "United States of America; Canada"
  dat_2$Habitat <- "Freshwater"
  names <- dat_2$Species_name
  dat_2$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName


  #Save
  write.xlsx(dat_2, "InputFiles/freshwatersubset_bailey_et_al_2021.xlsx")
}
