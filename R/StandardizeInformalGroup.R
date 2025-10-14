StandardizeInformalGroup <- function() {

  # Correspondence table
  equivs <- read_excel("TablesToStandardize/standardization_tables.xlsx", sheet = "informalgroup_table", col_names = TRUE) %>%
    mutate(
      Informal_group = tolower(Informal_group),
      Levels = tolower(Levels),
      Keywords = str_to_lower(str_trim(Keywords))
    ) %>%
    mutate(Keywords = str_split(Keywords, ";")) %>%
    unnest(Keywords) %>%
    mutate(Keywords = str_trim(Keywords))

  # Masterlist
  dat <- read_excel("OutputFiles/Intermediate/Step5_ObtainTaxonNAS_MasterList.xlsx") %>%
    rename(
      phylum = Phylum,
      kingdom = Kingdom,
      class = Class,
      order = Order,
      family = Family
    ) %>%
    mutate(across(-c(AcceptedNameGBIF, OriginalNameDB), ~str_to_lower(as.character(.)))) %>%
    mutate(
      group = Group %>%
        str_replace_all("[_/\\-]", "") %>%
        str_split(";")
    ) %>%
    unnest(group) %>%
    mutate(group = str_trim(group)) %>%
    select(-Group) %>%
    mutate(Informal_group = NA_character_)

  tiers <- unique(equivs$Levels)
  tiers <- tiers[tiers %in% colnames(dat)]

  #Applying standardization
  for (level in tiers) {
    cat("Processing level:", level, "\n")
    sub_equivs <- equivs %>% filter(Levels == level)
    dat <- dat %>%
      rowwise() %>%
      mutate(
        Informal_group = if_else(
          is.na(Informal_group) & get(level) %in% sub_equivs$Keywords,
          sub_equivs$Informal_group[match(get(level), sub_equivs$Keywords)],
          Informal_group
        )
      ) %>%
      ungroup()
  }

  #No duplicates, merge
  source(file.path("R","noduplicates.r"))
  MasterlistStandardized <- noduplicates(dat, "AcceptedNameGBIF")


  #Finally:
  MasterlistStandardized <- MasterlistStandardized[,c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF","kingdom","phylum","class","order",
                                  "family","EstablishmentMeans","Pathways","FunctionalGroup", "group","Informal_group",
                                  "NativeCountry","AffectedNativeSpecies","EICATImpact",
                                  "Mechanisms","EICAT_by_Mechanism","Habitat","FirstRecords","OldestDate",
                                  "InvadedCountry","InvadedCountryISO3","Source_Data")]
  MasterlistStandardized <- MasterlistStandardized %>%
    rename(Group = Informal_group) %>%
    rename(Kingdom = kingdom)%>%
    rename(phylum = phylum)%>%
    rename(Class = class)%>%
    rename(Order = order)%>%
    rename(Family = family)
  cols <- setdiff(names(MasterlistStandardized), "Source_Data")
  MasterlistStandardized[cols] <- lapply(MasterlistStandardized[cols], function(x) {
    x[x == "" | x == "NA"] <- NA
    x
  })

  #Save
  write_xlsx(MasterlistStandardized, "OutputFiles/Intermediate/Step6_StandardizedInformalGroup_Masterlist.xlsx")
}
