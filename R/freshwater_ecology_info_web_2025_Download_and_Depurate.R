freshwater_ecology_info_web_2025_Download_and_Depurate <- function(){
  #Depurate
  # --- Phytoplankton ---
  freshwater_ecology_info_web_2025Phytoplankton <- read_csv("InputFiles/originaldatabase_freshwaterecologyinfo_phytoplankton.csv") %>%
    mutate(Status = "Alien", Invaded_regions = "")
  # --- Phytobentos ---
  freshwater_ecology_info_web_2025Phytobentos <- read_csv("InputFiles/originaldatabase_freshwaterecologyinfo_phytobentos.csv") %>%
    mutate(Status = "Alien", Invaded_regions = "")
  # --- Alien Macrophytes ---
  freshwater_ecology_info_web_2025AlienMacrophytes <- read_excel("InputFiles/originaldatabase_freshwaterecologyinfo_alienmacrophytes.xlsx") %>%
    mutate(Status = "Alien", Invaded_regions = "")
  # --- Macroinvertebrates ---
  freshwater_ecology_info_web_2025macroinvertebrates <- read_csv("InputFiles/originaldatabase_freshwaterecologyinfo_macroinvertebrates.csv")
  # --- Fish ---
  freshwater_ecology_info_web_2025fish <- read_csv("InputFiles/originaldatabase_freshwaterecologyinfo_fish.csv")
  # --- Macrophytes ---
  freshwater_ecology_info_web_2025macrophytes <- read_csv("InputFiles/originaldatabase_freshwaterecologyinfo_macrophytes.csv")
  # --- Alien fish and regions ---
  freshwater_ecology_info_web_2025AlienSpeciesandRegions <- read_excel("InputFiles/originaldatabase_freshwaterecologyinfo_alienfishandregions.xlsx") %>%
    rename(Invaded_regions = `alien in catchment region(s)`) %>%
    mutate(Status = "Alien")
  DATASET_freshwater_ecology_info_web_2025 <- bind_rows(
    freshwater_ecology_info_web_2025Phytoplankton,
    freshwater_ecology_info_web_2025Phytobentos,
    freshwater_ecology_info_web_2025AlienMacrophytes,
    freshwater_ecology_info_web_2025macroinvertebrates,
    freshwater_ecology_info_web_2025fish,
    freshwater_ecology_info_web_2025macrophytes,
    freshwater_ecology_info_web_2025AlienSpeciesandRegions
  )
  DATASET_freshwater_ecology_info_web_2025 <- DATASET_freshwater_ecology_info_web_2025 %>%
    separate(
      col = Class,
      into = c("Class", "Taxon", "river", "lake", "Ref"),  # nombres deseados
      sep = ",",    # separador: prueba con "," o ";"
      fill = "right",
      extra = "drop"
    ) %>%
    mutate(across(where(is.character), ~ gsub('"', '', .)))
  DATASET_freshwater_ecology_info_web_2025 <- DATASET_freshwater_ecology_info_web_2025[-1,]
  DATASET_freshwater_ecology_info_web_2025_final <- noduplicates(DATASET_freshwater_ecology_info_web_2025,"Taxon")
  DATASET_freshwater_ecology_info_web_2025_final2 <- DATASET_freshwater_ecology_info_web_2025_final %>%
    mutate(
      river = na_if(river, ""),
      lake  = na_if(lake, "")
    ) %>%
    filter(!is.na(river) & !is.na(lake)) %>%
    select(Taxon, Class, Status)
  DATASET_freshwater_ecology_info_web_2025_final2$Habitat <- "Freshwater"
  DATASET_freshwater_ecology_info_web_2025_final2$InvadedRange <- "Europe"
  names <- DATASET_freshwater_ecology_info_web_2025_final2$Taxon
  DATASET_freshwater_ecology_info_web_2025_final2$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName

  #Save
  write.xlsx(DATASET_freshwater_ecology_info_web_2025_final2, "./InputFiles/freshwatersubset_freshwater_ecology_info_web_2025.xlsx")
}


