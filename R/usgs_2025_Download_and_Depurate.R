usgs_2025_Download_and_Depurate <- function() {
  #Depurate
  dat <- read.csv("InputFiles/originaldatabase_usgs_2025.csv", sep=",") #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.
  usgs_2025_freshwater_01 <- dat %>%
    filter(Native.Habitat %in% c("Freshwater","Freshwater-Marine","Marine-Freshwater"))
  origin <- unique(usgs_2025_freshwater_01$Species.Origin)

  usgs_2025_freshwater_02 <- usgs_2025_freshwater_01 %>%
    filter(Species.Origin %in% c("Exotic","Unknown","Exotic Hybrid"))
  usgs_2025_freshwater_03 <- usgs_2025_freshwater_02 %>%
    select(-Common.Name)
  usgs_2025_noduplicates <- noduplicates(usgs_2025_freshwater_03, "Scientific.Name")
  names(usgs_2025_noduplicates)[names(usgs_2025_noduplicates) == "Scientific.Name"] <- "Scientific_Name"
  names(usgs_2025_noduplicates)[names(usgs_2025_noduplicates) == "Species.Origin"] <- "Species_Origin"
  names(usgs_2025_noduplicates)[names(usgs_2025_noduplicates) == "Native.Habitat"] <- "Native_Habitat"
  usgs_2025_noduplicates <- usgs_2025_noduplicates[, c("Scientific_Name", setdiff(names(usgs_2025_noduplicates), "Scientific_Name"))]
  usgs_2025_noduplicates$InvadedCountry <- "United States of America"

  #Save
  write.xlsx(usgs_2025_noduplicates, "./InputFiles/freshwatersubset_usgs_2025.xlsx")
}
