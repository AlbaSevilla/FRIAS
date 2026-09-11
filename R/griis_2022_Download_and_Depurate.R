griis_2022_Download_and_Depurate <- function() {
  #Download
  url <- "https://zenodo.org/records/6348164/files/GRIIS%20-%20Country%20Compendium%20V1_0.csv?download=1"
  destfile <- "InputFiles/originaldatabase_griis_2022.csv"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dat <- read.csv("InputFiles/originaldatabase_griis_2022.csv")
  dat$habitat <- factor(dat$habitat)
  dat$establishmentMeans <- as.character(dat$establishmentMeans)
  dat$establishmentMeans[dat$establishmentMeans == "NATIVE|ALIEN"] <-
    "Native and Introduced in different areas but same country"
  dat$establishmentMeans <- as.factor(dat$establishmentMeans)
  dat$taxonRank <- factor(dat$taxonRank)
  dat$isHybrid <- factor(dat$isHybrid)
  griis_2022_freshwater <- dat %>%
    filter(habitat %in% c("FRESHWATER",
                          "FRESHWATER|BRACKISH", "FRESHWATER|BRACKISH|MARINE",
                          "FRESHWATER|MARINE", #"TERRESTRIAL|FRESHWATER",
                          "TERRESTRIAL|FRESHWATER|MARINE",
                          "TERRESTRIAL|FRESHWATER|BRACKISH|MARINE",
                          "TERRESTRIAL|FRESHWATER|BRACKISH")) %>%
    filter(establishmentMeans %in% c("ALIEN", "Native and Introduced in different areas but same country")) %>%
    filter(taxonRank %in% c("SPECIES")) %>%
    filter(isHybrid %in% c("FALSE"))
  griis_2022_noduplicates <- noduplicates(griis_2022_freshwater, column_name_species = "species")
  nrow(griis_2022_noduplicates)

  #Save
  write.xlsx(griis_2022_noduplicates, "./InputFiles/freshwatersubset_griis_2022.xlsx")
}
