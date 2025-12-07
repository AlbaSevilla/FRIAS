gidias_Download_and_Depurate <- function() {
  #Download
  url <- "https://springernature.figshare.com/ndownloader/articles/27908838/versions/1"
  destfile <- "InputFiles/originaldatabase_gidias_2025.zip"
  download.file(url, destfile, mode = "wb")
  unzip("InputFiles/originaldatabase_gidias_2025.zip", exdir = "InputFiles")
  original_path <- getwd()

  #Depurate
  dat <- read_excel("InputFiles/GIDIAS_20250417_Excel.xlsx")
  gidias_freshwater_01 <- dat %>%
    filter(Realm %in% c("Freshwater"))
  gidias_freshwater_02 <- subset(gidias_freshwater_01, Year.of.impact > 0 & !is.na(Year.of.impact))
  gidias_freshwater_02 <- gidias_freshwater_02 %>%
    mutate(EICAT_by_Mechanism = paste(magnitude.Nature, mechanism.Nature.clean, sep=" -"))
  gidias_noduplicates <- noduplicates(gidias_freshwater_02, column_name_species = "IAS.Species.Name")
  gidias_noduplicates <- as.data.frame(gidias_noduplicates)
  gidias_noduplicates <- gidias_noduplicates %>%
    select(-Reference)
  gidias_noduplicates <- gidias_noduplicates %>%
    select(-Text.excerpt)
  write.xlsx(gidias_noduplicates, "InputFiles/originaldatabase_gidias_2024.xlsx")
  gidias_noduplicates <- read_excel("InputFiles/originaldatabase_gidias_2024.xlsx")
  gidias_noduplicates <- OldestDate(gidias_noduplicates, "Year.of.impact")
  gidias_freshwater_03 <- gidias_noduplicates
  names(gidias_freshwater_03)[names(gidias_freshwater_03) == "IAS.Taxon"] <- "Group"
  names(gidias_freshwater_03) <- gsub("\\.", "_", names(gidias_freshwater_03))
  gidias_final <- gidias_freshwater_03 %>% filter(direction_Nature == "Negative")
  gidias_final$EICAT_by_Mechanism <- gsub("\\([^)]*\\)", "", gidias_final$EICAT_by_Mechanism)
  gidias_final$EICAT_by_Mechanism <- gsub("\\s{2,}", " ", gidias_final$EICAT_by_Mechanism)
  gidias_final$EICAT_by_Mechanism <- gsub("\\s+,", ",", gidias_final$EICAT_by_Mechanism)
  gidias_final$EICAT_by_Mechanism <- trimws(gidias_final$EICAT_by_Mechanism)

  #Save
  write.xlsx(gidias_final, "InputFiles/freshwatersubset_gidias_2025.xlsx")
}
