tedesco_et_al_2017_Download_and_Depurate <- function(){
  #Download
  url <- "https://figshare.com/ndownloader/files/8964583"
  destfile <- "InputFiles/originaldatabase_tedesco_et_al_2017.zip"
  download.file(url, destfile, mode = "wb")
  unzip("InputFiles/originaldatabase_tedesco_et_al_2017.zip", exdir = "InputFiles")
  original_path <- getwd()
  dat <- read.csv("InputFiles/Occurence_Table.csv", sep=";")
  names(dat) <- gsub("\\.", "_", names(dat))
  rownames(dat) <- NULL
  write.csv2(dat, "InputFiles/originaldatabase_tedesco_et_al_2017.csv")

  #Depurate
  dat2 <- dat %>% filter(X3_Native_Exotic_Status == "exotic")
  dat_noduplicates <- noduplicates(dat2, "X6_Fishbase_Valid_Species_Name")
  dat_noduplicates[] <- lapply(dat_noduplicates, \(x) if (is.character(x)) gsub("\\.", " ", x) else x)
  dataset <- dat_noduplicates
  nombres <- dataset$X6_Fishbase_Valid_Species_Name
  acept_name <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(acept_name, dataset)
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh
  DatabaseBasin <-dat_fresh
  locations_table <- read.csv2("InputFiles/Drainage_Basins_Table.csv")
  DatabaseBasin$BasinCountries <- mapply(function(basins_str) {
    basins <- unlist(strsplit(basins_str, ","))
    COUNTRIES <- unlist(sapply(basins, function(country) {
      country <- trimws(country)
      country_match <- locations_table$X2.Country[locations_table$X1.Basin.Name == country]
      if (length(country_match) == 0) {
        country_match <- locations_table$X2.Country[str_detect(locations_table$X1.Basin.Name, country)]
      }
      if (length(country_match) == 0) return(NA)
      split_match <- unlist(strsplit(country_match, ";"))
      trimws(split_match)
    }))
    COUNTRIES <- unique(na.omit(COUNTRIES))
    if (length(COUNTRIES) == 0) {
      return(NA)
    } else {
      return(paste(COUNTRIES, collapse = "; "))
    }
  }, DatabaseBasin$X1_Basin_Name)


  DatabaseBasin <- DatabaseBasin %>%
    mutate(
      BasinCountries = case_when(
        grepl("Paraiba do Sul", X1_Basin_Name, ignore.case = TRUE) ~ "Brazil",
        grepl("Issyk Kul", X1_Basin_Name, ignore.case = TRUE) ~ "Kyrgyzstan",
        grepl("Saint Laurent", X1_Basin_Name, ignore.case = TRUE) ~ "Canada",
        grepl("Don Russia", X1_Basin_Name, ignore.case = TRUE) ~ "Russia",
        grepl("Colorado USA|Colorado Texas", X1_Basin_Name, ignore.case = TRUE) ~ "United States",
        grepl("La Trobe", X1_Basin_Name, ignore.case = TRUE) ~ "Australia",
        grepl("Grande Panama", X1_Basin_Name, ignore.case = TRUE) ~ "Panama",
        grepl("Negro Argentina", X1_Basin_Name, ignore.case = TRUE) ~ "Argentina",
        grepl("Victoria lake", X1_Basin_Name, ignore.case = TRUE) ~ "Tanzania; Uganda; Kenya",
        grepl("Sao Francisco", X1_Basin_Name, ignore.case = TRUE) ~ "Brazil",
        grepl("Daliao He", X1_Basin_Name, ignore.case = TRUE) ~ "China",
        grepl("Pinios Pel", X1_Basin_Name, ignore.case = TRUE) ~ "Greece",
        grepl("Blackwater USA", X1_Basin_Name, ignore.case = TRUE) ~ "United States",
        grepl("Bear lake|Bear river", X1_Basin_Name, ignore.case = TRUE) ~ "United States",
        grepl("Murray Darling|Lake Eyre Basin", X1_Basin_Name, ignore.case = TRUE) ~ "Australia",
        grepl("Wadi Nashu", X1_Basin_Name, ignore.case = TRUE) ~ "Libya",
        grepl("Ebinur Hu", X1_Basin_Name, ignore.case = TRUE) ~ "China",
        grepl("San Juan Nicaragua", X1_Basin_Name, ignore.case = TRUE) ~ "Nicaragua",
        grepl("Thu Bon", X1_Basin_Name, ignore.case = TRUE) ~ "Vietnam",
        grepl("Shatt al Arab", X1_Basin_Name, ignore.case = TRUE) ~ "Iraq; Iran",
        grepl("Little Manatee", X1_Basin_Name, ignore.case = TRUE) ~ "United States",
        grepl("Hai He", X1_Basin_Name, ignore.case = TRUE) ~ "China",
        grepl("Cape Fear", X1_Basin_Name, ignore.case = TRUE) ~ "United States",
        grepl("Sao Mateus", X1_Basin_Name, ignore.case = TRUE) ~ "Brazil",
        grepl("Yoshii Okayama", X1_Basin_Name, ignore.case = TRUE) ~ "Japan",
        grepl("Crni Drim", X1_Basin_Name, ignore.case = TRUE) ~ "Albania; North Macedonia",
        grepl("Calleguas creek, Death Valley, Santa Clara",X1_Basin_Name, ignore.case = TRUE) ~ "United States",
        grepl("Calleguas creek, Death Valley, Santa Ana",X1_Basin_Name, ignore.case = TRUE) ~ "United States",
        grepl("Clyde Scotland, Tweed UK",X1_Basin_Name, ignore.case = TRUE) ~ "United Kingdom",
        grepl("Antelope Wash",X1_Basin_Name, ignore.case = TRUE) ~ "United States",
        grepl("Alemeda creek, Calleguas creek, Death Valley, Los Angeles, Santa Clara",X1_Basin_Name, ignore.case = TRUE) ~ "United States",
        grepl("Grande USA",X1_Basin_Name, ignore.case = TRUE) ~ "United States",

        TRUE ~ BasinCountries
      )
    )

  #Save
  write.xlsx(DatabaseBasin, "InputFiles/freshwatersubset_tedesco_et_al_2017.xlsx")
}
