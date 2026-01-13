tedesco_et_al_2017_Download_and_Depurate <- function(){
  #Download
  url <- "https://figshare.com/ndownloader/files/8964583"
  destfile <- "InputFiles/originaldatabase_tedesco_et_al_2017.zip"
  download.file(url, destfile, mode = "wb")
  unzip("InputFiles/originaldatabase_tedesco_et_al_2017.zip", exdir = "InputFiles")
  original_path <- getwd()
  dat <- read.csv("InputFiles/Occurence_Table.csv", sep=";") #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.
  names(dat) <- gsub("\\.", "_", names(dat))
  rownames(dat) <- NULL
  write.csv2(dat, "Inputfiles/originaldatabase_tedesco_et_al_2017.csv")

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
  locations_table <- read.csv2("Inputfiles/Drainage_Basins_Table.csv")
  DatabaseBasin$BasinCountries <- mapply(function(basins_str) {
    basins <- unlist(strsplit(basins_str, ","))  # Separar cuencas por coma
    COUNTRIES <- unlist(sapply(basins, function(country) {
      country <- trimws(country)  # Quitar espacios
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

  #Save
  write.xlsx(DatabaseBasin, "Inputfiles/freshwatersubset_tedesco_et_al_2017.xlsx")
}
