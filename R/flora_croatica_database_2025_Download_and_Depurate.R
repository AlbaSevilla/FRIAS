flora_croatica_database_2025_Download_and_Depurate <- function(){
  #Download
  url <- "https://hirc.botanic.hr/tmp/7bdf66b2-4b44-4b5b-b772-61a62d20ea7d.xlsx.zip"
  destfile <- "InputFiles/originaldatabase_flora_croatica_database_2025.zip"
  download.file(url, destfile, mode = "wb")
  unzip("InputFiles/originaldatabase_flora_croatica_database_2025.zip", exdir = "InputFiles")
  original_path <- getwd()
  dat <- read_excel("InputFiles/FCD.xlsx", col_names = FALSE)
  names(dat) <- gsub("\\.", "_", names(dat))
  names(dat) <- c("CodeID", "Species_name")
  write.xlsx(dat, "InputFiles/originaldatabase_flora_croatica_database_2025.xlsx")

  #Depurate
  dat_noduplicates <- noduplicates(dat, "Species_name")
  dataset <- dat_noduplicates
  names <- dataset$Species_name
  acept_name <- name_backbone_checklist(names)$canonicalName
  dat_act <- check_habitat(acept_name, dataset)
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh$Invaded_country <- "Croatia"

  #Save
  write.xlsx(dat_fresh, "InputFiles/freshwatersubset_flora_croatica_database_2025.xlsx")
}
