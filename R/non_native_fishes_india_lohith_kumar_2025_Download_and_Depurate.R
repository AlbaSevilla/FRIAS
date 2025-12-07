non_native_fishes_india_lohith_kumar_2025_Download_and_Depurate <- function() {
  #Depurate
  dat <- read.xlsx("InputFiles/originaldatabase_non_native_fishes_india_lohith_kumar_2025.xlsx") #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.
  names(dat) <- gsub("[:.]", "_", names(dat))
  NonNativeFishesIndia_noduplicates <- noduplicates(dat, "Species")
  NonNativeFishesIndia_noduplicates$Habitat <- "Freshwater"
  rownames(NonNativeFishesIndia_noduplicates) <- NULL
  NonNativeFishesIndia_noduplicates$Oldest_Record <- NonNativeFishesIndia_noduplicates$`Time_of_introduction/first_report`
  NonNativeFishesIndia_noduplicates <- NonNativeFishesIndia_noduplicates[, c("Species", setdiff(names(NonNativeFishesIndia_noduplicates), "Species"))]
  NonNativeFishesIndia_noduplicates$InvadedCountry <- "India"

  #Save
  write.xlsx(NonNativeFishesIndia_noduplicates, "./InputFiles/freshwatersubset_non_native_fishes_india_lohith_kumar_2025.xlsx")
}
