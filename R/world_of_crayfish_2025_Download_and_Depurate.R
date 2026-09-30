world_of_crayfish_2025_Download_and_Depurate.xlsx <- function() {
  #Depurate
  dat <- read.xlsx(file.path("InputFiles","originaldatabase_world_of_crayfish_2025.xlsx"))
  dat <- dat[,-20]
  dat2_noduplicates <- noduplicates(dat, "Crayfish_scientific_name")
  Habitat <- "Freshwater"
  dat2_noduplicates$Habitat <- Habitat
  names(dat2_noduplicates)
  selected_columns <- c("Crayfish_scientific_name", "Status", "Year_of_record")
  dat3_noduplicates <- dat2_noduplicates[,selected_columns]
  dat3 <- OldestDate(dat3_noduplicates,"Year_of_record")
  dat4 <- as.data.frame(dat3)
  dat4$Habitat_database <- "Freshwater"
  names <- dat4$Crayfish_scientific_name
  dat4$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName
  #Save
  write.xlsx(dat4, "./InputFiles/freshwatersubset_world_of_crayfish_2025.xlsx")
}
