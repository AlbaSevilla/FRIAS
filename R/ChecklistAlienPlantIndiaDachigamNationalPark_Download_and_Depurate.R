ChecklistAlienPlantIndiaDachigamNationalPark_Download_and_Depurate <- function(){

url <- "https://link.springer.com/article/10.1007/s43538-022-00063-8/tables/1"
res <- read_html(url)
res2 <- res %>% html_table()
res3 <- as.data.frame(res2)
res3
write.xlsx(res3, "Inputfiles/Step0_OriginalDatabase_ChecklistAlienPlantIndiaDachigamNationalPark.xlsx")



#Invaded_country
res3$Invaded_country <- "India"


#Habitat
source(file.path("R", "check_habitat.r"))
dataset <- res3
names <- dataset$Species
acep_names <- name_backbone_checklist(names)$canonicalName
dat_act <- check_habitat(acep_names, dataset)
dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))


write.xlsx(dat_fresh, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_ChecklistAlienPlantIndiaDachigamNationalPark.xlsx")





}

