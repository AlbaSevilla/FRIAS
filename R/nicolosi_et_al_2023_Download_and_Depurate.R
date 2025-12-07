nicolosi_et_al_2023_Download_and_Depurate <- function(){
  #Depurate
  dat <- read_excel("InputFiles/originaldatabase_nicolosi_et_al_2023.xlsx", col_names = TRUE)
  colnames(dat) <- dat[2,]
  dat_noduplicates <- noduplicates(dat, "Species")
  nombres <- dat_noduplicates$Species
  nombres_aceptados <- name_backbone_checklist(nombres)$canonicalName
  dat_habitat <- check_habitat(nombres_aceptados, dat_noduplicates)
  dat_fresh <- dat_habitat %>% filter(grepl("FRESHWATER", Habitat))

  #Save
  write.xlsx(dat_fresh, "InputFiles/freshwatersubset_nicolosi_et_al_2023.xlsx")
}
