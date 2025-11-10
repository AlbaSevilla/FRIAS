mikkola_et_al_2012_Download_and_Depurate <- function(){
  ##################################################
  ############## DESCARGAR #########################

  url <- "https://www.intechopen.com/chapters/38666"
  res <- read_html(url)
  res2 <- res %>% html_table()
  res3 <- res2[[1]]
  res4 <- as.data.frame(res3)
  res4
  colnames(res4) <- as.character(unlist(res4[1, ]))
  res4 <- res4[-1, ]
  res4$Invaded_country <- "Kyrgyzstan"
  names(res4) <- gsub(" ", "_", names(res4))

  write.xlsx(res4, "InputFiles/originaldatabase_mikkola_et_al_2012.xlsx")

  #### HABITAT
  source(file.path("R", "check_habitat.r"))
  dataset <- res4
  names <- res4$Scientific_name
  acep_names <- name_backbone_checklist(names)$canonicalName
  dat_act <- check_habitat(acep_names, dataset)
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh2 <- dat_fresh %>% filter(Introduced=="+")
  names(dat_fresh2) <- gsub(" ", "_", names(dat_fresh2))
  write.xlsx(dat_fresh2, "InputFiles/freshwatersubset_mikkola_et_al_2012.xlsx")

}
