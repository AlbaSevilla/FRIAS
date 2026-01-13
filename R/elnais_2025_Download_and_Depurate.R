elnais_2025_Download_and_Depurate <- function(){
  #Download
  url <- "https://elnais.hcmr.gr/elnais-species/"
  res <- read_html(url)
  res2 <- res %>% html_table()
  res3 <- res2[[1]]
  res4 <- as.data.frame(res3)
  colnames(res4) <- as.character(unlist(res4[1, ]))
  res4 <- res4[-1, ]
  rownames(df) <- NULL
  names(res4) <- gsub(" ", "_", names(res4))
  columnas_seleccionar <- c("SPECIES_NAME", "FIRST_SIGHTING", "PATHWAY_NEW/CBD",
                            "STATUS","ESTABLISHMENT_SUCCESS")
  res5 <- res4[,columnas_seleccionar]
  res6 <- res5 %>% select("SPECIES_NAME", everything())
  names(res6)[names(res6) == "FIRST_SIGHTING"] <- "FirstRecord"
  res6$Invaded_country <- "Greece"
  write.xlsx(res6, "Inputfiles/originaldatabase_elnais_2025.xlsx")

  #Depurate
  dataset <- res6
  nombres <- dataset$SPECIES_NAME
  nom_acep <- name_backbone_checklist(nombres)$canonicalName
  dat_act <- check_habitat(nom_acep, dataset)
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  dat_fresh
  dat_fresh$Invaded_country <- "Greece"

  #Save
  write.xlsx(dat_fresh, "Inputfiles/freshwatersubset_elnais_2025.xlsx")
}
