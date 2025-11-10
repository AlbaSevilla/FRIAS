mahapatra_et_al_2023_Download_and_Depurate <- function(){
  #DESCARGAR
  url <- "https://link.springer.com/article/10.1007/s10750-023-05299-z/tables/1"
  res <- read_html(url)
  res2 <- res %>% html_table()
  res2
  res3 <- as.data.frame(res2)
  res3
  res3$Habitat_Database <- "Freshwater"
  colnames(res3) <- gsub(" ", "_", colnames(res3))
  colnames(res3) <- gsub("\\.", "_", colnames(res3))


  res3 <- res3[, c("Species", setdiff(names(res3), "Species"))]
  res3 <- res3 %>%
    mutate(Species = str_extract(Species, "^\\S+\\s+\\S+"))
  write.xlsx(res3, "Inputfiles/originaldatabase_mahapatra_et_al_2023.xlsx")
  write.xlsx(res3, "Inputfiles/freshwatersubset_mahapatra_et_al_2023.xlsx")
  ##################################
  ######### HABITAT ################

  #Son de base Freshwater, así que no hay que comprobar nada

}
