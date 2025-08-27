InvasiveSpeciesPhilippines_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  #A mano de
  #url <- "https://www.researchgate.net/profile/Lorenzo-Vilizzi/publication/382254947_Quantifying_the_current_and_future_risk_of_invasiveness_of_the_non-native_fishes_in_Ramsar-listed_Lake_Naujan_Philippines/links/66f3c93c553d245f9e34fe2b/Quantifying-the-current-and-future-risk-of-invasiveness-of-the-non-native-fishes-in-Ramsar-listed-Lake-Naujan-Philippines.pdf"


  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  destfile <- "Inputfiles/Step0_OriginalDatabase_InvasiveSpeciesPhilippines.pdf"
  text_pages <- pdf_text(destfile)[5]
  texto <- text_pages
  texto

  species_name <- c("Anabas testudineus", "Carassius auratus", "Channa striata", "Clarias batrachus",
                    "Coptodon zillii", "Cyprinus carpio", "Leiopotherapon plumbeus", "Oreochromis niloticus",
                    "Poecilia reticulata", "Poecilia sphenops", "Pterygoplichthys disjunctivus", "Trichopodus pectoralis",
                    "trichopodus trichopterus")
  Outcome <- c("Non-invasive", "Invasive","Invasive","Invasive","Invasive","Invasive",
               "Non-invasive", "Invasive","Invasive","Invasive","Invasive","Invasive",
               "Non-invasive")
  Tabla_final <- cbind(species_name, Outcome)
  Tabla_final <- as.data.frame(Tabla_final)
  Tabla_final$Invaded_country <- "Philippines"
  Tabla_final$Habitat_database <- "FRESHWATER"
  Tabla_final$AcceptedNameGBIF <- name_backbone_checklist(Tabla_final$species_name)$canonicalName
  write_xlsx(Tabla_final, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_InvasiveSpeciesPhilippines.xlsx")
}
