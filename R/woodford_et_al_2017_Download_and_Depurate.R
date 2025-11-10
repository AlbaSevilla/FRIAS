woodford_et_al_2017_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  #a mano
  #url <- "https://www.dffe.gov.za/sites/default/files/legislations/nemba_invasivespecieslist_g43726gon1003.pdf"
  destfile <- "InputFiles/originaldatabase_woodford_et_al_2017.pdf"
  #download.file(url, destfile, mode = "wb")

  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  #Necesitamos: List 1, 6, 7, 9 (statutory order, spp. are present or just regulated?)
  lista1 <- pdf_text(destfile)[7:30]
  texto1 <- lista1
  lista6 <- pdf_text(destfile)[37]
  texto6 <- lista6
  lista7 <- pdf_text(destfile)[38:41]
  texto7 <- lista7
  lista9 <- pdf_text(destfile)[43]
  texto9 <- lista9
  extraer_nombres_cientificos <- function(texto) {
    # Combina todas las líneas en un solo string
    texto_colapsado <- paste(texto, collapse = " ")

    # Usa expresiones regulares para encontrar nombres científicos
    matches <- gregexpr("\\b[A-Z][a-z]+\\s[a-z]+(?:\\s\\([^\\)]+\\))?(?:\\s[a-zA-Z\\.]+)*", texto_colapsado, perl = TRUE)

    # Extrae los nombres encontrados
    nombres <- regmatches(texto_colapsado, matches)[[1]]

    # Elimina duplicados y devuelve el resultado
    unique(trimws(nombres))
  }
  especies1 <- extraer_nombres_cientificos(texto1)
  especies6 <- extraer_nombres_cientificos(texto6)
  especies7 <- extraer_nombres_cientificos(texto7)
  especies9 <- extraer_nombres_cientificos(texto9)

  especies <- c(especies1, especies6, especies7, especies9)
  dataset <- as.data.frame(especies)
  names(dataset)
  write.xlsx(dataset, "Inputfiles/originaldatabase_woodford_et_al_2017.xlsx")

  #Habitat
  dataset <- read_excel("Inputfiles/originaldatabase_woodford_et_al_2017.xlsx")
  source(file.path("R", "check_habitat.r"))
  dataset <- dataset
  nombres <- dataset$especies
  nom_acep <- name_backbone_checklist(nombres)$canonicalName
  habitat_dat <- check_habitat(nom_acep, dataset)

  #Freshwater
  freshwater_species <- habitat_dat %>% filter(grepl("FRESHWATER", Habitat))
  freshwater_species$Invaded_country <- "South Africa"

  freshwater_species2 <- freshwater_species %>%
    mutate(across(everything(), ~str_replace_all(as.character(.), ";", ",")))

  freshwater_species2 <- freshwater_species2[,-1]
  write.xlsx(freshwater_species2, "Inputfiles/freshwatersubset_woodford_et_al_2017.xlsx")
}
