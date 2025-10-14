Lüskow_et_Al_2023_Download_and_Depurate <- function() {
  ################################################################################
  ######## OBTENCIÓN DE LA BASE DE DATOS A TRAVÉS DE ZENODO ######################
  ################################################################################
#
#   url <- "https://download.pangaea.de/dataset/962186/files/L%C3%BCskow_et_al_jellyfish_literature.xlsx"
#   destfile <- "InputFiles/Step0_OriginalDatabase_Lüskow_et_Al_2023.xlsx" #Directorio donde se guardará y con qué nombre
#   if (!dir.exists("InputFiles")) { #Para comprobar si la carpeta Inputfiles existe y si no se crea
#     dir.create("InputFiles")
#   }
#   download.file(url, destfile, mode = "wb") #Descarga del archivo
#
#   # Confirmación de que el archivo se descargó correctamente
#   cat("Archivo descargado correctamente:", destfile, "\n")



  datos <- read.xlsx(file.path("Inputfiles","Step0_OriginalDatabase_Lüskow_et_Al_2023.xlsx"),sep=";")
  # Agregar la columna 'SpeciesName' con condiciones
  datos <- datos %>%
    mutate(SpeciesName = case_when(
      Genus == "Craspedacusta" ~ "Craspedacusta sowerbii",
      Genus == "Astrohydra" ~ "Astrohydra japonica",
      Genus == "Limnocnida" ~ "Limnocnida indica",
      TRUE ~ NA_character_  # Para los que no cumplen ninguna condición
    ))

  datos <- datos %>% select(SpeciesName, everything())

  datos <- datos %>%
    select(SpeciesName)

  datos <- unique(datos)

  # Guardar el dataframe en un archivo Excel
  write.xlsx(datos,file.path("Inputfiles", paste( "Step0_OriginalDatabaseFreshwaterNODUPLICATES_Lüskow_et_Al_2023.xlsx")), rowNames = FALSE)
  destfile <- "InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Lüskow_et_Al_2023.xlsx" #Directorio donde se guardará y con qué nombre
  cat("Archivo descargado correctamente:", destfile, "\n")
}
