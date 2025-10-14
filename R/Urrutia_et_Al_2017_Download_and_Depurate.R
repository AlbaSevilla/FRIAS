Urrutia_et_Al_2017_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  url <- "https://gayanabotanica.cl/index.php/gb/article/download/245/81/1891"
  destfile <- "InputFiles/Step0_OriginalDatabase_Urrutia_et_Al_2017.pdf"
  download.file(url, destfile, mode = "wb")

  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  lista9 <- pdf_text(destfile)[5]
  texto_tabla <- lista9
  library(dplyr)
  library(stringr)
  library(readr)
  # Suponiendo que 'texto_tabla' es tu texto original:
  lineas <- unlist(strsplit(texto_tabla, "\n"))

  # Elimina líneas vacías y encabezados innecesarios
  lineas <- lineas[!grepl("^\\s*$", lineas)]
  lineas <- lineas[!grepl("TABLA 1|Family, geographical origin|Plantas acuáticas invasoras", lineas)]

  # Unir líneas partidas
  tabla_limpia <- c()
  for (i in seq_along(lineas)) {
    if (i == 1) {
      tabla_limpia <- lineas[i]
    } else if (grepl("^\\s{10,}", lineas[i])) { # Si la línea empieza con muchos espacios, es continuación
      tabla_limpia[length(tabla_limpia)] <- paste(tabla_limpia[length(tabla_limpia)], trimws(lineas[i]))
    } else {
      tabla_limpia <- c(tabla_limpia, lineas[i])
    }
  }
  # Elimina filas que no tienen sentido (por ejemplo, sólo números)
  tabla_limpia <- tabla_limpia[!grepl("^\\d+\\s*$", tabla_limpia)]
  tabla_lista <- strsplit(tabla_limpia, " {2,}")
  tabla_df <- as.data.frame(do.call(rbind, tabla_lista), stringsAsFactors = FALSE)
  tabla_df <- tabla_df[,c(1:4, 8)]
  colnames(tabla_df) <- tabla_df[1,]
  colnames(tabla_df) <- gsub(" ", "_", colnames(tabla_df))
  tabla_df <- tabla_df[-1,]
  write.xlsx(tabla_df, "Inputfiles/Step0_OriginalDatabase_Urrutia_et_Al_2017.xlsx")

  #Habitat
  source(file.path("R", "check_habitat.r"))
  dataset <- tabla_df
  nombres <- dataset$Especie
  nom_acep <- name_backbone_checklist(nombres)$canonicalName
  habitat_dat <- check_habitat(nom_acep, dataset)
  # Elimina columnas con nombre NA o ""
  habitat_dat <- habitat_dat[, !is.na(names(habitat_dat)) & names(habitat_dat) != ""]

  #Freshwater
  freshwater_species <- habitat_dat %>% dplyr::filter(grepl("FRESHWATER", Habitat))

  freshwater_species$Invaded_country <- "Chile"

  write.xlsx(freshwater_species, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Urrutia_et_Al_2017.xlsx")
}
