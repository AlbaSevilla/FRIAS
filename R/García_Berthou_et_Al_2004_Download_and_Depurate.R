García_Berthou_et_Al_2004_Download_and_Depurate <- function(){
  ###################################################################
  ####### García_Berthou_et_Al_2004 "otros" #########################################
  ###################################################################
  url <- "https://invasiber2.org/fitxa_llista.php?taxonomic=1"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[5]]
  res3
  # Crear los nombres científicos combinando Género (X1) y Especie (X2)
  nombres_cientificos <- res3 %>%
    select(X1, X2) %>%  # Seleccionar las columnas de Género y Especie
    filter(row_number() > 1) %>%  # Eliminar la primera fila de encabezado
    mutate(nombre_cientifico = paste(X1, X2)) %>%  # Unir Género y Especie
    select(-X1) %>%
    select(-X2)
  # Ver los resultados
  enlaces <- html_nodes(res, "a")
  hrefs_especies <- html_attr(enlaces, "href")
  enlaces_fitxa <- hrefs_especies[grepl("fitxa_detalls\\.php", hrefs_especies)]
  enlaces_completos_busqueda <- paste0("https://invasiber2.org/", enlaces_fitxa)
  enlaces_especies1 <- (unique(enlaces_completos_busqueda))

  ###################################################################
  ####### García_Berthou_et_Al_2004 "Algas" #########################################
  ###################################################################
  url <- "https://invasiber2.org/fitxa_llista.php?taxonomic=2"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[5]]
  res3
  # Crear los nombres científicos combinando Género (X1) y Especie (X2)
  nombres_cientificos <- res3 %>%
    select(X1, X2) %>%  # Seleccionar las columnas de Género y Especie
    filter(row_number() > 1) %>%  # Eliminar la primera fila de encabezado
    mutate(nombre_cientifico = paste(X1, X2)) %>%  # Unir Género y Especie
    select(-X1) %>%
    select(-X2)
  # Ver los resultados
  enlaces <- html_nodes(res, "a")
  hrefs_especies <- html_attr(enlaces, "href")
  enlaces_fitxa <- hrefs_especies[grepl("fitxa_detalls\\.php", hrefs_especies)]
  enlaces_completos_busqueda <- paste0("https://invasiber2.org/", enlaces_fitxa)
  enlaces_especies2 <- (unique(enlaces_completos_busqueda))
  enlaces_especies2

  ###################################################################
  ####### García_Berthou_et_Al_2004 "Otras plantas" #################################
  ###################################################################
  url <- "https://invasiber2.org/fitxa_llista.php?taxonomic=3"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[5]]
  res3
  # Crear los nombres científicos combinando Género (X1) y Especie (X2)
  nombres_cientificos <- res3 %>%
    select(X1, X2) %>%  # Seleccionar las columnas de Género y Especie
    filter(row_number() > 1) %>%  # Eliminar la primera fila de encabezado
    mutate(nombre_cientifico = paste(X1, X2)) %>%  # Unir Género y Especie
    select(-X1) %>%
    select(-X2)
  # Ver los resultados
  enlaces <- html_nodes(res, "a")
  hrefs_especies <- html_attr(enlaces, "href")
  enlaces_fitxa <- hrefs_especies[grepl("fitxa_detalls\\.php", hrefs_especies)]
  enlaces_completos_busqueda <- paste0("https://invasiber2.org/", enlaces_fitxa)
  enlaces_especies3 <- (unique(enlaces_completos_busqueda))

  ###################################################################
  ####### INVASIBER "Insectos"      #################################
  ###################################################################
  url <- "https://invasiber2.org/fitxa_llista.php?taxonomic=4"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[5]]
  res3
  # Crear los nombres científicos combinando Género (X1) y Especie (X2)
  nombres_cientificos <- res3 %>%
    select(X1, X2) %>%  # Seleccionar las columnas de Género y Especie
    filter(row_number() > 1) %>%  # Eliminar la primera fila de encabezado
    mutate(nombre_cientifico = paste(X1, X2)) %>%  # Unir Género y Especie
    select(-X1) %>%
    select(-X2)
  # Ver los resultados
  enlaces <- html_nodes(res, "a")
  hrefs_especies <- html_attr(enlaces, "href")
  enlaces_fitxa <- hrefs_especies[grepl("fitxa_detalls\\.php", hrefs_especies)]
  enlaces_completos_busqueda <- paste0("https://invasiber2.org/", enlaces_fitxa)
  enlaces_especies4 <- (unique(enlaces_completos_busqueda))

  url <- "https://invasiber2.org/fitxa_llista.php?pageNum_rsFitxa=1&taxonomic=4&totalRows_rsFitxa=11"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[5]]
  res3
  # Crear los nombres científicos combinando Género (X1) y Especie (X2)
  nombres_cientificos <- res3 %>%
    select(X1, X2) %>%  # Seleccionar las columnas de Género y Especie
    filter(row_number() > 1) %>%  # Eliminar la primera fila de encabezado
    mutate(nombre_cientifico = paste(X1, X2)) %>%  # Unir Género y Especie
    select(-X1) %>%
    select(-X2)
  # Ver los resultados
  enlaces <- html_nodes(res, "a")
  hrefs_especies <- html_attr(enlaces, "href")
  enlaces_fitxa <- hrefs_especies[grepl("fitxa_detalls\\.php", hrefs_especies)]
  enlaces_completos_busqueda <- paste0("https://invasiber2.org/", enlaces_fitxa)
  enlaces_especies5 <- (unique(enlaces_completos_busqueda))

  ###################################################################
  ####### INVASIBER "Crustaceos"    #################################
  ###################################################################
  url <- "https://invasiber2.org/fitxa_llista.php?taxonomic=5"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[5]]
  res3
  # Crear los nombres científicos combinando Género (X1) y Especie (X2)
  nombres_cientificos <- res3 %>%
    select(X1, X2) %>%  # Seleccionar las columnas de Género y Especie
    filter(row_number() > 1) %>%  # Eliminar la primera fila de encabezado
    mutate(nombre_cientifico = paste(X1, X2)) %>%  # Unir Género y Especie
    select(-X1) %>%
    select(-X2)
  # Ver los resultados
  enlaces <- html_nodes(res, "a")
  hrefs_especies <- html_attr(enlaces, "href")
  enlaces_fitxa <- hrefs_especies[grepl("fitxa_detalls\\.php", hrefs_especies)]
  enlaces_completos_busqueda <- paste0("https://invasiber2.org/", enlaces_fitxa)
  enlaces_especies6 <- (unique(enlaces_completos_busqueda))

  ###################################################################
  ####### INVASIBER "Otros invertebrados"    ########################
  ###################################################################
  url <- "https://invasiber2.org/fitxa_llista.php?taxonomic=6"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[5]]
  res3
  # Crear los nombres científicos combinando Género (X1) y Especie (X2)
  nombres_cientificos <- res3 %>%
    select(X1, X2) %>%  # Seleccionar las columnas de Género y Especie
    filter(row_number() > 1) %>%  # Eliminar la primera fila de encabezado
    mutate(nombre_cientifico = paste(X1, X2)) %>%  # Unir Género y Especie
    select(-X1) %>%
    select(-X2)
  # Ver los resultados
  enlaces <- html_nodes(res, "a")
  hrefs_especies <- html_attr(enlaces, "href")
  enlaces_fitxa <- hrefs_especies[grepl("fitxa_detalls\\.php", hrefs_especies)]
  enlaces_completos_busqueda <- paste0("https://invasiber2.org/", enlaces_fitxa)
  enlaces_especies7 <- (unique(enlaces_completos_busqueda))


  ###################################################################
  ####### INVASIBER "Peces"    ######################################
  ###################################################################
  url <- "https://invasiber2.org/fitxa_llista.php?taxonomic=7"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[5]]
  res3
  # Crear los nombres científicos combinando Género (X1) y Especie (X2)
  nombres_cientificos <- res3 %>%
    select(X1, X2) %>%  # Seleccionar las columnas de Género y Especie
    filter(row_number() > 1) %>%  # Eliminar la primera fila de encabezado
    mutate(nombre_cientifico = paste(X1, X2)) %>%  # Unir Género y Especie
    select(-X1) %>%
    select(-X2)
  # Ver los resultados
  enlaces <- html_nodes(res, "a")
  hrefs_especies <- html_attr(enlaces, "href")
  enlaces_fitxa <- hrefs_especies[grepl("fitxa_detalls\\.php", hrefs_especies)]
  enlaces_completos_busqueda <- paste0("https://invasiber2.org/", enlaces_fitxa)
  enlaces_especies8 <- (unique(enlaces_completos_busqueda))


  url <- "https://invasiber2.org/fitxa_llista.php?pageNum_rsFitxa=1&taxonomic=7&totalRows_rsFitxa=17"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[5]]
  res3
  # Crear los nombres científicos combinando Género (X1) y Especie (X2)
  nombres_cientificos <- res3 %>%
    select(X1, X2) %>%  # Seleccionar las columnas de Género y Especie
    filter(row_number() > 1) %>%  # Eliminar la primera fila de encabezado
    mutate(nombre_cientifico = paste(X1, X2)) %>%  # Unir Género y Especie
    select(-X1) %>%
    select(-X2)
  # Ver los resultados
  enlaces <- html_nodes(res, "a")
  hrefs_especies <- html_attr(enlaces, "href")
  enlaces_fitxa <- hrefs_especies[grepl("fitxa_detalls\\.php", hrefs_especies)]
  enlaces_completos_busqueda <- paste0("https://invasiber2.org/", enlaces_fitxa)
  enlaces_especies9 <- (unique(enlaces_completos_busqueda))

  ###################################################################
  ######### INVASIBER "Amfibios"    #################################
  ###################################################################
  url <- "https://invasiber2.org/fitxa_llista.php?taxonomic=8"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[5]]
  res3
  # Crear los nombres científicos combinando Género (X1) y Especie (X2)
  nombres_cientificos <- res3 %>%
    select(X1, X2) %>%  # Seleccionar las columnas de Género y Especie
    filter(row_number() > 1) %>%  # Eliminar la primera fila de encabezado
    mutate(nombre_cientifico = paste(X1, X2)) %>%  # Unir Género y Especie
    select(-X1) %>%
    select(-X2)
  # Ver los resultados
  enlaces <- html_nodes(res, "a")
  hrefs_especies <- html_attr(enlaces, "href")
  enlaces_fitxa <- hrefs_especies[grepl("fitxa_detalls\\.php", hrefs_especies)]
  enlaces_completos_busqueda <- paste0("https://invasiber2.org/", enlaces_fitxa)
  enlaces_especies10 <- (unique(enlaces_completos_busqueda))

  ###################################################################
  ######### INVASIBER "Reptiles"    #################################
  ###################################################################
  url <- "https://invasiber2.org/fitxa_llista.php?taxonomic=9"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[5]]
  res3
  # Crear los nombres científicos combinando Género (X1) y Especie (X2)
  nombres_cientificos <- res3 %>%
    select(X1, X2) %>%  # Seleccionar las columnas de Género y Especie
    filter(row_number() > 1) %>%  # Eliminar la primera fila de encabezado
    mutate(nombre_cientifico = paste(X1, X2)) %>%  # Unir Género y Especie
    select(-X1) %>%
    select(-X2)
  # Ver los resultados
  enlaces <- html_nodes(res, "a")
  hrefs_especies <- html_attr(enlaces, "href")
  enlaces_fitxa <- hrefs_especies[grepl("fitxa_detalls\\.php", hrefs_especies)]
  enlaces_completos_busqueda <- paste0("https://invasiber2.org/", enlaces_fitxa)
  enlaces_especies11 <- (unique(enlaces_completos_busqueda))

  ###################################################################
  ######### INVASIBER "Aves"    #####################################
  ###################################################################
  url <- "https://invasiber2.org/fitxa_llista.php?taxonomic=10"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[5]]
  res3
  # Crear los nombres científicos combinando Género (X1) y Especie (X2)
  nombres_cientificos <- res3 %>%
    select(X1, X2) %>%  # Seleccionar las columnas de Género y Especie
    filter(row_number() > 1) %>%  # Eliminar la primera fila de encabezado
    mutate(nombre_cientifico = paste(X1, X2)) %>%  # Unir Género y Especie
    select(-X1) %>%
    select(-X2)
  # Ver los resultados
  enlaces <- html_nodes(res, "a")
  hrefs_especies <- html_attr(enlaces, "href")
  enlaces_fitxa <- hrefs_especies[grepl("fitxa_detalls\\.php", hrefs_especies)]
  enlaces_completos_busqueda <- paste0("https://invasiber2.org/", enlaces_fitxa)
  enlaces_especies12 <- (unique(enlaces_completos_busqueda))

  ###################################################################
  ######### INVASIBER "Mamíferos"    ################################
  ###################################################################
  url <- "https://invasiber2.org/fitxa_llista.php?taxonomic=11"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[5]]
  res3
  # Crear los nombres científicos combinando Género (X1) y Especie (X2)
  nombres_cientificos <- res3 %>%
    select(X1, X2) %>%  # Seleccionar las columnas de Género y Especie
    filter(row_number() > 1) %>%  # Eliminar la primera fila de encabezado
    mutate(nombre_cientifico = paste(X1, X2)) %>%  # Unir Género y Especie
    select(-X1) %>%
    select(-X2)
  # Ver los resultados
  enlaces <- html_nodes(res, "a")
  hrefs_especies <- html_attr(enlaces, "href")
  enlaces_fitxa <- hrefs_especies[grepl("fitxa_detalls\\.php", hrefs_especies)]
  enlaces_completos_busqueda <- paste0("https://invasiber2.org/", enlaces_fitxa)
  enlaces_especies13 <- (unique(enlaces_completos_busqueda))




  Enlaces_especies <- c(enlaces_especies1,enlaces_especies2,enlaces_especies3,
                            enlaces_especies4,enlaces_especies5,enlaces_especies6,
                            enlaces_especies7,enlaces_especies8,enlaces_especies9,
                            enlaces_especies10,enlaces_especies11,enlaces_especies12,
                            enlaces_especies13)

  ###################################################################
  ######### ENTRAMOS A CADA ENLACE ##################################
  ###################################################################
  procesar_enlace <- function(url) {
    tryCatch({
      pagina <- read_html(url)

      tablas <- html_table(pagina, fill = TRUE)

      # Extraer nombre cientifico
      nombre <- pagina %>%
        html_nodes("em") %>%
        html_text(trim = TRUE) %>%
        .[1:2] %>%
        paste(collapse = " ")  # nombre científico unido

      if (length(tablas) >= 8) {
        tabla_raw <- tablas[[8]]

        if (ncol(tabla_raw) >= 2) {
          tabla <- tabla_raw %>%
            select(X1, X2) %>%
            filter(!is.na(X1) & X1 != "") %>%
            filter(!duplicated(X1)) %>%
            pivot_wider(names_from = X1, values_from = X2)

          # Añadir columna 'Especie' al inicio
          tabla <- tabla %>%
            mutate(Especie = nombre) %>%
            select(Especie, everything())
        } else {
          tabla <- NULL
        }
      } else {
        tabla <- NULL
      }

      list(
        nombre = nombre,
        url = url,
        tabla8 = tabla
      )
    }, error = function(e) {
      warning(paste("Error procesando:", url))
      return(NULL)
    })
  }
  resultados <- map(Enlaces_especies, procesar_enlace)
  resultados_validos <- compact(resultados)


  tablas_seleccionadas <- resultados_validos %>%
    map(~ .x$tabla8) %>%  # Extrae solo la tabla8 de cada elemento
    compact()

  df_unido <- bind_rows(tablas_seleccionadas)
  dataset_García_Berthou_et_Al_2004 <- as.data.frame(df_unido)
  dataset_García_Berthou_et_Al_20042 <- dataset_García_Berthou_et_Al_2004[,-c(9:12)]

  names(dataset_García_Berthou_et_Al_20042)
  names(dataset_García_Berthou_et_Al_20042)[names(dataset_García_Berthou_et_Al_20042) == "Especie:"] <- "Especie"
  names(dataset_García_Berthou_et_Al_20042)[names(dataset_García_Berthou_et_Al_20042) == "Descripción:"] <- "Descripcion"
  names(dataset_García_Berthou_et_Al_20042)[names(dataset_García_Berthou_et_Al_20042) == "Biología\n                      y hábitat:"] <- "BiologiayHabitat"
  names(dataset_García_Berthou_et_Al_20042)[names(dataset_García_Berthou_et_Al_20042) == "Distribución\n                      geográfica nativa:"] <- "DistribucionGeograficaNativa"
  colnames(dataset_García_Berthou_et_Al_20042)[5] <- "Distribucion"
  names(dataset_García_Berthou_et_Al_20042)[names(dataset_García_Berthou_et_Al_20042) == "Mecanismo\n                      de introducción:"] <- "MecanismoIntroduccion"
  names(dataset_García_Berthou_et_Al_20042)[names(dataset_García_Berthou_et_Al_20042) == "Impacto\n                      ecológico:"] <- "ImpactoEcologico"
  names(dataset_García_Berthou_et_Al_20042)[names(dataset_García_Berthou_et_Al_20042) == "Impacto\n                      socioeconómico:"] <- "ImpactoSocioeconomico"

  write_xlsx(dataset_García_Berthou_et_Al_20042,"Inputfiles/Step0_OriginalDatabase_García_Berthou_et_Al_2004.xlsx")
  cat("Archivo guardado correctamente : Inputfiles/Step0_OriginalDatabase_García_Berthou_et_Al_2004.xlsx")

  ############################################
  ###### ESPECIES FRESHWATER #################
  ############################################
  source(file.path("R", "check_habitat.r"))
  dataset <- dataset_García_Berthou_et_Al_20042
  nombre_especies <- dataset_García_Berthou_et_Al_20042$Especie
  nombres_aceptados_gbif <- name_backbone_checklist(nombre_especies)$canonicalName
  dataset_habitat <- check_habitat(nombres_aceptados_gbif, dataset)

  #NOS QUEDAMOS CON LAS FRESHWATER
  dataset_freshwater <- dataset_habitat %>%
    filter(grepl("FRESHWATER", Habitat))

  dataset_freshwater2 <- dataset_freshwater %>%
    mutate(across(everything(), ~str_replace_all(as.character(.), ";", ",")))

  write_xlsx(dataset_freshwater2, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_García_Berthou_et_Al_2004.xlsx")
  cat("Archivo guardado correctamente : Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_García_Berthou_et_Al_2004.xlsx")
}
