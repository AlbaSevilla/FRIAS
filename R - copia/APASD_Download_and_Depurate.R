APASD_Download_and_Depurate <- function(){
  ##############################
  ##### DESCARGA ###############
  ##############################


  #############################################################
  #########             BACTERIUM             #################
  #############################################################
  url <- "https://www.naro.affrc.go.jp/archive/niaes/techdoc/apasd/bacterium.html"
  res <- read_html(url)
  res2 <- res %>% html_table
  tabla_interes <- res2[[4]]
  tabla_interes <- tabla_interes[,-c(1:3)]
  tabla_interes <- as.data.frame(tabla_interes)
  colnames(tabla_interes) <- as.character(tabla_interes[1, ])
  tabla_interes <- tabla_interes[-1,]
  seleccionar_columnas <- c("Species name","Organism group", "Order name", "Family name",
                            "Year of invasion or detection", "Native region")
  tabla_interes <- tabla_interes[,seleccionar_columnas]
  especies <- tabla_interes$`Species name`
  tabla_interes$rank <- name_backbone_checklist(especies)$rank
  tabla_interes <- tabla_interes %>% filter(rank == "SPECIES")
  especies2 <- tabla_interes$`Species name`
  tabla_interes$AcceptedNameGBIF <- name_backbone_checklist(especies2)$canonicalName
  tabla_interes1 <- tabla_interes


  #############################################################
  #########             fungus                #################
  #############################################################
  url <- "https://www.naro.affrc.go.jp/archive/niaes/techdoc/apasd/fungus.html"
  res <- read_html(url)
  res2 <- res %>% html_table
  tabla_interes <- res2[[4]]
  tabla_interes <- tabla_interes[,-c(1:3)]
  tabla_interes <- as.data.frame(tabla_interes)
  colnames(tabla_interes) <- as.character(tabla_interes[1, ])
  tabla_interes <- tabla_interes[-1,]
  seleccionar_columnas <- c("Species name","Organism\n      group", "Order \n      name",
                            "Family name",
                            "Year of invasion\n      or detection", "Native region")
  tabla_interes <- tabla_interes[,seleccionar_columnas]
  especies <- tabla_interes$`Species name`
  tabla_interes$rank <- name_backbone_checklist(especies)$rank
  tabla_interes <- tabla_interes %>% filter(rank == "SPECIES")
  especies2 <- tabla_interes$`Species name`
  tabla_interes$AcceptedNameGBIF <- name_backbone_checklist(especies2)$canonicalName
  tabla_interes2 <- tabla_interes


  #############################################################
  #########             insect                #################
  #############################################################
  url <- "https://www.naro.affrc.go.jp/archive/niaes/techdoc/apasd/insect.html"
  res <- read_html(url)
  res2 <- res %>% html_table
  tabla_interes <- res2[[5]]
  tabla_interes <- tabla_interes[,-c(1:3)]
  tabla_interes <- as.data.frame(tabla_interes)
  colnames(tabla_interes) <- as.character(tabla_interes[1, ])
  tabla_interes <- tabla_interes[-1,]
  seleccionar_columnas <- c("Species name","Organism group", "Order \n      name",
                            "Family \n      name",
                            "Year of invasion or detection", "Native region")
  tabla_interes <- tabla_interes[,seleccionar_columnas]
  especies <- tabla_interes$`Species name`
  tabla_interes$rank <- name_backbone_checklist(especies)$rank
  tabla_interes <- tabla_interes %>% filter(rank == "SPECIES")
  especies2 <- tabla_interes$`Species name`
  tabla_interes$AcceptedNameGBIF <- name_backbone_checklist(especies2)$canonicalName
  tabla_interes3 <- tabla_interes


  #############################################################
  #########             mammal                #################
  #############################################################
  url <- "https://www.naro.affrc.go.jp/archive/niaes/techdoc/apasd/mammal.html"
  res <- read_html(url)
  res2 <- res %>% html_table
  tabla_interes <- res2[[4]]
  tabla_interes <- tabla_interes[,-c(1:3)]
  tabla_interes <- as.data.frame(tabla_interes)
  colnames(tabla_interes) <- as.character(tabla_interes[1, ])
  tabla_interes <- tabla_interes[-1,]
  seleccionar_columnas <- c("Species name","Organism group", "Order name",
                            "Family \n      name",
                            "Year of \n      invasion or detection", "Native region")
  tabla_interes <- tabla_interes[,seleccionar_columnas]
  especies <- tabla_interes$`Species name`
  tabla_interes$rank <- name_backbone_checklist(especies)$rank
  tabla_interes <- tabla_interes %>% filter(rank == "SPECIES")
  especies2 <- tabla_interes$`Species name`
  tabla_interes$AcceptedNameGBIF <- name_backbone_checklist(especies2)$canonicalName
  tabla_interes4 <- tabla_interes



  #############################################################
  #########             nematode              #################
  #############################################################
  url <- "https://www.naro.affrc.go.jp/archive/niaes/techdoc/apasd/nematode%20-B.html"
  res <- read_html(url)
  res2 <- res %>% html_table
  tabla_interes <- res2[[3]]
  tabla_interes <- tabla_interes[,-c(1:3)]
  tabla_interes <- as.data.frame(tabla_interes)
  colnames(tabla_interes) <- as.character(tabla_interes[1, ])
  tabla_interes <- tabla_interes[-1,]
  seleccionar_columnas <- c("Species name","Organism group", "Order name",
                            "Family \n      name",
                            "Year of i\n      nvasion or detection", "Native region")
  tabla_interes <- tabla_interes[,seleccionar_columnas]
  especies <- tabla_interes$`Species name`
  tabla_interes$rank <- name_backbone_checklist(especies)$rank
  tabla_interes <- tabla_interes %>% filter(rank == "SPECIES")
  especies2 <- tabla_interes$`Species name`
  tabla_interes$AcceptedNameGBIF <- name_backbone_checklist(especies2)$canonicalName
  tabla_interes5 <- tabla_interes



  #############################################################
  #########             other animal          #################
  #############################################################
  url <- "https://www.naro.affrc.go.jp/archive/niaes/techdoc/apasd/other%20animals.html"
  res <- read_html(url)
  res2 <- res %>% html_table
  tabla_interes <- res2[[4]]
  tabla_interes <- tabla_interes[,-c(1:3)]
  tabla_interes <- as.data.frame(tabla_interes)
  colnames(tabla_interes) <- as.character(tabla_interes[1, ])
  tabla_interes <- tabla_interes[-1,]
  seleccionar_columnas <- c("Species name","Organism group", "Order\n      name",
                            "Family \n      name",
                            "Year of invasion or detection", "Native region")
  tabla_interes <- tabla_interes[,seleccionar_columnas]
  especies <- tabla_interes$`Species name`
  tabla_interes$rank <- name_backbone_checklist(especies)$rank
  tabla_interes <- tabla_interes %>% filter(rank == "SPECIES")
  especies2 <- tabla_interes$`Species name`
  tabla_interes$AcceptedNameGBIF <- name_backbone_checklist(especies2)$canonicalName
  tabla_interes6 <- tabla_interes


  #############################################################
  #########                plant              #################
  #############################################################
  url <- "https://www.naro.affrc.go.jp/archive/niaes/techdoc/apasd/plant.html"
  res <- read_html(url)
  res2 <- res %>% html_table
  tabla_interes <- res2[[5]]
  tabla_interes <- tabla_interes[,-c(1:3)]
  tabla_interes <- as.data.frame(tabla_interes)
  colnames(tabla_interes) <- as.character(tabla_interes[1, ])
  tabla_interes <- tabla_interes[-1,]
  seleccionar_columnas <- c("Species name","Organism group", "Order \n      name",
                            "Family \n      name",
                            "Year of invasion or detection", "Native region")
  tabla_interes <- tabla_interes[,seleccionar_columnas]
  especies <- tabla_interes$`Species name`
  tabla_interes$rank <- name_backbone_checklist(especies)$rank
  tabla_interes <- tabla_interes %>% filter(rank == "SPECIES")
  especies2 <- tabla_interes$`Species name`
  tabla_interes$AcceptedNameGBIF <- name_backbone_checklist(especies2)$canonicalName
  tabla_interes7 <- tabla_interes



  #############################################################
  #########                virus              #################
  #############################################################
  # url <- "https://www.naro.affrc.go.jp/archive/niaes/techdoc/apasd/virus.html"
  # res <- read_html(url)
  # res2 <- res %>% html_table
  # tabla_interes <- res2[[4]]
  # tabla_interes <- tabla_interes[,-c(1:3)]
  # tabla_interes <- as.data.frame(tabla_interes)
  # colnames(tabla_interes) <- as.character(tabla_interes[1, ])
  # tabla_interes <- tabla_interes[-1,]
  # seleccionar_columnas <- c("English common name","Species name","Organism group", "Order name",
  #                           "Family \n      name",
  #                           "Year of invasion or detection", "Native region")
  # tabla_interes <- tabla_interes[,seleccionar_columnas]
  # especies <- tabla_interes$`English common name`
  # tabla_interes$rank <- name_backbone_checklist(especies)$rank
  # tabla_interes <- tabla_interes %>% filter(rank == "SPECIES")
  # especies2 <- tabla_interes$`Species name`
  # tabla_interes$AcceptedNameGBIF <- name_backbone_checklist(especies2)$canonicalName
  # tabla_interes <- tabla_interes[,-1]
  # tabla_interes8 <- tabla_interes

  # Función para limpiar nombres de columnas y dejar todas iguales
  normalizar_columnas <- function(df) {
    colnames(df) <- tolower(colnames(df))
    colnames(df) <- str_replace_all(colnames(df), "\n", " ")
    colnames(df) <- str_replace_all(colnames(df), "\\s+", " ")
    colnames(df) <- str_trim(colnames(df))

    colnames(df) <- case_when(
      colnames(df) %in% c("species name") ~ "Species name",
      colnames(df) %in% c("organism group", "organism group ") ~ "Organism group",
      colnames(df) %in% c("order name", "order name ") ~ "Order name",
      str_detect(colnames(df), "order") ~ "Order name",
      colnames(df) %in% c("family name", "family name ") ~ "Family name",
      str_detect(colnames(df), "family") ~ "Family name",
      str_detect(colnames(df), "year") ~ "Year of invasion or detection",
      colnames(df) %in% c("native region") ~ "Native region",
      colnames(df) == "rank" ~ "Rank",
      colnames(df) == "acceptednamegbif" ~ "AcceptedNameGBIF",
      TRUE ~ colnames(df)
    )

    return(df)
  }

  # Aplica la función de normalización a todas tus tablas
  tabla_interes1 <- normalizar_columnas(tabla_interes1)
  tabla_interes2 <- normalizar_columnas(tabla_interes2)
  tabla_interes3 <- normalizar_columnas(tabla_interes3)
  tabla_interes4 <- normalizar_columnas(tabla_interes4)
  tabla_interes5 <- normalizar_columnas(tabla_interes5)
  tabla_interes6 <- normalizar_columnas(tabla_interes6)
  tabla_interes7 <- normalizar_columnas(tabla_interes7)

  # Une todas las tablas en una sola
  final_dataset <- bind_rows(tabla_interes1,tabla_interes2,tabla_interes3,tabla_interes4,
                             tabla_interes5,tabla_interes6,tabla_interes7)

  #ELIMINAMOS DUPLICAODS
  df_list <- split(final_dataset, final_dataset$AcceptedNameGBIF)
  dataset_sinduplicados <- do.call(rbind, lapply(df_list, function(df) {
    res <- sapply(df, function(col) {
      texto <- as.character(col)
      texto <- gsub("https://doi[^[:space:]]+", "", texto)
      vals <- unlist(strsplit(texto, "[,;|]+|\\s{2,}"))
      vals <- trimws(vals)       # Quitar espacios al inicio y final
      vals <- vals[vals != ""]   # Quitar elementos vacíos
      vals <- unique(vals)       # Eliminar duplicados
      paste(vals, collapse = ", ")
    })
    as.data.frame(t(res), stringsAsFactors = FALSE)
  }))
  rownames(dataset_sinduplicados) <- NULL

  names(dataset_sinduplicados) <- gsub(" ", "_", names(dataset_sinduplicados))

  write.xlsx (dataset_sinduplicados, "./InputFiles/Step0_OriginalDatabase_APASD.xlsx")

  ##############################################
  ##### obtenemos habitat ######################
  ##############################################
  source(file.path("R", "check_habitat.R"))
  dataset <- final_dataset
  especies <- dataset$AcceptedNameGBIF
  dataset_habitat <- check_habitat(especies, dataset)

  ########## FRESHWATER
  dataset_freshwater <- dataset_habitat %>% filter(grepl("FRESHWATER", Habitat))
  names(dataset_freshwater) <- gsub(" ", "_", names(dataset_freshwater))

  write.xlsx (dataset_freshwater, "./InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_APASD.xlsx")
  cat("El archivo se descargó correctamente: InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_APASD.xlsx")
}
