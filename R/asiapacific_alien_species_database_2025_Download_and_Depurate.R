asiapacific_alien_species_database_2025_Download_and_Depurate <- function(){
  ########################################################################
  ##### DESCARGA  ########################################################
  ########################################################################

  #############################################################
  #########             BACTERIUM             #################
  #############################################################
  url <- "https://www.naro.affrc.go.jp/archive/niaes/techdoc/apasd/bacterium.html"
  res <- read_html(url)
  res2 <- res %>% html_table
  interest_table <- res2[[4]]
  interest_table <- interest_table[,-c(1:3)]
  interest_table <- as.data.frame(interest_table)
  colnames(interest_table) <- as.character(interest_table[1, ])
  interest_table <- interest_table[-1,]
  col_select <- c("Species name","Organism group", "Order name", "Family name",
                            "Year of invasion or detection", "Native region")
  interest_table <- interest_table[,col_select]
  especies <- interest_table$`Species name`
  interest_table$rank <- name_backbone_checklist(especies)$rank
  interest_table <- interest_table %>% filter(rank == "SPECIES")
  especies2 <- interest_table$`Species name`
  interest_table$AcceptedNameGBIF <- name_backbone_checklist(especies2)$canonicalName
  interest_table1 <- interest_table

  #############################################################
  #########             fungus                #################
  #############################################################
  url <- "https://www.naro.affrc.go.jp/archive/niaes/techdoc/apasd/fungus.html"
  res <- read_html(url)
  res2 <- res %>% html_table
  interest_table <- res2[[4]]
  interest_table <- interest_table[,-c(1:3)]
  interest_table <- as.data.frame(interest_table)
  colnames(interest_table) <- as.character(interest_table[1, ])
  interest_table <- interest_table[-1,]
  col_select <- c("Species name","Organism\n      group", "Order \n      name",
                            "Family name",
                            "Year of invasion\n      or detection", "Native region")
  interest_table <- interest_table[,col_select]
  especies <- interest_table$`Species name`
  interest_table$rank <- name_backbone_checklist(especies)$rank
  interest_table <- interest_table %>% filter(rank == "SPECIES")
  especies2 <- interest_table$`Species name`
  interest_table$AcceptedNameGBIF <- name_backbone_checklist(especies2)$canonicalName
  interest_table2 <- interest_table

  #############################################################
  #########             insect                #################
  #############################################################
  url <- "https://www.naro.affrc.go.jp/archive/niaes/techdoc/apasd/insect.html"
  res <- read_html(url)
  res2 <- res %>% html_table
  interest_table <- res2[[5]]
  interest_table <- interest_table[,-c(1:3)]
  interest_table <- as.data.frame(interest_table)
  colnames(interest_table) <- as.character(interest_table[1, ])
  interest_table <- interest_table[-1,]
  col_select <- c("Species name","Organism group", "Order \n      name",
                            "Family \n      name",
                            "Year of invasion or detection", "Native region")
  interest_table <- interest_table[,col_select]
  especies <- interest_table$`Species name`
  interest_table$rank <- name_backbone_checklist(especies)$rank
  interest_table <- interest_table %>% filter(rank == "SPECIES")
  especies2 <- interest_table$`Species name`
  interest_table$AcceptedNameGBIF <- name_backbone_checklist(especies2)$canonicalName
  interest_table3 <- interest_table

  #############################################################
  #########             mammal                #################
  #############################################################
  url <- "https://www.naro.affrc.go.jp/archive/niaes/techdoc/apasd/mammal.html"
  res <- read_html(url)
  res2 <- res %>% html_table
  interest_table <- res2[[4]]
  interest_table <- interest_table[,-c(1:3)]
  interest_table <- as.data.frame(interest_table)
  colnames(interest_table) <- as.character(interest_table[1, ])
  interest_table <- interest_table[-1,]
  col_select <- c("Species name","Organism group", "Order name",
                            "Family \n      name",
                            "Year of \n      invasion or detection", "Native region")
  interest_table <- interest_table[,col_select]
  especies <- interest_table$`Species name`
  interest_table$rank <- name_backbone_checklist(especies)$rank
  interest_table <- interest_table %>% filter(rank == "SPECIES")
  especies2 <- interest_table$`Species name`
  interest_table$AcceptedNameGBIF <- name_backbone_checklist(especies2)$canonicalName
  interest_table4 <- interest_table

  #############################################################
  #########             nematode              #################
  #############################################################
  url <- "https://www.naro.affrc.go.jp/archive/niaes/techdoc/apasd/nematode%20-B.html"
  res <- read_html(url)
  res2 <- res %>% html_table
  interest_table <- res2[[3]]
  interest_table <- interest_table[,-c(1:3)]
  interest_table <- as.data.frame(interest_table)
  colnames(interest_table) <- as.character(interest_table[1, ])
  interest_table <- interest_table[-1,]
  col_select <- c("Species name","Organism group", "Order name",
                            "Family \n      name",
                            "Year of i\n      nvasion or detection", "Native region")
  interest_table <- interest_table[,col_select]
  especies <- interest_table$`Species name`
  interest_table$rank <- name_backbone_checklist(especies)$rank
  interest_table <- interest_table %>% filter(rank == "SPECIES")
  especies2 <- interest_table$`Species name`
  interest_table$AcceptedNameGBIF <- name_backbone_checklist(especies2)$canonicalName
  interest_table5 <- interest_table

  #############################################################
  #########             other animal          #################
  #############################################################
  url <- "https://www.naro.affrc.go.jp/archive/niaes/techdoc/apasd/other%20animals.html"
  res <- read_html(url)
  res2 <- res %>% html_table
  interest_table <- res2[[4]]
  interest_table <- interest_table[,-c(1:3)]
  interest_table <- as.data.frame(interest_table)
  colnames(interest_table) <- as.character(interest_table[1, ])
  interest_table <- interest_table[-1,]
  col_select <- c("Species name","Organism group", "Order\n      name",
                            "Family \n      name",
                            "Year of invasion or detection", "Native region")
  interest_table <- interest_table[,col_select]
  especies <- interest_table$`Species name`
  interest_table$rank <- name_backbone_checklist(especies)$rank
  interest_table <- interest_table %>% filter(rank == "SPECIES")
  especies2 <- interest_table$`Species name`
  interest_table$AcceptedNameGBIF <- name_backbone_checklist(especies2)$canonicalName
  interest_table6 <- interest_table

  #############################################################
  #########                plant              #################
  #############################################################
  url <- "https://www.naro.affrc.go.jp/archive/niaes/techdoc/apasd/plant.html"
  res <- read_html(url)
  res2 <- res %>% html_table
  interest_table <- res2[[5]]
  interest_table <- interest_table[,-c(1:3)]
  interest_table <- as.data.frame(interest_table)
  colnames(interest_table) <- as.character(interest_table[1, ])
  interest_table <- interest_table[-1,]
  col_select <- c("Species name","Organism group", "Order \n      name",
                            "Family \n      name",
                            "Year of invasion or detection", "Native region")
  interest_table <- interest_table[,col_select]
  especies <- interest_table$`Species name`
  interest_table$rank <- name_backbone_checklist(especies)$rank
  interest_table <- interest_table %>% filter(rank == "SPECIES")
  especies2 <- interest_table$`Species name`
  interest_table$AcceptedNameGBIF <- name_backbone_checklist(especies2)$canonicalName
  interest_table7 <- interest_table



  # Función para limpiar nombres de columnas y dejar todas iguales
  normalize_cols <- function(df) {
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
  interest_table1 <- normalize_cols(interest_table1)
  interest_table2 <- normalize_cols(interest_table2)
  interest_table3 <- normalize_cols(interest_table3)
  interest_table4 <- normalize_cols(interest_table4)
  interest_table5 <- normalize_cols(interest_table5)
  interest_table6 <- normalize_cols(interest_table6)
  interest_table7 <- normalize_cols(interest_table7)
  final_dataset <- bind_rows(interest_table1,interest_table2,interest_table3,interest_table4,
                             interest_table5,interest_table6,interest_table7)

  #ELIMINAMOS DUPLICAODS
  df_list <- split(final_dataset, final_dataset$AcceptedNameGBIF)
  dataset_sinduplicados <- do.call(rbind, lapply(df_list, function(df) {
    res <- sapply(df, function(col) {
      texto <- as.character(col)
      texto <- gsub("https://doi[^[:space:]]+", "", texto)
      vals <- unlist(strsplit(texto, "[,;|]+|\\s{2,}"))
      vals <- trimws(vals)
      vals <- vals[vals != ""]
      vals <- unique(vals)
      paste(vals, collapse = ", ")
    })
    as.data.frame(t(res), stringsAsFactors = FALSE)
  }))
  rownames(dataset_sinduplicados) <- NULL
  names(dataset_sinduplicados) <- gsub(" ", "_", names(dataset_sinduplicados))
  write.xlsx (dataset_sinduplicados, "./InputFiles/originaldatabase_asiapacific_alien_species_database_2025.xlsx")

  ##############################################
  ##### obtenemos habitat ######################
  ##############################################
  final_dataset <- read_excel("./InputFiles/originaldatabase_asiapacific_alien_species_database_2025.xlsx")
  source(file.path("R", "check_habitat.R"))
  dataset <- final_dataset
  especies <- dataset$AcceptedNameGBIF
  dataset_habitat <- check_habitat(especies, dataset)

  ########## FRESHWATER
  dataset_freshwater <- dataset_habitat %>% filter(grepl("FRESHWATER", Habitat))
  names(dataset_freshwater) <- gsub(" ", "_", names(dataset_freshwater))

  write.xlsx (dataset_freshwater, "./InputFiles/freshwatersubset_asiapacific_alien_species_database_2025.xlsx")
}
