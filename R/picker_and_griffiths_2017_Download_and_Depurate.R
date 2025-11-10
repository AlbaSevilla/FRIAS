picker_and_griffiths_2017_Download_and_Depurate<- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  #A mano
  #"https://journals.co.za/doi/epdf/10.4102/abc.v47i2.2147"


  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  file <- "InputFiles/originaldatabase_picker_and_griffiths_2017.pdf"
  selected_text <- pdf_text(file)[9:19]
  table_text <- selected_text
  text <- unlist(strsplit(table_text, "\n"))
  text <- trimws(text)
  text <- text[text != ""]
  text <- text[!grepl("Page|Original Research|APPENDIX|Alien animals|Group +Species|Common name|Origin|Habitat|Date of introduction|first detection", text, ignore.case = TRUE)]
  current_group <- NA
  current_family <- NA

  dataset_list <- list()
  for (l in text) {
    if (grepl("^[A-Z][a-z]+$", l)) {
      current_group <- l
      next
    }
    if (grepl("^\\s*[A-Z][a-z]+idae", l)) {
      current_family <- trimws(l)
      next
    }
    if (grepl("^\\s{2,}", l)) {
      l <- paste(current_family, trimws(l))
    }

    words <- unlist(strsplit(l, "\\s+"))

    if (length(words) < 5) next  # línea muy corta, ignora

    habitat <- words[length(words)-1]
    date_intro <- words[length(words)]

    fam <- words[1]

    # Extraemos nombre científico: puede ser 2 o 3 palabras (género + especie + autor opcional)
    # Buscamos la posición donde termina el nombre científico: asumimos que empieza en 2da palabra y puede incluir palabra 3 si empieza con letra mayúscula (autor)

    if (length(words) >= 4) {
      if (grepl("^[A-Z]", words[3])) {
        scientific_name <- paste(words[2], words[3], sep = " ")
        start_common <- 4
      } else {
        scientific_name <- words[2]
        start_common <- 3
      }
    } else {
      scientific_name <- words[2]
      start_common <- 3
    }

   if (length(words) > start_common + 1) {
      common_origin <- paste(words[start_common:(length(words)-2)], collapse = " ")
    } else {
      common_origin <- NA
    }

    dataset_list[[length(dataset_list) + 1]] <- data.frame(
      Group = current_group,
      Family = fam,
      Scientific_name = scientific_name,
      Common_and_Origin = common_origin,
      Habitat = habitat,
      Date_of_introduction = date_intro,
      stringsAsFactors = FALSE
    )
  }


  dataset_table <- do.call(rbind, dataset_list)
  rownames(dataset_table) <- NULL
  dataset_table <- dataset_table %>%
    select(Scientific_name, everything())

  rownames(dataset_table) <- NULL
  dataset_table$Common_and_Origin <- sapply(dataset_table$Common_and_Origin, function(x) {
    palabras <- unlist(strsplit(x, "\\s+"))
    solo_mayus <- palabras[grepl("^[A-Z]", palabras)]
    paste(solo_mayus, collapse = " ")
  })
  write.xlsx(dataset_table, "InputFiles/originaldatabase_picker_and_griffiths_2017.xlsx", rowNames = FALSE)

  #Seleccionamos las established
  dataset_table3 <- dataset_table %>% filter(Habitat == "F")


  #Habitat ya sabemos que son freshwater
  dataset_table3$Habitat_Database <- "Freshwater"
  dataset_table3$Invaded_country <- "South Africa"
  final_table <- as.data.frame(dataset_table3)

  nombres <- final_table$Scientific_name
  final_table$AcceptedNameGBIF <- name_backbone_checklist(nombres)$canonicalName

  final_table2 <- final_table %>%
    mutate(Date_of_introduction = gsub("[^0-9]", "", Date_of_introduction))

  final_table3 <- final_table2 %>%
    mutate(Habitat = gsub("F", "FRESHWATER", Habitat))

  write.xlsx(final_table3, "InputFiles/freshwatersubset_picker_and_griffiths_2017.xlsx")
}
