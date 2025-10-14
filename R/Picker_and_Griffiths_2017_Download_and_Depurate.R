Picker_and_Griffiths_2017_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  #A mano
  #"https://journals.co.za/doi/epdf/10.4102/abc.v47i2.2147"


  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  archivo <- "InputFiles/Step0_OriginalPDF_Picker_and_Griffiths_2017.pdf"
  lista9 <- pdf_text(archivo)[9:19]
  texto_tabla <- lista9



  # 1. Divide el texto en líneas
  lineas <- unlist(strsplit(texto_tabla, "\n"))

  # 2. Limpia líneas vacías y encabezados
  lineas <- trimws(lineas)
  lineas <- lineas[lineas != ""]
  lineas <- lineas[!grepl("Page|Original Research|APPENDIX|Alien animals|Group +Species|Common name|Origin|Habitat|Date of introduction|first detection", lineas, ignore.case = TRUE)]

  # 3. Variables para Group y Family
  current_group <- NA
  current_family <- NA

  # 4. Procesa líneas
  df_list <- list()
  for (l in lineas) {
    # Detecta Group
    if (grepl("^[A-Z][a-z]+$", l)) {
      current_group <- l
      next
    }
    # Detecta Family
    if (grepl("^\\s*[A-Z][a-z]+idae", l)) {
      current_family <- trimws(l)
      next
    }
    # Si línea empieza con espacios, es continuación de familia
    if (grepl("^\\s{2,}", l)) {
      l <- paste(current_family, trimws(l))
    }

    # Divide la línea en palabras
    words <- unlist(strsplit(l, "\\s+"))

    # Extrae habitat y fecha (últimas dos palabras)
    if (length(words) < 5) next  # línea muy corta, ignora

    habitat <- words[length(words)-1]
    date_intro <- words[length(words)]

    # Ahora extraemos Family (primera palabra)
    fam <- words[1]

    # Extraemos nombre científico: puede ser 2 o 3 palabras (género + especie + autor opcional)
    # Buscamos la posición donde termina el nombre científico: asumimos que empieza en 2da palabra y puede incluir palabra 3 si empieza con letra mayúscula (autor)

    # Por defecto, nombre científico = palabras 2 y 3
    if (length(words) >= 4) {
      # Si la 3ra palabra empieza con mayúscula (autor), inclúyela
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

    # El resto hasta habitat y fecha es nombre común y origen
    # Para simplificar, juntamos todo en nombre común (puedes separar si quieres)
    if (length(words) > start_common + 1) {
      common_origin <- paste(words[start_common:(length(words)-2)], collapse = " ")
    } else {
      common_origin <- NA
    }

    # Añade fila
    df_list[[length(df_list) + 1]] <- data.frame(
      Group = current_group,
      Family = fam,
      Scientific_name = scientific_name,
      Common_and_Origin = common_origin,
      Habitat = habitat,
      Date_of_introduction = date_intro,
      stringsAsFactors = FALSE
    )
  }

  # 5. Une en data.frame
  tabla_df <- do.call(rbind, df_list)
  rownames(tabla_df) <- NULL
  tabla_df <- tabla_df %>%
    select(Scientific_name, everything())

  rownames(tabla_df) <- NULL
  tabla_df$Common_and_Origin <- sapply(tabla_df$Common_and_Origin, function(x) {
    palabras <- unlist(strsplit(x, "\\s+"))
    solo_mayus <- palabras[grepl("^[A-Z]", palabras)]
    paste(solo_mayus, collapse = " ")
  })
  write.xlsx(tabla_df, "InputFiles/Step0_OriginalDatabase_Picker_and_Griffiths_2017.xlsx", rowNames = FALSE)

  #Seleccionamos las established
  tabla_df3 <- tabla_df %>% filter(Habitat == "F")


  #Habitat ya sabemos que son freshwater
  tabla_df3$Habitat_Database <- "Freshwater"
  tabla_df3$Invaded_country <- "South Africa"
  tabla_final <- as.data.frame(tabla_df3)

  nombres <- tabla_final$Scientific_name
  tabla_final$AcceptedNameGBIF <- name_backbone_checklist(nombres)$canonicalName

  tabla_final2 <- tabla_final %>%
    mutate(Date_of_introduction = gsub("[^0-9]", "", Date_of_introduction))

  tabla_final3 <- tabla_final2 %>%
    mutate(Habitat = gsub("F", "FRESHWATER", Habitat))

  write.xlsx(tabla_final3, "InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Picker_and_Griffiths_2017.xlsx")
}
