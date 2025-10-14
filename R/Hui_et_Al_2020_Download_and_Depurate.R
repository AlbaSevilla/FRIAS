Hui_et_Al_2020_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  #A mano
  #"https://lkcnhm.nus.edu.sg/app/uploads/2020/01/RBZ-2020-0016.pdf"


  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################
  archivo <- "Inputfiles/Step0_OriginalPDF_Hui_et_Al_2020.pdf"
  lista9 <- pdf_text(archivo)[3:5]
  texto_tabla <- lista9

  # 1. Divide el texto en líneas
  lineas <- unlist(strsplit(texto_tabla, "\n"))

  # 2. Elimina líneas vacías y encabezados innecesarios
  lineas <- trimws(lineas)
  lineas <- lineas[lineas != ""]
  lineas <- lineas[!grepl("Tan et al.|Table 1a|Order and Family|Region of Origin|Remarks", lineas, ignore.case = TRUE)]

  # 3. Inicializa variables para Order y Family (se irán "heredando" a las filas siguientes)
  current_order <- NA
  current_family <- NA

  # 4. Procesa cada línea y extrae columnas
  df_list <- list()
  for (i in seq_along(lineas)) {
    l <- lineas[i]

    # Identifica Order
    if (grepl("^ORDER ", l)) {
      current_order <- gsub("^ORDER\\s+", "", l)
      next
    }

    # Identifica Family
    if (grepl("^Family ", l)) {
      current_family <- gsub("^Family\\s+", "", l)
      next
    }

    # Si la línea empieza con espacio, es continuación de la family anterior
    if (grepl("^\\s", l)) {
      # Pega la family anterior al principio de la línea
      l <- paste(current_family, trimws(l))
    }

    # Divide la línea en columnas por 2 o más espacios
    cols <- unlist(strsplit(l, " {2,}"))

    # Si la family está en la primera columna, sepárala
    if (length(cols) >= 2 && grepl("^[A-Z][a-z]+idae$", cols[1])) {
      fam <- cols[1]
      species <- cols[2]
      origin <- ifelse(length(cols) > 2, cols[3], NA)
      remarks <- ifelse(length(cols) > 3, cols[4], NA)
    } else if (length(cols) >= 1) {
      fam <- current_family
      species <- cols[1]
      origin <- ifelse(length(cols) > 1, cols[2], NA)
      remarks <- ifelse(length(cols) > 2, cols[3], NA)
    } else {
      next
    }

    # Añade la fila a la lista
    df_list[[length(df_list) + 1]] <- data.frame(
      Order = current_order,
      Family = fam,
      Species = species,
      Region_of_Origin = origin,
      Remarks = remarks,
      stringsAsFactors = FALSE
    )
  }

  # 5. Une todas las filas en un data.frame
  tabla_df <- do.call(rbind, df_list)
  rownames(tabla_df) <- NULL

  tabla_df2 <- tabla_df[,c(3:5)]


  # 11. Exporta a Excel
  write.xlsx(tabla_df2, "Inputfiles/Step0_OriginalDatabase_Hui_et_Al_2020.xlsx", rowNames = FALSE)

  #Seleccionamos las established
  tabla_df3 <- tabla_df2 %>% filter(Remarks == "established")


  #Habitat ya sabemos que son freshwater
  tabla_df3$Habitat_Database <- "Freshwater"
  tabla_df3$Invaded_country <- "Singapore"

  write.xlsx(tabla_df3, "Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Hui_et_Al_2020.xlsx")
}
