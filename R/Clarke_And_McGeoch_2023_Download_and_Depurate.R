Clarke_And_McGeoch_2023_Download_and_Depurate <- function(){
  ###########################################################
  ################## DESCARGAR ##############################
  ###########################################################
  # Leer texto de las páginas 10 a 19 del PDF
  pdf_file <- "Inputfiles/Step0_OriginalDatabase_Clarke_And_McGeoch_2023.pdf"

  all_pages <- pdf_text(pdf_file)

  pages_10_19 <- all_pages[10:19]

  text_10_19 <- paste(pages_10_19, collapse = "\n")
  cat(text_10_19)

  txt <- text_10_19
  lines <- strsplit(txt, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  data <- list()
  current_order <- NA
  current_family <- NA
  current_sci_name <- NA
  current_mechanism <- NA
  current_severity <- NA

  for (line in lines) {
    parts <- unlist(strsplit(line, "\\s{2,}"))

    if (nchar(parts[1]) > 0) {
      if (!is.na(current_order)) {
        data[[length(data) + 1]] <- list(
          Order = current_order,
          Family = current_family,
          Scientific_name = current_sci_name,
          Mechanism = current_mechanism,
          Severity = current_severity
        )
      }
      current_order <- parts[1]
      current_family <- parts[2]
      current_sci_name <- parts[3]
      if (length(parts) > 5) {
        current_sci_name <- paste(parts[3:(length(parts) - 2)], collapse = " ")
        current_mechanism <- parts[length(parts) - 1]
        current_severity <- parts[length(parts)]
      } else {
        current_mechanism <- parts[4]
        current_severity <- parts[5]
      }
    } else {
       current_sci_name <- paste(current_sci_name, trimws(line))
    }
  }
  if (!is.na(current_order)) {
    data[[length(data) + 1]] <- list(
      Order = current_order,
      Family = current_family,
      Scientific_name = current_sci_name,
      Mechanism = current_mechanism,
      Severity = current_severity
    )
  }
  final_dataset  <- do.call(rbind, lapply(data, as.data.frame))
  write.xlsx(final_dataset , "Inputfiles/Step0_OriginalDatabase_Clarke_And_McGeoch_2023.xlsx")
  ##########################################################
  ############### obtener información de interés ###########
  ##########################################################

  dataset <- read_excel("Inputfiles/Step0_OriginalDatabase_Clarke_And_McGeoch_2023.xlsx")
  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))

  #Obtain habitats
  # Obtener nombres científicos desde el campo "Especie"
  especies_lista0 <- dataset$Scientific_name
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)


  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater <- dataset_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))

  write.xlsx(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Clarke_And_McGeoch_2023.xlsx")
  cat("Archivo descargado correctamente: /Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_Clarke_And_McGeoch_2023.xlsx")

  names(dataset_freshwater)
  str(dataset_freshwater)
}
