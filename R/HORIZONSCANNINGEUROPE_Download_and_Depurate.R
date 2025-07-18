HORIZONSCANNINGEUROPE_Download_and_Depurate <- function() {

  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  #Base de datos proporcionada por Belinda


  # CON DAT
  ############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  dat <- read_excel("Inputfiles/Supp Mat 2. Final species lists.xlsx", sheet="Top 57 Species")
  nrow(dat)
  #Cambiamos los . de los nombres en las columnas por _ para facilitar el manejo de las columnas
  names(dat) <- gsub("[:.]", "_", names(dat))
  names(dat) <- gsub(" ", "_", names(dat))
  names(dat) <- gsub("[1234567890]", "", names(dat))
  names(dat) <- gsub("__", "", names(dat))

  names(dat)

  # Nombres de las columnas a combinar
  cols_mechanism <- c("Competition",	"Predation",	"Hybridisation",
                      "Transmission_of_disease",	"Parasitism",
                      "Poisoning/toxicity",	"Biofouling_or_other_direct_physical_disturbance",
                      "Grazing/herbivory/browsing",	"Chemical,_physical,_structural_impact_on_ecosystem",
                      "Indirect_impacts_through_interactions_with_other_species")

  names(dat)
  dat2 <- dat %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Mechanism_Impact = paste(
      cols_mechanism[which(c_across(all_of(cols_mechanism)) == "X")],
      collapse = ", "
    )) %>%
    ungroup()

  dat2 <- dat2 %>%
    mutate(Mechanism_Impact = str_split(Mechanism_Impact, ",")) %>%
    unnest(Mechanism_Impact) %>%
    mutate(
      Mechanism_Impact = tolower(str_trim(Mechanism_Impact)))

  #OBTENEMOS LA COLUMNA EICAT_by_MECHANISM
  dat2 <- dat2 %>%
    mutate(EICAT_by_Mechanism = paste(Likelihood_of_impact_on_biodiversity, Mechanism_Impact, sep=" -"))


  colnames(dat2) <- gsub(" ", "_", colnames(dat2))

  dat <- dat2 %>%
    select(Thematic_Group, Scientific_name, Order, Family, Native_range_detailed_area,
           Invaded_range_detailed_area, Mechanism_Impact, Likelihood_of_impact_on_biodiversity, EICAT_by_Mechanism)

  #OBTENER EL FILTRADO QUE QUEREMOS DE ESPECIES, ESTABLISHMENTMEANS Y TAXONRANK
  HORIZONSCANNINGEUROPE_freshwater <- dat

  #NODUPLICADOS
  source(file.path("R","noduplicates.r"))
  HORIZONSCANNINGEUROPE_freshwater <- noduplicates(HORIZONSCANNINGEUROPE_freshwater, "Scientific_name")

  #####################################################################################
  ################## FUNCTION TO OBTAIN SPECIES HABITAT ###############################
  #####################################################################################
  source(file.path("R", "check_habitat.r"))
  #Obtain habitats
  dataset <- HORIZONSCANNINGEUROPE_freshwater
  especies_lista0 <- dataset$Scientific_name
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  HORIZON_actualizado <- check_habitat(especies_lista, dataset)


  ##############################################################
  ######### AHORA NOS QUEDAMOS CON LAS ESPECIES FRESHWATER #####
  ##############################################################
  dataset_freshwater1 <- HORIZON_actualizado %>%
    filter(grepl("FRESHWATER", Habitat))


  write.xlsx(dataset_freshwater1, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES1_HORIZONSCANNINGEUROPE.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES1_HORIZONSCANNINGEUROPE.xlsx", "\n")
  # write.csv2(dataset_freshwater, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_HORIZONSCANNINGEUROPE.csv")
  # cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_HORIZONSCANNINGEUROPE.csv", "\n")


  #CON DAT2
  dat2 <-  read_excel("Inputfiles/Supp Mat 1. Lists species _no data_ and _widely spread_.xlsx",
                      sheet = "List no data")
  nrow(dat2)
  #Seleccionamos las columnas
  colnames(dat2) <- c("ID","Thematic_group","Scientific_Name",
                      "Kingdom",	"Phylum",	"Class"	,"Order",	"Family"	,
                      "Original_Source",	"Total_grid_cells_EU",	"N_grid_cells/country"	,
                      "Reason_for_exclusion")

  dat2 <- dat2[-c(1:3),]

  #No queremos duplicados
  source(file.path("R", "noduplicates.r"))
  dat2_noduplicados <- noduplicates(dat2, "Scientific_Name")

  #Obtenemos habitat
  dataset <- dat2_noduplicados
  nombres <- dataset$Scientific_Name
  nombres_cientificos <- name_backbone_checklist(nombres)$canonicalName
  habitat_dat <- check_habitat(nombres_cientificos, dataset)
  #Nos quedamos con freshwater
  dataset_freshwater2 <- habitat_dat %>% filter(grepl("FRESHWATER", Habitat))

  write.xlsx(dataset_freshwater2, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES2_HORIZONSCANNINGEUROPE.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES2_HORIZONSCANNINGEUROPE.xlsx", "\n")

  #CON DAT3
  dat3 <-  read_excel("Inputfiles/Supp Mat 1. Lists species _no data_ and _widely spread_.xlsx",
                      sheet = "List widely spread")
  nrow(dat3)
  dat3 <- dat3[-c(1:4),]
  colnames(dat3) <- dat3[1,]
  colnames(dat3) <- gsub(" ", "_", colnames(dat3))

  #No queremos duplicados
  dat3_noduplicados <- noduplicates(dat3, "Scientific_Name")

  #Obtenemos habitat
  dataset <- dat3_noduplicados
  nombres <- dataset$Scientific_Name
  nombres_cientificos <- name_backbone_checklist(nombres)$canonicalName
  habitat_dat <- check_habitat(nombres_cientificos, dataset)
  #Nos quedamos con freshwater
  dataset_freshwater3 <- habitat_dat %>% filter(grepl("FRESHWATER", Habitat))

  write.xlsx(dataset_freshwater3, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES3_HORIZONSCANNINGEUROPE.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES3_HORIZONSCANNINGEUROPE.xlsx", "\n")


  #AHORA JUNTAMOS TODOS LOS DATASETS:
  dataset_freshwater1 <- read_excel("Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES1_HORIZONSCANNINGEUROPE.xlsx")
  dataset_freshwater2 <- read_excel("Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES2_HORIZONSCANNINGEUROPE.xlsx")
  dataset_freshwater3 <- read_excel("Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES3_HORIZONSCANNINGEUROPE.xlsx")

  names(dataset_freshwater1)
  names(dataset_freshwater2)
  names(dataset_freshwater3)
  # Lista de nombres de columnas del dataset de referencia
  cols_ref <- names(dataset_freshwater1)

  # Función para añadir columnas faltantes
  add_missing_columns <- function(df, cols_ref) {
    missing_cols <- setdiff(cols_ref, names(df)) # columnas que faltan
    for (col in missing_cols) {
      df[[col]] <- ""  # agrega la columna con valores ""
    }
    # Reordenar columnas según cols_ref
    df <- df[, union(names(df), cols_ref)]
    df <- df[, cols_ref]
    return(df)
  }

  # Aplicar a los otros dos datasets
  dataset_freshwater2 <- add_missing_columns(dataset_freshwater2, cols_ref)
  dataset_freshwater3 <- add_missing_columns(dataset_freshwater3, cols_ref)

  dataset_final <- rbind(dataset_freshwater1, dataset_freshwater2, dataset_freshwater3)
  nrow(dataset_final)
  dataset <- dataset_final
  dataset <- dataset[,c("Species", "AcceptedNameGBIF","Habitat", "Mechanism_Impact",
                        "Invaded_range_detailed_area", "Native_range_detailed_area", "Family", "Order",
                        "Thematic_Group","Likelihood_of_impact_on_biodiversity", "EICAT_by_Mechanism")]

  write.xlsx(dataset, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_HORIZONSCANNINGEUROPE.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_HORIZONSCANNINGEUROPE.xlsx", "\n")
}
