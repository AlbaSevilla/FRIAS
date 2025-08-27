Final_MasterLists <- function(){
  MasterList <- read.xlsx("OutputFiles/Intermediate/Step12_Standardizedpathway_Masterlist.xlsx")

  #eliminamos cosas que se habian colado:
  MasterList <- MasterList %>%
    mutate(EstablishmentMeans = str_split(EstablishmentMeans, ",")) %>%
    unnest(EstablishmentMeans) %>%
    mutate(
      EstablishmentMeans = tolower(str_trim(EstablishmentMeans)),
    )
  unique(MasterList$EstablishmentMeans)

  # Filtrar el dataframe excluyendo los valores deseados
  MasterList2 <- MasterList[!(MasterList$EstablishmentMeans %in% c("cas", "est", "native")), ]
  unique(MasterList2$EstablishmentMeans)


  source(file.path("R", "noduplicates.r"))
  MasterList <- noduplicates(MasterList2, "AcceptedNameGBIF")
  nrow(MasterList)
  head(MasterList$Source_Data)
  names(MasterList)

  MasterList <- MasterList %>%
    mutate(InvadedRangeISO3 = if_else(
      str_detect(InvadedRangeISO3, "^[a-zA-Z]{6}$"),  # Exactamente 6 letras, sin comas
      str_replace(InvadedRangeISO3, "^(.{3})(.{3})$", "\\1,\\2"),  # Insertar coma después de 3 letras
      InvadedRangeISO3  # Si no cumple, deja como está
    ))

  MasterList <- MasterList %>% rename(FunctionalGroup = FunctionalGroupFinal)

  #CONVERTIMOS La primera letra de cada columna a Mayuscula
  # Función para poner en mayúscula la primera letra de cada palabra
  to_title_case <- function(x) {
    str_to_title(x)
  }
  # Nombres de columnas que quieres transformar
  cols_to_modify <- c("OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF",
                      "Kingdom", "Phylum", "Class", "Order", "Family",
                      "FunctionalGroup", "Group")

  # Aplica la transformación solo a esas columnas
  MasterList[cols_to_modify] <- lapply(MasterList[cols_to_modify], to_title_case)

  names(MasterList)

  ############################################################
  ################ MASTERLIST CON TODOS LOS IMPACTOS #########
  ############################################################

  #################################################
  ######### CREAMOS CARPETA FinalMasterlists #################
  #################################################
  if (!file.exists("OutputFiles")){
    dir.create(file.path("OutputFiles/FinalMasterLists"), recursive=TRUE)
  }
  MasterList_impacts <- MasterList
  unique(MasterList_impacts$Mechanisms)
  nrow(MasterList_impacts)
  MasterList_impacts <- MasterList_impacts %>%
    mutate(EICATImpact = str_split(EICATImpact, ",")) %>%      # Separar por ";"
    unnest(EICATImpact) %>%                                 # Crear una fila por cada keyword
    mutate(EICATImpact = str_trim(tolower(EICATImpact)))       #
  MasterList_impacts <- MasterList_impacts %>%
    mutate(Mechanisms = str_split(Mechanisms, ", ")) %>%      # Separar por ";"
    unnest(Mechanisms) %>%                                 # Crear una fila por cada keyword
    mutate(Mechanisms = str_trim(tolower(Mechanisms)))       #

  #ESTANDARIZAMOS LOS VALORES DE LETRAS A NUMEROS
  Standardization_EICATImpact <- read_excel("TablesToStandardize/Standardization_EICATImpact.xlsx", sheet=2)
  MasterList_impacts$EICATImpact
  AbbreviationToNumbers <- setNames(Standardization_EICATImpact$Number, Standardization_EICATImpact$Abbreviation)
  # Reemplazar las abreviaturas por sus valores numéricos
  MasterList_impacts$EICATImpact <- AbbreviationToNumbers[MasterList_impacts$EICATImpact]
  unique(MasterList_impacts$EICATImpact)

  names(MasterList_impacts)
  MasterList_impacts2 <- MasterList_impacts[,c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF",
                                              "Kingdom","Phylum","Class","Order", "Family", "FunctionalGroup", "Group",
                                              "NativeCountry","NativeCountryISO3","NativeRangeBioregions",
                                              "InvadedRange","InvadedRangeISO3","InvadedBioregions",
                                              "EstablishmentMeans","Pathways","EICATImpact","Mechanisms","EICAT_by_Mechanism",
                                              "AffectedNativeSpecies","Habitat","Source_Data")]
  write.xlsx(MasterList_impacts2,
             file.path("FinalFiles", paste0("FRIAS_AllImpactsList_MasterList.xlsx")),
             sep = ";",
             row.names = FALSE,
             col.names = TRUE)


  ###############################################################################
  ####### MASTERLIST DE LISTA DE NOMBRES Y columna de MAXIMO/MINIMO IMPACTO #####
  ###############################################################################
  #GENERAMOS LA MASTERLIST SIN IMPACTOS
  columnas_specieslist <- c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF",
                              "Kingdom","Phylum","Class","Order", "Family", "FunctionalGroup", "Group",
                              "NativeCountry","NativeCountryISO3","NativeRangeBioregions",
                              "InvadedRange","InvadedRangeISO3","InvadedBioregions",
                              "EstablishmentMeans","Pathways","EICATImpact","Mechanisms","EICAT_by_Mechanism",
                              "AffectedNativeSpecies","Habitat","Source_Data")
  # 1. Separar y limpiar datos como ya tienes
  MasterList_specieslist <- MasterList %>%
    mutate(EICATImpact = str_split(EICATImpact, ",")) %>%
    unnest(EICATImpact) %>%
    mutate(EICATImpact = str_trim(tolower(EICATImpact))) %>%
    mutate(Mechanisms = str_split(Mechanisms, ";")) %>%
    unnest(Mechanisms) %>%
    mutate(Mechanisms = str_trim(tolower(Mechanisms)))

  # 2. Leer tabla de estandarización
  Standardization_EICATImpact <- read_excel("TablesToStandardize/Standardization_EICATImpact.xlsx", sheet = 2)
  AbbreviationToNumbers <- setNames(Standardization_EICATImpact$Number, tolower(Standardization_EICATImpact$Abbreviation))
  NumbersToAbbreviation <- setNames(tolower(Standardization_EICATImpact$Abbreviation), Standardization_EICATImpact$Number)

  # 3. Mapear abreviaturas a valores numéricos
  MasterList_specieslist <- MasterList_specieslist %>%
    mutate(EICATImpactNum = AbbreviationToNumbers[EICATImpact])

  # 4. Obtener la combinación mínima por especie
  df_min <- MasterList_specieslist %>%
    group_by(OriginalNameDB) %>%
    filter(!is.na(EICATImpactNum)) %>%
    slice_min(order_by = EICATImpactNum, n = 1, with_ties = FALSE) %>%
    mutate(MIN_EICATIMPACTBYMECHANISM = paste0(
      toupper(NumbersToAbbreviation[as.character(EICATImpactNum)]), " - ", Mechanisms
    )) %>%
    select(OriginalNameDB, MIN_EICATIMPACTBYMECHANISM)

  # 5. Obtener la combinación máxima por especie
  df_max <- MasterList_specieslist %>%
    group_by(OriginalNameDB) %>%
    filter(!is.na(EICATImpactNum)) %>%
    slice_max(order_by = EICATImpactNum, n = 1, with_ties = FALSE) %>%
    mutate(MAX_EICATIMPACTBYMECHANISM = paste0(
      toupper(NumbersToAbbreviation[as.character(EICATImpactNum)]), " - ", Mechanisms
    )) %>%
    select(OriginalNameDB, MAX_EICATIMPACTBYMECHANISM)

  # 6. Unir mínimo y máximo a MasterList
  MasterList <- MasterList %>%
    left_join(df_min, by = "OriginalNameDB") %>%
    left_join(df_max, by = "OriginalNameDB")

  ordered_columns <- c(
    # Identificación taxonómica
    "OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF",
    # Clasificación taxonómica
    "Kingdom", "Phylum", "Class", "Order", "Family",
    # Agrupaciones funcionales
    "FunctionalGroup", "Group",
    # Establecimiento e introducción
    "EstablishmentMeans", "Pathways",
    # Distribución nativa
    "NativeCountry", "NativeCountryISO3", "NativeRangeBioregions",
    # Distribución invadida
    "InvadedRange", "InvadedRangeISO3", "InvadedBioregions",
    # Impacto ecológico
    "EICATImpact", "Mechanisms", "MIN_EICATIMPACTBYMECHANISM", "MAX_EICATIMPACTBYMECHANISM",
    # Especies afectadas y hábitat
    "AffectedNativeSpecies", "Habitat",
    # Fechas
    "FirstRecords", "OldestDate",
    # Fuente
    "Source_Data"
  )
  df_maxmin <- MasterList[, ordered_columns]


  source(file.path("R", "noduplicates.r"))
  MasterList_specieslist <- noduplicates(df_maxmin, "AcceptedNameGBIF")

  write.xlsx(MasterList_specieslist,
             file.path("FinalFiles", paste0("FRIAS_SpeciesList_MasterList.xlsx")),
             sep = ";",
             row.names = FALSE,
             col.names = TRUE)

 numero <- nrow(MasterList_specieslist)
 cat("🐟 FRIAS MasterList contains :",  numero, "Alien Freshwater Species 🦀.", "\n")

}

