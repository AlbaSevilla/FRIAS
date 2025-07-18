Final_MasterLists <- function(){
  MasterList <- read.xlsx("OutputFiles/Intermediate/Step12_SelectedFreshwaterGBIF_Masterlist.xlsx")

  #eliminamos cosas que se habian colado:
  MasterList <- MasterList %>%
    mutate(establishmentMeans = str_split(establishmentMeans, ";")) %>%
    unnest(establishmentMeans) %>%
    mutate(
      establishmentMeans = tolower(str_trim(establishmentMeans)),
    )
  unique(MasterList$establishmentMeans)

  # Filtrar el dataframe excluyendo los valores deseados
  MasterList2 <- MasterList[!(MasterList$establishmentMeans %in% c("cas", "est", "native")), ]
  unique(MasterList2$establishmentMeans)


  source(file.path("R", "colapsar_por_AcceptedNameGBIF.r"))
  MasterList <- colapsar_por_AcceptedNameGBIF(MasterList2)
  nrow(MasterList)
  head(MasterList$Source_Data)
  names(MasterList)

  MasterList <- MasterList %>%
    mutate(InvadedCountriesISO3_List = if_else(
      str_detect(InvadedCountriesISO3_List, "^[a-zA-Z]{6}$"),  # Exactamente 6 letras, sin comas
      str_replace(InvadedCountriesISO3_List, "^(.{3})(.{3})$", "\\1,\\2"),  # Insertar coma después de 3 letras
      InvadedCountriesISO3_List  # Si no cumple, deja como está
    ))

  #CONVERTIMOS La primera letra de cada columna a Mayuscula
  # Función para poner en mayúscula la primera letra de cada palabra
  to_title_case <- function(x) {
    str_to_title(x)
  }
  # Nombres de columnas que quieres transformar
  cols_to_modify <- c("OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF",
                      "Kingdom", "Phylum", "Class", "Order", "Family",
                      "FunctionalGroup", "Group",)

  # Aplica la transformación solo a esas columnas
  MasterList[cols_to_modify] <- lapply(MasterList[cols_to_modify], to_title_case)



  ############################################################
  ################ MASTERLIST CON TODOS LOS IMPACTOS #########
  ############################################################
  nrow(MasterList)
  names(MasterList)
  #OLDEST DATE
  MasterList <- MasterList %>%
    mutate(
      OldestDate = sapply(strsplit(DateList, ";"), function(x) {
        x <- str_trim(x)
        if (length(x) == 0 || is.na(x[1]) || x[1] == "") {
          NA  # en caso de vacío o NA
        } else {
          x[1]  # primer valor
        }
      })
    )

  head(MasterList$OldestDate,20)
  #################################################
  ######### CREAMOS CARPETA FINAL #################
  #################################################
  if (!file.exists("OutputFiles")){
    dir.create(file.path("OutputFiles/Final"), recursive=TRUE)
  }
  MasterList_impacts <- MasterList
  unique(MasterList_impacts$Mechanism)
  nrow(MasterList_impacts)
  MasterList_impacts <- MasterList_impacts %>%
    mutate(EICATImpact = str_split(EICATImpact, ";")) %>%      # Separar por ";"
    unnest(EICATImpact) %>%                                 # Crear una fila por cada keyword
    mutate(EICATImpact = str_trim(tolower(EICATImpact)))       #
  MasterList_impacts <- MasterList_impacts %>%
    mutate(Mechanism = str_split(Mechanism, "; ")) %>%      # Separar por ";"
    unnest(Mechanism) %>%                                 # Crear una fila por cada keyword
    mutate(Mechanism = str_trim(tolower(Mechanism)))       #

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
                                              "NativeCountry","NativeCountries_ISO3","NativeRangeBioregions",
                                              "invaded_country_list","InvadedCountriesISO3_List","InvadedBioregions",
                                              "establishmentMeans","pathway","EICATImpact","Mechanism","EICAT_by_Mechanism",
                                              "AffectedNativeSpecies","habitat","Source_Data")]
  write.xlsx(MasterList_impacts2,
             file.path("OutputFiles", "Final", paste0("FRIAS_AllImpactsList_MasterList.xlsx")),
             sep = ";",
             row.names = FALSE,
             col.names = TRUE)


  ###############################################################################
  ####### MASTERLIST DE LISTA DE NOMBRES Y columna de MAXIMO/MINIMO IMPACTO #####
  ###############################################################################
  #GENERAMOS LA MASTERLIST SIN IMPACTOS
  columnas_specieslist <- c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF",
                              "Kingdom","Phylum","Class","Order", "Family", "FunctionalGroup", "Group",
                              "NativeCountry","NativeCountries_ISO3","NativeRangeBioregions",
                              "invaded_country_list","InvadedCountriesISO3_List","InvadedBioregions",
                              "establishmentMeans","pathway","EICATImpact","Mechanism","EICAT_by_Mechanism",
                              "AffectedNativeSpecies","habitat","Source_Data")
  # 1. Separar y limpiar datos como ya tienes
  MasterList_specieslist <- MasterList %>%
    mutate(EICATImpact = str_split(EICATImpact, ";")) %>%
    unnest(EICATImpact) %>%
    mutate(EICATImpact = str_trim(tolower(EICATImpact))) %>%
    mutate(Mechanism = str_split(Mechanism, ";")) %>%
    unnest(Mechanism) %>%
    mutate(Mechanism = str_trim(tolower(Mechanism)))

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
      toupper(NumbersToAbbreviation[as.character(EICATImpactNum)]), " - ", Mechanism
    )) %>%
    select(OriginalNameDB, MIN_EICATIMPACTBYMECHANISM)

  # 5. Obtener la combinación máxima por especie
  df_max <- MasterList_specieslist %>%
    group_by(OriginalNameDB) %>%
    filter(!is.na(EICATImpactNum)) %>%
    slice_max(order_by = EICATImpactNum, n = 1, with_ties = FALSE) %>%
    mutate(MAX_EICATIMPACTBYMECHANISM = paste0(
      toupper(NumbersToAbbreviation[as.character(EICATImpactNum)]), " - ", Mechanism
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
    "establishmentMeans", "pathway",
    # Distribución nativa
    "NativeCountry", "NativeCountries_ISO3", "NativeRangeBioregions",
    # Distribución invadida
    "invaded_country_list", "InvadedCountriesISO3_List", "InvadedBioregions",
    # Impacto ecológico
    "EICATImpact", "Mechanism", "MIN_EICATIMPACTBYMECHANISM", "MAX_EICATIMPACTBYMECHANISM",
    # Especies afectadas y hábitat
    "AffectedNativeSpecies", "habitat",
    # Fechas
    "DateList", "OldestDate",
    # Fuente
    "Source_Data"
  )
  df_maxmin <- MasterList[, ordered_columns]

  source(file.path("R", "colapsar_por_AcceptedNameGBIF.r"))
  MasterList_specieslist <- colapsar_por_AcceptedNameGBIF(df_maxmin)

  write.xlsx(MasterList_specieslist,
             file.path("OutputFiles", "Final", paste0("FRIAS_SpeciesList_MasterList.xlsx")),
             sep = ";",
             row.names = FALSE,
             col.names = TRUE)

 numero <- nrow(MasterList_specieslist)
 cat("🐟 FRIAS MasterList contains :",  numero, "Alien Freshwater Species 🦀.", "\n")

}
