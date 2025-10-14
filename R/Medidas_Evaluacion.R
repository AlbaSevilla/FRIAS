Medidas_Evaluacion <- function(){

  #################################################################
  ############ DATASET AMFIBIOS CESAR CAPINHA #####################
  #################################################################

  #MEDIDAS DE EVALUACIÓN

  #PRECISIÓN
  #precision = (n11+n22)/n

  # #CASO AMPHIBIOS
  #    |  0   | 1
  # ----------------
  # 0  |  162 | 34
  # -----------------
  # 1  |  7   | 73      n=276

  precision_amfibios = (162+73)/276
  precision_amfibios
  #Es una medida para saber el procentaje de individuos que han sido correctamente clasificados
  #es decir que su valor predicho y real son el mismo


  #Sensibilidad: Porcentaje de individuos con valor Y=1 que hemos clasificado bien
  #(tasa de verdaderos positivos)
  Sensibilidad_amfibios = 73/(34+73)
  Sensibilidad_amfibios

  #Especificidad: Porcentaje de individuos con valor Y=0 que hemos clasificado bien
  #(Tasa de verdaderos negativos)
  Especificidad_amfibios = 162/(162+7)
  Especificidad_amfibios


  #Valor predictivo positivo: Porcentaje de individuos de entre aquellos que clasificamos como
  #Y=1, para los que acertamos
  Valor_predictivopositivo_amfibios = 73/(73+7)
  Valor_predictivopositivo_amfibios


  #Valor predictivo negativo: Porcentaje de individuos de entre aquellos que clasificamos como
  #Y=0 y acertamos
  Valor_predictivonegativo_amfibios= 162/(162+34)
  Valor_predictivonegativo_amfibios


  #F-Score
  #F_1=(2*Precision*Sensibilidad)/(Precision+Sensibilidad)=(n22)/(n22+(n12+n21)/2)
  F_1 = (2*precision_amfibios*Sensibilidad_amfibios)/(precision_amfibios+Sensibilidad_amfibios)
  F_1
  #falta coeficiente k de cohen

  ###########################################################################
  ############# AMFIBIOS ####################################################
  ###########################################################################
  # Cargar datos
  dataset_amfibios <- read_excel("Inputfiles/Step0_OriginalDatabasewithHabitat_AmphibiansReptilesCapinha_Cesar.xlsx") %>%
    select(AcceptedNameGBIF, `1_Cesar`)

  # Evaluar hábitat
  source("R/check_habitat.r")
  especies_lista <- dataset_amfibios$AcceptedNameGBIF
  dataset_actualizado <- check_habitat(especies_lista, dataset_amfibios)
  # 1. Convertir columna `1_Cesar`
  dataset_actualizado$`1_Cesar` <- ifelse(dataset_actualizado$`1_Cesar` == 1, "FRESHWATER", "NO FRESHWATER")

  # 2. Limpiar la columna `Habitat`
  dataset_actualizado$Habitat <- ifelse(grepl("FRESHWATER", dataset_actualizado$Habitat, ignore.case = TRUE),
                       "FRESHWATER",
                       "NO FRESHWATER")
  # Evaluación
  valores_pred <- dataset_actualizado$Habitat
  valores_reales <- dataset_actualizado$`1_Cesar`

  table(model_output = valores_pred, reference = valores_reales)
  caret::confusionMatrix(factor(valores_pred), factor(valores_reales), positive = "FRESHWATER")

  # Exportar resultados
  write.xlsx(dataset_actualizado, "Evaluacion/AMPHIBIANSCESAR_DATASET_HABITAT.xlsx")


  # #######################################
  # ########## GISD  ######################
  # #######################################
  # gisd_dataset <- read_excel("Inputfiles/Step0_OriginalDatabase_GISD.xlsx")
  # gisd_dataset$System <- as.factor(gisd_dataset$System)
  # # Definir los valores que consideramos como '1'
  #
  # # Obtener lista de nombres de especies
  # #SIN DUPLICADOS
  # # Obtener la lista única de especies
  # especies <- unique(gisd_dataset$Species)
  #
  # # Crear una función para procesar cada grupo por especie
  # procesar_especie <- function(specie_name) {
  #   grupo <- gisd_dataset[gisd_dataset$Species == specie_name, ]
  #   resultado <- sapply(grupo, function(col) {
  #     # Separar por coma, quitar espacios, aplanar, quitar duplicados
  #     valores <- unique(unlist(strsplit(as.character(col), ",\\s*")))
  #     paste(valores, collapse = ", ")
  #   })
  #   return(as.data.frame(t(resultado), stringsAsFactors = FALSE))
  # }
  #
  # # Aplicar la función a cada especie
  # gisd_subset_sinduplicados <- do.call(rbind, lapply(especies, procesar_especie))
  #
  # # Resetear nombres de fila
  # rownames(gisd_subset_sinduplicados) <- NULL
  # names(gisd_subset_sinduplicados)
  # gisd_subset_sinduplicados <- gisd_subset_sinduplicados[,c(1,7)]
  #
  # # Obtener lista de nombres de especies
  # dataset <- gisd_subset_sinduplicados
  # especies_lista0 <- dataset$Species
  # especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  # dataset_actualizado <- check_habitat(especies_lista, dataset)
  # # 1. Crear una nueva columna a partir de `System`
  # dataset_actualizado$Species <- ifelse(grepl("Freshwater", dataset_actualizado$System, ignore.case = TRUE),
  #                                         "FRESHWATER", "NO FRESHWATER")
  #
  # # 2. Limpiar la columna `Habitat`
  # dataset_actualizado$Habitat <- ifelse(grepl("FRESHWATER", dataset_actualizado$Habitat, ignore.case = TRUE),
  #                                       "FRESHWATER", "NO FRESHWATER")
  #
  #
  # # Evaluación
  # valores_pred <- dataset_actualizado$Habitat
  # valores_reales <- dataset_actualizado$Species
  #
  # # Tabla de contingencia
  # print(table(model_output = valores_pred, reference = valores_reales))
  #
  # # Matriz de confusión
  # confusionMatrix(factor(valores_pred), factor(valores_reales), positive = "FRESHWATER")
  #
  #
  # # Exportar resultados
  # write.xlsx(dataset_actualizado, "Evaluacion/GISD_DATASET_HABITAT.xlsx")


  #####################################
  ######### USGS ######################
  #####################################
  USGS_dataset <- read.csv("Inputfiles/Step0_OriginalDatabase_USGS.csv", sep=",")
  USGS_dataset$Native.Habitat <- as.factor(USGS_dataset$Native.Habitat)
  unique(USGS_dataset$Native.Habitat)

  USGS_dataset <- USGS_dataset[,c("Scientific.Name", "Native.Habitat")]

  # Obtener lista de nombres de especies
  dataset <- USGS_dataset
  especies_lista0 <- dataset$Scientific.Name
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)

  # 1. Limpiar la columna Native.Habitat
  dataset_actualizado$Native.Habitat <- ifelse(grepl("Freshwater", dataset_actualizado$Native.Habitat, ignore.case = TRUE),
                                               "FRESHWATER", "NO FRESHWATER")

  # 2. Limpiar la columna Habitat
  dataset_actualizado$Habitat <- ifelse(grepl("FRESHWATER", dataset_actualizado$Habitat, ignore.case = TRUE),
                                        "FRESHWATER", "NO FRESHWATER")

  # Evaluación
  valores_pred <- dataset_actualizado$Habitat
  valores_reales <- dataset_actualizado$Native.Habitat

  # Tabla de contingencia
  print(table(model_output = valores_pred, reference = valores_reales))

  # Matriz de confusión
  confusionMatrix(factor(valores_pred), factor(valores_reales), positive = "FRESHWATER")


  # Exportar resultados
  write.xlsx(dataset_actualizado, "Evaluacion/USGS_DATASET_HABITAT.xlsx")




  #####################################
  ######### ZENNI ######################
  #####################################
  ZENNI_dataset <- read_excel("Inputfiles/Step0_OriginalDatabase_ZENNI.xlsx", sheet=3)
  ZENNI_dataset$Habitat <- as.factor(ZENNI_dataset$Habitat)

  # Obtener las especies únicas
  especies <- unique(ZENNI_dataset$`Species name`)

  # Función para procesar cada especie
  procesar_especie <- function(nombre_especie) {
    grupo <- ZENNI_dataset[ZENNI_dataset$`Species name` == nombre_especie, ]

    resultado <- sapply(grupo, function(columna) {
      valores <- unique(unlist(strsplit(as.character(columna), ",\\s*")))
      paste(valores, collapse = ", ")
    })

    as.data.frame(t(resultado), stringsAsFactors = FALSE)
  }

  # Aplicar la función a todas las especies
  ZENNI_subset_sinduplicados <- do.call(rbind, lapply(especies, procesar_especie))

  # Quitar rownames
  rownames(ZENNI_subset_sinduplicados) <- NULL
  names(ZENNI_subset_sinduplicados)
  ZENNI_subset_sinduplicados <- ZENNI_subset_sinduplicados[,c("Species name", "Habitat")]
  # Cambiar el nombre de la columna en el dataframe
  colnames(ZENNI_subset_sinduplicados)[colnames(ZENNI_subset_sinduplicados) == "Habitat"] <- "Habitat_Original"


  # Probar con un subset de 10 especies (puedes ajustar esto)
  dataset <- ZENNI_subset_sinduplicados
  especies_lista0 <- dataset$`Species name`  # Verifica que el nombre de la columna sea correcto
  especies_lista <- name_backbone_checklist(especies_lista0)$canonicalName
  dataset_actualizado <- check_habitat(especies_lista, dataset)

  # 1. Limpiar la columna Native.Habitat
  dataset_actualizado$Habitat_Original <- ifelse(grepl("Freshwater", dataset_actualizado$Habitat_Original, ignore.case = TRUE),
                                               "FRESHWATER", "NO FRESHWATER")

  # 2. Limpiar la columna Habitat
  dataset_actualizado$Habitat <- ifelse(grepl("FRESHWATER", dataset_actualizado$Habitat, ignore.case = TRUE),
                                        "FRESHWATER", "NO FRESHWATER")

  # Evaluación
  valores_pred <- dataset_actualizado$Habitat
  valores_reales <- dataset_actualizado$Habitat_Original

  # Tabla de contingencia
  print(table(model_output = valores_pred, reference = valores_reales))

  # Matriz de confusión
  confusionMatrix(factor(valores_pred), factor(valores_reales), positive = "FRESHWATER")

  # Exportar resultados
  write.xlsx(dataset_actualizado, "Evaluacion/ZENNI_DATASET_HABITAT.xlsx")

}
