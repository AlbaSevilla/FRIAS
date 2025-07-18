###################################################################
###################################################################
########         FRIAS WORKFLOW                      ##############
########       2-. StandardizeDatasets()     ##############
###################################################################
###################################################################

## 2-. Cargaremos las bases de datos y sus respectivas transformaciones
StandardizeDatasets <- function (CorrespondenceTable){

  ############################################
  ################# 1 ########################
  ############################################
  if (!file.exists("OutputFiles")){
    dir.create("OutputFiles")
    dir.create(file.path("OutputFiles","Intermediate"))
    dir.create(file.path("OutputFiles","Check"))
  }

  ############################################
  ################ 2 #########################
  ############################################

  for (i in 1:nrow(CorrespondenceTable)){# Bucle que recorre todas las filas del archivo CorrespondenceTable, el excel que contiene las variables a utilizar de cada bd

    ## Cargamos los objetos necesarios
    #i<-33
    data_name <- CorrespondenceTable[i,"File_name_to_load"] #Aquí estamos cargando el nombre de los archivos de excel dentro de CorrespondenceTable. No el nombre de las bases de datos.
    data_name
    dataset_NAMECOLUMN <- CorrespondenceTable[i,"Dataset_FRIAS_name"] #Aquí estamos cargando el nombre de las bases de datos indicadas dentro de CorrespondenceTable
    dataset_NAMECOLUMN
    dataset_SourceData <- CorrespondenceTable[i,"Column_SourceData"] #Aquí estamos cargando el nombre de las bases de datos indicadas dentro de CorrespondenceTable
    dataset_SourceData
    cat("Preparing", i, "/", nrow(CorrespondenceTable), ":", dataset_NAMECOLUMN, "database", "\n")



    dat <- read.xlsx(data_name)
    dat2 <- dat[,1]
    head(dat)
    colnames(dat)
    dataset_SourceData <- CorrespondenceTable[i,"Column_SourceData"] #Aquí estamos cargando el nombre de las bases de datos indicadas dentro de CorrespondenceTable
    dataset_SourceData
    names(dat)

    #REEMPLAZAR ; POR ,
    dat <- dat %>%
      dplyr::mutate(across(where(is.character), ~ gsub(";", ",", .)))

    ## Hacemos la importación de los nombres de las columnas evitando errores de espacios, puntos, guiones, comas.
    col_names_import <- colnames(dat)
    # col_names_import <- gsub("\\.+"," ",col_names_import)
    # col_names_import <- gsub("^\\s+|\\s+$", "",col_names_import) # trim leading and trailing whitespace
    col_names_import <- gsub("^\\s+|\\s+$", "", col_names_import)  # Eliminar espacios al principio y al final


    ## Hacemos un check-in y renombramos los nombres de las columnas requeridas
    all_column_names <- vector() #Creamos un vector que contendrá el nombre de las columnas


    # #COLUMNA : Taxon_orig . El nombre original del Taxón.
    # if (!is.na(CorrespondenceTable[i,"Column_Taxon"]) & CorrespondenceTable[i,"Column_Taxon"]!=""){ #Comprobamos si la columna 'Column_Taxon' existe
    #   col_spec_names <- CorrespondenceTable[i,"Column_Taxon"] #En el caso en el que exista le asignamos la información de la columna a una nueva llamada col_spec_names
    #   if (is.na(col_spec_names)) stop(paste("Column with taxon names not found in",CorrespondenceTable[i,"Dataset_FRIAS_name"],"file!")) #Verificamos si el nombre de la columna de taxones es NA, si lo es, detenemos el código
    #   colnames(dat)[col_names_import==col_spec_names] <- "Taxon_orig" #Renombramos la columna del dataframe que corresponde a col_spec_names a "Taxon_orig"
    #    all_cfolumn_names <- c(all_column_names,"Taxon_orig") #Le añadimos a la lista de nombres de columnas 'Taxon_orig'
    #   # COLUMNA : Author.
    #    if (!is.na(CorrespondenceTable[i,"Column_author"]) & CorrespondenceTable[i,"Column_author"]!=""){
    #     col_author <- CorrespondenceTable[i,"Column_author"]
    #     colnames(dat)[col_names_import==col_author] <- "Author"
    #     all_column_names <- c(all_column_names,"Author")
    #     dat$Taxon_orig <- paste(dat$Taxon_orig,dat$Author) # Añadimos el autor al nombre del taxón
    #     dat$Taxon_orig <- gsub(" NA","",dat$Taxon_orig) # Eliminamos nombres de autor faltantes
    #   }
    # }

    #COLUMNA : Scientific_Name
    if (!is.na(CorrespondenceTable[i,"Column_scientificName"]) & CorrespondenceTable[i,"Column_scientificName"]!=""){
      col_scientific_names <- CorrespondenceTable[i,"Column_scientificName"]
      if (is.na(col_scientific_names)) stop(paste("Column with taxon names not found in",CorrespondenceTable[i,"Dataset_FRIAS_name"],"file!"))
      colnames(dat)[col_names_import==col_scientific_names] <- "Scientific_Name"
      all_column_names <- c(all_column_names,"Scientific_Name")
    }
    # #COLUMNA : Location_orig
    # if (!is.na(CorrespondenceTable[i,"Column_Location"]) & CorrespondenceTable[i,"Column_Location"]!=""){
    #   col_location <- CorrespondenceTable[i,"Column_Location"]
    #   if (is.na(col_location)) stop(paste("Column with Location names not found in",CorrespondenceTable[i,"Dataset_FRIAS_name"],"file!"))
    #   colnames(dat)[col_names_import==col_location] <- "Location_orig"
    #   all_column_names <- c(all_column_names,"Location_orig")
    # }

    ## check and rename optional column names
    # COLUMNA : Kingdom
    if (!is.na(CorrespondenceTable[i,"Column_kingdom"]) & CorrespondenceTable[i,"Column_kingdom"]!=""){
      col_kingdom <- CorrespondenceTable[i,"Column_kingdom"]
      colnames(dat)[col_names_import==col_kingdom] <- "Kingdom"
      all_column_names <- c(all_column_names,"Kingdom")
    }
    head(dat)
    # #COLUMNA : Country_ISO
    # if (!is.na(CorrespondenceTable[i,"Column_country_ISO"]) & CorrespondenceTable[i,"Column_country_ISO"]!=""){
    #   col_country_code <- CorrespondenceTable[i,"Column_country_ISO"]
    #   colnames(dat)[col_names_import==col_country_code] <- "Country_ISO"
    #   all_column_names <- c(all_column_names,"Country_ISO")
    # }
    #
    #COLUMNA : OldestDate
    if (!is.na(CorrespondenceTable[i,"Column_OldestDate"]) & CorrespondenceTable[i,"Column_OldestDate"]!=""){
      col_OldestDate <- CorrespondenceTable[i,"Column_OldestDate"]
      colnames(dat)[col_names_import==col_OldestDate] <- "OldestDate"
      all_column_names <- c(all_column_names,"OldestDate")
    }
    head(dat)
    #COLUMNA : establishmentMeans
    if (!is.na(CorrespondenceTable[i,"Column_establishmentMeans"]) & CorrespondenceTable[i,"Column_establishmentMeans"]!=""){
      col_establishmentMeans <- CorrespondenceTable[i,"Column_establishmentMeans"]
      colnames(dat)[col_names_import==col_establishmentMeans] <- "establishmentMeans"
      all_column_names <- c(all_column_names,"establishmentMeans")
      dat$establishmentMeans <- tolower(dat$establishmentMeans) #Convierte todo el texto a minúscula.
    }
    head(dat)
    #COLUMNA : pathway
    if (!is.na(CorrespondenceTable[i,"Column_pathway"]) & CorrespondenceTable[i,"Column_pathway"]!=""){
      col_pathway <- CorrespondenceTable[i,"Column_pathway"]
      colnames(dat)[col_names_import==col_pathway] <- "pathway"
      all_column_names <- c(all_column_names,"pathway")
      dat$pathway <- tolower(dat$pathway)                          # Convierte a minúsculas
      dat$pathway <- gsub("\\s+", "", dat$pathway)                 # Elimina todos los espacios
      dat$pathway <- sub(":.*", "", dat$pathway)                   # Elimina todo lo que está después (e incluyendo) de :
    }
    head(dat)
    #COLUMNA : Group
    if (!is.na(CorrespondenceTable[i,"Column_Group"]) & CorrespondenceTable[i,"Column_Group"]!=""){
      col_Group <- CorrespondenceTable[i,"Column_Group"]
      colnames(dat)[col_names_import==col_Group] <- "Group"
      all_column_names <- c(all_column_names,"Group")
      dat$Group <- tolower(dat$Group) #Convierte todo el texto a minúscula.
    }
    head(dat)
    #COLUMNA : AffectedNativeSpecies
    if (!is.na(CorrespondenceTable[i,"Column_AffectedNativeSpecies"]) & CorrespondenceTable[i,"Column_AffectedNativeSpecies"]!=""){
      col_AffectedNativeSpecies <- CorrespondenceTable[i,"Column_AffectedNativeSpecies"]
      colnames(dat)[col_names_import==col_AffectedNativeSpecies] <- "AffectedNativeSpecies"
      all_column_names <- c(all_column_names,"AffectedNativeSpecies")
      dat$AffectedNativeSpecies <- tolower(dat$AffectedNativeSpecies) #Convierte todo el texto a minúscula.
    }
    head(dat)
    #COLUMNA : NativeRangeofIAS_list
    if (!is.na(CorrespondenceTable[i,"Column_NativeRangeofIAS_list"]) & CorrespondenceTable[i,"Column_NativeRangeofIAS_list"]!=""){
      col_NativeRangeofIAS_list <- CorrespondenceTable[i,"Column_NativeRangeofIAS_list"]
      colnames(dat)[col_names_import==col_NativeRangeofIAS_list] <- "NativeRangeofIAS_list"
      all_column_names <- c(all_column_names,"NativeRangeofIAS_list")
      dat$NativeRangeofIAS_list <- tolower(dat$NativeRangeofIAS_list) #Convierte todo el texto a minúscula.
    }
    head(dat)

    ###############################################
    ######## STANDARDIZE EICAT IMPACT #############
    ###############################################
    # Leer el archivo de equivalencias
    equivs <- read_excel("TablesToStandardize/Standardization_EICATImpact.xlsx", sheet = 1, col_names = TRUE) %>%
      mutate(
        FileName = FileName,
        OriginalCategories = tolower(trimws(OriginalCategories)),
        StandardizedCategoriesEICAT = trimws(StandardizedCategoriesEICAT)
      )

    # COLUMNA : EICATImpact
    if (!is.na(CorrespondenceTable[i, "Column_EICATImpact"]) & CorrespondenceTable[i, "Column_EICATImpact"] != "") {

      col_EICATImpact <- CorrespondenceTable[i, "Column_EICATImpact"]
      colnames(dat)[col_names_import == col_EICATImpact] <- "EICATImpact"
      all_column_names <- c(all_column_names, "EICATImpact")

      # Convertir a minúsculas para facilitar el match
      dat$EICATImpact <- tolower(dat$EICATImpact)

      # Obtener nombre del archivo actual
      data_name <- CorrespondenceTable[i, "File_name_to_load"]

      # Filtrar equivalencias correspondientes al archivo actual
      equivs_filtered <- equivs %>%
        filter(FileName == data_name)

      # Reemplazar valores usando match
      match_idx <- match(trimws(dat$EICATImpact), equivs_filtered$OriginalCategories)
      reemplazos <- equivs_filtered$StandardizedCategoriesEICAT[match_idx]

      # Solo reemplazar donde haya coincidencia
      dat$EICATImpact[!is.na(reemplazos)] <- reemplazos[!is.na(reemplazos)]
    }

    #################################################################################
    #COLUMNA : Mechanism
    if (!is.na(CorrespondenceTable[i,"Column_Mechanism"]) & CorrespondenceTable[i,"Column_Mechanism"]!=""){
      col_Mechanism <- CorrespondenceTable[i,"Column_Mechanism"]
      colnames(dat)[col_names_import==col_Mechanism] <- "Mechanism"
      all_column_names <- c(all_column_names,"Mechanism")
      dat$Mechanism <- tolower(dat$Mechanism) #Convierte todo el texto a minúscula.
    }
    head(dat)

    #COLUMNA : EICAT_by_Mechanism
    if (!is.na(CorrespondenceTable[i,"Column_EICAT_by_Mechanism"]) & CorrespondenceTable[i,"Column_EICAT_by_Mechanism"]!=""){
      col_EICAT_by_Mechanism <- CorrespondenceTable[i,"EICAT_by_Mechanism"]
      colnames(dat)[col_names_import==col_EICAT_by_Mechanism] <- "EICAT_by_Mechanism"
      all_column_names <- c(all_column_names,"EICAT_by_Mechanism")
      dat$EICAT_by_Mechanism <- tolower(dat$EICAT_by_Mechanism) #Convierte todo el texto a minúscula.
    }

    #
    # # COLUMNA : habitat
    # if (!is.na(CorrespondenceTable[i,"Column_habitat"]) & CorrespondenceTable[i,"Column_habitat"] != ""){
    #   col_habitat <- CorrespondenceTable[i,"Column_habitat"]
    #   # Cambiar el nombre de la columna
    #   colnames(dat)[col_names_import == col_habitat] <- "habitat"
    #   # Añadir el nombre de la columna "habitat" al vector all_column_names
    #   all_column_names <- c(all_column_names, "habitat")
    #   # Convertir los valores de la columna 'habitat' a minúsculas
    #   dat$habitat <- tolower(dat$habitat)
    #   # Reemplazar los caracteres _, ;, | o - por -
    #   dat$habitat <- gsub("[_;|\\-]", "-", dat$habitat)
    #
    #COLUMNA : Habitat
    if (!is.na(CorrespondenceTable[i,"Column_habitat"]) & CorrespondenceTable[i,"Column_habitat"]!=""){
      col_habitat <- CorrespondenceTable[i,"Column_habitat"]
      colnames(dat)[col_names_import==col_habitat] <- "habitat"
      all_column_names <- c(all_column_names,"habitat")
      dat$habitat <- tolower(dat$habitat) #Convierte todo el texto a minúscula.
      dat$habitat <- gsub(",", "-", dat$habitat)
    }

    #COLUMNA : DateList
    if (!is.na(CorrespondenceTable[i,"Column_DateList"]) & CorrespondenceTable[i,"Column_DateList"]!=""){
      col_DateList <- CorrespondenceTable[i,"Column_DateList"]
      colnames(dat)[col_names_import==col_DateList] <- "DateList"
      all_column_names <- c(all_column_names,"DateList")
    }
    head(dat)
    names(dat)
    #COLUMNA : Invaded_Countries
    if (!is.na(CorrespondenceTable[i,"Column_InvadedCountries"]) & CorrespondenceTable[i,"Column_InvadedCountries"]!=""){
      col_InvadedCountries <- CorrespondenceTable[i,"Column_InvadedCountries"]
      colnames(dat)[col_names_import==col_InvadedCountries] <- "invaded_country_list"
      all_column_names <- c(all_column_names,"invaded_country_list")
    }
    names(dat)

    #COLUMNA : ISO3
    if (!is.na(CorrespondenceTable[i,"Column_ISO3"]) & CorrespondenceTable[i,"Column_ISO3"]!=""){
      col_iso3list <- CorrespondenceTable[i,"Column_ISO3"]
      colnames(dat)[col_names_import==col_iso3list] <- "ISO3_invaded_country_list"
      all_column_names <- c(all_column_names,"ISO3_invaded_country_list")
    }
    names(dat)

    if (!is.na(CorrespondenceTable[i, "Column_AcceptedNameGBIF"]) & CorrespondenceTable[i, "Column_AcceptedNameGBIF"] != "") {
      col_acceptednamegbif <- CorrespondenceTable[i, "Column_AcceptedNameGBIF"]
      colnames(dat)[col_names_import == col_acceptednamegbif] <- "AcceptedNameGBIF"
      all_column_names <- c(all_column_names, "AcceptedNameGBIF")

      # Solo aplicar estandarización si el nombre original de columna no era ya "AcceptedNameGBIF"
      if (col_acceptednamegbif != "AcceptedNameGBIF") {
        nombre <- dat$AcceptedNameGBIF
        nombre <- as.data.frame(nombre)
        nombreaceptadogbif <- name_backbone_checklist(nombre)$species
        dat$AcceptedNameGBIF <- nombreaceptadogbif
      }
    }
    #COLUMNA : Order
    if (!is.na(CorrespondenceTable[i,"Column_Order"]) & CorrespondenceTable[i,"Column_Order"]!=""){
      col_order <- CorrespondenceTable[i,"Column_Order"]
      colnames(dat)[col_names_import==col_order] <- "Order"
      all_column_names <- c(all_column_names,"Order")
      dat$Order <- tolower(dat$Order)
    }
    names(dat)
    #COLUMNA : Family
    if (!is.na(CorrespondenceTable[i,"Column_Family"]) & CorrespondenceTable[i,"Column_Family"]!=""){
      col_family <- CorrespondenceTable[i,"Column_Family"]
      colnames(dat)[col_names_import==col_family] <- "Family"
      all_column_names <- c(all_column_names,"Family")
      dat$family <- tolower(dat$Family)
    }
    # names(dat)
    # #COLUMNA : Group
    # if (!is.na(CorrespondenceTable[i,"Column_Group"]) & CorrespondenceTable[i,"Column_Group"]!=""){
    #   col_group <- CorrespondenceTable[i,"Column_Group"]
    #   colnames(dat)[col_names_import==col_group] <- "Group"
    #   all_column_names <- c(all_column_names,"Group")
    #   dat$group <- tolower(dat$Group)
    # }
    names(dat)
    #COLUMNA : Group
    if (!is.na(CorrespondenceTable[i,"Column_Group"]) & CorrespondenceTable[i,"Column_Group"]!=""){
      col_group <- CorrespondenceTable[i,"Column_Group"]
      colnames(dat)[col_names_import==col_group] <- "Group"
      all_column_names <- c(all_column_names,"Group")
      dat$group <- tolower(dat$Group)
    }
    names(dat)

    #COLUMNA : Group
    if (!is.na(CorrespondenceTable[i,"Column_FunctionalGroup"]) & CorrespondenceTable[i,"Column_FunctionalGroup"]!=""){
      col_FunctionalGroup <- CorrespondenceTable[i,"Column_FunctionalGroup"]
      colnames(dat)[col_names_import==col_FunctionalGroup] <- "FunctionalGroup"
      all_column_names <- c(all_column_names,"FunctionalGroup")
      dat$FunctionalGroup <- tolower(dat$FunctionalGroup)
    }
    ## Creamos el dataset final que contiene las variables requeridas, opcionales y adicionales
    dat_out <- dat[ , names(dat) %in% all_column_names]
    if(dataset_SourceData == "JELLYFISH_2025"){
      dat_out = dat
    }
    names(dat_out)
    dat_out[dat_out=="Null"] <- ""
    dat_out[is.na(dat_out)] <- ""

    # COLUMNA : ID_GBIF
    # Inicializamos la nueva columna 'ID_GBIF' con valores NA
    dat_out$ID_GBIF <- NA

    for (i in 1:nrow(dat_out)) {
      nombre_especie <- dat_out$AcceptedNameGBIF[i]
      backboneGBIF <- name_backbone_checklist(name = nombre_especie)
      if (!is.null(backboneGBIF) && length(backboneGBIF$speciesKey) > 0) {
        dat_out$ID_GBIF[i] <- backboneGBIF$speciesKey[1]  # Tomamos el primer valor de 'speciesKey'
      }
    }
    colnames(dat_out) <- gsub("\\.+","_",colnames(dat_out))


    #Añadimos una nueva columna con el nombre del dataset que estamos utilizando
    dat_out$Source_Database <- dataset_NAMECOLUMN
    dat_out$Source_Date <- dataset_SourceData
    dat_final <- cbind(dat2,dat_out)

    # Cambiar el nombre de la primera columna
    colnames(dat_final)[1] <- "OriginalNameDB"
    dat_final <- dat_final %>%
      mutate(OriginalNameDB = str_extract(OriginalNameDB, "^\\S+\\s+\\S+"))

    ######################################################################
    ########## ORDENAMOS LAS COLUMNAS ####################################
    ######################################################################
    # Lista de columnas deseadas
    desired_columns <- c("AcceptedNameGBIF", "OriginalNameDB", "ID_GBIF",
                         "Kingdom", "Order", "Family", "establishmentMeans",
                         "pathway", "FunctionalGroup", "Group",
                         "NativeRangeofIAS_list", "AffectedNativeSpecies",
                         "EICATImpact", "Mechanism","EICAT_by_Mechanism","habitat", "DateList",
                         "OldestDate","invaded_country_list", "ISO3_invaded_country_list",
                         "Source_Date")

    # Añadir columnas faltantes con NA
    missing_columns <- setdiff(desired_columns, names(dat_final))
    dat_final[missing_columns] <- NA

    # Reordenar columnas en el orden deseado
    dat_final <- dat_final[desired_columns]
    head(dat_final)

    no_accents <- function(df) {
      df[] <- lapply(df, function(col) {
        if (is.character(col)) {
          iconv(col, from = "UTF-8", to = "ASCII//TRANSLIT")
        } else {
          col  # no modifica columnas que no sean texto
        }
      })
      return(df)
    }
    dat_final <- no_accents(dat_final)


    # Identificar las filas con NA en AcceptedNameGBIF o ID_GBIF
    NA_rows <- dat_final %>% filter(is.na(AcceptedNameGBIF) | AcceptedNameGBIF == "" | is.na(ID_GBIF) | ID_GBIF == "")

    if (nrow(NA_rows) > 0) {
      # Añadir la columna con el nombre de la base de datos
      NA_rows$DatabaseName <- dataset_NAMECOLUMN

      # Guardar las filas con NA en AcceptedNameGBIF o ID_GBIF en un archivo Excel específico para esa base de datos
      write.xlsx(NA_rows,
                 file.path("OutputFiles", "Check", paste("NA_AcceptedNameGBIF_ID_GBIF_", dataset_NAMECOLUMN, ".xlsx")),
                 row.names = FALSE,
                 col.names = TRUE,
                 quote = FALSE)  # Evitar que los valores estén entre comillas
    }


    #######################################################
    ###### ELMINAR ESPECIES SP SPP ETC ####################
    #######################################################

    # Registros eliminados: AcceptedNameGBIF vacío Y además cumplen alguna de las condiciones
    eliminados_hibridos <- dat_final %>%
      filter(is.na(AcceptedNameGBIF) | AcceptedNameGBIF == "") %>%
      filter(
        str_count(OriginalNameDB, "\\w+") != 2 |
          str_detect(OriginalNameDB, regex("sp\\.|spp\\.|ssp\\.", ignore_case = TRUE))
      )

    # Registros finales: se eliminan solo si AcceptedNameGBIF está vacío Y cumplen alguna condición
    dat_final_filtrado <- dat_final %>%
      filter(
        !(
          (is.na(AcceptedNameGBIF) | AcceptedNameGBIF == "") &
            (
              str_count(OriginalNameDB, "\\w+") != 2 |
                str_detect(OriginalNameDB, regex("sp\\.|spp\\.|ssp\\.", ignore_case = TRUE))
            )
        )
      )



    # Guardar el dataset limpio en el directorio "Intermediate"
    write.table(dat_final_filtrado,
                file.path("OutputFiles", "Intermediate", paste0("Step1_Prepared_", dataset_NAMECOLUMN, ".csv")),
                sep=";",       # Usar ; como delimitador
                row.names=FALSE,  # Evitar escribir los nombres de las filas
                col.names=TRUE,   # Incluir los nombres de las columnas
                quote=FALSE)      # Evitar que los valores estén entre comillas
  }
}
