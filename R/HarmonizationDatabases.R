######################################################
###        F R I A S     W O R K F L O W         #####
###            HarmonizationDatabases()          #####
######################################################

########################################################################################
#                                                                                      #
#  📂 CorrespondenceTable (with metadata)                                              #
#             │                                                                        #
#             ▼                                                                        #
#  ╔══════════════════════════════════════╗                                            #
#  ║ For each row in CorrespondenceTable  ║                                            #
#  ╚══════════════════════════════════════╝                                            #
#             │                                                                        #
#             ▼                                                                        #
#  ┌──────────────────────────────────────────┐                                        #
#  │  📥 Load source dataset (.xlsx)          │ ← File_name_to_load                    #
#  └──────────────────────────────────────────┘                                        #
#             │                                                                        #
#             ▼                                                                        #
#  ┌──────────────────────────────────────────┐                                        #
#  │  🧽 Clean/trim column names              │                                        #
#  └──────────────────────────────────────────┘                                        #
#             │                                                                        #
#             ▼                                                                        #
#  ┌──────────────────────────────────────────┐                                        #
#  │  🔄 Rename relevant columns              │ ← Based on column mapping in table     #
#  └──────────────────────────────────────────┘                                        #
#             │                                                                        #
#             ▼                                                                        #
#  ┌──────────────────────────────────────────┐                                        #
#  │  🔠 Convert values to lowercase          │                                        #
#  │  🚫 Remove punctuation/spaces            │                                        #
#  └──────────────────────────────────────────┘                                        #
#             │                                                                        #
#             ▼                                                                        #
#  ┌──────────────────────────────────────────┐                                        #
#  │  📊 Standardize EICAT Impact using       │                                        #
#  │     external category equivalence table  │                                        #
#  └──────────────────────────────────────────┘                                        #
#             │                                                                        #
#             ▼                                                                        #
#  ┌──────────────────────────────────────────┐                                        #
#  │  🔍 Match AcceptedNameGBIF + ID_GBIF     │ ← GBIF Backbone Checklist              #
#  └──────────────────────────────────────────┘                                        #
#             │                                                                        #
#             ▼                                                                        #
#  ┌──────────────────────────────────────────┐                                        #
#  │  🏷️ Add metadata: Source_Data, DB name   │                                        #
#  └──────────────────────────────────────────┘                                        #
#             │                                                                        #
#             ▼                                                                        #
#  ┌──────────────────────────────────────────┐                                        #
#  │  🧹 Remove accents and invalid species    │ ("sp.", "spp.", missing names, etc.)  #
#  └──────────────────────────────────────────┘                                        #
#             │                                                                        #
#     ┌───────┴────────┐                                                               #
#     ▼                ▼                                                               #
#  Save clean      Save unmatched species                                              #
#  dataset         (NA AcceptedNameGBIF or ID_GBIF)                                    #
#  as .csv         in /OutputFiles/Check                                               #
#                                                                                      #
########################################################################################



HarmonizationDatabases <- function (CorrespondenceTable){

  for (i in 1:nrow(CorrespondenceTable)){
    #i<-78
    #Load neccessary objects
    data_name <- CorrespondenceTable[i,"File_name_to_load"] #Aquí estamos cargando el nombre de los archivos de excel dentro de CorrespondenceTable. No el nombre de las bases de datos.
    dataset_NAMECOLUMN <- CorrespondenceTable[i,"Dataset_FRIAS_name"] #Aquí estamos cargando el nombre de las bases de datos indicadas dentro de CorrespondenceTable
    dataset_NAMECOLUMN
    dataset_SourceData <- CorrespondenceTable[i,"Column_SourceData"] #Aquí estamos cargando el nombre de las bases de datos indicadas dentro de CorrespondenceTable

    #cat("Preparing", i, "/", nrow(CorrespondenceTable), ":", dataset_NAMECOLUMN, "database", "\n")
    dat <- read.xlsx(data_name)
    dat2 <- dat[,1] #This is to save original species name
    dataset_SourceData <- CorrespondenceTable[i,"Column_SourceData"]
    data_column_names <- colnames(dat) #This is to save column names
    data_column_names <- gsub("^\\s+|\\s+$", "", data_column_names)

    #Just in case, replace ; to ,
    dat <- dat %>%
      dplyr::mutate(across(where(is.character), ~ gsub(";", ",", .)))



    # +-----------------------------------------------------------+
    # |Harmonization of Column Names📊                            |
    # +-----------------------------------------------------------+
    all_column_names <- vector()

    #📚Column:Scientific_Name
    if (!is.na(CorrespondenceTable[i,"Column_scientificName"]) & CorrespondenceTable[i,"Column_scientificName"]!=""){
      col_scientific_names <- CorrespondenceTable[i,"Column_scientificName"]
      if (is.na(col_scientific_names)) stop(paste("Column with taxon names not found in",CorrespondenceTable[i,"Dataset_FRIAS_name"],"file!"))
      colnames(dat)[data_column_names==col_scientific_names] <- "Scientific_Name"
      all_column_names <- c(all_column_names,"Scientific_Name")
    }
    #📚Column:Kingdom
    if (!is.na(CorrespondenceTable[i,"Column_kingdom"]) & CorrespondenceTable[i,"Column_kingdom"]!=""){
      col_kingdom <- CorrespondenceTable[i,"Column_kingdom"]
      colnames(dat)[data_column_names==col_kingdom] <- "Kingdom"
      all_column_names <- c(all_column_names,"Kingdom")
    }
    #📚Column:Order
    if (!is.na(CorrespondenceTable[i,"Column_Order"]) & CorrespondenceTable[i,"Column_Order"]!=""){
      col_order <- CorrespondenceTable[i,"Column_Order"]
      colnames(dat)[data_column_names==col_order] <- "Order"
      all_column_names <- c(all_column_names,"Order")
      dat$Order <- tolower(dat$Order)
    }
    #📚Column:Family
    if (!is.na(CorrespondenceTable[i,"Column_Family"]) & CorrespondenceTable[i,"Column_Family"]!=""){
      col_family <- CorrespondenceTable[i,"Column_Family"]
      colnames(dat)[data_column_names==col_family] <- "Family"
      all_column_names <- c(all_column_names,"Family")
      dat$family <- tolower(dat$Family)
    }
    #📚Column:Phylum
    if (!is.na(CorrespondenceTable[i,"Column_Phylum"]) & CorrespondenceTable[i,"Column_Phylum"]!=""){
      col_Phylum <- CorrespondenceTable[i,"Column_Phylum"]
      colnames(dat)[data_column_names==col_Phylum] <- "Phylum"
      all_column_names <- c(all_column_names,"Phylum")
      dat$Phylum <- tolower(dat$Phylum)
    }
    #📚Column:Class
    if (!is.na(CorrespondenceTable[i,"Column_Class"]) & CorrespondenceTable[i,"Column_Class"]!=""){
      col_Class <- CorrespondenceTable[i,"Column_Class"]
      colnames(dat)[data_column_names==col_Class] <- "Class"
      all_column_names <- c(all_column_names,"Class")
      dat$Class <- tolower(dat$Class)
    }

    #📚Column:Group
    if (!is.na(CorrespondenceTable[i,"Column_Group"]) & CorrespondenceTable[i,"Column_Group"]!=""){
      col_Group <- CorrespondenceTable[i,"Column_Group"]
      colnames(dat)[data_column_names==col_Group] <- "Group"
      all_column_names <- c(all_column_names,"Group")
      dat$Group <- tolower(dat$Group) #Convierte todo el texto a minúscula.
    }
    #📚Column:FunctionalGroup
    if (!is.na(CorrespondenceTable[i,"Column_FunctionalGroup"]) & CorrespondenceTable[i,"Column_FunctionalGroup"]!=""){
      col_FunctionalGroup <- CorrespondenceTable[i,"Column_FunctionalGroup"]
      colnames(dat)[data_column_names==col_FunctionalGroup] <- "FunctionalGroup"
      all_column_names <- c(all_column_names,"FunctionalGroup")
      dat$FunctionalGroup <- tolower(dat$FunctionalGroup)
    }
    #📚Column:FirstRecords
    if (!is.na(CorrespondenceTable[i,"Column_FirstRecords"]) & CorrespondenceTable[i,"Column_FirstRecords"]!=""){
      col_FirstRecords <- CorrespondenceTable[i,"Column_FirstRecords"]
      colnames(dat)[data_column_names==col_FirstRecords] <- "FirstRecords"
      all_column_names <- c(all_column_names,"FirstRecords")
    }
    #📚Column:OldestDate
    if (!is.na(CorrespondenceTable[i,"Column_OldestDate"]) & CorrespondenceTable[i,"Column_OldestDate"]!=""){
      col_OldestDate <- CorrespondenceTable[i,"Column_OldestDate"]
      colnames(dat)[data_column_names==col_OldestDate] <- "OldestDate"
      all_column_names <- c(all_column_names,"OldestDate")
    }
    #📚Column:NativeRange
    if (!is.na(CorrespondenceTable[i,"Column_NativeRange"]) & CorrespondenceTable[i,"Column_NativeRange"]!=""){
      col_NativeRange <- CorrespondenceTable[i,"Column_NativeRange"]
      colnames(dat)[data_column_names==col_NativeRange] <- "NativeRange"
      all_column_names <- c(all_column_names,"NativeRange")
      dat$NativeRange <- tolower(dat$NativeRange) #Convierte todo el texto a minúscula.
    }
    #📚Column:InvadedRange
    if (!is.na(CorrespondenceTable[i,"Column_InvadedRange"]) & CorrespondenceTable[i,"Column_InvadedRange"]!=""){
      col_InvadedRange <- CorrespondenceTable[i,"Column_InvadedRange"]
      colnames(dat)[data_column_names==col_InvadedRange] <- "InvadedRange"
      all_column_names <- c(all_column_names,"InvadedRange")
    }
    #📚Column:InvadedRangeISO3
    if (!is.na(CorrespondenceTable[i,"Column_InvadedRangeISO3"]) & CorrespondenceTable[i,"Column_InvadedRangeISO3"]!=""){
      col_iso3list <- CorrespondenceTable[i,"Column_InvadedRangeISO3"]
      colnames(dat)[data_column_names==col_iso3list] <- "InvadedRangeISO3"
      all_column_names <- c(all_column_names,"InvadedRangeISO3")
    }
    #📚Column:EstablishmentMeans
    if (!is.na(CorrespondenceTable[i,"Column_EstablishmentMeans"]) & CorrespondenceTable[i,"Column_EstablishmentMeans"]!=""){
      col_EstablishmentMeans <- CorrespondenceTable[i,"Column_EstablishmentMeans"]
      colnames(dat)[data_column_names==col_EstablishmentMeans] <- "EstablishmentMeans"
      all_column_names <- c(all_column_names,"EstablishmentMeans")
      dat$EstablishmentMeans <- tolower(dat$EstablishmentMeans) #Convierte todo el texto a minúscula.
    }
    #📚Column:Pathways
    if (!is.na(CorrespondenceTable[i,"Column_Pathways"]) & CorrespondenceTable[i,"Column_Pathways"]!=""){
      col_Pathways <- CorrespondenceTable[i,"Column_Pathways"]
      colnames(dat)[data_column_names==col_Pathways] <- "Pathways"
      all_column_names <- c(all_column_names,"Pathways")
      dat$Pathways <- tolower(dat$Pathways)                          # Convierte a minúsculas
      dat$Pathways <- gsub("\\s+", "", dat$Pathways)                 # Elimina todos los espacios
      dat$Pathways <- sub(":.*", "", dat$Pathways)                   # Elimina todo lo que está después (e incluyendo) de :
    }
    #📚Column:AffectedNativeSpecies
    if (!is.na(CorrespondenceTable[i,"Column_AffectedNativeSpecies"]) & CorrespondenceTable[i,"Column_AffectedNativeSpecies"]!=""){
      col_AffectedNativeSpecies <- CorrespondenceTable[i,"Column_AffectedNativeSpecies"]
      colnames(dat)[data_column_names==col_AffectedNativeSpecies] <- "AffectedNativeSpecies"
      all_column_names <- c(all_column_names,"AffectedNativeSpecies")
      dat$AffectedNativeSpecies <- tolower(dat$AffectedNativeSpecies) #Convierte todo el texto a minúscula.
    }
            ###############################################
            ######## STANDARDIZE EICAT IMPACT #############
            ###############################################
            equivs <- read_excel("TablesToStandardize/Standardization_EICATImpact.xlsx", sheet = 1, col_names = TRUE) %>%
              mutate(
                FileName = FileName,
                OriginalCategories = tolower(trimws(OriginalCategories)),
                StandardizedCategoriesEICAT = trimws(StandardizedCategoriesEICAT)
              )

            #📚Column:EICATImpact
            if (!is.na(CorrespondenceTable[i, "Column_EICATImpact"]) & CorrespondenceTable[i, "Column_EICATImpact"] != "") {

              col_EICATImpact <- CorrespondenceTable[i, "Column_EICATImpact"]
              colnames(dat)[data_column_names == col_EICATImpact] <- "EICATImpact"
              all_column_names <- c(all_column_names, "EICATImpact")
              dat$EICATImpact <- tolower(dat$EICATImpact)
              data_name <- CorrespondenceTable[i, "File_name_to_load"] # We apply to the file if the name is found in the equivs list.
              equivs_filtered <- equivs %>%
                filter(FileName == data_name)

              #Replacement
              match_idx <- match(trimws(dat$EICATImpact), equivs_filtered$OriginalCategories)
              reemplazos <- equivs_filtered$StandardizedCategoriesEICAT[match_idx]
              dat$EICATImpact[!is.na(reemplazos)] <- reemplazos[!is.na(reemplazos)]
            }

            #################################################################################

    #📚Column:Mechanisms
    if (!is.na(CorrespondenceTable[i,"Column_Mechanisms"]) & CorrespondenceTable[i,"Column_Mechanisms"]!=""){
      col_Mechanisms <- CorrespondenceTable[i,"Column_Mechanisms"]
      colnames(dat)[data_column_names==col_Mechanisms] <- "Mechanisms"
      all_column_names <- c(all_column_names,"Mechanisms")
      dat$Mechanisms <- tolower(dat$Mechanisms) #Convierte todo el texto a minúscula.
    }
    #📚Column:EICAT_by_Mechanism
    if (!is.na(CorrespondenceTable[i,"Column_EICAT_by_Mechanism"]) & CorrespondenceTable[i,"Column_EICAT_by_Mechanism"]!=""){
      col_EICAT_by_Mechanism <- CorrespondenceTable[i,"EICAT_by_Mechanism"]
      colnames(dat)[data_column_names==col_EICAT_by_Mechanism] <- "EICAT_by_Mechanism"
      all_column_names <- c(all_column_names,"EICAT_by_Mechanism")
      dat$EICAT_by_Mechanism <- tolower(dat$EICAT_by_Mechanism) #Convierte todo el texto a minúscula.
    }
    #📚Column:Habitat
    if (!is.na(CorrespondenceTable[i,"Column_Habitat"]) & CorrespondenceTable[i,"Column_Habitat"]!=""){
      col_Habitat <- CorrespondenceTable[i,"Column_Habitat"]
      colnames(dat)[data_column_names==col_Habitat] <- "Habitat"
      all_column_names <- c(all_column_names,"Habitat")
      dat$Habitat <- tolower(dat$Habitat) #Convierte todo el texto a minúscula.
      dat$Habitat <- gsub(",", "-", dat$Habitat)
    }

    #📚Column:AcceptedNameGBIF
    if (!is.na(CorrespondenceTable[i, "Column_AcceptedNameGBIF"]) & CorrespondenceTable[i, "Column_AcceptedNameGBIF"] != "") {
      col_acceptednamegbif <- CorrespondenceTable[i, "Column_AcceptedNameGBIF"]
      colnames(dat)[data_column_names == col_acceptednamegbif] <- "AcceptedNameGBIF"
      all_column_names <- c(all_column_names, "AcceptedNameGBIF")

      # Solo aplicar estandarización si el nombre original de Column no era ya "AcceptedNameGBIF"
      if (col_acceptednamegbif != "AcceptedNameGBIF") {
        nombre <- dat$AcceptedNameGBIF
        nombre <- as.data.frame(nombre)
        nombreaceptadogbif <- name_backbone_checklist(nombre)$species
        dat$AcceptedNameGBIF <- nombreaceptadogbif
      }
    }


    # +-----------------------------------------------------------+
    # |Final Dataset (1/2) 📊                                     |
    # +-----------------------------------------------------------+
    ## Creamos el dataset final que contiene las variables requeridas, opcionales y adicionales
    dat_out <- dat[ , names(dat) %in% all_column_names]
    names(dat_out)
    dat_out[dat_out=="Null"] <- ""
    dat_out[is.na(dat_out)] <- ""

    # 📚📚Column:ID_GBIF
    dat_out$ID_GBIF <- NA
    for (i in 1:nrow(dat_out)) {
      nombre_especie <- dat_out$AcceptedNameGBIF[i]
      backboneGBIF <- name_backbone_checklist(name = nombre_especie)
      if (!is.null(backboneGBIF) && length(backboneGBIF$speciesKey) > 0) {
        dat_out$ID_GBIF[i] <- backboneGBIF$speciesKey[1]
      }
    }
    colnames(dat_out) <- gsub("\\.+","_",colnames(dat_out))

    #We add a new column with the name of the dataset we are using.
    dat_out$Source_Database <- dataset_NAMECOLUMN
    dat_out$Source_Data <- dataset_SourceData
    dat_final <- cbind(dat2,dat_out)

    # Change the name of the first column with the species names given by each database
    colnames(dat_final)[1] <- "OriginalNameDB"
    dat_final <- dat_final %>%
      mutate(OriginalNameDB = str_extract(OriginalNameDB, "^\\S+\\s+\\S+"))

    ##################
    ##     ORDER    ##
    ##    columns   ##
    ##################
    desired_order <- c("AcceptedNameGBIF", "OriginalNameDB", "ID_GBIF",
                         "Kingdom", "Order", "Family", "Phylum","Class", "EstablishmentMeans",
                         "Pathways", "FunctionalGroup", "Group",
                         "NativeRange","InvadedRange", "InvadedRangeISO3",
                         "AffectedNativeSpecies", "EICATImpact", "Mechanisms","EICAT_by_Mechanism",
                         "Habitat",
                         "FirstRecords", "OldestDate","Source_Data")

    #In case one column is not in the database, we add it with empty rows
    missing_columns <- setdiff(desired_order, names(dat_final))
    dat_final[missing_columns] <- NA

    #Reorder columns in desired way
    dat_final <- dat_final[desired_order]
    head(dat_final)

    ##################
    ##     REMOVE   ##
    ##    ACCENTS   ##
    ##################
    no_accents <- function(df) {
      df[] <- lapply(df, function(col) {
        if (is.character(col)) {
          iconv(col, from = "UTF-8", to = "ASCII//TRANSLIT")
        } else {
          col
        }
      })
      return(df)
    }
    dat_final <- no_accents(dat_final)


    # +-----------------------------------------------------------+
    # |Save Species with not found Accepted Name in GBIF          |
    # |or ID in Check Subfolder 📊                                |
    # +-----------------------------------------------------------+
    NA_rows <- dat_final %>% filter(is.na(AcceptedNameGBIF) | AcceptedNameGBIF == "" | is.na(ID_GBIF) | ID_GBIF == "")

    if (nrow(NA_rows) > 0) {
      NA_rows$DatabaseName <- dataset_NAMECOLUMN
      write.xlsx(NA_rows,
                 file.path("OutputFiles", "Check", paste("NA_AcceptedNameGBIF_ID_GBIF_", dataset_NAMECOLUMN, ".xlsx")),
                 row.names = FALSE,
                 col.names = TRUE,
                 quote = FALSE)
    }


    #######################
    ##   ELMINATE        ##
    ##   SPECIES LIKE:   ##
    ##   SP SPP ETC      ##
    #######################
    final_dataset <- dat_final %>%
      filter(
        !(
          (is.na(AcceptedNameGBIF) | AcceptedNameGBIF == "") &
            (
              str_count(OriginalNameDB, "\\w+") != 2 |
                str_detect(OriginalNameDB, regex("sp\\.|spp\\.|ssp\\.", ignore_case = TRUE))
            )
        )
      )


    # +-----------------------------------------------------------+
    # |Final Dataset (2/2) 📊                                     |
    # +-----------------------------------------------------------+
    write.table(final_dataset,
                file.path("OutputFiles", "Intermediate", paste0("Step1_Prepared_", dataset_NAMECOLUMN, ".csv")),
                sep=";",
                row.names=FALSE,
                col.names=TRUE,
                quote=FALSE)
    cat(paste0("Step1_Prepared_", dataset_NAMECOLUMN, " saved ✅ ", "\n"))
  }
}
