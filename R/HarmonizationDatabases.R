######################################################
###        F R I A S     W O R K F L O W         #####
###            HarmonizationDatabases()          #####
######################################################

HarmonizationDatabases <- function (CorrespondenceTable){

  for (i in 1:nrow(CorrespondenceTable)){
    #Load neccessary objects
    #i<-1
    data_name <- CorrespondenceTable[i,"File_name_to_load"]
    dataset_NAMECOLUMN <- CorrespondenceTable[i,"FRIAS_name"]
    dataset_NAMECOLUMN
    dataset_SourceData <- gsub(" ", "_", CorrespondenceTable[i, "FRIAS_name"])
    data_name
    dat <- read.xlsx(data_name)
    dat2 <- dat[,1] #This is to save original species name
    colnames(dat) <- colnames(dat) #This is to save column names
    colnames(dat) <- gsub("^\\s+|\\s+$", "", colnames(dat))
    dat <- dat %>%
      dplyr::mutate(across(where(is.character), ~ gsub(";", ",", .)))
    head(dat)
    # +-----------------------------------------------------------+
    # |Harmonization of Column Names                              |
    # +-----------------------------------------------------------+
    all_column_names <- vector()

    #Column:Scientific_Name
    if (!is.na(CorrespondenceTable[i,"Column_scientificName"]) & CorrespondenceTable[i,"Column_scientificName"]!=""){
      col_scientific_names <- CorrespondenceTable[i,"Column_scientificName"]
      if (is.na(col_scientific_names)) stop(paste("Column with taxon names not found in",CorrespondenceTable[i,"Dataset_FRIAS_name"],"file!"))
      colnames(dat)[colnames(dat)==col_scientific_names] <- "Scientific_Name"
      all_column_names <- c(all_column_names,"Scientific_Name")
    }
    #Column:Kingdom
    if (!is.na(CorrespondenceTable[i,"Column_kingdom"]) & CorrespondenceTable[i,"Column_kingdom"]!=""){
      col_kingdom <- CorrespondenceTable[i,"Column_kingdom"]
      colnames(dat)[colnames(dat)==col_kingdom] <- "Kingdom"
      all_column_names <- c(all_column_names,"Kingdom")
    }
    #Column:Order
    if (!is.na(CorrespondenceTable[i,"Column_Order"]) & CorrespondenceTable[i,"Column_Order"]!=""){
      col_order <- CorrespondenceTable[i,"Column_Order"]
      colnames(dat)[colnames(dat)==col_order] <- "Order"
      all_column_names <- c(all_column_names,"Order")
      dat$Order <- tolower(dat$Order)
    }
    #Column:Family
    if (!is.na(CorrespondenceTable[i,"Column_Family"]) & CorrespondenceTable[i,"Column_Family"]!=""){
      col_family <- CorrespondenceTable[i,"Column_Family"]
      colnames(dat)[colnames(dat)==col_family] <- "Family"
      all_column_names <- c(all_column_names,"Family")
      dat$family <- tolower(dat$Family)
    }
    #Column:Phylum
    if (!is.na(CorrespondenceTable[i,"Column_Phylum"]) & CorrespondenceTable[i,"Column_Phylum"]!=""){
      col_Phylum <- CorrespondenceTable[i,"Column_Phylum"]
      colnames(dat)[colnames(dat)==col_Phylum] <- "Phylum"
      all_column_names <- c(all_column_names,"Phylum")
      dat$Phylum <- tolower(dat$Phylum)
    }
    #Column:Class
    if (!is.na(CorrespondenceTable[i,"Column_Class"]) & CorrespondenceTable[i,"Column_Class"]!=""){
      col_Class <- CorrespondenceTable[i,"Column_Class"]
      colnames(dat)[colnames(dat)==col_Class] <- "Class"
      all_column_names <- c(all_column_names,"Class")
      dat$Class <- tolower(dat$Class)
    }

    #Column:Group
    if (!is.na(CorrespondenceTable[i,"Column_Group"]) & CorrespondenceTable[i,"Column_Group"]!=""){
      col_Group <- CorrespondenceTable[i,"Column_Group"]
      colnames(dat)[colnames(dat)==col_Group] <- "Group"
      all_column_names <- c(all_column_names,"Group")
      dat$Group <- tolower(dat$Group)
    }
    #Column:FunctionalGroup
    if (!is.na(CorrespondenceTable[i,"Column_FunctionalGroup"]) & CorrespondenceTable[i,"Column_FunctionalGroup"]!=""){
      col_FunctionalGroup <- CorrespondenceTable[i,"Column_FunctionalGroup"]
      colnames(dat)[colnames(dat)==col_FunctionalGroup] <- "FunctionalGroup"
      all_column_names <- c(all_column_names,"FunctionalGroup")
      dat$FunctionalGroup <- tolower(dat$FunctionalGroup)
    }

    #Column: ReportYear
    if (!is.na(CorrespondenceTable[i,"Column_ReportYear"]) & CorrespondenceTable[i,"Column_ReportYear"] != ""){
      col_ReportYear <- CorrespondenceTable[i,"Column_ReportYear"]
      colnames(dat)[colnames(dat) == col_ReportYear] <- "ReportYear"
      all_column_names <- c(all_column_names, "ReportYear")
      dat$ReportYear <- sapply(dat$ReportYear, function(x) {
        if (is.na(x)) return(NA)
        nums <- as.numeric(unlist(regmatches(x, gregexpr("\\b(1[0-9]{3}|20[0-9]{2}|2100)\\b", x))))
        if (length(nums) == 0) return(NA)
        paste(nums, collapse = ", ")
      })
    }

    # Column: EarliestReport

    if (!is.na(CorrespondenceTable[i,"Column_EarliestReport"]) &
        CorrespondenceTable[i,"Column_EarliestReport"] != "") {

      col_EarliestReport <- CorrespondenceTable[i,"Column_EarliestReport"]
      colnames(dat)[colnames(dat) == col_EarliestReport] <- "EarliestReport"
      all_column_names <- c(all_column_names, "EarliestReport")

    } else {

      if ("ReportYear" %in% colnames(dat)) {

        dat <- dat %>%
          # Nos aseguramos de que sea character
          mutate(ReportYear_chr = as.character(ReportYear)) %>%

          # Guardamos el ID único para reconstruir la tabla después
          mutate(.row_id = row_number()) %>%

          # NO expandir si es NA → así NA quedará NA
          mutate(ReportYear_chr = ifelse(is.na(ReportYear_chr), NA, ReportYear_chr)) %>%

          # Dividir solo si NO es NA
          separate_rows(ReportYear_chr, sep = ",|;", convert = FALSE) %>%

          mutate(
            ReportYear_clean = trimws(ReportYear_chr),
            ReportYear_num   = suppressWarnings(as.numeric(ReportYear_clean))
          ) %>%

          # Volver a una fila por registro original
          group_by(.row_id) %>%
          dplyr::summarise(
            across(everything(), ~ first(.)),
            EarliestReport = if (all(is.na(ReportYear_num))) NA_real_ else min(ReportYear_num, na.rm = TRUE),
            .groups = "drop"
          ) %>%

          # Quitar columnas temporales
          select(-ReportYear_chr, -ReportYear_clean, -ReportYear_num, -.row_id)

        all_column_names <- c(all_column_names, "EarliestReport")
      }
    }

    #Column:NativeRange
    if (!is.na(CorrespondenceTable[i,"Column_NativeRange"]) & CorrespondenceTable[i,"Column_NativeRange"]!=""){
      col_NativeRange <- CorrespondenceTable[i,"Column_NativeRange"]
      colnames(dat)[colnames(dat)==col_NativeRange] <- "NativeRange"
      all_column_names <- c(all_column_names,"NativeRange")
      dat$NativeRange <- tolower(dat$NativeRange)
    }
    #Column:RecipientRange
    if (!is.na(CorrespondenceTable[i,"Column_RecipientRange"]) & CorrespondenceTable[i,"Column_RecipientRange"]!=""){
      col_RecipientRange <- CorrespondenceTable[i,"Column_RecipientRange"]
      colnames(dat)[colnames(dat)==col_RecipientRange] <- "RecipientRange"
      all_column_names <- c(all_column_names,"RecipientRange")
    }
    #Column:RecipientRangeISO3
    if (!is.na(CorrespondenceTable[i,"Column_RecipientRangeISO3"]) & CorrespondenceTable[i,"Column_RecipientRangeISO3"]!=""){
      col_iso3list <- CorrespondenceTable[i,"Column_RecipientRangeISO3"]
      colnames(dat)[colnames(dat)==col_iso3list] <- "RecipientRangeISO3"
      all_column_names <- c(all_column_names,"RecipientRangeISO3")
    }
    #Column:establishmentMeans
    if (!is.na(CorrespondenceTable[i,"Column_establishmentMeans"]) & CorrespondenceTable[i,"Column_establishmentMeans"]!=""){
      col_establishmentMeans <- CorrespondenceTable[i,"Column_establishmentMeans"]
      colnames(dat)[colnames(dat)==col_establishmentMeans] <- "establishmentMeans"
      all_column_names <- c(all_column_names,"establishmentMeans")
      dat$establishmentMeans <- tolower(dat$establishmentMeans)
    }
    #Column:pathway
    if (!is.na(CorrespondenceTable[i,"Column_pathway"]) & CorrespondenceTable[i,"Column_pathway"]!=""){
      col_pathway <- CorrespondenceTable[i,"Column_pathway"]
      colnames(dat)[colnames(dat)==col_pathway] <- "pathway"
      all_column_names <- c(all_column_names,"pathway")
      dat$pathway <- tolower(dat$pathway)
      dat$pathway <- str_squish(dat$pathway)   # <- aquí conserva espacios simples
      dat$pathway <- sub(":.*", "", dat$pathway)
    }

    #Column:AffectedTaxa
    if (!is.na(CorrespondenceTable[i,"Column_AffectedTaxa"]) & CorrespondenceTable[i,"Column_AffectedTaxa"]!=""){
      col_AffectedTaxa <- CorrespondenceTable[i,"Column_AffectedTaxa"]
      colnames(dat)[colnames(dat)==col_AffectedTaxa] <- "AffectedTaxa"
      all_column_names <- c(all_column_names,"AffectedTaxa")
      dat$AffectedTaxa <- tolower(dat$AffectedTaxa)
    }
    ###############################################
    ######## STANDARDIZE EICAT IMPACT #############
    ###############################################
    equivs <- read_excel("TablesToStandardize/Table S2.xlsx", sheet = "9.1 EICAT Impact", col_names = TRUE) %>%
      mutate(
        Database = Database,
        OriginalCategories = tolower(trimws(OriginalCategories)),
        StandardizedCategoriesEICAT = tolower(trimws(StandardizedCategoriesEICAT))
      )

    # Column: EICATImpact
    if (!is.na(CorrespondenceTable[i, "Column_EICATImpact"]) &&
        CorrespondenceTable[i, "Column_EICATImpact"] != "") {
      col_EICATImpact <- CorrespondenceTable[i, "Column_EICATImpact"]
      colnames(dat)[colnames(dat) == col_EICATImpact] <- "EICATImpact"
      all_column_names <- c(all_column_names, "EICATImpact")
      data_name <- CorrespondenceTable[i, "FRIAS_name"]
      dat$EICATImpact <- as.character(dat$EICATImpact)
      dat_match <- trimws(str_to_lower(dat$EICATImpact))
      equivs_filtered <- equivs %>%
        filter(Database == data_name) %>%
        mutate(
          OriginalCategories = trimws(str_to_lower(as.character(OriginalCategories))),
          StandardizedCategoriesEICAT = as.character(StandardizedCategoriesEICAT)
        )
      if (nrow(equivs_filtered) > 0) {
        match_idx <- match(dat_match, equivs_filtered$OriginalCategories)
        replacements <- equivs_filtered$StandardizedCategoriesEICAT[match_idx]
        non_matching <- dat %>%
          filter(
            is.na(replacements) &
              !is.na(EICATImpact) &
              trimws(EICATImpact) != ""
          )
        if (nrow(non_matching) > 0) {
          filename <- paste0("OutputFiles/Check/NA_EICATImpact_", data_name, ".xlsx")
          write_xlsx(non_matching, filename)
        }
        dat$EICATImpact[!is.na(replacements)] <- replacements[!is.na(replacements)]
        dat$EICATImpact[is.na(replacements)] <- NA
      }
      dat$EICATImpact <- trimws(str_to_lower(dat$EICATImpact))
    }


    #################################################################################
    #Column:Mechanisms
    if (!is.na(CorrespondenceTable[i,"Column_Mechanisms"]) & CorrespondenceTable[i,"Column_Mechanisms"]!=""){
      col_Mechanisms <- CorrespondenceTable[i,"Column_Mechanisms"]
      colnames(dat)[colnames(dat)==col_Mechanisms] <- "Mechanisms"
      all_column_names <- c(all_column_names,"Mechanisms")
      dat$Mechanisms <- tolower(dat$Mechanisms)
    }
    #Column:EICAT_by_Mechanism
    if (!is.na(CorrespondenceTable[i,"Column_EICAT_by_Mechanism"]) & CorrespondenceTable[i,"Column_EICAT_by_Mechanism"]!=""){
      col_EICAT_by_Mechanism <- CorrespondenceTable[i,"Column_EICAT_by_Mechanism"]
      colnames(dat)[colnames(dat)==col_EICAT_by_Mechanism] <- "EICAT_by_Mechanism"
      all_column_names <- c(all_column_names,"EICAT_by_Mechanism")
      dat$EICAT_by_Mechanism <- tolower(dat$EICAT_by_Mechanism)
    }
    #Column:Habitat
    if (!is.na(CorrespondenceTable[i,"Column_Habitat"]) & CorrespondenceTable[i,"Column_Habitat"]!=""){
      col_Habitat <- CorrespondenceTable[i,"Column_Habitat"]
      colnames(dat)[colnames(dat)==col_Habitat] <- "Habitat"
      all_column_names <- c(all_column_names,"Habitat")
      dat$Habitat <- tolower(dat$Habitat)
      dat$Habitat <- gsub(",", "-", dat$Habitat)
    }

    #Column:AcceptedNameGBIF
    if (!is.na(CorrespondenceTable[i, "Column_AcceptedNameGBIF"]) & CorrespondenceTable[i, "Column_AcceptedNameGBIF"] != "") {
      col_acceptednamegbif <- CorrespondenceTable[i, "Column_AcceptedNameGBIF"]
      colnames(dat)[colnames(dat) == col_acceptednamegbif] <- "AcceptedNameGBIF"
      all_column_names <- c(all_column_names, "AcceptedNameGBIF")

      if (col_acceptednamegbif != "AcceptedNameGBIF") {
        nombre <- as.character(dat$AcceptedNameGBIF)
        nombreaceptadogbif <- character(length(nombre))
        for (j in seq_along(nombre)) {
          especie <- nombre[j]
          message("Obtaining GBIF accepted name to: ", especie, " (", j, "/", length(nombre), ")")
          tryCatch({
            res <- name_backbone_checklist(especie)
            nombreaceptadogbif[j] <- res$canonicalName
          }, error = function(e) {
            message("Not accepted name found to: ", especie, ": ", e$message)
            nombreaceptadogbif[j] <- NA
          })
          Sys.sleep(0.01)
        }
        dat$AcceptedNameGBIF <- nombreaceptadogbif
      }
    }



    # +-----------------------------------------------------------+
    # |Final Dataset (1/2)                                        |
    # +-----------------------------------------------------------+
    dat_out <- dat[ , names(dat) %in% all_column_names]
    dat_out<- as.data.frame(dat_out)
    names(dat_out)
    if (ncol(dat_out) == 1) {
      colnames(dat_out) <- "AcceptedNameGBIF"
    }
    dat_out[dat_out=="Null"] <- ""
    dat_out[is.na(dat_out)] <- ""

    # Column:ID_GBIF
    dat_out$ID_GBIF <- NA
    total <- nrow(dat_out)
    for (i in seq_len(total)) {
      nombre_especie <- dat_out$AcceptedNameGBIF[i]
      message(sprintf("Obtaining GBIF ID of species (%d/%d): %s", i, total, nombre_especie))
      backboneGBIF <- name_backbone_checklist(name = nombre_especie)
        if (!is.null(backboneGBIF) && !is.null(backboneGBIF$speciesKey) && length(backboneGBIF$speciesKey) > 0) {
          dat_out$ID_GBIF[i] <- backboneGBIF$speciesKey[1]
        }
      Sys.sleep(0.01)
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

    #Replace the string "NA" to empty cells
    cols <- setdiff(names(dat_final), "Source_Data")
    dat_final[cols] <- lapply(dat_final[cols], function(x) {
      x[x == "" | x == "NA"] <- NA
      x
    })

    ##################
    ##     ORDER    ##
    ##    columns   ##
    ##################
    desired_order <- c("AcceptedNameGBIF", "OriginalNameDB", "ID_GBIF",
                       "Kingdom", "Order", "Family", "Phylum","Class", "establishmentMeans",
                       "pathway", "FunctionalGroup", "Group",
                       "NativeRange","RecipientRange", "RecipientRangeISO3",
                       "AffectedTaxa", "EICATImpact", "Mechanisms","EICAT_by_Mechanism",
                       "Habitat",
                       "ReportYear", "EarliestReport","Source_Data")

    #In case one column is not in the database, we add it with empty rows
    missing_columns <- setdiff(desired_order, names(dat_final))
    dat_final[missing_columns] <- NA
    cols <- setdiff(names(dat_final), "Source_Data")
    dat_final[cols] <- lapply(dat_final[cols], function(x) {
      x[x == "" | x == "NA" ] <- NA
      x
    })
    #Reorder columns in desired way
    dat_final <- dat_final[desired_order]
    head(dat_final)

    ##################
    ##     REMOVE   ##
    ##    ACCENTS   ##
    ##################
    no_accents <- function(data) {
      data[] <- lapply(data, function(col) {
        if (is.character(col)) {
          iconv(col, from = "UTF-8", to = "ASCII//TRANSLIT")
        } else {
          col
        }
      })
      return(data)
    }
    dat_final <- no_accents(dat_final)


    # +-----------------------------------------------------------+
    # |Save Species with not found Accepted Name in GBIF          |
    # |or ID in Check Subfolder                                   |
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
                str_detect(OriginalNameDB, regex("sp\\.|spp\\.|ssp\\.|sp.; ", ignore_case = TRUE))
            )
        )
      )
    final_dataset$AcceptedNameGBIF <- gsub("[\\.;:]+", "",  final_dataset$AcceptedNameGBIF)
    final_dataset$OriginalNameDB <- gsub("[\\.;:]+", "", final_dataset$OriginalNameDB)

    # +-----------------------------------------------------------+
    # |Final Dataset (2/2)                                        |
    # +-----------------------------------------------------------+
    Source_Data <- unique(final_dataset$Source_Data)
    write.table(final_dataset,
                file.path("OutputFiles", "Intermediate", paste0("step1_prepared_", Source_Data, ".csv")),
                sep=";",
                row.names=FALSE,
                col.names=TRUE,
                quote=FALSE)
    cat(paste0("step1_prepared_", Source_Data, " saved ", "\n"))
  }
}

