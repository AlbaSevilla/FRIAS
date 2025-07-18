FRIAS_DescriptiveVariables <- function(){
  ############################################
  ################# 1 ########################
  ############################################
  if (!file.exists("OutputFiles")){
    dir.create(file.path("OutputFiles/Final"), recursive=TRUE)
  }

  ############################################
  ############## 2 ###########################
  ############################################
  dat <- read_excel("OutputFiles/Intermediate/Step6_CorrectedAcceptedNameGBIF_Masterlist.xlsx")

  cols_no_desglosar <- c("OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF",
                         "Kingdom", "Order", "Family", "Group", "FunctionalGroup")


  df_separado <- dat %>%
    separate_rows(invaded_country_list, sep = ";")
  df_separado2 <- df_separado %>%
    separate_rows(invaded_country_list, sep = ",")
  unique(df_separado2$invaded_country_list)



  Dataset <- data.frame(
    Variables_Names = names(dat),
    Format_Variables = sapply(dat, class),
    Unique_registers = sapply(names(dat), function(col) {
      if (col %in% cols_no_desglosar) {
        length(unique(dat[[col]]))
      } else {
        valores <- unlist(strsplit(as.character(dat[[col]]), "[,;]\\s*"))
        valores <- valores[valores != ""]
        length(unique(valores))
      }
    }),
    row.names = NULL
  )

  Dataset$Variable_Range <- NA
  idx <- which(Dataset$Variables_Names == "DateList")
  if (length(idx) == 1 && "DateList" %in% names(dat)) {
    valores <- unlist(strsplit(as.character(dat$DateList), "[,;]\\s*"))
    valores <- valores[valores != ""]
    fechas <- suppressWarnings(as.numeric(valores))
    fechas <- fechas[!is.na(fechas)]

    # Calcula el rango si hay años válidos
    if (length(fechas) > 0) {
      rango <- paste0("De ", min(fechas), " a ", max(fechas))
    } else {
      rango <- NA
    }
    Dataset$Variable_Range[idx] <- rango
  }

  write.xlsx(Dataset, "OutputFiles/Final/FRIAS_DescriptiveVariables.xlsx")
  cat("FRIAS_DescriptiveVariables.xlsx is created at OutputFiles/Final")
}
