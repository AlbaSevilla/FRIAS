FRIAS_DescriptiveVariables <- function(){
  ############################################
  ############## 2 ###########################
  ############################################
  dat <- read_excel("FinalFiles/FRIAS_SpeciesList_Masterlist.xlsx")

  cols_no_desglosar <- c("OriginalNameDB", "AcceptedNameGBIF", "ID_GBIF",
                         "Kingdom", "Order", "Family", "Group", "FunctionalGroup")

  names(dat)
  df_separado <- dat %>%
    separate_rows(InvadedRange, sep = ";")
  df_separado2 <- df_separado %>%
    separate_rows(InvadedRange, sep = ",")
  unique(df_separado2$InvadedRange)



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
