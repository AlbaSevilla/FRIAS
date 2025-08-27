############################################################
###         F R I A S       W O R K F L O W            #####
###             Dates_of_use() function                #####
############################################################


Dates_of_use <- function(){
  path_inputfiles <- file.path("Inputfiles")
  databases <- list.files(path = path_inputfiles, pattern = "^Step0_OriginalDatabaseFreshwaterNODUPLICATES_", full.names = TRUE)

  Dates_Hours <- lapply(databases, function(Database) {
    atime <- file.info(Database)$mtime
    list(
      Database = Database,
      Date = format(atime, "%Y-%m-%d"),
      Hour = format(atime, "%H:%M:%S")
    )
  })

  Dates_Hours_databases <- do.call(rbind, lapply(Dates_Hours, as.data.frame))
  rownames(Dates_Hours_databases) <- NULL

  #We remove redundat text in the name of databases
  Dates_Hours_databases$Database <- sub(
    "^.*Step0_OriginalDatabaseFreshwaterNODUPLICATES_(.*)\\.xlsx$",
    "\\1",
    Dates_Hours_databases$Database
  )


  write.xlsx(Dates_Hours_databases, "./TablesToStandardize/Dates_of_use.xlsx")
  cat("File Downloaded: ./TablesToStandardize/Dates_of_use.xlsx", "\n")
}
