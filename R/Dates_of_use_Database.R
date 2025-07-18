#FUNCIÓN PARA OBTENER LAS DateS DE CONSULTAS DE LAS BASES DE DATOS

Dates_of_use_Database <- function(){
  ruta_inputfiles <- file.path("Inputfiles")
  databases <- list.files(path = ruta_inputfiles, pattern = "^Step0_OriginalDatabaseFreshwaterNODUPLICATES_", full.names = TRUE)

  Dates_Hours <- lapply(databases, function(Database) {
    atime <- file.info(Database)$mtime
    list(
      Database = Database,
      Date = format(atime, "%Y-%m-%d"),
      Hour = format(atime, "%H:%M:%S")
    )
  })

  Dates_Hours_basesdatos <- do.call(rbind, lapply(Dates_Hours, as.data.frame))
  rownames(Dates_Hours_basesdatos) <- NULL

  #ELIMINAMOS TEXTO REDUNDANTE EN EL NOMBRE DE LAS BASES DE DATOS
  Dates_Hours_basesdatos$Database <- sub(
    "^.*Step0_OriginalDatabaseFreshwaterNODUPLICATES_(.*)\\.xlsx$",
    "\\1",
    Dates_Hours_basesdatos$Database
  )


  write.xlsx(Dates_Hours_basesdatos, "./TablesToStandardize/Dates_of_use_Database.xlsx")
  cat("Archivo descargado correctamente: ./TablesToStandardize/Dates_of_use_Database.xlsx", "\n")
}
