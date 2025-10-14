Downloads_and_Dates_Databases <- function(){

  #First we read the Harmonization databases file with information about name, path and download function associated with each database
  Harmonization_Table <- read_excel("TablesToStandardize/Harmonization_Databases.xlsx")

  #Here we check the hour and date of each database
  dates_hours <- lapply(Harmonization_Table$File_name_to_load, function(file_path) {
    if (file.exists(file_path)) {
      atime <- file.info(file_path)$mtime
      list(
        File_name_to_load = file_path,
        AssociatedRFunctionToDownload = Harmonization_Table$AssociatedRFunctionToDownload[
          Harmonization_Table$File_name_to_load == file_path
        ],
        Date = format(atime, "%Y-%m-%d"),
        Hour = format(atime, "%H:%M:%S")
      )
    } else {
      list(
        File_name_to_load = file_path,
        AssociatedRFunctionToDownload = Harmonization_Table$AssociatedRFunctionToDownload[
          Harmonization_Table$File_name_to_load == file_path
        ],
        Date = NA,
        Hour = NA
      )
    }
  })
  dates_hours_df <- do.call(rbind, lapply(dates_hours, as.data.frame))
  rownames(dates_hours_df) <- NULL
  write_xlsx(dates_hours_df, "TablesToStandardize/Dates_of_use_Database.xlsx")
  dates_hours_df <- read_excel("TablesToStandardize/Dates_of_use_Database.xlsx")

  #We read the Downloads_Frequency file where we indicate the frequency of each database download, by defect we have assigned 30 days to each database
  downloads_freq <- read_excel("TablesToStandardize/Downloads_Frequency.xlsx")
  merged <- merge(dates_hours_df, downloads_freq, by.x = "File_name_to_load", by.y="Files", all.x = TRUE)

  #We calculate the days since the last download
  merged$Days_since <- as.numeric(Sys.Date() - as.Date(merged$Date))

  #We select those that exceed the established number of days
  to_download <- merged[is.na(merged$Days_since) | merged$Days_since >= merged$Frequency_days, ]
  to_download

  #We execute those functions to download that databases that exceed the established limit of days
  # Comprobar si hay filas válidas
  valid_rows <- which(!is.na(to_download$File_name_to_load))

  if (length(valid_rows) == 0) {
    cat("All databases are updated\n")
  } else {
    for (i in valid_rows) {
      script_name <- to_download$AssociatedRFunctionToDownload[i]

      if (!is.na(script_name)) {
        script_path <- file.path("R", script_name)

        if (file.exists(script_path)) {
          cat("Running scripts from files that exceed the update frequency:", script_path, "\n")

          # Cargar el script
          source(script_path, local = TRUE)

          # Obtener el nombre de la función (sin extensión)
          func_name <- sub("\\.r$", "", script_name, ignore.case = TRUE)

          # Ejecutar la función
          do.call(func_name, list())

          # Actualizar fecha/hora en merged
          merged$Date[merged$Database == to_download$Database[i]] <- as.character(Sys.Date())
          merged$Hour[merged$Database == to_download$Database[i]] <- format(Sys.time(), "%H:%M:%S")
        } else {
          cat("Script not found for :", script_path, "\n")
        }
      } else {
        cat("Script not found for : NA\n")
      }
    }
    cat("All databases are updated\n")  # Se imprime al final del loop
  }


  #We save the updated file
  dates_hours <- lapply(Harmonization_Table$File_name_to_load, function(file_path) {
    if (file.exists(file_path)) {
      atime <- file.info(file_path)$mtime
      list(
        File_name_to_load = file_path,
        AssociatedRFunctionToDownload = Harmonization_Table$AssociatedRFunctionToDownload[
          Harmonization_Table$File_name_to_load == file_path
        ],
        Date = format(atime, "%Y-%m-%d"),
        Hour = format(atime, "%H:%M:%S")
      )
    } else {
      list(
        File_name_to_load = file_path,
        AssociatedRFunctionToDownload = Harmonization_Table$AssociatedRFunctionToDownload[
          Harmonization_Table$File_name_to_load == file_path
        ],
        Date = NA,
        Hour = NA
      )
    }
  })
  dates_hours_df <- do.call(rbind, lapply(dates_hours, as.data.frame))
  write_xlsx(dates_hours_df, "TablesToStandardize/Dates_of_use_Database.xlsx")

  cat("Updated file: TablesToStandardize/Dates_of_use_Database.xlsx \n")
}
