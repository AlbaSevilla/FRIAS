Downloads_and_Dates_Databases <- function(){

  #First we read the Harmonization databases file with information about name, path and download function associated with each database
  Harmonization_Table <- read_excel("TablesToStandardize/Table S1.xlsx", sheet="Databases")

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
  write_xlsx(dates_hours_df, "TablesToStandardize/Table S3.xlsx")
  dates_hours_df <- read_excel("TablesToStandardize/Table S3.xlsx")

  #We read the Downloads_Frequency file where we indicate the frequency of each database download, by defect we have assigned 30 days to each database
  downloads_freq <- read_excel("TablesToStandardize/Table S1.xlsx", sheet="Databases")
  merged <- merge(dates_hours_df, downloads_freq, by.x = "File_name_to_load", by.y="File_name_to_load", all.x = TRUE)

  #We calculate the days since the last download
  merged$Days_since <- as.numeric(Sys.Date() - as.Date(merged$Date))

  #A message for databases that have to be downloaded with manual download
  cat("Databases that need to be manually downloaded for data extraction: " , "\n ")
  for (i in 1:nrow(merged)) {
    total <- nrow(merged)
    if (merged$Frequency_days[i] == "-") {
      name_database <- merged$Title[i]
      message(paste(i, "/", total, " ",name_database, "\n"))
    }
  }

  #We select those that exceed the established number of days
  to_download <- merged[is.na(merged$Days_since) | merged$Days_since >= merged$Frequency_days, ]
  to_download

  #We execute those functions to download that databases that exceed the established limit of days
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
          source(script_path, local = TRUE)
          func_name <- sub("\\.r$", "", script_name, ignore.case = TRUE)
          do.call(func_name, list())
          merged$Date[merged$Database == to_download$Database[i]] <- as.character(Sys.Date())
          merged$Hour[merged$Database == to_download$Database[i]] <- format(Sys.time(), "%H:%M:%S")
        } else {
          cat("Script not found for :", script_path, "\n")
        }
      } else {
        cat("Script not found for : NA\n")
      }
    }
    cat("All databases are updated\n")
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
  write_xlsx(dates_hours_df, "TablesToStandardize/Table S3.xlsx")
}
