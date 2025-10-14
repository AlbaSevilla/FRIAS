CONABIO_2025_Download_and_Depurate <- function() {

  ######################################################
  ######### DOWNLOAD ###################################
  ######################################################
  url <- "https://enciclovida.mx/exoticas_invasoras/lista_sp_enciclovida_2022.xlsx"
  destfile <- "InputFiles/Step0_OriginalDatabase_CONABIO_2025.xlsx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  #############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  dat <- read_excel("Inputfiles/Step0_OriginalDatabase_CONABIO_2025.xlsx")
  colnames(dat) <- dat[1,]
  colnames(dat) <- gsub("[:.]", "_", colnames(dat))
  colnames(dat) <- gsub(" ", "_", colnames(dat))
  nrow(dat)

  #SIN DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  CONABIO_2025_sinduplicados <- noduplicates(dat, "Nombre_científico")

  #HABITAT
  CONABIO_2025_freshwater <- CONABIO_2025_sinduplicados %>%
    filter(Ambiente == "Dulceacuícola") %>%
    filter(Origen =="Exótica")

  CONABIO_2025_freshwater2 <- data.frame(lapply(CONABIO_2025_freshwater, function(x) stri_trans_general(x, "Latin-ASCII")))
  # Reordenar columnas colocando 'Nombre_científico' al inicio
  colnames(CONABIO_2025_freshwater2) <- iconv(colnames(CONABIO_2025_freshwater2), from = "UTF-8", to = "ASCII//TRANSLIT")
  names(CONABIO_2025_freshwater2)

  write.xlsx(CONABIO_2025_freshwater2, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_CONABIO_2025.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_CONABIO_2025.xlsx", "\n")
}
