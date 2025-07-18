CONABIO_Download_and_Depurate <- function() {

  ######################################################
  ######### DOWNLOAD ###################################
  ######################################################
  url <- "https://enciclovida.mx/exoticas_invasoras/lista_sp_enciclovida_2022.xlsx"
  destfile <- "InputFiles/Step0_OriginalDatabase_CONABIO.xlsx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  #############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  dat <- read_excel("Inputfiles/Step0_OriginalDatabase_CONABIO.xlsx")
  colnames(dat) <- dat[1,]
  colnames(dat) <- gsub("[:.]", "_", colnames(dat))
  colnames(dat) <- gsub(" ", "_", colnames(dat))
  nrow(dat)

  #SIN DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  CONABIO_sinduplicados <- noduplicates(dat, "Nombre_científico")

  #HABITAT
  CONABIO_freshwater <- CONABIO_sinduplicados %>%
    filter(Ambiente == "Dulceacuícola") %>%
    filter(Origen =="Exótica")

  CONABIO_freshwater2 <- data.frame(lapply(CONABIO_freshwater, function(x) stri_trans_general(x, "Latin-ASCII")))
  # Reordenar columnas colocando 'Nombre_científico' al inicio
  colnames(CONABIO_freshwater2) <- iconv(colnames(CONABIO_freshwater2), from = "UTF-8", to = "ASCII//TRANSLIT")
  names(CONABIO_freshwater2)

  write.xlsx(CONABIO_freshwater2, "./Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_CONABIO.xlsx")
  cat("Archivo descargado correctamente: Step0_OriginalDatabaseFreshwaterNODUPLICATES_CONABIO.xlsx", "\n")
}
