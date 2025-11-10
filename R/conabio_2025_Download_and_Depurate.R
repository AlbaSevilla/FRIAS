conabio_2025_Download_and_Depurate <- function() {

  ######################################################
  ######### DOWNLOAD ###################################
  ######################################################
  url <- "https://enciclovida.mx/exoticas_invasoras/lista_sp_enciclovida_2022.xlsx"
  destfile <- "InputFiles/originaldatabase_conabio_2025.xlsx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")

  #############################################################
  ######## PARA OBTENER NUESTRAS ESPECIES DE INTERÉS #########
  ############################################################
  dat <- read_excel("Inputfiles/originaldatabase_conabio_2025.xlsx")
  colnames(dat) <- dat[1,]
  colnames(dat) <- gsub("[:.]", "_", colnames(dat))
  colnames(dat) <- gsub(" ", "_", colnames(dat))
  nrow(dat)

  #SIN DUPLICADOS
  source(file.path("R", "noduplicates.r"))
  conabio_2025_sinduplicados <- noduplicates(dat, "Nombre_científico")

  #HABITAT
  conabio_2025_freshwater <- conabio_2025_sinduplicados %>%
    filter(Ambiente == "Dulceacuícola") %>%
    filter(Origen =="Exótica")

  conabio_2025_freshwater2 <- data.frame(lapply(conabio_2025_freshwater, function(x) stri_trans_general(x, "Latin-ASCII")))
  # Reordenar columnas colocando 'Nombre_científico' al inicio
  colnames(conabio_2025_freshwater2) <- iconv(colnames(conabio_2025_freshwater2), from = "UTF-8", to = "ASCII//TRANSLIT")
  names(conabio_2025_freshwater2)

  write.xlsx(conabio_2025_freshwater2, "./Inputfiles/freshwatersubset_conabio_2025.xlsx")
}
