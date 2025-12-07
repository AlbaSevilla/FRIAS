conabio_2025_Download_and_Depurate <- function() {
  #Download
  url <- "https://enciclovida.mx/exoticas_invasoras/lista_sp_enciclovida_2025.xlsx"
  destfile <- "InputFiles/originaldatabase_conabio_2025.xlsx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dat <- read_excel("InputFiles/originaldatabase_conabio_2025.xlsx")
  colnames(dat) <- dat[1,]
  colnames(dat) <- gsub("[:.]", "_", colnames(dat))
  colnames(dat) <- gsub(" ", "_", colnames(dat))
  conabio_2025_noduplicates <- noduplicates(dat, "Nombre_científico")
  conabio_2025_freshwater <- conabio_2025_noduplicates %>%
    filter(Ambiente == "Dulceacuícola") %>%
    filter(Origen =="Exótica")
  conabio_2025_freshwater2 <- data.frame(lapply(conabio_2025_freshwater, function(x) stri_trans_general(x, "Latin-ASCII")))
  colnames(conabio_2025_freshwater2) <- iconv(colnames(conabio_2025_freshwater2), from = "UTF-8", to = "ASCII//TRANSLIT")
  conabio_2025_freshwater2$InvadedCountry <- 'Mexico'

  #Save
  write.xlsx(conabio_2025_freshwater2, "./InputFiles/freshwatersubset_conabio_2025.xlsx")
}
