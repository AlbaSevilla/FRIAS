dehnenschmutz_et_al_2022_Download_and_Depurate <- function(){
  #Download
  url <- "https://neobiota.pensoft.net/article/89448/download/suppl/32/"
  destfile <- "InputFiles/originaldatabase_dehnenschmutz_et_al_2022.csv"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dat <- read.csv("InputFiles/originaldatabase_dehnenschmutz_et_al_2022.csv")
  unique(dat$Habitat)
  freshwater_habitats <- c(
    "Littoral habitat",
    "Standing Open Waters/Canals",
    "Rivers and Streams",
    "Fen, Marsh, Swamp",
    "Bogs"
  )
  dat2 <- dat %>% filter(Habitat %in% freshwater_habitats)

  #save
  write.xlsx(dat2, "InputFiles/freshwatersubset_dehnenschmutz_et_al_2022.xlsx")
}


