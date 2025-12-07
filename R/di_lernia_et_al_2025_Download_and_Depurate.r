di_lernia_et_al_2025_Download_and_Depurate <- function(){
  #Download
  url1 <- "https://neobiota.pensoft.net/article/146280/download/csv/22/"
  destfile1 <- "InputFiles/originaldatabase_di_lernia_et_al_2025_1.csv"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url1, destfile1, mode = "wb")
  #2
  url2 <- "https://neobiota.pensoft.net/article/146280/download/csv/23/"
  destfile2 <- "InputFiles/originaldatabase_di_lernia_et_al_2025_2.csv"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url2, destfile2, mode = "wb")

  #Depurate
  file1 <- read.csv2(destfile1)
  file2 <- read.csv2(destfile2)
  merged <- merge(file1, file2, by="IAAP")
  merged$Habitat <- "Freshwater"

  #Save
  write.xlsx(merged, "InputFiles/freshwatersubset_di_lernia_et_al_2025.xlsx")
}
