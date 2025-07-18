NeobiotaChina_Download_and_Depurate <- function(){
  ########### DESCARGAR ####################
  ##########################################
  #A mano:
  #url <- "https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2F1755-0998.12528&file=men12528-sup-0002-TableS1.xlsx"
  url <- "https://neobiota.pensoft.net/articles.php?id=1236"
  res <- read_html(url)
  res2 <- res %>% html_table()
  res2
  cat(res)




}
