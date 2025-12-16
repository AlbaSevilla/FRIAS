##################################################################
###            F R I A S     W O R K F L O W                 #####
###            Packages_download() function                  #####
##################################################################


Packages_download <- function() {
  necessary_packages <- c("rgbif","tidyverse","viridis","openxlsx","data.table","readxl","dplyr","tidyr",
    "writexl","sqldf","officer","stringi","patchwork","rglobi","rfishbase","readr",
    "DescTools","lubridate","stringr","pdftools","httr","qpdf","zip","htmltools",
    "archive","plyr","jsonlite","purrr","pbapply","hrbrthemes","networkD3","circlize",
    "parallel","rvest","progress","countrycode","ggplot2","pROC","caret","plotly",
    "RColorBrewer","pheatmap","stringdist","taxize","future.apply","rnaturalearth",
    "rnaturalearthdata","sf","paletteer","vcd","vcdExtra","ggmosaic","ggsci", "rlang",
    "treemapify", "forcats", "palette", "plotrix", "patchwork",
    "xml2", "worrms", "natserv", "tibble", "ggpattern")

  for(package in necessary_packages){
    if (!requireNamespace(package, quietly = TRUE)) {
      message(paste("Installing and uploading :", package))
      install.packages(package)
    } else {
      message(paste("Already installed and uploaded:", package))
    }
    library(package, character.only = TRUE)
  }
}
