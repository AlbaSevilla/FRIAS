##################################################################
###            F R I A S     W O R K F L O W                 #####
###            Packages_download() function                  #####
##################################################################


Packages_download <- function() {
  packages <- c(
    "rgbif","tidyverse","viridis","openxlsx","data.table","readxl","dplyr","tidyr",
    "writexl","sqldf","officer","stringi","patchwork","rglobi","rfishbase","readr",
    "DescTools","lubridate","stringr","pdftools","httr","qpdf","zip","htmltools",
    "archive","plyr","jsonlite","purrr","pbapply","hrbrthemes","networkD3","circlize",
    "parallel","rvest","progress","countrycode","ggplot2","pROC","caret","plotly",
    "RColorBrewer","pheatmap","stringdist","taxize","future.apply","rnaturalearth",
    "rnaturalearthdata","sf","paletteer","vcd","vcdExtra","ggmosaic","ggsci","ggpattern", "rlang",
    "treemapify", "forcats", "palette", "plotrix", "patchwork", "rnaturalearthhires"
  )

  for(p in packages){
    if (!requireNamespace(p, quietly = TRUE)) {
      message(paste("Downloading package:", p))
      install.packages(p)
    } else {
      message(paste("Already installed and uploaded:", p))
    }
    library(p, character.only = TRUE)
  }
}
