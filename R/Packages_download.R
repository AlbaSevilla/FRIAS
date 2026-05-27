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
                          "rnaturalearthdata","sf","paletteer","vcd","ggmosaic","ggsci", "rlang",
                          "treemapify", "forcats", "palette", "plotrix", "patchwork",
                          "xml2", "worrms", "natserv", "tibble", "vcdExtra", "viridis")

  failed_packages <- c()

  for(package in necessary_packages){

    message(paste("Processing:", package))

    tryCatch({

      if (!requireNamespace(package, quietly = TRUE)) {
        message(paste("Installing:", package))
        install.packages(package, type = "binary")
      }

      library(package, character.only = TRUE)
      message(paste("Loaded:", package))

    }, error = function(e){

      message(paste("Error with:", package))
      failed_packages <<- c(failed_packages, package)

    })
  }

  if(length(failed_packages) > 0){
    message("===================================")
    message("Packages with errors:")
    print(failed_packages)
  } else {
    message("All packages installed and loaded successfully")
  }
}
