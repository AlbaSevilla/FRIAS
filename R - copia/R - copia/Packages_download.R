Packages_download <- function() {
  packages <- c(
    "rgbif", "tidyverse", "viridis", "openxlsx", "data.table",
    "readxl", "dplyr", "tidyr", "writexl", "sqldf", "officer",
    "stringi", "patchwork", "rglobi", "rfishbase", "readr",
    "DescTools", "lubridate", "stringr", "pdftools", "httr",
    "qpdf", "zip", "htmltools", "archive", "plyr", "jsonlite",
    "purrr", "pbapply", "hrbrthemes", "networkD3", "circlize",
    "parallel", "rvest", "progress", "countrycode", "ggplot2",
    "pROC", "caret","plotly", "RColorBrewer","pheatmap", "rglobi","stringdist",
    "taxize", "future.apply"
  )

  packages <- unique(packages)  # elimina duplicados

  to_install <- packages[!packages %in% installed.packages()[, "Package"]]
  if (length(to_install) > 0) {
    install.packages(to_install)
  }

  invisible(lapply(packages, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE))
  }))
}
