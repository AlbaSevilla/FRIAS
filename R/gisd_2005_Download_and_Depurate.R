gisd_2005_Download_and_Depurate <- function() {
  #Download
  url <- "https://www.iucngisd.org/gisd/export_xlsx_extended.php?species_codes=947,2057,2058,625,1945,1946,1903,1241,1947,1948,1401,2063,2064,2035,107,1949,1950,1951,1427,2068,1952,1953,151,1206,2040,368,981,380,520,982,1692,1812,62,537,369,2076,973,1148,60,1954,2078,1955,1956,1957,775,1959,918,1980,1992,1993,1994,1904,1960,105,1981,1268,607,606,2003,38,1764,1983,1961,1962,1995,1996,1997,1963,126,617,1788,1998,1660,544,418,2010,1999,1984,2050,774,773,1991,1964,1965,403,89,613,1966,1967,416,1968,1985,1969,80,1970,1971,2000,1986,2001,2011,2012,1987,2014,94,1537,446,99,437,1857,1988,657,103,1233,217,218,813,1323,131,1322,1261,152,725,1905,2015,2002,2016,2046,2017,1906,2018,548,542,616,1766,1933,2019,2020,1920,2004,1974,2005,2021,683,2022,135,1712,1702,449,1975,608,1943,2006,2023,1058,1927,1659,1657,1656,1658,1561,2024,1934,2031,2025,2026,1972,1973,2013,1976,113,1765,78,1311,614,2027,1312,1313,1989,1977,1978,1703,2007,2008,2009,1913,2028,1979,2122,1430,1364,618,71,1079,1908,1990,1704,561,1270,2029,2030"
  destfile <- "InputFiles/originaldatabase_gisd_2005.xlsx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  data <- read.xlsx(destfile)
  colnames(data) <- gsub(" ", ".", colnames(data))
  names(data)[names(data) == "Countries.of.impact"] <- "Countries_of_impact"
  data[] <- lapply(data, as.character)
  data[] <- lapply(data, function(col) {
    str_replace_all(col, "\\r\\n|\\n|\\r", ";")
  })
  gisd_2005_FRESHWATER <- data %>%
    filter(grepl("freshwater", System, ignore.case = TRUE))
  gisd_2005_FRESHWATER <- gisd_2005_FRESHWATER %>%
    mutate(EICAT_by_Mechanism = if_else(
      !is.na(EICAT.Category) & !is.na(Impact.mechanism) & EICAT.Category != "" & Impact.mechanism != "",
      paste(EICAT.Category, Impact.mechanism, sep = " - "),
      NA_character_
    ))
  gisd_2005_noduplicates <- noduplicates(gisd_2005_FRESHWATER, column_name_species = "Species")
  names <- gisd_2005_noduplicates$Species
  gisd_2005_noduplicates$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName

  #Save
  write.xlsx(gisd_2005_noduplicates, "./InputFiles/freshwatersubset_gisd_2005.xlsx")
}

