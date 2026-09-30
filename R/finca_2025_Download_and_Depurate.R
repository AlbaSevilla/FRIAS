finca_2025_Download_and_Depurate <- function(){
  #Download
  url <- "https://static-content.springer.com/esm/art%3A10.1007%2Fs10530-022-02968-3/MediaObjects/10530_2022_2968_MOESM2_ESM.xlsx"
  destfile <- "InputFiles/originaldatabase_finca_2025.xlsx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dat <- read_excel("InputFiles/originaldatabase_finca_2025.xlsx", col_names = FALSE)
  dat <- dat[-c(1:8),]
  names(dat) <- dat[1,]
  dat <- dat[-1,]
  dat
  dat[] <- lapply(dat, function(col) {
    if (is.character(col)) {
      ifelse(col == "✓", "Yes", ifelse(is.na(col), "No", col))
    } else {
      col
    }
  })
  dat <- dat %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      Invaded_country = paste(
        names(across())[!is.na(c_across()) & c_across() == "Yes"],
        collapse = ", "
      )
    ) %>%
    dplyr::ungroup()
  names(dat) <- gsub(" ", "_", names(dat))
  dat <- as.data.frame(dat)
  dataset <- dat
  names <- dataset$Species
  acept_name <- name_backbone_checklist(names)$canonicalName
  dat_act <- check_habitat(acept_name, dataset)
  dat_act
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  names(dat_fresh)[names(dat_fresh) == "Life-span"] <- "Life_span"
  names(dat_fresh)[names(dat_fresh) == "Life-form"] <- "Life_form"
  rownames(dat_fresh) <- NULL
  dat_fresh <- dat_fresh %>% select(Species, everything())

  #Save
  write.csv2(dat_fresh, "InputFiles/freshwatersubset_finca_2025.csv")
  data <- read.csv("InputFiles/freshwatersubset_finca_2025.csv", stringsAsFactors = FALSE, sep=";")
  write_xlsx(dat_fresh, "InputFiles/freshwatersubset_finca_2025.xlsx")
}
