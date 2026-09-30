gallardo_et_al_2024_Download_and_Depurate <- function(){
  #DOwnload
  url1 <- "https://static-content.springer.com/esm/art%3A10.1007%2Fs10530-024-03451-x/MediaObjects/10530_2024_3451_MOESM1_ESM.xlsx"
  url2 <- "https://static-content.springer.com/esm/art%3A10.1007%2Fs10530-024-03451-x/MediaObjects/10530_2024_3451_MOESM4_ESM.xlsx"
  url3 <- "https://link.springer.com/article/10.1007/s10530-024-03451-x/tables/3"

  destfile1 <- "InputFiles/originaldatabase_gallardo_et_al_2024_1.xlsx"
  download.file(url1, destfile1, mode = "wb")
  destfile2 <- "InputFiles/originaldatabase_gallardo_et_al_2024_2.xlsx"
  download.file(url2, destfile2, mode = "wb")
  res <- read_html(url3)
  dat3 <- res %>% html_table
  dat3 <- as.data.frame(dat3)
  write.xlsx(dat3, "InputFiles/originaldatabase_gallardo_et_al_2024_3.xlsx")

  #DEpurate
  dat1 <- read.xlsx(destfile1)
  colnames(dat1)<- dat1[7,]
  dat1 <- dat1[-c(1:7),]
  dat1$Species

  dat2 <- read.xlsx(destfile2, sheet="Data")
  colnames(dat2)<- dat2[1,]
  dat2 <- dat2[-1,]
  dat2$`INVASIVE SPECIES`

  #Obtain expanded names on dat2$INVASIVE SPECIES
  dat1_clean <- dat1 %>%
    mutate(
      genus = sub(" .*", "", Species),
      epithet = sub("^[A-Za-z]+ ", "", Species)
    )
  expand_species <- function(abbrev, dat1_table) {
    parts <- unlist(strsplit(abbrev, " "))
    abbr_genus <- gsub("\\.", "", parts[1])
    epithet <- parts[2]
    candidates <- dat1_table %>% filter(startsWith(genus, abbr_genus))
    if(nrow(candidates) == 0) return(NA)
    candidates$dist <- stringdist(tolower(candidates$epithet),
                                  tolower(epithet),
                                  method = "lv")
    best <- candidates %>% slice_min(dist, n = 1)
    return(best$Species)
  }
  dat2$Species_full <- sapply(dat2$`INVASIVE SPECIES`,
                              expand_species,
                              dat1_clean)

  merged1 <- merge(dat1, dat2, by.x="Species", by.y="Species_full")
  merged2 <- merge(merged1, dat3, by.x="Species", by.y="Scientific.name")
  merged2$RecipientRange <- "Spain"
  merged_nodup <- noduplicates(merged2, "Species")
  dataset <- merged_nodup
  names <- dataset$Species
  accep_names <- name_backbone_checklist(names)$canonicalName
  dat_hab <- check_habitat(accep_names, dataset)
  dat_frw <- dat_hab %>% filter(grepl("FRESHWATER", Habitat))
  cols_x <- grep("\\.x$", names(dat_frw), value = TRUE)
  for (col_x in cols_x) {
    col_y <- sub("\\.x$", ".y", col_x)
    base_name <- sub("\\.x$", "", col_x)  # nombre base sin .x/.y
    if (col_y %in% names(dat_frw)) {
      dat_frw[[base_name]] <- paste(dat_frw[[col_x]], dat_frw[[col_y]], sep = ", ")
      dat_frw[[base_name]] <- gsub("NA, |, NA|NA", "", dat_frw[[base_name]])
      dat_frw[[col_x]] <- NULL
      dat_frw[[col_y]] <- NULL
    }
  }
  dat_frw$`Common name` <- NULL

  #Save
  write.xlsx(dat_frw, "InputFiles/freshwatersubset_gallardo_et_al_2024.xlsx")
}
