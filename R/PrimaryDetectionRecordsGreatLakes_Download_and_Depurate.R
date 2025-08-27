PrimaryDetectionRecordsGreatLakes_Download_and_Depurate <- function(){
  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  #a mano
  #url <- "https://datadryad.org/downloads/file_stream/421906"

  #DEPURAR
  dat <- read_excel("InputFiles/Step0_OriginalDatabasePrimaryDetectionRecordsGreatLakes.xlsx", sheet=2)
  head(dat)
  dat <- dat %>%
    filter(`Population Status`=="Established")

  source(file.path("R", "noduplicates.r"))
  names(dat)
  dat <- dat %>%
    rename(Species_name = `Species  Name`)
  sum(count(unique(dat$Species_name))$freq)
  noduplicates <- noduplicates(dat, "Species_name")
  dim(noduplicates)

  #Obtenemos cuales son freshwater, como no viene el campo habitat
  source(file.path("R", "check_habitat.r"))
  species <- noduplicates$Species_name
  nombre_aceptado <- name_backbone_checklist(species)$canonicalName
  habitat_dat <- check_habitat(nombre_aceptado, noduplicates)
  #Seleccionamos las freshwater
  freshwater_dat <- habitat_dat %>%
    filter(grepl("FRESHWATER", Habitat))
  #Nos quedamos con estas, ahora colapsamos por accepted name gbif
  #133 species
  dim(freshwater_dat)

  #Fusionamos los pathways
  names(noduplicates)

  library(dplyr)
  library(stringr)

  dat_2 <- noduplicates %>%
    mutate(
      pathways = paste(
        `Potential Pathway 1 \r\n(Ballast Water)`,
        `Potential Pathway 2 \r\n(Ship Fouling)`,
        `Potential Pathway 3`,
        `Potential Pathway 4`,
        sep = ","
      ),
      pathways = gsub("NA,", "", pathways),
      pathways = gsub(",NA", "", pathways)
    ) %>%
    select(-`Potential Pathway 1 \r\n(Ballast Water)`,
           -`Potential Pathway 2 \r\n(Ship Fouling)`,
           -`Potential Pathway 3`,
           -`Potential Pathway 4`) %>%
    # Reordenar los años dentro de cada fila y crear OldestDate
    rowwise() %>%
    mutate(
      Year_sorted = str_split(`Year of First Report`, ",")[[1]] %>%
        trimws() %>%
        as.numeric() %>%
        sort() %>%
        paste(collapse = ", "),
      OldestDate = min(as.numeric(str_split(`Year_sorted`, ",")[[1]]), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # Reemplazar Year_of_First_Report con la versión ordenada
    mutate(`Year of First Report` = Year_sorted) %>%
    select(-Year_sorted) %>%
    arrange(OldestDate)


  #Save the dataset
  write.xlsx(dat_2, "InputFiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_PrimaryDetectionRecordsGreatLakes.xlsx")
  cat("Step0_OriginalDatabaseFreshwaterNODUPLICATES_PrimaryDetectionRecordsGreatLakes.xlsx saved")
}
