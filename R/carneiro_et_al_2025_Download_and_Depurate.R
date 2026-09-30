carneiro_et_al_2025_Download_and_Depurate <- function(){
  #Download
  url <- "https://static-content.springer.com/esm/art%3A10.1007%2Fs10530-024-03498-w/MediaObjects/10530_2024_3498_MOESM2_ESM.xlsx"
  destfile <- "InputFiles/originaldatabase_carneiro_et_al_2025.xlsx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dat <- read.xlsx(destfile)
  dat2 <- dat %>%
    filter(grepl("FRESHWATER", ENVIRONMENT_NN))
  countries_list <- dat2$ISO3
  countries_list[countries_list == "CAD"] <- "CAN"
  countries_list[countries_list == "PTG"] <- "PRT"
  nombres_paises <- countrycode(countries_list, origin = "iso3c", destination = "country.name")
  dat2$RecipientRange <- nombres_paises
  dat2$Impact.mechanism <- gsub("^[0-9]+\\s*", "", dat2$Impact.mechanism)
  dat2$Impact.mechanism[dat2$Impact.mechanism == "Structural"] <- NA
  dat2 <- dat2 %>%
    mutate(
      EICAT_by_mechanism = if_else(
        !is.na(Impact.level) & Impact.level != "" &
          !is.na(Impact.mechanism) & Impact.mechanism != "",
        str_c(Impact.level, Impact.mechanism, sep = "-"),
        NA_character_
      )
    )

  dat_nodup <- noduplicates(dat2, "SPP_NN")
  names <- dat_nodup$SPP_NN
  dat_nodup$kingdom <- name_backbone_checklist(names)$kingdom
  dat_nodup$order <- name_backbone_checklist(names)$order
  dat_nodup$phylum <- name_backbone_checklist(names)$phylum
  dat_nodup$class <- name_backbone_checklist(names)$class
  dat_nodup$family <- name_backbone_checklist(names)$family
  names <- dat_nodup$SPP_NN
  dat_nodup$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName
  dat_nodup <- dat_nodup %>%
    rename(Impact = Impact.level)
  #Save
  write.xlsx(dat_nodup, "InputFiles/freshwatersubset_carneiro_et_al_2025.xlsx")
}
