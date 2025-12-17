mantintsilili_et_al_2024_Download_and_Depurate <- function(){
  #download
  url <- "https://ars.els-cdn.com/content/image/1-s2.0-S2351989424003706-mmc7.xlsx"
  destfile <- "InputFiles/originaldatabase_mantintsilili_et_al_2024.xlsx"
  download.file(url, destfile, mode = "wb")

  #depurate
  dat <- read.xlsx(destfile)
  head(dat)
  colnames(dat) <- dat[1,]
  dat <- dat[-1,]
  dat <- dat %>%
    mutate(
      EICAT_by_mechanism = if_else(
        !is.na(Impact) & Impact != "" &
          !is.na(Mechanism) & Mechanism != "",
        str_c(Impact, Mechanism, sep = "-"),
        NA_character_
      )
    )

  dat_noduplicates <- noduplicates(dat, "Scientific name")
  dataset <- dat_noduplicates
  names <- dataset$`Scientific name`
  accep_names <- name_backbone_checklist(names)$canonicalName
  dat_hab <- check_habitat(accep_names, dataset)
  dat_hab_frw <- dat_hab %>% filter(grepl("FRESHWATER", Habitat))
  dat_hab_frw$RecipientRange <- "South Africa"
  dat_hab_frw$Establishment <- "Introduced"
  dat_hab_frw$kingdom <- name_backbone_checklist(dat_hab_frw$AcceptedNameGBIF)$kingdom
  dat_hab_frw$order <- name_backbone_checklist(dat_hab_frw$AcceptedNameGBIF)$order
  dat_hab_frw$family <- name_backbone_checklist(dat_hab_frw$AcceptedNameGBIF)$family
  dat_hab_frw$phylum <- name_backbone_checklist(dat_hab_frw$AcceptedNameGBIF)$phylum
  dat_hab_frw$class <- name_backbone_checklist(dat_hab_frw$AcceptedNameGBIF)$class
  dat_hab_frw <- dat_hab_frw %>% select(-Citation)

  #save
  write.xlsx(dat_hab_frw, "InputFiles/freshwatersubset_mantintsilili_et_al_2024.xlsx")
}
