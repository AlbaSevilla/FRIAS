prestes_et_al_2024_Download_and_Depurate <- function(){
  #Depurate
  destfile <- "InputFiles/originaldatabase_prestes_et_al_2024.xlsx"
  dat <- read.xlsx(destfile)
  dat <- dat[,-c(1:6)]
  dat <- dat %>%
    separate_rows(Species.name, sep=",") %>%
    mutate(Species.name = trimws(Species.name))

  eicat_mechanisms <- c(
    "1"  = "competition",
    "2"  = "predation",
    "3"  = "hybridisation",
    "4"  = "disease transmission",
    "5"  = "parasitism",
    "6"  = "intoxication/toxicity",
    "7"  = "biofouling or other direct physical disturbance",
    "8"  = "grazing/herbivory/navigation",
    "9"  = "chemical impact on the ecosystem",
    "10" = "physical impact on the ecosystem",
    "11" = "structural impact on the ecosystem",
    "12" = "indirect impacts through interaction with other species"
  )

  dat <- dat %>%
    mutate(
      EICAT.impacts = if_else(
        is.na(EICAT.impacts),
        NA_character_,
        str_split(EICAT.impacts, ",") |>
          lapply(str_trim) |>
          lapply(\(x) eicat_mechanisms[x]) |>
          lapply(paste, collapse = "; ") |>
          unlist()
      )
    )
  dat <- dat %>%
    mutate(
      EICAT_by_mechanism = if_else(
        !is.na(X34) & X34 != "" &
          !is.na(EICAT.impacts) & EICAT.impacts != "",
        str_c(X34, EICAT.impacts, sep = "-"),
        NA_character_
      )
    )
  dat$Species.name <- iconv(dat$Species.name, from = "UTF-8", to = "ASCII//TRANSLIT")

  dat_nodup <- noduplicates(dat, "Species.name")
  names <- dat_nodup$Species.name
  dat_nodup$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName
  #save
  write.xlsx(dat_nodup, "InputFiles/freshwatersubset_prestes_et_al_2024.xlsx")
}
