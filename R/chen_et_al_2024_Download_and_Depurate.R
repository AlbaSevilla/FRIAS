chen_et_al_2024_Download_and_Depurate <- function(){
  #Download
  url <- "https://fred.igb-berlin.de/data/file_download/2004"
  destfile <- "InputFiles/originaldatabase_chen_et_al_2024.xlsx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dat <- read.xlsx(destfile)
  #Cleaning impact field
  dat <- dat %>%
    mutate(
      `EICAT.Category:.DD.-.Data.deficient.MC.-.Minimal.concern.MN.-.Minor.MO.-.Moderate.MR.-.Major.MV.-.Massive` =
        tolower(sub(" .*", "", as.character(
          `EICAT.Category:.DD.-.Data.deficient.MC.-.Minimal.concern.MN.-.Minor.MO.-.Moderate.MR.-.Major.MV.-.Massive`
        )))
    )
  dat <- dat %>%
    mutate(`EICAT.Criteria.-.impact.mechanism:.(1).Competition.(2).Predation.(3).Hybridisation.(4).Transmission.of.diseases.to.native.species.(5).Parasitism.(6).Poisoning/.toxicity.(7).Bio-fouling.or.other.direct.physical.disturbance.(8).Grazing/herbivory/browsing.(9).Chemical.Impact.on.ecosystem.(10).Physical.Impact.on.ecosystem.(11).Structural.Impact.on.ecosystem.(12).Indirect.impacts.through.interactions.with.other.species` = str_remove_all(as.character(`EICAT.Criteria.-.impact.mechanism:.(1).Competition.(2).Predation.(3).Hybridisation.(4).Transmission.of.diseases.to.native.species.(5).Parasitism.(6).Poisoning/.toxicity.(7).Bio-fouling.or.other.direct.physical.disturbance.(8).Grazing/herbivory/browsing.(9).Chemical.Impact.on.ecosystem.(10).Physical.Impact.on.ecosystem.(11).Structural.Impact.on.ecosystem.(12).Indirect.impacts.through.interactions.with.other.species`), "\\(\\d+\\)") %>% str_squish())
  dat <- dat %>%
    mutate(
      EICAT_by_mechanism = if_else(
        !is.na(`EICAT.Category:.DD.-.Data.deficient.MC.-.Minimal.concern.MN.-.Minor.MO.-.Moderate.MR.-.Major.MV.-.Massive`) & !is.na(`EICAT.Criteria.-.impact.mechanism:.(1).Competition.(2).Predation.(3).Hybridisation.(4).Transmission.of.diseases.to.native.species.(5).Parasitism.(6).Poisoning/.toxicity.(7).Bio-fouling.or.other.direct.physical.disturbance.(8).Grazing/herbivory/browsing.(9).Chemical.Impact.on.ecosystem.(10).Physical.Impact.on.ecosystem.(11).Structural.Impact.on.ecosystem.(12).Indirect.impacts.through.interactions.with.other.species`),
        str_c(
          `EICAT.Category:.DD.-.Data.deficient.MC.-.Minimal.concern.MN.-.Minor.MO.-.Moderate.MR.-.Major.MV.-.Massive`,
          `EICAT.Criteria.-.impact.mechanism:.(1).Competition.(2).Predation.(3).Hybridisation.(4).Transmission.of.diseases.to.native.species.(5).Parasitism.(6).Poisoning/.toxicity.(7).Bio-fouling.or.other.direct.physical.disturbance.(8).Grazing/herbivory/browsing.(9).Chemical.Impact.on.ecosystem.(10).Physical.Impact.on.ecosystem.(11).Structural.Impact.on.ecosystem.(12).Indirect.impacts.through.interactions.with.other.species` ,
          sep = "-"
        ),
        NA_character_
      )
    )
  #NO dup
  dat_nodup <- noduplicates(dat, "Scientific.name.of.alien.species")
  dim(dat_nodup)
  dat_nodup <- dat_nodup[,-c(12,13,14,15)]

  dat_nodup$Habitat <- "Freshwater"
  names <- dat_nodup$Scientific.name.of.alien.species
  dat_nodup$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName

  #Save
  write.xlsx(dat_nodup, "InputFiles/freshwatersubset_chen_et_al_2024.xlsx")
}
