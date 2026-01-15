gfid_2025_Download_and_Depurate <- function(){
  #Download
  url <- "https://github.com/IsmaSA/GFID/raw/refs/heads/master/GFID.xlsx"
  destfile <- "InputFiles/originaldatabase_gfid_2025.xlsx"
  download.file(url, destfile, mode = "wb")

  #Depurate
  GFID <- read.xlsx("InputFiles/originaldatabase_gfid_2025.xlsx")
  GFID_nodup <- noduplicates(GFID, "Taxon")
  GFID_FRW <- GFID_nodup %>%
    filter(grepl("FRESHWATER", Habitat))

  GFID_mech <- read.xlsx(
    "/home/albasevilla/Descargas/GFID.xlsx",
    sheet = "Species_spec_impacts"
  )
  GFID_mech_nodup <- noduplicates(GFID_mech, "Taxon")
  GFID_mech_FRW <- GFID_mech_nodup %>%
    filter(Taxon %in% GFID_FRW$Taxon)
  GFID_mech_FRW <- GFID_mech_FRW %>%
    filter(Impact_environmental_ecol == 1)
  GFID_mech_FRW_step1 <- GFID_mech_FRW %>%
    mutate(
      across(
        starts_with("Mech_"),
        ~ ifelse(. == "1", str_remove(cur_column(), "^Mech_"), .)
      )
    )
  GFID_mech_FRW_step2 <- GFID_mech_FRW_step1 %>%
    rowwise() %>%
    mutate(
      Mechanisms = paste(
        c_across(starts_with("Mech_"))[
          c_across(starts_with("Mech_")) != "NA" &
            c_across(starts_with("Mech_")) != ""
        ],
        collapse = "; "
      )
    ) %>%
    ungroup()
  GFID_mech_FRW_final <- GFID_mech_FRW_step2 %>%
    select(-starts_with("Mech_")) %>%
    select(
      -Family,
      -Impact_economic,
      -Impact_environmental_ecol,
      -impact_social,
      -Source,
      -COMMENT
    ) %>%
    mutate(
      across(
        everything(),
        ~ ifelse(. == "NA", "", .)
      )
    )

  GFID_FRW_joined <- GFID_FRW %>%
    full_join(GFID_mech_FRW_final, by = "Taxon")
  GFID_FRW_joined <- as.data.frame(GFID_FRW_joined)
  GFID_FRW_joined_names <- GFID_FRW_joined$Taxon
  GFID_FRW_joined$AcceptedNameGBIF <- name_backbone_checklist(GFID_FRW_joined_names)$canonicalName

  #Save
  write.xlsx(GFID_FRW_joined, "InputFiles/freshwatersubset_gfid_2025.xlsx")
}
