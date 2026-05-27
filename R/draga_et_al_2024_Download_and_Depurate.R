draga_et_al_2024_Download_and_Depurate <- function(){
  #Depurate
  destfile <- "InputFiles/originaldatabase_draga_et_al_2024.pdf"
  dat <- extract_tables(destfile)
  table <- dat[[1]]
  table <- as.data.frame(table)
  table <- table %>%
    mutate(
      # IS – invasion status
      IS = recode(IS,
                  "cas" = "casual",
                  "inv" = "invasive",
                  "nat" = "naturalized"),
      # RT – residence time
      RT = recode(RT,
                  "eph" = "ephemerophyte",
                  "neo" = "neophyte"),
      # IM – introduction mode
      IM = recode(IM,
                  "d" = "deliberate",
                  "a" = "accidental",
                  "b" = "both means"),
      # WT – water types
      WT = recode(WT,
                  "Cold" = "freshwater",
                  "Therm" = "thermal water",
                  "Both" = "freshwater and thermal water"),
      # GO – geographical origin
      GO = recode(GO,
                  "Af" = "Africa",
                  "Am" = "America",
                  "As" = "Asia",
                  "Unk" = "unknown")
    )
  table <- table %>%
    mutate(`Species / family` = str_extract(`Species / family`, "^\\S+\\s+\\S+"))
  table <- table %>%
    filter(`Species / family` != "" & !is.na(`Species / family`))
  dataset <- table
  names <- dataset$`Species / family`
  accep_names <- name_backbone_checklist(names)$canonicalName
  dat_hab <- check_habitat(accep_names, dataset)
  dat_frw <- dat_hab %>% filter(grepl("FRESHWATER", Habitat))
  dat_frw$RecipientRange <- "Poland"

  #Save
  write.xlsx(dat_frw, "InputFiles/freshwatersubset_draga_et_al_2024.xlsx")
}
