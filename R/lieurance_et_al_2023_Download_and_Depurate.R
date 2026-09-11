lieurance_et_al_2023_Download_and_Depurate <- function(){
  #Depurate
  sheet1 <- read.xlsx("InputFiles/originaldatabase_lieurance_et_al_2023.xlsx", sheet="All Taxa Assessment")
  cols_select <- c(
    "Scientific.name",
    "Horizon.scan.team",
    "Functional.group.(primary.producer,.herbivore,.omnivore,.predator,.and.parasite)",
    "Native.range",
    "Environment.(T=terrestrial;.F=freshwater;.M=marine)",
    "Likely.pathway.of.arrival.(See.CBD.pathway.categorization)",
    "Impacts.(biodversity,.ecosystems,.economy,.or.human.health).(1-5)"
  )
  sheet1 <- sheet1[, cols_select]
  sheet1 <- sheet1 %>% filter(`Environment.(T=terrestrial;.F=freshwater;.M=marine)` == "F")
  sheet2 <- read.xlsx("InputFiles/originaldatabase_lieurance_et_al_2023.xlsx", sheet="HR Path_Impacts")
  colnames(sheet2) <- c(
    "Scientific.name",
    "Common.name",
    "Taxonomic.team",
    "Level.1.CBD.pathway",
    "Level.2.CBD.pathway",
    "Impact_score",
    "Certainty",
    "Impact.category",
    "Impact.details",
    "Mechanisms",
    "Citations"
  )
  sheet2 <- sheet2 %>% filter(grepl("Ecological", Impact.category)) %>%
    select(-Impact.details, -Citations)
  dataset <- merge(sheet1, sheet2, by = "Scientific.name", all.x = TRUE)
  dataset$RecipientRange <- "United States"
  names <- dataset$Scientific.name
  dataset$AcceptedNameGBIF <- name_backbone_checklist(names)$canonicalName
  dataset <- dataset %>%
    mutate(
      Mechanisms = str_remove_all(Mechanisms, "\\(.*?\\)") %>%
        str_squish()
    )
  dataset <- dataset %>%
    mutate(
      Impact_score = c("0"="dd","1"="mc","2"="mn","3"="mo","4"="mr","5"="mv")[as.character(Impact_score)]
    )
  dataset <- dataset %>%
    separate_rows(Mechanisms, sep=":|;") %>%
    mutate(Mechanisms = trimws(tolower(Mechanisms)))
  dataset <- dataset %>%
    mutate(
      EICAT_by_mechanism = if_else(
        !is.na(Impact_score) & Impact_score != "" &
          !is.na(Mechanisms) & Mechanisms != "",
        str_c(Impact_score, Mechanisms, sep = "-"),
        NA_character_
      )
    )
  dataset_nodup <- noduplicates(dataset, "Scientific.name")

  #Save
  write.xlsx(dataset_nodup, "InputFiles/freshwatersubset_lieurance_et_al_2023.xlsx")
}
