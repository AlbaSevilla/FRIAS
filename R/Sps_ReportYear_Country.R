#Species Report Year x Country
Sps_ReportYear_Country <- function(){

  #Download databases with first record per country without collapse
  # Databases: EASIN, USRIIS, GIATAR, AQUANIS, FAO, fishbase, seebens 2023,
  #dyer et al 2016, bailey et al 2021, GIDIAS 2025, asiapacific alien species database 2025,
  #NISIC, elias et al 2022, ELNAIS 2025, xie et al 2000, carneiro et al 2025, draga et al 2024,
  #zamora marin et al 2023, GFID 2025

  ################################################################
  ################## 1   -     DOWNLOAD ##########################
  ################################################################
  #Easin
  url <- "https://easin.jrc.ec.europa.eu/apixg/catxg/env/FRW/skip/0/take/15000"
  res <- GET(url) # 1. Download JSON data
  json_raw <- content(res, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(json_raw, flatten = TRUE) # 2. Parse JSON content
  dataset <- as.data.frame(json_data) # Convert to a dataframe
  dataset_freshwater <- dataset[, c(
    "Name", "Status", "EASINID", "FirstIntroductionsInEU"
  )]
  dataset_freshwater <- dataset_freshwater %>%
    mutate(
      FirstIntroductionsInEU = map(
        FirstIntroductionsInEU,
        ~ if (is.null(.x) || nrow(.x) == 0) {
          tibble(
            Country = NA_character_,
            Year = NA_character_,
            Ref = NA_character_
          )
        } else {
          as_tibble(.x)
        }
      )
    ) %>%   unnest(FirstIntroductionsInEU)
  final_dataset <- dataset_freshwater %>%
    select(Name, Country, Year)
  final_dataset_EASIN <- final_dataset
  write_xlsx(final_dataset_EASIN, "InputFiles/easin.xlsx")

  ################################################################################
  # USRIIS 2022
  dat_usriis <- read.xlsx("InputFiles/originaldatabase_usriis_2022.xlsx")
  dat_usriis <- dat_usriis %>%
    filter(taxonRank %in% c("Species")) %>%
    filter(isHybrid %in% c("FALSE"))
  dat_usriis <- dat_usriis %>%
    mutate(occurrenceID = sub("^[^-]+-([^-]+)-.*$", "\\1", occurrenceID))
  dat_usriis$InvadedCountry <- "United States"
  dat_usriis <- dat_usriis %>%
    select(Name, FirstIntroductionsInEU) %>%
    unnest_wider(FirstIntroductionsInEU) %>%
    select(Name, Country, Year) %>%
    mutate(
      Country = map_chr(Country, ~ if (length(.x) == 0) NA_character_ else .x[1]),
      Year    = map_chr(Year,    ~ if (length(.x) == 0) NA_character_ else .x[1])
    )
  final_dataset_USRIIS <- dat_usriis
  write_xlsx(final_dataset_USRIIS, "InputFiles/usriis.xlsx")

  ################################################################################

  #GIATAR 2024
  #Native Ranges
  native_ranges <- read.csv("InputFiles/dataset/native ranges/all_sources_native_ranges.csv")
  native_ranges <- native_ranges[,-1]
  selected_columns <- c("genus_species", "bioregion", "DAISIE_region", "usageKey")
  native_ranges <- native_ranges[,selected_columns]
  colnames(native_ranges) <- c("ScientificName", "Bioregion", "Native_Range", "usageKey")
  native_ranges_noduplicates <- native_ranges

  #CABI first records
  CABI_firstrecords <- read.csv("InputFiles/originaldatabase_giatar_2024/dataset/CABI data/CABI_tables/tointroductions.csv")
  #Eliminate inconsistences in year field (like 1970s, <1900, etc..)
  CABI_firstrecords <- CABI_firstrecords %>%
    mutate(
      Year = ifelse(grepl("^[0-9]+$", Year), Year, NA)
    )
  CABI_firstrecords <- CABI_firstrecords
  CABI_firstrecords <- CABI_firstrecords %>%
    filter(grepl("^[0-9]+$", usageKey))

  #EPPO first records
  EPPO_first_reports <- read.csv("InputFiles/originaldatabase_giatar_2024/dataset/EPPO data/EPPO_first_reports.csv")
  EPPO_first_reports <- EPPO_first_reports

  #Merge CABI_firstrecords and EPPO_first_reports by usageKey
  merged <- full_join(
    CABI_firstrecords,
    EPPO_first_reports,
    by = "usageKey"
  )
  merged <- merged %>%
    mutate(usageKey = as.character(usageKey))

  native_ranges_noduplicates <- native_ranges_noduplicates %>%
    mutate(usageKey = as.character(usageKey))

  merged <- full_join(
    merged,
    native_ranges_noduplicates,
    by = "usageKey"
  )

  merged <- merged[,c("usageKey", "ScientificName","Introduced.to", "Introduced.from", "Year", "year", "location")]
  merged <- merged %>%
    mutate(
      RecipientRange = paste(Introduced.to, location, sep = ", "),
      RecipientRange = gsub("^, |, $", "", RecipientRange),
      ReportYear = paste(Year, year, sep = ", "),
      ReportYear = gsub("^, |, $", "", ReportYear),
      ReportYear = gsub(", NA|NA,", "", ReportYear),
      ReportYear = trimws(ReportYear)
    ) %>%
    select(
      usageKey,
      RecipientRange,
      ReportYear,
      ScientificName
    )
  first_records_noduplicates <- merged
  first_records_desglosado <- first_records_noduplicates %>%
    mutate(
      RecipientRange = str_split(RecipientRange, ",\\s*"),
      Year = str_split(as.character(ReportYear), ",\\s*")
    ) %>%
    unnest_longer(RecipientRange, indices_to = "position") %>%
    mutate(
      Year = map2_chr(
        Year,
        position,
        ~ if (length(.x) >= .y) .x[.y] else NA_character_
      )
    ) %>%
    select(usageKey, RecipientRange, Year, ScientificName)
  first_records_desglosado <- first_records_desglosado %>%
    filter(!is.na(RecipientRange)) %>%
    filter(RecipientRange != "NA") %>%
    filter(!is.na(ScientificName))
  final_dataset_GIATAR <- first_records_desglosado
  write_xlsx(final_dataset_GIATAR, "InputFiles/giatar.xlsx")


  ################################################################################

  #AQUANIS 2025
  #Depurate
  #forbidden, no webscrapping now allowed

  ################################################################################

  #FAO
  #Download
  extract_regions <- function(text_column) {
    continents <- unique(countrycode::codelist$continent)
    countries <- na.omit(unique(countrycode::codelist$country.name.en))
    map(text_column, function(text) {
      if (is.na(text)) return(NA_character_)
      results <- c(
        continents[str_detect(text, fixed(continents))],
        countries[str_detect(text, fixed(countries))]
      )
      extract_words <- str_extract_all(text, "\\b[A-Z][a-z]+(?:\\s[A-Z][a-z]+)*\\b")[[1]]
      subregions <- setdiff(extract_words, results)
      unique(c(results, subregions))
    })
  }
  links <- c(
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e08.htm",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e09.htm",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0a.htm",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0b.htm",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0c.htm",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0d.htm",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0e.htm",
    "https://www.fao.org/fishery/docs/CDrom/aquaculture/a0805e/documents/x5628e/x5628e0f.htm"
  )

  columns <- c("Native range", "From", "To", "Year", "Reason")
  processing_link <- function(url) {
    res <- read_html(url)
    names <- res %>%
      html_nodes("h2") %>%
      html_text() %>%
      str_trim()
    content <- res %>%
      html_nodes("h2") %>%
      map(function(node) {
        merged_node <- node %>% html_nodes(xpath = "following-sibling::*")
        info <- merged_node %>%
          keep(~ html_name(.x) != "h2") %>%
          map_chr(html_text) %>%
          str_replace_all("\r\n", " ") %>%
          str_squish() %>%
          paste(collapse = " ")
        stract_range <- str_split(info, "Native range", n = 3)[[1]]
        if (length(stract_range) >= 2) {
          info <- paste0("Native range", stract_range[2])
        }
        info
      })
    dataset <- tibble(species = names, info = content) %>%
      mutate(info = str_replace_all(info, "\r\n", " ")) %>%
      mutate(
        `Native range` = str_extract_all(info, "(?<=Native range: ).*?(?=To:|From:|Year:|Reason:|$)"),
        `From` = str_extract_all(info, "(?<=From: ).*?(?=To:|Native range:|Year:|Reason:|$)"),
        `To` = str_extract_all(info, "(?<=To: ).*?(?=From:|Native range:|Year:|Reason:|$)"),
        `Year` = str_extract_all(info, "(?<=Year: ).*?(?=To:|From:|Native range:|Reason:|$)"),
        `Reason` = str_extract_all(info, "(?<=Reason: ).*?(?=To:|From:|Native range:|Year:|$)")
      )
    dataset
  }
  final_dataset <- map_dfr(links, processing_link) %>%
    select(species, To, Year)
  final_dataset <- final_dataset %>%
    mutate(
      datos = map2(To, Year, \(to, year) {
        to <- str_trim(as.character(to))
        year <- str_trim(as.character(year))
        n <- max(length(to), length(year))

        tibble(
          To = c(to, rep(NA_character_, n - length(to))),
          Year = c(year, rep(NA_character_, n - length(year)))
        )
      })
    ) %>%
    select(species, datos) %>%
    unnest(datos) %>%
    filter(
      !is.na(Year),
      Year != "Unknown"
    )
  final_dataset <- final_dataset %>%
    separate_rows(Year, sep=";|,")%>%
    mutate(Year = trimws(Year))
  final_dataset <- final_dataset %>%
    mutate(
      species = str_extract(species, "^\\S+(?:\\s+\\S+)?")
    )
  final_dataset_FAO <- final_dataset
  write_xlsx(final_dataset_FAO, "InputFiles/fao.xlsx")

  ################################################################################

  #GFID
  GFID <- read.xlsx("InputFiles/originaldatabase_gfid_2025.xlsx")
  GFID_FRW <- GFID %>%
    filter(grepl("FRESHWATER", Habitat))
  GFID_FRW <- GFID_FRW %>%
    select(Taxon, Location, First_record)
  GFID_FRW <- GFID_FRW %>%
    filter(First_record != "NA")
  write_xlsx(GFID_FRW, "InputFiles/gfid.xlsx")

  ################################################################################

  # zamora marin et al 2023
  dat <- read_excel("InputFiles/freshwatersubset_zamoramarin_et_al_2023.xlsx")
  dat <- dat %>%
    select(`Scientific.Name`, `Year.intro..in.SP`, RecipientCountry)
  write_xlsx(dat, "InputFiles/zamoramarin.xlsx")

  ################################################################################

  # draga et al 2024
  freshwatersubset_draga_et_al_2024 <- read_excel("InputFiles/freshwatersubset_draga_et_al_2024.xlsx")
  freshwatersubset_draga_et_al_2024 <- freshwatersubset_draga_et_al_2024 %>%
    select(`Species / family`, FTO, RecipientRange)
  write_xlsx(freshwatersubset_draga_et_al_2024, "InputFiles/draga.xlsx")

  ################################################################################

  #carneiro et al 2025
  carneiro <- read_excel("InputFiles/freshwatersubset_carneiro_et_al_2025.xlsx") %>%
    select(SPP_NN, STATUS_YR, RecipientRange) %>%
    mutate(
      datos = map2(RecipientRange, STATUS_YR, \(country, year) {
        country <- str_trim(unlist(str_split(as.character(country), ",")))
        year    <- str_trim(unlist(str_split(as.character(year), ",")))
        n <- max(length(country), length(year))
        tibble(
          RecipientRange = map_chr(
            seq_len(n),
            \(i) if (i <= length(country)) country[i] else NA_character_
          ),
          STATUS_YR = map_chr(
            seq_len(n),
            \(i) if (i <= length(year)) year[i] else NA_character_
          )
        )
      })
    ) %>%
    select(SPP_NN, datos) %>%
    unnest(datos) %>%
    filter(!is.na(RecipientRange)) %>%
    filter(STATUS_YR != "Not Reported")
  write_xlsx(carneiro, "InputFiles/carneiro.xlsx")

  ################################################################################

  # xie et al 2000
  xie <- read_excel("InputFiles/freshwatersubset_xie_et_al_2000.xlsx")
  xie <- xie %>%
    select(Species, First_Record, Invaded_Country) %>%
    separate_rows(First_Record, sep=";|,") %>%
    mutate(First_Record = trimws(First_Record)) %>%
    filter(!is.na(First_Record))
  write_xlsx(xie, "InputFiles/xie.xlsx")

  ################################################################################

  #elias et al 2022
  elias <- read_excel("InputFiles/freshwatersubset_elias_et_al_2022.xlsx")
  elias <- elias %>%
    select(ScientificName, Year, Invaded_country) %>%
    separate_rows(Year, sep=";|,") %>%
    mutate(Year = trimws(Year)) %>%
    filter(!is.na(Year))
  write_xlsx(elias, "InputFiles/elias.xlsx")

  ################################################################################

  # NISIC 2025
  nisic <- read_excel("InputFiles/freshwatersubset_nisic_national_invasive_species_information_center_2025.xlsx")
  nisic <- nisic %>%
    select(ScientificName, Date_of_US_Introduction, RecipientCountry) %>%
    separate_rows(Date_of_US_Introduction, sep=";|,") %>%
    mutate(Date_of_US_Introduction = trimws(Date_of_US_Introduction)) %>%
    filter(!is.na(Date_of_US_Introduction))
  write_xlsx(nisic, "InputFiles/nisic.xlsx")

  ################################################################################

  #gidias
  dat <- read_excel("InputFiles/GIDIAS_20250417_Excel.xlsx")
  gidias_freshwater_01 <- dat %>%
    filter(Realm %in% c("Freshwater"))
  gidias_freshwater_02 <- subset(gidias_freshwater_01, Year.of.impact > 0 & !is.na(Year.of.impact))
  names(gidias_freshwater_02)[names(gidias_freshwater_02) == "IAS.Taxon"] <- "Group"
  names(gidias_freshwater_02) <- gsub("\\.", "_", names(gidias_freshwater_02))
  gidias_final <- gidias_freshwater_02 %>% filter(direction_Nature == "Negative")
  gidias_final <- gidias_final %>%
    select(Verified_Name_GBIF_Taxon, Year, Country_Location)
  write_xlsx(gidias_final, "InputFiles/gidias.xlsx")

  ################################################################################

  # seebens 2023
  dataset <- read.xlsx(file.path("InputFiles","originaldatabase_seebens_2023.xlsx"), sheet=2)
  dataset <- dataset[dataset$FirstRecord >= 0, ]
  dataset <- dataset %>% filter(PresentStatus %in% c("alien", "Established invasive"))
  dataset <- dataset %>%
    mutate(
      Habitat_freshwater = as.numeric(Habitat_freshwater),
      Habitat_terrestrial = as.numeric(Habitat_terrestrial),
      Habitat_marine = as.numeric(Habitat_marine)
    ) %>%
    filter(Habitat_freshwater == 1 |
             (Habitat_freshwater == 0 & Habitat_terrestrial == 0 & Habitat_marine == 0) |
             (is.na(Habitat_freshwater) & is.na(Habitat_terrestrial) & is.na(Habitat_marine)) |
             (Habitat_freshwater == 0 & is.na(Habitat_terrestrial) & is.na(Habitat_marine)) |
             (is.na(Habitat_freshwater) & Habitat_terrestrial == 0 & is.na(Habitat_marine)) |
             (is.na(Habitat_freshwater) & is.na(Habitat_terrestrial) & Habitat_marine == 0) |
             (is.na(Habitat_freshwater) & Habitat_terrestrial == 0 & Habitat_marine == 0) |
             (Habitat_freshwater == 0 & is.na(Habitat_terrestrial) & Habitat_marine == 0) |
             (Habitat_freshwater == 0 & Habitat_terrestrial == 0 & is.na(Habitat_marine))
    )
  dataset[] <- lapply(dataset, as.character)
  dataset <- OldestDate(dataset, "FirstRecord")
  colnames(dataset)[colnames(dataset) == "LifeForm"] <- "Group"
  dataset <- dataset %>%
    select(TaxonName, Region, FirstRecord)
  write_xlsx(dataset, "InputFiles/seebens.xlsx")


  ################################################################################

  # ELNAIS 2025
  elnais <- read_excel("InputFiles/freshwatersubset_elnais_2025.xlsx")
  elnais <- elnais %>%
    select(AcceptedNameGBIF, FirstRecord, Invaded_country)
  write_xlsx(elnais, "InputFiles/elnais.xlsx")


  ################################################################################

  # dyer et al 2016
  dat <- read.csv("InputFiles/originaldatabase_dyer_et_al_2016.csv", sep=",") #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.
  colnames(dat)[colnames(dat) == "CountryName"] <- "InvadedCountry"
  colnames(dat)[colnames(dat) == "Binomial"] <- "ScientificName"
  dat$IntroducedDateGrouped <- as.numeric(dat$IntroducedDateGrouped)
  dat <- OldestDate(dat, "IntroducedDateGrouped")
  dataset <- dat %>%
    filter(StatusCat == "Established")
  dataset <- dataset %>%
    select(ScientificName, InvadedCountry, MappingDate)
  write_xlsx(dataset, "InputFiles/dyer.xlsx")

  ################################################################################

  # asiapacific alien species database 2025
  final_dataset <- read_excel("./InputFiles/freshwatersubset_asiapacific_alien_species_database_2025.xlsx")
  final_dataset <- final_dataset %>%
    select(Species_name, Year_of_invasion_or_detection,country_or_region_name) %>%
    separate_rows(Year_of_invasion_or_detection, sep=";|,") %>%
    mutate(Year_of_invasion_or_detection = trimws(Year_of_invasion_or_detection)) %>%
    filter(!is.na(Year_of_invasion_or_detection)) %>%
    separate_rows(country_or_region_name, sep=";|,") %>%
    mutate(country_or_region_name = trimws(country_or_region_name)) %>%
    filter(!is.na(country_or_region_name))
  write_xlsx(final_dataset, "FinalFiles/asiapacific.xlsx")


  ################################################################################
  # bailey et al 2021
  bailey <- read_excel("InputFiles/freshwatersubset_bailey_et_al_2021.xlsx")
  bailey <- bailey %>%
    select(Species_name,`Year of First Report`, InvadedCountry)
  write_xlsx(bailey, "FinalFiles/bailey.xlsx")


  ################################################################################

  #fishbase
  fishbase_dataset <- species_names(
    server = c("fishbase", "sealifebase"),
    version = "latest"
  )
  dataframe_species_dataset <- as.data.frame(fishbase_dataset)
  fish_names <- dataframe_species_dataset$Species
  fish_information <- ecosystem(
    species_list = fish_names
  )
  fish_information$Status <- factor(fish_information$Status)
  fish_information <- fish_information %>%
    filter(Status == "introduced")
  fish_information$Salinity <- factor(fish_information$Salinity)
  fish_information <- fish_information %>%
    filter(Salinity == "freshwater")
  fish_information$EcosystemType <- factor(fish_information$EcosystemType)
  fish_information <- fish_information %>%
    filter(
      EcosystemType == "River (basin)" |
        EcosystemType == "Lake" |
        EcosystemType == "Zoogreographic realm"
    )
  fish_subset <- fish_information
  merged_dataset_fish <- fishbase_dataset %>%
    full_join(
      fish_subset,
      by = "SpecCode"
    ) %>%
    filter(!is.na(Status))
  selected_columns <- c(
    "SpecCode",
    "Species.x",
    "Family",
    "Order",
    "Class",
    "Status",
    "Salinity",
    "EcosystemType",
    "EcosystemName"
  )
  data_subset_fishbase <- merged_dataset_fish %>%
    select(all_of(selected_columns))
  dataset_noduplicates <- data_subset_fishbase
  especies <- unique(dataset_noduplicates$Species.x)
  intro_df <- map_dfr(especies, function(sp) {
    tbl <- introductions(sp)
    if (nrow(tbl) == 0) {
      return(
        tibble(
          Species.x = sp,
          IntroducedRange = NA_character_,
          IntroductionYear = NA_real_
        )
      )
    }
    # If Year is empty, select RangeMin
    tbl <- tbl %>%
      mutate(
        IntroductionYear = coalesce(
          as.numeric(Year),
          as.numeric(RangeMin)
        )
      )
      tbl <- tbl %>%
      filter(!is.na(IntroductionYear))
    if (nrow(tbl) == 0) {
      return(
        tibble(
          Species.x = sp,
          IntroducedRange = NA_character_,
          IntroductionYear = NA_real_
        )
      )
    }
    tibble(
      Species.x = sp,
      IntroducedRange = str_trim(tbl$TO),
      IntroductionYear = tbl$IntroductionYear
    )
  })
  dataset_noduplicates3 <- intro_df %>%
    filter(
      !is.na(IntroducedRange),
      IntroducedRange != "Unknown"
    ) %>%
    mutate(
      Name = Species.x
    ) %>%
    select(
      Name,
      IntroducedRange,
      IntroductionYear
    )
  write_xlsx(dataset_noduplicates3, "InputFiles/fishbase.xlsx")

  #######################################################################
  #rename columns


  ########################################################################
  #rbind databases
  db1 <- read_excel("InputFiles/asiapacific.xlsx")
  db2 <- read_excel("InputFiles/bailey.xlsx")
  db3 <- read_excel("InputFiles/carneiro.xlsx")
  db4 <- read_excel("InputFiles/draga.xlsx")
  db5 <- read_excel("InputFiles/dyer.xlsx")
  db6 <- read_excel("InputFiles/easin.xlsx")
  db7 <- read_excel("InputFiles/elias.xlsx")
  db8 <- read_excel("InputFiles/elnais.xlsx")
  db9 <- read_excel("InputFiles/fao.xlsx")
  db10 <- read_excel("InputFiles/fishbase.xlsx")
  db11 <- read_excel("InputFiles/gfid.xlsx")
  db12 <- read_excel("InputFiles/giatar.xlsx")
  db13 <- read_excel("InputFiles/gidias.xlsx")
  db14 <- read_excel("InputFiles/nisic.xlsx")
  db15 <- read_excel("InputFiles/seebens.xlsx")
  db16 <- read_excel("InputFiles/usriis.xlsx")
  db17 <- read_excel("InputFiles/xie.xlsx")
  db18 <- read_excel("InputFiles/zamoramarin.xlsx")

  merged <- rbind(
    db1,
    db2,
    db3,
    db4,
    db5,
    db6,
    db7,
    db8,
    db9,
    db10,
    db11,
    db12,
    db13,
    db14,
    db15,
    db16,
    db17,
    db18
  )

  merged <- merged %>%
    arrange(Name)

  merged_names <- merged$Name
  total <- length(merged_names)

  accepted_names <- map_chr(seq_along(merged_names), function(i) {

    cat(sprintf(
      "\rBuscando especie %d/%d: %s",
      i, total, merged_names[i]
    ))

    result <- name_backbone_checklist(merged_names[i])$species

    if (length(result) == 0) {
      NA_character_
    } else {
      result[1]
    }
  })
  merged$AcceptedNameGBIF <- accepted_names
  write_xlsx(merged, "InputFiles/merged0.xlsx")

  #we select species contained on masterlist FRIAS
  masterlist <- read_excel("FinalFiles/copy_thispeciesonly_(Table S3) FRIAS_masterlist.xlsx",
                           sheet = "Masterlist")
  merged_final <- merged %>%
    left_join(
      masterlist %>%
        select(AcceptedNameGBIF, ID_GBIF),
      by = "AcceptedNameGBIF"
    ) %>%
    filter(!is.na(ID_GBIF))

  #lo guardamos
  merged_final <- merged_final %>%
    select(
      AcceptedNameGBIF,
      ID_GBIF,
      Year,
      Country
    )
  write_xlsx(merged_final, "InputFiles/merged1.xlsx")

  #######################################################################
  #harmonize locations

  MasterList <- read.xlsx("InputFiles/merged1.xlsx") %>%
    separate_rows(Country, sep=";|,") %>%
    mutate(Country = trimws(Country))
  MasterList <- MasterList %>%
    rename(
      RecipientRange = Country
    )
  ###############################
  ####### RECIPIENT RANGE #######
  ###############################
  #Clean cells text
  clean_text_column <- function(column) {
    column %>%
      gsub("[()]", "", .) %>%
      gsub(",", ";", .) %>%
      gsub(";\\s*", ";", .) %>%
      gsub("xml:space=\\\"preserve\\\">", "", .) %>%
      gsub("(^NA$|^NA;|;NA)", "", .) %>%
      trimws()
  }
  MasterList <- MasterList %>%
    mutate(
      RecipientRange = clean_text_column(RecipientRange),
      CopyRecipientRange = clean_text_column(RecipientRange)
    )


  #Replace "" and "NA" to NULL
  cols <- setdiff(names(MasterList), "Source_Data")
  MasterList[cols] <- lapply(MasterList[cols], function(x) {
    x[x == "" | x == "NA"] <- NA
    x
  })

  #Load locations_table
  locations_table <- read.xlsx("TablesToStandardize/Table S2.xlsx", sheet="6. Countries") %>%
    mutate(keywords = gsub("[()]", "", keywords))%>%
    mutate(
      keywords = map(keywords, ~ str_trim(tolower(.x))),
      Location = str_trim(tolower(Location))
    ) %>%
    unnest(keywords)

  #Matching functions
  match_by_keywords <- function(name, loc_table) {
    name_clean <- tolower(str_trim(name))
    matched_row <- loc_table[!is.na(loc_table$keywords) &
                               grepl(paste0("(^|;)\\s*", name_clean, "\\s*(;|$)"),
                                     tolower(loc_table$keywords)), ]
    if (nrow(matched_row) > 0) return(matched_row$Location[1])
    return(NA_character_)
  }

  match_by_iso2 <- function(iso2_code, loc_table) {
    iso2_clean <- toupper(str_trim(iso2_code))
    matched_row <- loc_table[!is.na(loc_table$ISO2) & toupper(loc_table$ISO2) == iso2_clean, ]
    if (nrow(matched_row) > 0) return(matched_row$Location[1])
    return(NA_character_)
  }

  replace_with_location <- function(name, loc_table) {
    name_clean <- str_trim(name)
    loc <- match_by_keywords(name_clean, loc_table)
    if (!is.na(loc)) return(loc)
    loc <- match_by_iso2(name_clean, loc_table)
    if (!is.na(loc)) return(loc)
    match_location <- loc_table$Location[tolower(loc_table$Location) == tolower(name_clean)]
    if (length(match_location) > 0) return(match_location[1])
    return(NA_character_)
  }

  #REmove duplicated iso2 codes
  iso2_transformed_Range <- function(a, b) {
    unique_items <- unique(trimws(c(unlist(strsplit(a, ";")), unlist(strsplit(b, ";")))))
    paste(na.omit(unique_items[unique_items != ""]), collapse = ";")
  }

  #Standardize Recipient Range column
  MasterList$RecipientRange <- pbmapply(iso2_transformed_Range,
                                        MasterList$RecipientRange,
                                        MasterList$RecipientRange)
  MasterList <- MasterList %>%
    mutate(
      RecipientRange_cleaned = pbapply::pblapply(RecipientRange, function(cell) {
        if (is.na(cell) || cell == "") return(list(matched = NA_character_, not_matched = NA_character_))
        items <- unlist(strsplit(cell, "[,;]"))
        items <- str_trim(items)
        items <- items[items != ""]
        replaced <- sapply(items, function(name) replace_with_location(name, locations_table))
        not_matched <- items[is.na(replaced)]
        replaced <- na.omit(unique(replaced))
        list(
          matched = if (length(replaced) == 0) NA_character_ else paste(replaced, collapse = "; "),
          not_matched = if (length(not_matched) == 0) NA_character_ else paste(not_matched, collapse = "; ")
        )
      })
    ) %>%
    mutate(
      RecipientRange = sapply(RecipientRange_cleaned, `[[`, "matched"),       # Solo los que hicieron match
      RecipientRange_no_match = sapply(RecipientRange_cleaned, `[[`, "not_matched") # Lo que no hizo match
    ) %>%
    select(-RecipientRange_cleaned)

  #SAve not matches cases of Recipient Range
  no_match_RecipientRange <- MasterList %>%
    filter(!is.na(RecipientRange_no_match))

  MasterList <- MasterList %>% select(-RecipientRange_no_match)

  #Obtain ISO3 of Recipient Ranges
  get_ISO3 <- function(countries_str) {
    countries <- unlist(strsplit(countries_str, ";"))
    ISO3 <- unlist(sapply(trimws(countries), function(c) {
      iso_match <- locations_table$ISO3[locations_table$Location == c]
      if (length(iso_match) == 0)
        iso_match <- locations_table$ISO3[str_detect(locations_table$keywords, c)]
      if (length(iso_match) == 0) return(NA)
      unique(trimws(unlist(strsplit(iso_match, ";"))))
    }))
    ISO3 <- unique(na.omit(ISO3))
    if (length(ISO3) == 0) return(NA)
    paste(ISO3, collapse = "; ")
  }
  MasterList$RecipientRangeISO3 <- sapply(MasterList$RecipientRange, get_ISO3)

  #Obtain Recipient bioregions
  regions_table <- read.xlsx("TablesToStandardize/Table S2.xlsx", sheet="7. Regions") %>%
    mutate(keywords = str_split(keywords, ";")) %>%
    unnest(keywords) %>%
    mutate(keywords = str_trim(tolower(keywords)))
  find_region <- function(iso3_str, keywords, locations) {
    if (is.na(iso3_str) || iso3_str == "") return(NA)
    matches <- which(keywords %in% str_trim(tolower(unlist(strsplit(iso3_str, ";")))))
    if (length(matches) == 0) return(NA)
    paste(unique(locations[matches]), collapse = "; ")
  }
  MasterList$RecipientBioregions1 <- map_chr(
    tolower(MasterList$RecipientRangeISO3),
    ~find_region(.x, regions_table$keywords, regions_table$Location)
  )

  MasterList <- MasterList %>%
    rename(
      AcceptedNameGBIF = AcceptedNameGBIF,
      ID_GBIF = ID_GBIF,
      Report_Year = Year,
      RecipientRange = RecipientRange,
      CopyRecipientRange = CopyRecipientRange,
      RecipientRangeISO3 = RecipientRangeISO3,
      RecipientBioregions = RecipientBioregions1
    ) %>%
    select(-CopyRecipientRange)
  MasterList <- MasterList %>%
    filter(!is.na(RecipientRange)) %>%
    filter(!is.na(Report_Year)) %>%
    filter(grepl("^\\d+$", Report_Year)) %>%
    filter(
      as.numeric(Report_Year) >= 1000,
      as.numeric(Report_Year) <= as.numeric(format(Sys.Date(), "%Y"))
    ) %>%
    distinct()

  #######################################################################
  ########           Save final table        ############################
  #######################################################################
  write_xlsx(MasterList, "InputFiles/ReportYear_Country.xlsx")


}
