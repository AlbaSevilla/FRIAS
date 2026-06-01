kortz_et_al_2025_Download_and_Depurate <- function(){
  #Depurate
  url <- "https://neobiota.pensoft.net/article/151156/download/suppl/31/"
  destfile <- "InputFiles/originaldatabase_kortz_et_al_2025.docx"
  download.file(url, destfile, mode = "wb")
  text_data <- "
    Albania\tEurope
    Algeria\tAfrica
    Austria\tEurope
    Bangladesh\tTropical Asia
    Belize\tSouth America
    Benin\tAfrica
    Bermuda\tSouth America
    Bhutan\tTropical Asia
    Bulgaria\tEurope
    Burundi\tAfrica
    Caribbean: Bahamas, Cuba, Dominican Republic, Jamaica, Puerto Rico, St. Lucia, St. Martin, Trinidad and Tobago, Virgin Islands\tSouth America
    Chile\tSouth America
    China\tTemperate Asia
    Democratic Republic of the Congo\tAfrica
    Costa Rica\tSouth America
    Czech Republic\tEurope
    El Salvador\tSouth America
    Ethiopia\tAfrica
    Flores, Faial and Santa Maria, Azores\tEurope
    France\tEurope
    Germany\tEurope
    Ghana\tAfrica
    Great Britain\tEurope
    Greece\tEurope
    Guatemala\tSouth America
    Honduras\tSouth America
    Iran\tTemperate Asia
    Italy\tEurope
    Kenya\tAfrica
    Kyrgyzstan\tTemperate Asia
    Lithuania\tEurope
    Malesia: Indonesia\tTropical Asia
    Malesia: Papua New Guinea\tTropical Asia
    Malesia: Timor\tTropical Asia
    Morocco\tAfrica
    Myanmar\tTropical Asia
    Nepal\tTropical Asia
    New South Wales\tAustralasia
    New Zealand\tAustralasia
    Nicaragua\tSouth America
    Nigeria\tAfrica
    Oman\tTemperate Asia
    Pakistan\tTropical Asia
    Panama\tSouth America
    Philippines\tTropical Asia
    Poland\tEurope
    Portugal\tEurope
    Romania\tEurope
    Russia: Kostroma\tEurope
    Russia: Middle Volga\tEurope
    Russia: Novosibirsk oblast\tTemperate Asia
    Rwanda\tAfrica
    Sierra Leone\tAfrica
    South Sudan\tAfrica
    Spain\tEurope
    Sri Lanka\tTropical Asia
    Sudan\tAfrica
    Switzerland\tEurope
    Tanzania\tAfrica
    Thailand\tTropical Asia
    Türkiye\tTemperate Asia
    Uganda\tAfrica
    US: Alaska\tNorth America
    US: Arizona\tNorth America
    US: California\tNorth America
    US: Colorado\tNorth America
    US: Florida\tNorth America
    US: Idaho\tNorth America
    US: Illinois\tNorth America
    US: Nebraska\tNorth America
    US: Oregon\tNorth America
    US: Texas\tNorth America
    US: Vermont\tNorth America
    US: Virginia\tNorth America
    US: Washington\tNorth America
    Uzbekistan\tTemperate Asia
    Venezuela\tSouth America
    Zimbabwe\tAfrica
    "
  regions <- read.delim(textConnection(text_data), header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  colnames(regions) <- c("Region_Country", "Continent")
  species_list <- c(
    "Ludwigia palustris",
    "Azolla pinnata",
    "Callitriche hamulata",
    "Echinochloa colonum subsp. edulis",
    "Glyceria maxima",
    "Gymnocoronis spilanthoides",
    "Hydrocotyle bonariensis",
    "Ludwigia peruviana",
    "Nuphar lutea",
    "Nymphoides geminata",
    "Oenanthe aquatica",
    "Oenanthe sarmentosa",
    "Ottelia ovalifolia",
    "Ranunculus trichophyllus",
    "Sagittaria montevidensis",
    "Sagittaria platyphylla",
    "Typha latifolia",
    "Utricularia geminiscapa",
    "Utricularia livida",
    "Vallisneria nana",
    "Hydrocotyle verticillata",
    "Callitriche stagnalis",
    "Hydrocleys nymphoides",
    "Marsilea mutica",
    "Utricularia gibba",
    "Myriophyllum aquaticum",
    "Aponogeton distachyos",
    "Nymphaea elegans",
    "Lagarosiphon major",
    "Spirodela punctata",
    "Lemna minuta",
    "Elodea nuttallii",
    "Myriophyllum spicatum",
    "Veronica anagallis subsp. aquatica",
    "Typha angustifolia",
    "Elodea canadensis",
    "Potamogeton crispus",
    "Hydrilla verticillata",
    "Azolla filiculoides",
    "Salvinia molesta",
    "Elodea densa",
    "Pontederia crassipes"
  )
  dataset <- as.data.frame(species_list)
  names_to_check <- dataset$species_list
  accepted_names <- name_backbone_checklist(names_to_check)$canonicalName
  dataset <- check_habitat(accepted_names, dataset)
  dataset$Region <- c(rep("Australasia", 30), rep("Europe", 2), rep("North America", 3),
                      "Australasia; Europe", "Australasia; North America",
                      "Australasia; South America", "Africa; Temperate Asia; Europe",
                      "Africa; Tropical Asia; Australasia", "Australasia; Europe; North America; South America",
                      "Africa; Temperate Asia; Tropical Asia; Australasia; South America")
  dataset <- dataset %>% filter(grepl("FRESHWATER", Habitat))
  regions_unique <- noduplicates(regions, "Continent")
  merged_dataset <- merge(dataset, regions_unique, by.x = "Region", by.y = "Continent")
  merged_dataset$Region_Country <- gsub("US: ", "", merged_dataset$Region_Country)
  merged_dataset[] <- lapply(merged_dataset, function(x) gsub(",", ";", x))

  #Save
  write.xlsx(merged_dataset, "InputFiles/freshwatersubset_kortz_et_al_2025.xlsx")
}
