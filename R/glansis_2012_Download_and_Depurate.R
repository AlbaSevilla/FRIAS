glansis_2012_Download_and_Depurate <- function(){
  #Download
  url <- "https://nas.er.usgs.gov/queries/greatLakes/SpeciesList.aspx?SpeciesCategory=3&Group=&HUCNumber=&Genus=&Species=&ComName=&status=0&pathway=0&Sortby=1"
  res <- read_html(url)
  res2 <- res %>% html_table
  res3 <- res2[[2]]
  write.xlsx(res3, "Inputfiles/originaldatabase_glansis_2012.xlsx")

  #Depurate
  dat <- read.xlsx("InputFiles/originaldatabase_glansis_2012.xlsx") #Aquí estamos indicando que sustraiga los excel de las bases de dat iniciales de la carpeta 'InputFiles'.
  dat <- dat[,-1]
  dat_noduplicates <- noduplicates(dat, "Scientific.Name(click.for.species.profile)")
  names(dat_noduplicates)[names(dat_noduplicates) ==
          "Scientific.Name(click.for.species.profile)"] <- "ScientificName"
  names(dat_noduplicates)[names(dat_noduplicates) ==
                            "Year.First.Collected"] <- "OldestDate"
  dataset <- dat_noduplicates
  species <- dataset$ScientificName
  names_acept <- name_backbone_checklist(species)$canonicalName
  dat_act <- check_habitat(names_acept, dataset)
  dat_fresh <- dat_act %>% filter(grepl("FRESHWATER", Habitat))
  names(dat_fresh) <- gsub("\\.", "_", names(dat_fresh))
  dat_fresh$Invaded_Country <- "United States of America; Canada"

  #Save
  write.xlsx(dat_fresh, "InputFiles/freshwatersubset_glansis_2012.xlsx")
}

