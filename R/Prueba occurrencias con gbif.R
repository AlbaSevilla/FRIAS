MasterList <- read_excel("OutputFiles/Final/FRIAS_SpeciesList_MasterList.xlsx")

# specify the scientific name of the species to be modelled; see an example below
MasterList <- MasterList[c(1:10),]
species <- MasterList$AcceptedNameGBIF

#--------------------------------------------
#-----------------Load packages--------------
#--------------------------------------------
packages <- c("rgbif", "dplyr", "purrr", "assertthat", "readr", "here", "qs")

for(package in packages) {
  print(package)
  if( ! package %in% rownames(installed.packages()) ) { install.packages( package ) }
  library(package, character.only = TRUE)
}


#--------------------------------------------
#-----------Retrieve GBIF taxonkeys----------
#--------------------------------------------
# Match species names with the GBIF backbone, retrieve taxon keys from GBIF when a match is found
taxon_df <- as.data.frame(species)

mapped_taxa <- purrr::map_dfr(
  taxon_df$species,
  ~ {
    tryCatch(
      {
        data <- rgbif::name_backbone(name = .x)
        if (length(data) == 0) {
          stop("No match with the GBIF backbone found")
        }
        data
      },
      error = function(e) {
        NULL
      }
    )
  }
)

#Make sure that only species info is stored as it is possible that genus information is captured when the species part of the name is not clear
mapped_taxa<-mapped_taxa %>%
  dplyr::filter(rank =="SPECIES")

#Make sure that all species were mapped to the GBIF backbone, if not an error will appear indicating which species are missing
assertthat::assert_that(
  nrow(mapped_taxa) == length(species),
  msg = paste0("The following species could not be found in the GBIF backbone taxonomy: ",
               paste(species[!sapply(species, function(x) any(grepl(x, mapped_taxa$scientificName)))], collapse = ", "))
)

not_accepted <- mapped_taxa %>%
  dplyr::filter(status !="ACCEPTED")

if (nrow(not_accepted)!=0) {
  warning(paste0("The following species do not have an accepted taxonomic status in the GBIF backbone: ",paste(unique(not_accepted$scientificName), collapse=", "),". Their corresponding accepted species names will be used for downloading occurrence data.")
  )
} else {
  paste0("All species are accepted taxa in the GBIF backbone 🎉")
}

#Extract taxonkeys of each species, for synonyms the acceptedUsageKey is stored
accepted_taxonkeys<-mapped_taxa %>%
  dplyr::filter(status =="ACCEPTED")%>%
  dplyr::pull(usageKey)

if(nrow(not_accepted!=0)){
  synonym_taxonkeys<-mapped_taxa %>%
    dplyr::filter(status !="ACCEPTED")%>%
    dplyr::pull(acceptedUsageKey)

  accepted_taxonkeys<-c(accepted_taxonkeys, synonym_taxonkeys)
}

#Keep unique accepted taxonkeys
accepted_taxonkeys<-unique(accepted_taxonkeys)


#--------------------------------------------
#-----------Define download settings---------
#--------------------------------------------
#All basis of record types, except `FOSSIL SPECIMEN` and `LIVING SPECIMEN`, which can have misleading location information (e.g. location of captive animal).
basis_of_record <- c(
  "OBSERVATION",
  "HUMAN_OBSERVATION",
  "MATERIAL_SAMPLE",
  "PRESERVED_SPECIMEN",
  "UNKNOWN",
  "MACHINE_OBSERVATION",
  "OCCURRENCE"
)

#Time period
year_begin <- 1500
year_end <- as.numeric(format(Sys.Date(), "%Y"))

#Only georeferenced points
hasCoordinate <- TRUE


#--------------------------------------------
#---------------Perform download-------------
#--------------------------------------------
#Note that GBIF credentials are required
gbif_download_key <- rgbif::occ_download(
  pred_in("taxonKey", accepted_taxonkeys),
  pred_in("basisOfRecord", basis_of_record),
  pred_gte("year", year_begin),
  pred_lte("year", year_end),
  pred("hasCoordinate", hasCoordinate),
  user = rstudioapi::askForPassword("GBIF username"),
  pwd = rstudioapi::askForPassword("GBIF password"),
  email = rstudioapi::askForPassword("Email address for notification")
)

rgbif::occ_download_wait(gbif_download_key)#Check download status


#--------------------------------------------
#--------------Retrieve download-------------
#--------------------------------------------
rgbif::occ_download_get(gbif_download_key, path = "./Occurrences/GBIF_Occurrences", overwrite = TRUE)
metadata <- rgbif::occ_download_meta(key = gbif_download_key)
gbif_download_key<-metadata$key

#extract_GBIF_occurrence
raw.path<- here::here("Occurrences", "GBIF_occurrences", gbif_download_key)
unzip(paste0(raw.path,".zip"),exdir=raw.path, overwrite=TRUE)
global<-as.data.frame(data.table::fread(paste0(raw.path,"/occurrence.txt"),header=TRUE))
global<-dplyr::select(global, c(speciesKey,species, decimalLatitude, decimalLongitude, kingdom, phylum, class, genus, coordinateUncertaintyInMeters, identificationVerificationStatus))

#-------------------------------
#------------------Save data-----------------
#--------------------------------------------
#Create dataset taxa_info containing scientific name, canonical name, taxonkeys, gbif download key,...

#Si hay + de un nombre aceptado para una misma especie?

taxa_info<-data.frame(speciesKey=unique(global$speciesKey),
                      acceptedScientificName=unique(global$species),
                      year_begin=metadata[["request"]][["predicate"]][["predicates"]][[3]][["value"]],
                      year_end=metadata[["request"]][["predicate"]][["predicates"]][[4]][["value"]],
                      gbif_download_key = gbif_download_key,
                      gbif_download_created = format(strptime(metadata$created, "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d %H:%M:%S"))

write.csv(
  taxa_info,
  file = file.path("./Occurrences/GBIF_occurrences", gbif_download_key, "taxa_info.csv"),
  row.names = FALSE
)

write.csv(global,
           file = file.path("./Occurrences/GBIF_occurrences", gbif_download_key, "occurrence.csv"))
