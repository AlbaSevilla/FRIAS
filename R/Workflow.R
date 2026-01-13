#╔═════════════════════════════════════════════════════════╗
#║               F R I A S   W O R K F L O W               ║
#║          Freshwater Invasive Alien Species (FRIAS)      ║
#╠═════════════════════════════════════════════════════════╣
#║   Robust workflow to acquire, clean, standardize, and
#║   integrate data, culminating in a comprehensive final  ║
#║   masterlist of freshwater invasive alien species       ║
#╚═════════════════════════════════════════════════════════╝



cat("
╔═════════════════════════════════════════════════════════╗
║               F R I A S   W O R K F L O W               ║
║          Freshwater Invasive Alien Species (FRIAS)      ║
╠═════════════════════════════════════════════════════════╣
║   Robust workflow to acquire, clean, standardize, and   ║
║   integrate data, culminating in a comprehensive final  ║
║   masterlist of freshwater invasive alien species       ║
╚═════════════════════════════════════════════════════════╝
")
graphics.off()
rm(list=ls())



cat("
╔═════════════════════════════════════════╗
║ Installation and loading of packages    ║
║ required for workflow execution         ║
╚═════════════════════════════════════════╝
")
source(file.path("R", "Packages_download.R"))
Packages_download()

cat("
╔════════════════════════════════════════════════════════════╗
║                     Load functions                         ║
╚════════════════════════════════════════════════════════════╝
")
source(file.path("R", "noduplicates.R"))
source(file.path("R", "check_habitat.R"))
source(file.path("R", "OldestDate.R"))
source(file.path("R", "colapse_through_AcceptedNameGBIF.R"))
source(file.path("R", "Downloads_and_Dates_Databases.R"))
source(file.path("R", "HarmonizationDatabases.R"))
source(file.path("R", "MergeDatasets.R")) # combine data sets
source(file.path("R", "iucnredlist_NativeRecipientRange.R"))
source(file.path("R", "worrms_NativeRecipientRange.R"))
source(file.path("R", "SInAS_NativeRange.R"))
source(file.path("R", "naturalearth_fillgapslocations.R"))
source(file.path("R", "StandardizeLocations.R"))
source(file.path("R", "StandardizeMechanism.R"))
source(file.path("R", "StandardizeHabitat.R"))
source(file.path("R", "StandardizePathways.R"))
source(file.path("R", "StandardizeInformalGroup.R"))
source(file.path("R", "StandardizeEstablishmentMeans.R"))
source(file.path("R", "Obtain_TaxonNAS.R"))
source(file.path("R", "Obtain_FunctionalGroup.R"))
source(file.path("R", "taxonomy_query.R"))
source(file.path("R", "CorrectNamesGBIF.R"))
source(file.path("R", "ExclusionSpeciesbyHabitatGBIF.R"))
source(file.path("R", "FinalMasterlist.R"))

source(file.path("R", "Dates_graphs.R"))
source(file.path("R", "Native_to_Recipient_maps.R"))
source(file.path("R", "Duplicates_uniques_species_databases.R"))
source(file.path("R", "Groups_through_RecipientBioregions.R"))
source(file.path("R", "Over_Sub_Representation_InformalGroup.R"))

cat("
╔════════════════════════════════════════════════════════════════════════════╗
║          Loading correspondence table files                                ║
╚════════════════════════════════════════════════════════════════════════════╝
")
CorrespondenceTable <- read.xlsx("TablesToStandardize/Table S1.xlsx", sheet="Databases") %>%
  filter(!is.na(FRIAS_name))
View(CorrespondenceTable)

cat("
╔════════════════════════════════════════════════════════════╗
║               Download databases                           ║
╚════════════════════════════════════════════════════════════╝
")
Downloads_and_Dates_Databases()



cat("
╔════════════════════════════════════════════════════════════════════════════╗
║      Column standardization and harmonization for data integration.        ║
║      Also standardization of impact data using EICAT values                ║
╚════════════════════════════════════════════════════════════════════════════╝
")
n <- nrow(CorrespondenceTable)
errores <- list()
ok <- character()

for (i in seq_len(n)) tryCatch({
  id <- if ("FRIAS_name" %in% names(CorrespondenceTable))
    as.character(CorrespondenceTable$FRIAS_name[i]) else paste0("Fila ", i)
  message("Preparing ", i, "/", n, ": ", id)
  suppressMessages(HarmonizationDatabases(CorrespondenceTable[i, , drop = FALSE]))
  ok <- c(ok, id)
}, error = function(e) errores[[id]] <<- conditionMessage(e))

message(if (length(errores))
  paste("Failed", length(errores), "of", n, ":\n-",
        paste(sprintf("%s: %s", names(errores), unlist(errores)), collapse = "\n- "))
  else "All was executed correctly")


cat("
╔══════════════════════════════════════════════════════════════════╗
║           Data merging into a unique masterlist                  ║
╚══════════════════════════════════════════════════════════════════╝
")
MergeDatasets()


cat("
╔════════════════════════════════════════════════════════════════════╗
║                   F I L L    M I S S I N G                         ║
║                     I N F O R M A T I O N                          ║
╠════════════════════════════════════════════════════════════════════╣
║ • Fill gaps in native ranges using IUCN Red List                   ║
║ • Standardize ISO3 codes for countries and obtain bioregions       ║
║ • Standardize mechanisms                                           ║
║ • Standardize habitats                                             ║
║ • Standardize pathways                                             ║
║ • Obtain NAS values for taxonomy                                   ║
║ • Removal of microorganisms                                        ║
║ • Gaps in Accepted Name GBIF using correspondece table             ║
║ • Fill NAS in Functional Group                                     ║
║ • Removal of incorrectly classified freshwater species from the    ║
║   original database with GBIF                                      ║
╚════════════════════════════════════════════════════════════════════╝
")
##
cat("\n Gaps in Accepted Name GBIF using correspondece table \n")
CorrectNamesGBIF()
cat("\n Removal of incorrectly classified freshwater species from the original database with GBIF \n")
ExclusionSpeciesbyHabitatGBIF()
###
cat("\n Obtain NAS values for taxonomy \n")
Obtain_TaxonNAS()
cat("\n Standardize Informal Group \n")
StandardizeInformalGroup()
##
cat("\n Fill gaps in Locations using IUCN Red List, Worrms and SInAS \n")
iucnredlist_NativeRecipientRange()
worrms_NativeRecipientRange()
SInAS_NativeRange()
cat("\n Standardize ISO3 codes for countries and obtain bioregions \n")
StandardizeLocations()
cat("\n Standardize Mechanisms \n")
StandardizeMechanism()
cat("\n Standardize Habitat \n")
StandardizeHabitat()
cat("\n Standardize Pathways \n")
StandardizePathways()
cat("\n Standardize EstablishmentMeans \n")
StandardizeEstablishmentMeans()
cat("\n Fill NAS in Functional Group \n")
Obtain_FunctionalGroup()

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                   F I N A L    M A S T E R L I S T S                         ║
╠══════════════════════════════════════════════════════════════════════════════╣
║ • Final Species List, 1 row 1 Species                                        ║
║ • Final Masterlist with differents impacts in each row for same species      ║
╚══════════════════════════════════════════════════════════════════════════════╝
")
FinalMasterlist()



cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                      D A T A   V I S U A L I Z A T I O N                     ║
╚══════════════════════════════════════════════════════════════════════════════╝
")
Dates_graphs()
Native_to_Recipient_maps()
Duplicates_uniques_species_databases()
Groups_through_RecipientBioregions()
Over_Sub_Representation_InformalGroup()
