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
source(file.path("R", "Packages_download.r"))
Packages_download()

cat("
╔════════════════════════════════════════════════════════════╗
║                     Load functions                         ║
╚════════════════════════════════════════════════════════════╝
")
source(file.path("R", "noduplicates.r"))
source(file.path("R", "check_habitat.r"))
source(file.path("R", "OldestDate.r"))
source(file.path("R", "Downloads_and_Dates_Databases.r"))
source(file.path("R","HarmonizationDatabases.r"))
source(file.path("R","MergeDatasets.r")) # combine data sets
source(file.path("R","iucnredlist_NativeRecipientRange.r"))
source(file.path("R", "worrms_NativeRecipientRange.r"))
source(file.path("R", "naturalearth_fillgapslocations.r"))
source(file.path("R","StandardizeLocations.r"))
source(file.path("R","StandardizeMechanism.r"))
source(file.path("R","StandardizeHabitat.r"))
source(file.path("R", "StandardizePathways.r"))
source(file.path("R", "StandardizeInformalGroup.r"))
source(file.path("R", "StandardizeEstablishmentMeans.r"))
source(file.path("R", "Obtain_TaxonNAS.r"))
source(file.path("R", "Obtain_FunctionalGroup.r"))
source(file.path("R", "taxonomy_query.r"))
source(file.path("R", "CorrectNamesGBIF.r"))
source(file.path("R", "ExclusionSpeciesbyHabitatGBIF.r"))
source(file.path("R", "InconsistenciesHabitat.r"))
source(file.path("R", "FinalMasterlist.r"))

source(file.path("R", "Sankey_bioregions.r"))
source(file.path("R", "Dates_graphs.r"))
source(file.path("R", "Native_to_Recipient_maps.r")) #hecho con python
source(file.path("R", "Pathways_invaded_bioregions.r"))
source(file.path("R", "Duplicates_uniques_species_databases.r"))
source(file.path("R", "Species_coverage.r"))
source(file.path("R", "Type_download.r"))
source(file.path("R", "Resolved_missing_information.r"))
source(file.path("R","Groups_through_RecipientBioregions.r"))

cat("
╔════════════════════════════════════════════════════════════════════════════╗
║          Loading correspondence table files                                ║
╚════════════════════════════════════════════════════════════════════════════╝
")
CorrespondenceTable <- read.xlsx("TablesToStandardize/Table S1.xlsx", sheet="Databases")
CorrespondenceTable <- CorrespondenceTable[c(1:91),]
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
  id <- if ("Dataset_FRIAS_name" %in% names(CorrespondenceTable))
    as.character(CorrespondenceTable$Dataset_FRIAS_name[i]) else paste0("Fila ", i)
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
cat("\n Fill gaps in Locations using IUCN Red List and Worrms \n")
iucnredlist_NativeRecipientRange()
worrms_NativeRecipientRange()
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
Groups_through_RecipientBioregions()
Dates_graphs()
Native_to_Recipient_maps()
Duplicates_uniques_species_databases()
Species_coverage()
Type_download()
Resolved_missing_information()

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                           S E N S I T I V I T Y                              ║
║                              A N A L Y S I S                                 ║
╚══════════════════════════════════════════════════════════════════════════════╝
")
Evaluation_concordance_habitats()
rmarkdown::render(file.path("R", "Validation.Rmd"), output_format = "pdf_document",
                  output_dir = file.path("Validation"))

#OBTENCIÓN EFICIENCIA CODIGO HABITAT
Habitat_DB_vs_GBIF()
rmarkdown::render(file.path("R", "Habitat_DB_vs_GBIF_RMARKDOWN.Rmd"), output_format = "html_document",
                  output_dir = file.path("Validation"))

