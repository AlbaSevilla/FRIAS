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
source(file.path("R", "Downloads_and_Dates_Databases.r"))
source(file.path("R","HarmonizationDatabases.r"))
source(file.path("R","MergeDatasets.r")) # combine data sets
source(file.path("R","iucnredlist_fillgapsNativeRange.r"))
source(file.path("R", "naturalearth_fillgapsLocations.r"))
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
source(file.path("r", "Final_MasterLists.r"))

source(file.path("R", "FRIAS_DescriptiveVariables.r"))
source(file.path("R", "SankeyDiagramBioregions.r"))
source(file.path("R", "DatesGraphs.r"))
source(file.path("R", "FigureNativeandInvadedCountry.r"))
source(file.path("R", "StackedBarChartSpeciesacrossBioregionsbyPathways.r"))
source(file.path("R", "FreshwaterAlienSpeciesCoverageComparative.r"))
source(file.path("R","Medidas_Evaluacion.R"))
source(file.path("R","SensitivityAnalysisCheckHabitat.r"))
source(file.path("R","ErrorRatioByHabitatByDatabase.r"))
source(file.path("R", "TreeMapSourcesvsHowDownloaded.r"))
source(file.path("R", "GapsPerColumn.R"))
source(file.path("R", "SuccessandFailRateofCheckHabitatFunction.r"))

cat("
╔════════════════════════════════════════════════════════════════════════════╗
║          Loading correspondence table files                                ║
╚════════════════════════════════════════════════════════════════════════════╝
")
CorrespondenceTable <- read.xlsx(file.path("TablesToStandardize","Harmonization_Databases.xlsx")) #Este es el archivo donde indicamos las columnas de los datasets



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
  message("Preparando ", i, "/", n, ": ", id)
  suppressMessages(HarmonizationDatabases(CorrespondenceTable[i, , drop = FALSE]))
  ok <- c(ok, id)
}, error = function(e) errores[[id]] <<- conditionMessage(e))

message(if (length(errores))
  paste("Fallaron", length(errores), "de", n, ":\n-",
        paste(sprintf("%s: %s", names(errores), unlist(errores)), collapse = "\n- "))
  else "Todo se ejecutó sin errores ✅")


cat("
╔══════════════════════════════════════════════════════════════════╗
║           Data merging into a unique masterlist                  ║
╚══════════════════════════════════════════════════════════════════╝
")
MergeDatasets()


cat("
╔════════════════════════════════════════════════════════════════════╗
║                       F I L L   G A P S                            ║
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

#REORDENADO

##
cat("\n Gaps in Accepted Name GBIF using correspondece table \n")
CorrectNamesGBIF()
cat("\n Removal of incorrectly classified freshwater species from the original database with GBIF \n")
ExclusionSpeciesbyHabitatGBIF()
###
cat("\n Obtain NAS values for taxonomy \n")
source(file.path("R", "Obtain_TaxonNAS.R"))
Obtain_TaxonNAS()
cat("\n Standardize Informal Group \n")
StandardizeInformalGroup()
cat("\n Fill NAS in Functional Group \n")
Obtain_FunctionalGroup()
##
cat("\n Fill gaps in native ranges using IUCN Red List \n")
iucnredlist_fillgapsNativeRange()
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


cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                   F I N A L    M A S T E R L I S T S                         ║
╠══════════════════════════════════════════════════════════════════════════════╣
║ • Final Species List, 1 row 1 Species                                        ║
║ • Final Masterlist with differents impacts in each row for same species      ║
╚══════════════════════════════════════════════════════════════════════════════╝
")
Final_MasterLists()



cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                           D E S C R I P T I V E                              ║
║                              A N A L Y S I S                                 ║
╠══════════════════════════════════════════════════════════════════════════════╣
║ • FRIAS descriptive variables table                                          ║
║ • Sankey Diagram, native regions -> invaded regions                          ║
║ • Graph of growth of invasive species                                        ║
║ • Sensibility analysis to check obtain habitat function                      ║
╚══════════════════════════════════════════════════════════════════════════════╝
")
FRIAS_DescriptiveVariables()
SankeyDiagramBioregions()
FigureNativeandInvadedCountry()
StackedBarChartSpeciesacrossBioregionsbyPathways()
FreshwaterAlienSpeciesCoverageComparative()
TreeMapSourcesvsHowDownloaded()
GapsPerColumn()
SuccessandFailRateofCheckHabitatFunction()

cat("\n Se procede a obtener el análisis de sensibilidad en la obtención del habitat
    'Freshwater' en las bases de datos de las que sí se tiene a priori este campo,
    para ello, se ha aplicado este análisis a las bases de datos: \n")
cat("\n Amphibians and Reptiles by César Capinha \n")
cat("\n USGS \n")
cat("\n Invasive non-native species in Brazil by Rafael D. Zenni \n")
Medidas_Evaluacion()
rmarkdown::render(file.path("R", "Validation.Rmd"), output_format = "pdf_document",
                  output_dir = file.path("Validation"))

#OBTENCIÓN EFICIENCIA CODIGO HABITAT
Habitat_DB_vs_GBIF()
rmarkdown::render(file.path("R", "Habitat_DB_vs_GBIF_RMARKDOWN.Rmd"), output_format = "html_document",
                  output_dir = file.path("Validation"))

