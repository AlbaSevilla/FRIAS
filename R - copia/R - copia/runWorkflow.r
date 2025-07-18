###############################################################################
######### FRIAS : Freshwater Invasive Alien Species ###########################
###############         WORKFLOW            ###################################
###############################################################################
graphics.off()
rm(list=ls())

##################################################################################################
## Cargar los paquetes (los descarga automáticamente en el caso en el que no estén instalados) ###
##################################################################################################
cat("\n Instalación y carga de los paquetes necesarios para el Workflow")
source(file.path("R", "Packages_download.r")) #To install and open necessary libraries
Packages_download()


################################################################################
### load functions #######################################################
source(file.path("R","DownloadDatasets.r"))
source(file.path("R", "Dates_of_use_Database.r"))

source(file.path("R","StandardizeDatasets.r"))

source(file.path("R","MergeDatasets.r")) # combine data sets

source(file.path("R","StandardizeLocations.r"))

source(file.path("R", "Obtain_TaxonNAS.r"))

source(file.path("R", "NoMicroorganism_MasterList.r"))

source(file.path("R", "CorrectNamesGBIF.r"))

source(file.path("R", "Final_MasterLists.r"))

source(file.path("R", "FRIAS_DescriptiveVariables.r"))

source(file.path("R","Medidas_Evaluacion.R"))

source(file.path("R","Habitat_DB_vs_GBIF.r"))
source(file.path("R","ErrorRatioByHabitatByDatabase.r"))
################################################################################

################################################################################
######## CARGAMOS LOS ARCHIVOS DE TABLAS TRADUCTORAS ###########################
################################################################################
CorrespondenceTable <- read.xlsx(file.path("TablesToStandardize","DatabasesInformation.xlsx")) #Este es el archivo donde indicamos las columnas de los datasets

####################################################################
####### DESCARGA DE LOS DATASETS ###################################
####################################################################
cat("\n Step 0 : Obtencion de datasets de internet, filtrando para obtener las especies de interés")
DownloadDatasets()
######## Obtenemos las fechas de las consultas de las bases de datos
Dates_of_use_Database()

######################################################################
##### OBTENEMOS Y ESTANDARIZAMOS LOS NOMBRES DE LAS COLUMNAS DE ######
##### INTERÉS EN CADA DATASET, GRACIAS A LA TABLA TRADUCTORA 1 #######
######################################################################
cat("\n Estandarización de las bases de datos y de su impacto EICAT \n")
source(file.path("R","StandardizeDatasets.r"))
CorrespondenceTable2 <- CorrespondenceTable[c(65:76),]
StandardizeDatasets(CorrespondenceTable2)

########################################################################
### UNIÓN DE LAS BASES DE DATOS PARA OBTENER LA FINAL ##################
########################################################################
cat("\n Fusión de las bases de datos utilizadas \n")
source(file.path("R","MergeDatasets.r")) # combine data sets
MergeDatasets()

####################################################################
######## Fill gaps and standardize locations #######################
####################################################################
cat("\n Obtención de los vacíos en rangos nativos gracias a la lista roja de la IUCN \n")
source(file.path("R","iucnredlist_fillgapsNativeRange.r"))
iucnredlist_fillgapsNativeRange()

cat("\n Obtención de los códigos ISO3 de los países y regiones \n")
source(file.path("R","StandardizeLocations.r"))
StandardizeLocations()

####################################################################
####### STANDARDIZE MECHANISM #######################################
####################################################################
source(file.path("R","StandardizeMechanism.r"))
StandardizeMechanism()
#STEP 6

####################################################################
####### STANDARDIZE HABITAT ########################################
####################################################################
source(file.path("R","StandardizeHabitat.r"))
StandardizeHabitat()
#STEP 6

####################################################################
####### STANDARDIZE PATHWAYS #######################################
####################################################################
source(file.path("R", "StandardizePathways.r"))
StandardizePathways()
#STEP 7

####################################################################
######## OBTENCIÓN DE LOS NAS DE TAXONOMÍA #########################
####################################################################
cat("\n Obtención de los valores NAS de Kingdom, Family y Order de la Masterlist \n")
source(file.path("R","Obtain_TaxonNAS.r"))
Obtain_TaxonNAS()
#Step 6
#HE DEJADO SOLO GBIF PORQUE TARDA MUCHO CON TAXIZE Y LO EJECUTO EN EL ORDENADOR DEL IPE

############################################################################
######## NO MICROORGANISM IN MASTERLIST ####################################
############################################################################
source(file.path("R", "NoMicroorganism_MasterList.R"))
NoMicroorganism_MasterList()
#Step7

############################################################################
######## CORRECCIÓN NOMBRES NO ENCONTRADOS EN GBIF, TABLA AUX ##############
############################################################################
source(file.path("R", "CorrectNamesGBIF.r"))
CorrectNamesGBIF()
#Step8

############################################################################
####### OBTAIN FUNCTIONAL GROUP ############################################
############################################################################
source(file.path("R", "Obtain_FunctionalGroup.r"))
Obtain_FunctionalGroup()

############################################################################
####### ExclusionSpeciesbyHabitatGBIF ######################################
############################################################################
source(file.path("R", "ExclusionSpeciesbyHabitatGBIF.r"))
ExclusionSpeciesbyHabitatGBIF()

############################################################################
###### FINAL MASTERLISTS ###################################################
############################################################################
source(file.path("r", "Final_MasterLists.r"))
Final_MasterLists()

############################################################################
######## FRIAS DESCRIPTIVE VARIABLES TABLE  ################################
############################################################################
source(file.path("R", "FRIAS_DescriptiveVariables.r"))
FRIAS_DescriptiveVariables()

############################################################################
######### SANKEY DIAGRAM NATIVE REGIONS -> INVADED REGIONS #################
############################################################################
source(file.path("R", "SankeyDiagramBioregions.r"))
SankeyDiagramBioregions()

############################################################################
####### GROWTH OF INVASIVE SPECIES #########################################
############################################################################
source(file.path("R", "DatesGraphs.r"))
DatesGraphs()

#################################################################################
###### OBTENCIÓN DEL ANÁLISIS DE SENSIBILIDAD EN LA OBTENCIÓN DEL HÁBITAT #######
#################################################################################
cat("\n Se procede a obtener el análisis de sensibilidad en la obtención del habitat
    'Freshwater' en las bases de datos de las que sí se tiene a priori este campo,
    para ello, se ha aplicado este análisis a las bases de datos: \n")
cat("\n Amphibians and Reptiles by César Capinha \n")
cat("\n USGS \n")
cat("\n Invasive non-native species in Brazil by Rafael D. Zenni \n")
Medidas_Evaluacion()
rmarkdown::render(file.path("R", "Evaluacion.Rmd"), output_format = "pdf_document",
                  output_dir = file.path("Evaluacion"))

###########################################################
######### OBTENCIÓN EFICIENCIA CODIGO HABITAT #############
###########################################################
Habitat_DB_vs_GBIF()
rmarkdown::render(file.path("R", "Habitat_DB_vs_GBIF_RMARKDOWN.Rmd"), output_format = "html_document",
                  output_dir = file.path("OutputFiles/Check/CheckHabitat"))

