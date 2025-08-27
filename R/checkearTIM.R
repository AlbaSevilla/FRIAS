#Comprobar que las especies de tim no están ya en la masterlist final
#HORIZON SCANNING EU 1
EUHorizonScanning1 <- read_excel("Databases Tim checkear/Supplementary_Material1.xlsx",sheet = "List widely spread")
EUHorizonScanning1 <- EUHorizonScanning1[-c(1:4),]
colnames(EUHorizonScanning1) <- EUHorizonScanning1[1,]
EUHorizonScanning1 <- EUHorizonScanning1[-1,]
nombres_EUHorizonScanning1 <- EUHorizonScanning1$`Scientific Name`
nombres_EUHorizonScanning1 <- name_backbone_checklist(nombres_EUHorizonScanning1)$canonicalName
nombres_EUHorizonScanning1
#Which are freshwater
source(file.path("R", "check_habitat.r"))
EUHorizonScanning1_frw <- check_habitat(nombres_EUHorizonScanning1, EUHorizonScanning1)
EUHorizonScanning1_frw <- as.data.frame(EUHorizonScanning1_frw)
#Save freshwaters
EUHorizonScanning1_FRESHWATER <- EUHorizonScanning1_frw %>%
  filter(grepl("FRESHWATER", Habitat))
especies_frw_HS1 <- EUHorizonScanning1_FRESHWATER$AcceptedNameGBIF

#HORIZON SCANNING EU 2
EUHorizonScanning2 <- read_excel("Databases Tim checkear/Supplementary_Material2.xlsx")
EUHorizonScanning2
nombres_EUHorizonScanning2 <- EUHorizonScanning2$`Scientific name`
nombres_EUHorizonScanning2 <- name_backbone_checklist(nombres_EUHorizonScanning2)$canonicalName
#Which are freshwater
EUHorizonScanning2_frw <- check_habitat(nombres_EUHorizonScanning2, EUHorizonScanning2)
#Save freshwater
EUHorizonScanning2_FRESHWATER <- EUHorizonScanning2_frw %>%
  filter(grepl("FRESHWATER", Habitat))
especies_frw_HS2 <- EUHorizonScanning2_FRESHWATER$AcceptedNameGBIF

#FRIAS SPECIES LIST MASTERLIST
FRIAS_SpeciesList_MasterList <- read_excel("FinalFiles/FRIAS_SpeciesList_MasterList.xlsx")
nombres_FRIAS <- FRIAS_SpeciesList_MasterList$AcceptedNameGBIF


print("Elements of vector especies_frw_HS1 that are not in vector nombres_FRIAS are:")
ans1 = setdiff(especies_frw_HS1, nombres_FRIAS)
print(ans1)
print("Elements of vector especies_frw_HS2 that are not in vector nombres_FRIAS are:")
ans2 = setdiff(especies_frw_HS2, nombres_FRIAS)
print(ans2)



#####################
#Abrimos FRÍAS data sources y nos quedamos con los nombres
FRIAS_Data_Sources <- read_excel("TablesToStandardize/FRIAS Data Sources (3).xlsx")
FRIAS_Data_Sources_names <- FRIAS_Data_Sources$Dataset_FRIAS_name
#Abrimos harmonization databases
Harmonization_Databases <- read_excel("TablesToStandardize/Harmonization_Databases.xlsx")
Harmonization_Databases_names <- Harmonization_Databases$Dataset_FRIAS_name
print("Elements of vector FRIAS_Data_Sources_names that are not in vector Harmonization_Databases_names are:")
ans1 = setdiff(FRIAS_Data_Sources_names, Harmonization_Databases_names)
print(ans1)
print("Elements of vector Harmonization_Databases_names that are not in vector FRIAS_Data_Sources_names are:")
ans1 = setdiff(Harmonization_Databases_names, FRIAS_Data_Sources_names)
print(ans1)
