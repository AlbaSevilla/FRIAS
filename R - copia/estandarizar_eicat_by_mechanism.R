GIDIAS_dataset <- read_excel("Inputfiles/Step0_OriginalDatabaseFreshwaterNODUPLICATES_GIDIAS.xlsx")

GIDIAS_dataset <- GIDIAS_dataset %>%
  select(IAS_Species_Name, EICAT_by_Mechanism)

GIDIAS_dataset <- GIDIAS_dataset %>%
  mutate(EICAT_by_Mechanism = str_split(EICAT_by_Mechanism, ",")) %>%
  unnest(EICAT_by_Mechanism) %>%
  mutate(
    EICAT_by_Mechanism = tolower(str_trim(EICAT_by_Mechanism)))

unique(GIDIAS_dataset$EICAT_by_Mechanism)

#SEPARAMOS LA COLUMNA EICAT_by_Mechanism
GIDIAS_dataset <- GIDIAS_dataset %>%
  mutate(EICAT_by_Mechanism = ifelse(is.na(EICAT_by_Mechanism), NA, EICAT_by_Mechanism)) %>%
  separate(
    EICAT_by_Mechanism,
    into = c("eicat", "mecanismo"),
    sep = " - ",
    fill = "right",
    remove = TRUE
  )


#################################################
############ ESTANDARIZAMOS EICAT ###############
#################################################
equivs <- read_excel("TablesToStandardize/Standardization_EICATImpact.xlsx", sheet = 1, col_names = TRUE) %>%
  mutate(
    FileName = FileName,
    OriginalCategories = tolower(trimws(OriginalCategories)),
    StandardizedCategoriesEICAT = trimws(StandardizedCategoriesEICAT)
  )

# Convertir a minúsculas para facilitar el match
GIDIAS_dataset$eicat <- tolower(GIDIAS_dataset$eicat)

# Obtener nombre del archivo actual
data_name <- "Inputfiles\\Step0_OriginalDatabaseFreshwaterNODUPLICATES_GIDIAS.xlsx"

# Filtrar equivalencias correspondientes al archivo actual
equivs_filtered <- equivs %>%
  dplyr::filter(FileName == data_name)

# Reemplazar valores usando match
match_idx <- match(trimws(GIDIAS_dataset$eicat), equivs_filtered$OriginalCategories)
reemplazos <- equivs_filtered$StandardizedCategoriesEICAT[match_idx]

# Solo reemplazar donde haya coincidencia
GIDIAS_dataset$eicat[!is.na(reemplazos)] <- reemplazos[!is.na(reemplazos)]


GIDIAS_dataset <- GIDIAS_dataset %>%
  mutate(eicat = str_split(eicat, ",")) %>%
  unnest(eicat) %>%
  mutate(
    eicat = tolower(str_trim(eicat)))


#####################################################################################
####################### ESTANDARIZAMOS EL MECANISMO #################################
#####################################################################################

# Leer archivo de equivalencias
equivs <- read_excel("TablesToStandardize/Standardization_Mechanisms.xlsx", sheet = 1, col_names = TRUE) %>%
  mutate(
    OriginalCategories = tolower(trimws(OriginalCategories)),
    StandardizedCategoriesMechanisms = trimws(StandardizedCategoriesMechanisms)
  )

# Leer datos desde archivo
dat <- GIDIAS_dataset

# Separar mecanismos múltiples, limpiar
dat <- dat %>%
  mutate(mecanismo = str_split(mecanismo, ",")) %>%
  unnest(mecanismo) %>%
  mutate(
    mecanismo = tolower(str_trim(mecanismo)))

dat$mecanismo <- gsub("[_/\\-]", "", dat$mecanismo)

# Hacer match con equivalencias
match_idx <- match(dat$mecanismo, equivs$OriginalCategories)
reemplazos <- equivs$StandardizedCategoriesMechanisms[match_idx]

# Reemplazar solo donde hay match
dat$mecanismo[!is.na(reemplazos)] <- reemplazos[!is.na(reemplazos)]
dat$mecanismo <- tolower(dat$mecanismo)

#volvemos a unir las columnas y las borramos
dat <- dat %>%
  unite("EICAT_by_Mechanism", eicat, mecanismo, sep = " - ", remove = FALSE) %>%
  select(-eicat,-mecanismo)

#noduplicados
source(file.path("R", "noduplicates.r"))
dat2 <- noduplicates(dat, "IAS_Species_Name")

write_xlsx(noduplicados, "OutputFiles/Intermediate/Step5_Standardizedmecanismo_Masterlist.xlsx")
