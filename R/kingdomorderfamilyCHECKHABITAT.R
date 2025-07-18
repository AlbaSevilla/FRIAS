sample_GRIIS_GBIF_habitat <- read_excel("OutputFiles/Check/CheckHabitat/sample_10%_GRIIS_GBIF_NOcoincidentes_borrador.xlsx")
nombres <- sample_GRIIS_GBIF_habitat$AcceptedNameGBIF
sample_GRIIS_GBIF_habitat$kingdom <- name_backbone_checklist(nombres)$kingdom
sample_GRIIS_GBIF_habitat$phylum <- name_backbone_checklist(nombres)$phylum
sample_GRIIS_GBIF_habitat$order <- name_backbone_checklist(nombres)$order
sample_GRIIS_GBIF_habitat$family <- name_backbone_checklist(nombres)$family
sample_GRIIS_GBIF_habitat$genus <- name_backbone_checklist(nombres)$genus
write_xlsx(sample_GRIIS_GBIF_habitat, "OutputFiles/Check/CheckHabitat/sample_10%_GRIIS_GBIF_NOcoincidentes_borrador2.xlsx")


sample_db_GBIF_habitat <- read_excel("OutputFiles/Check/CheckHabitat/sample_10%_DB_vs_GBIF_CheckHabitat_NOcoincidentes.xlsx")
nombres <- sample_db_GBIF_habitat$AcceptedNameGBIF
sample_db_GBIF_habitat$kingdom <- name_backbone_checklist(nombres)$kingdom
sample_db_GBIF_habitat$phylum <- name_backbone_checklist(nombres)$phylum
sample_db_GBIF_habitat$order <- name_backbone_checklist(nombres)$order
sample_db_GBIF_habitat$family <- name_backbone_checklist(nombres)$family
sample_db_GBIF_habitat$genus <- name_backbone_checklist(nombres)$genus
write_xlsx(sample_db_GBIF_habitat, "OutputFiles/Check/CheckHabitat/sample_10%_DB_vs_GBIF_CheckHabitat_NOcoincidentes_borrador2.xlsx")


