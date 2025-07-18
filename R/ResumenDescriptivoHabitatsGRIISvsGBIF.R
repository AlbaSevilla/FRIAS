library(readxl)
sample_10_borrador <- read_excel("Output/Check/CheckHabitat/sample_10%_borrador2.xlsx")

resumen <- aggregate(cbind(ACIERTO_GRIIS, ACIERTO_GBIF) ~ Habitat_GRIIS1, data = sample_10_borrador,
                     FUN = function(x) sum(x, na.rm = TRUE))
casos <- as.data.frame(table(sample_10_borrador$Habitat_GRIIS1))
names(casos) <- c("Habitat_GRIIS1", "Casos")
resumen <- merge(resumen, casos, by = "Habitat_GRIIS1")
resumen$`%_Exito_GRIIS` <- resumen$ACIERTO_GRIIS / resumen$Casos
resumen$`%_Exito_GBIF` <- resumen$ACIERTO_GBIF / resumen$Casos
resumen <- resumen[order(-resumen$Casos), ]


resumen
write_xlsx(resumen, "Output/Check/CheckHabitat/Aciertos_GRIIS_vs_GBIF.xlsx")
