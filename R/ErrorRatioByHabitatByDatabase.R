ErrorRatioByHabitatByDatabase <- function(){
  sample_habitat <- read_excel("OutputFiles/Check/CheckHabitat/sample_10%_DB_vs_GBIF_CheckHabitat_NOcoincidentes_borrador3.xlsx")

  DescriptiveTotalTable <- sample_habitat %>%
    dplyr::group_by(Source_Date) %>%
    dplyr::summarise(
      Total_Aciertos_DB = sum(ACIERTO_DB),
      Total_Aciertos_GBIF = sum(ACIERTO_GBIF),
      Total_Registros = n()
    ) %>%
    arrange(desc(Total_Registros))

  write.xlsx(DescriptiveTotalTable, "./OutputFiles/Check/CheckHabitat/DescriptiveTotalTableSample.xlsx")


  DescriptiveTotalTable_by_DataSource <- function(dataset, fuente) {
    dataset %>%
      dplyr::filter(Source_Date == fuente) %>%
      dplyr::group_by(HABITAT_DATABASE) %>%
      dplyr::summarise(
        ACIERTO_DB = sum(ACIERTO_DB),
        ACIERTO_GBIF = sum(ACIERTO_GBIF),
        Casos = n(),
        `%_Exito_DB` = round(ACIERTO_DB / Casos, 4),
        `%_Exito_GBIF` = round(ACIERTO_GBIF / Casos, 4)
      ) %>%
      bind_rows(
        dplyr::summarise(
          .,
          HABITAT_DATABASE = "SUMA:",
          ACIERTO_DB = sum(ACIERTO_DB),
          ACIERTO_GBIF = sum(ACIERTO_GBIF),
          Casos = sum(Casos),
          `%_Exito_DB` = NA,
          `%_Exito_GBIF` = NA
        )
      )
  }

  fuentes <- unique(sample_habitat$Source_Date)

  tablas_combinadas <- lapply(fuentes, function(f) {
    tabla <- DescriptiveTotalTable_by_DataSource(sample_habitat, f)
    tabla <- mutate(tabla, Fuente = f)
    return(tabla)
  })

  tabla_final <- bind_rows(tablas_combinadas) %>%
    relocate(Fuente, .before = HABITAT_DATABASE)

  write.xlsx(tabla_final, "./OutputFiles/Check/CheckHabitat/DescriptivebyDataSourceTableSample.xlsx")

}
