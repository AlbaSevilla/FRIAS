taxonomy_query <- function(nombre_especie) {
  backbone <- name_backbone_checklist(name = nombre_especie)
  list(
    AcceptedNameGBIF = nombre_especie,
    Kingdom = if (!is.null(backbone$kingdom)) backbone$kingdom else NA,
    Order = if (!is.null(backbone$order)) backbone$order else NA,
    Family = if (!is.null(backbone$family)) backbone$family else NA,
    Class = if (!is.null(backbone$class)) backbone$class else NA,
    Phylum = if (!is.null(backbone$phylum)) backbone$phylum else NA
  )
}
