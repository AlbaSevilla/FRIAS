masterlist <- read_excel("Outputfiles/Intermediate/Step6_ObtainTaxonNAS_MasterList.xlsx")


source(file.path("R", "noduplicates.r"))
masterlist <- noduplicates(masterlist, "OriginalNameDB")

species <- masterlist$AcceptedNameGBIF
taxonomy_list <- classification(species, db = "ncbi")
ranks <- c("kingdom", "phylum", "class", "order", "family", "species")
taxonomy_df <- map_df(taxonomy_list, function(x) {
  if (!is.data.frame(x)) {
    return(setNames(as.list(rep(NA_character_, length(ranks))), ranks))
  }

  setNames(sapply(ranks, function(r) {
    val <- x$name[x$rank == r]
    if (length(val) > 0) val else NA_character_
  }), ranks)
}, .id = "AcceptedNameGBIF")

masterlist_updated <- masterlist %>%
  left_join(taxonomy_df, by = "AcceptedNameGBIF") %>%
  mutate(
    Kingdom = ifelse(is.na(Kingdom) | Kingdom == "NA", kingdom, Kingdom),
    Phylum  = ifelse(is.na(Phylum)  | Phylum  == "NA", phylum,  Phylum),
    Class   = ifelse(is.na(Class)   | Class   == "NA", class,   Class),
    Order   = ifelse(is.na(Order)   | Order   == "NA", order,   Order),
    Family  = ifelse(is.na(Family)  | Family  == "NA", family,  Family)
  ) %>%
  select(-kingdom, -phylum, -class, -order, -family, -species)

View(masterlist_updated)

cols_interes <- c("Kingdom", "Phylum", "Class", "Order", "Family")
filas_con_NA <- masterlist_updated %>%
  filter(if_any(all_of(cols_interes), ~ is.na(.) | . == "NA"))


#JUNTARIAMOS ESTE METODO CON EL QUE OBTIENE LOS NAS DE TAXONOMIA DE GBIF


#
#
# #Ahora probamos para esas columnas con "eol"
# species <- filas_con_NA$AcceptedNameGBIF
# taxonomy_list <- classification(species, db = "eol")
# ranks <- c("kingdom", "phylum", "class", "order", "family", "species")
# taxonomy_df <- map_df(taxonomy_list, function(x) {
#   if (!is.data.frame(x)) {
#     return(setNames(as.list(rep(NA_character_, length(ranks))), ranks))
#   }
#
#   setNames(sapply(ranks, function(r) {
#     val <- x$name[x$rank == r]
#     if (length(val) > 0) val else NA_character_
#   }), ranks)
# }, .id = "AcceptedNameGBIF")
#
# masterlist_updated2 <- masterlist_updated %>%
#   left_join(taxonomy_df, by = "AcceptedNameGBIF") %>%
#   mutate(
#     Kingdom = ifelse(is.na(Kingdom) | Kingdom == "NA", kingdom, Kingdom),
#     Phylum  = ifelse(is.na(Phylum)  | Phylum  == "NA", phylum,  Phylum),
#     Class   = ifelse(is.na(Class)   | Class   == "NA", class,   Class),
#     Order   = ifelse(is.na(Order)   | Order   == "NA", order,   Order),
#     Family  = ifelse(is.na(Family)  | Family  == "NA", family,  Family)
#   ) %>%
#   select(-kingdom, -phylum, -class, -order, -family, -species)
#
# View(masterlist_updated2)
