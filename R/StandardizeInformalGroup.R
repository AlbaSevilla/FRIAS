StandardizeInformalGroup <- function() {

  equivs <- read_excel("TablesToStandardize/Table S2.xlsx",
                       sheet = "2. Informal group", col_names = TRUE) %>%
    mutate(
      Informal_group = tolower(Informal_group),
      Levels = tolower(Levels),
      Keywords = str_to_lower(str_trim(Keywords))
    ) %>%
    mutate(Keywords = str_split(Keywords, ";")) %>%
    unnest(Keywords) %>%
    mutate(Keywords = str_trim(Keywords)) %>%
    arrange(factor(Levels, levels = c("phylum", "class", "order", "family", "group")))
  dat <- read.csv("OutputFiles/Intermediate/step5_obtaintaxonnas_masterlist.csv")
  dat <- dat %>%
    rename(
      phylum = Phylum,
      kingdom = Kingdom,
      class = Class,
      order = Order,
      family = Family
    )
  cols_to_lower <- setdiff(names(dat), c("AcceptedNameGBIF", "OriginalNameDB", "Source_Data"))
  dat <- dat %>%
    mutate(across(all_of(cols_to_lower), ~str_to_lower(as.character(.))))
  dat <- dat %>%
    mutate(
      group = Group %>%
        str_replace_all("[_/\\-]", "") %>%   # eliminar caracteres especiales
        str_split(";|,")                     # separar por ; o ,
    ) %>%
    unnest(group) %>%
    mutate(group = str_trim(group)) %>%
    select(-Group) %>%                        # eliminar columna original
    mutate(Informal_group = NA_character_)    # crear columna vacía

  hierarchy <- c("phylum", "class", "order", "family", "group")
  tiers <- hierarchy[hierarchy %in% colnames(dat)]
  for (level in tiers) {
    cat("Processing level:", level, "\n")
    sub_equivs <- equivs %>% filter(Levels == level)

    dat <- dat %>%
      rowwise() %>%
      mutate(
        Informal_group = if_else(
          # Solo asigna si aún no tiene grupo asignado
          is.na(Informal_group) &
            get(level) %in% sub_equivs$Keywords,
          sub_equivs$Informal_group[match(get(level), sub_equivs$Keywords)],
          Informal_group
        )
      ) %>%
      ungroup()
  }

  #No duplicates, merge
  MasterlistStandardized <- noduplicates(dat, "AcceptedNameGBIF")

  #Finally:
  MasterlistStandardized <- MasterlistStandardized[,c("OriginalNameDB","AcceptedNameGBIF","ID_GBIF","kingdom","phylum","class","order",
                                  "family","establishmentMeans","pathway","FunctionalGroup", "group","Informal_group",
                                  "NativeRange","AffectedTaxa","EICATImpact",
                                  "Mechanisms","EICAT_by_Mechanism","Habitat","ReportYear","EarliestReport",
                                  "RecipientRange","RecipientRangeISO3","Source_Data")]
  MasterlistStandardized <- MasterlistStandardized %>%
    rename(Group = Informal_group) %>%
    rename(Kingdom = kingdom)%>%
    rename(phylum = phylum)%>%
    rename(Class = class)%>%
    rename(Order = order)%>%
    rename(Family = family)
  cols <- setdiff(names(MasterlistStandardized), "Source_Data")
  MasterlistStandardized[cols] <- lapply(MasterlistStandardized[cols], function(x) {
    x[x == "" | x == "NA"] <- NA
    x
  })
  capitalize <- function(x) {
    x <- tolower(x)                         # todo en minúsculas
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))   # primera letra mayúscula
    x
  }
  cols <- c("Group", "Kingdom", "phylum", "Class", "Order", "Family")
  MasterlistStandardized[cols] <- lapply(MasterlistStandardized[cols], capitalize)
  #NA cases
  MasterlistStandardized_NAS <- MasterlistStandardized %>% filter(is.na(Group))

  #Save
  write_xlsx(MasterlistStandardized, "OutputFiles/Intermediate/step6_standardizedinformalgroup_masterlist.xlsx")
  write.csv(MasterlistStandardized, "OutputFiles/Intermediate/step6_standardizedinformalgroup_masterlist.csv")
  write_xlsx(MasterlistStandardized_NAS, "OutputFiles/Check/NA_Informalgroup_masterlist.xlsx")
  write.csv(MasterlistStandardized_NAS, "OutputFiles/Check/NA_Informalgroup_masterlist.csv")
}
