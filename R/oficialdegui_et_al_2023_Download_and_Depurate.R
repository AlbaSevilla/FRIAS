oficialdegui_et_al_2023_Download_and_Depurate <- function(){
  #Download
  #Concern list
  url <- "https://ars.els-cdn.com/content/image/1-s2.0-S0048969723004138-mmc2.xlsx"
  destfile <- "InputFiles/originaldatabase_oficialdegui_et_al_2023_concernlist.xlsx"
  download.file(url, destfile, mode = "wb")

  #Alert list
  url2 <- "https://ars.els-cdn.com/content/image/1-s2.0-S0048969723004138-mmc3.xlsx"
  destfile2 <- "InputFiles/originaldatabase_oficialdegui_et_al_2023_alertlist.xlsx"
  download.file(url2, destfile2, mode = "wb")


  #DEpurate concern list
  concernlist <- read.xlsx(destfile, sheet="Concern List")
  names <- concernlist$Scientific.name
  acceptednames <- name_backbone_checklist(names)$canonicalName
  dat_hab <- check_habitat(acceptednames, concernlist)
  dat_frw <- dat_hab %>%
    filter(grepl("FRESHWATER", Habitat))
  replacements_NR <- c(
    "Eur" = "Europe",
    "Afr" = "Africa",
    "As"  = "Asia-temperate",
    "At"  = "Asia-tropical",
    "Pac" = "Pacific",
    "NAm" = "North America",
    "SAm" = "South America",
    "Ant" = "Antarctica",
    "Aus" = "Australia"
  )

  dat_frw$Native.Range <- stringr::str_replace_all(
    dat_frw$Native.Range,
    replacements_NR
  )
  replacements_pathways <- c(
      "1" = "Release",
      "2" = "Escape",
      "3" = "Contaminant",
      "4" = "Stowaway",
      "5" = "Corridor",
      "6" = "Unaided",
      "7" = "Unknown"
  )
  dat_frw$Main.category.for.pathways.of.introduction <- stringr::str_replace_all(
    dat_frw$Main.category.for.pathways.of.introduction,
    replacements_pathways
  )
  replacements_detailed_pathways <- c(
    # 1. Release
    "BC"    = "Biological Control",
    "EC"    = "Erosion control / dune stabilisation",
    "F"     = "Fishery in the wild",
    "H"     = "Hunting",
    "L"     = "Landscape/flora/fauna improvement in the wild",
    "Cons"  = "Introduction for conservation or wildlife management",
    "Other" = "Other intentional release",

    # 2. Escape
    "Ag"    = "Agriculture",
    "Aq"    = "Aquaculture",
    "BZA"   = "Botanical garden / zoo / aquaria",
    "Pet"   = "Pet / aquarium / terrarium",
    "Farm"  = "Farmed animals",
    "For"   = "Forestry",
    "FF"    = "Fur farm",
    "Hort"  = "Horticulture",
    "Orn"   = "Ornamental (non-horticulture)",
    "Res"   = "Research",
    "Live"  = "Live food and live bait",
    "Other escape" = "Other escape from confinement",

    # 3. Contaminant
    "CNM"      = "Contaminant nursery material",
    "Bait"     = "Contaminated bait",
    "Food"     = "Food contaminant",
    "Con Anim" = "Contaminant on animals",
    "Par Anim" = "Parasites on animals",
    "Con Plant"= "Contaminant on plants",
    "Par Plant"= "Parasites on plants",
    "Seed"     = "Seed contaminant",
    "TT"       = "Timber trade",
    "THM"      = "Transportation of habitat material",

    # 4. Stowaway
    "Ang"      = "Angling / fishing equipment",
    "Container"= "Container / bulk",
    "Air"      = "Hitchhikers on airplane",
    "Ship"     = "Hitchhikers on ship / boat",
    "Mach"     = "Machinery / equipment",
    "Lug"      = "People and luggage / equipment",
    "Org"      = "Organic packing material",
    "Ballast"  = "Ship / boat ballast water",
    "Hull"     = "Ship / boat hull fouling",
    "Veh"      = "Vehicles",
    "Other Transport" = "Other means of transport",

    # 5. Corridor
    "Water" = "Interconnected waterways",
    "Tun"   = "Water tunnels and bridges",

    # 6. Unaided
    "Nat"   = "Natural dispersal across borders",

    # 7. Unknown
    "Unknown" = "Unknown"
  )
  dat_frw$Subcategory.for.pathways.of.introduction <- stringr::str_replace_all(
    dat_frw$Subcategory.for.pathways.of.introduction,
    replacements_detailed_pathways
  )
  dat_frw$RecipientRange <- "Spain"

  #DEpurate alertlist
  alertlist <- read.xlsx(destfile2, sheet="Alert List")
  names2 <- alertlist$Scientific.name
  acceptednames2 <- name_backbone_checklist(names2)$canonicalName
  dat_hab2 <- check_habitat(acceptednames2, alertlist)
  dat_frw2 <- dat_hab2 %>%
    filter(grepl("FRESHWATER", Habitat))
  replacements_NR <- c(
    "Eur" = "Europe",
    "Afr" = "Africa",
    "As"  = "Asia-temperate",
    "At"  = "Asia-tropical",
    "Pac" = "Pacific",
    "NAm" = "North America",
    "SAm" = "South America",
    "Ant" = "Antarctica",
    "Aus" = "Australia"
  )

  dat_frw2$Native.Range <- stringr::str_replace_all(
    dat_frw2$Native.Range,
    replacements_NR
  )
  replacements_pathways <- c(
    "1" = "Release",
    "2" = "Escape",
    "3" = "Contaminant",
    "4" = "Stowaway",
    "5" = "Corridor",
    "6" = "Unaided",
    "7" = "Unknown"
  )
  dat_frw2$Main.category.for.pathways.of.introduction <- stringr::str_replace_all(
    dat_frw2$Main.category.for.pathways.of.introduction,
    replacements_pathways
  )
  replacements_detailed_pathways <- c(
    # 1. Release
    "BC"    = "Biological Control",
    "EC"    = "Erosion control / dune stabilisation",
    "F"     = "Fishery in the wild",
    "H"     = "Hunting",
    "L"     = "Landscape/flora/fauna improvement in the wild",
    "Cons"  = "Introduction for conservation or wildlife management",
    "Other" = "Other intentional release",

    # 2. Escape
    "Ag"    = "Agriculture",
    "Aq"    = "Aquaculture",
    "BZA"   = "Botanical garden / zoo / aquaria",
    "Pet"   = "Pet / aquarium / terrarium",
    "Farm"  = "Farmed animals",
    "For"   = "Forestry",
    "FF"    = "Fur farm",
    "Hort"  = "Horticulture",
    "Orn"   = "Ornamental (non-horticulture)",
    "Res"   = "Research",
    "Live"  = "Live food and live bait",
    "Other escape" = "Other escape from confinement",

    # 3. Contaminant
    "CNM"      = "Contaminant nursery material",
    "Bait"     = "Contaminated bait",
    "Food"     = "Food contaminant",
    "Con Anim" = "Contaminant on animals",
    "Par Anim" = "Parasites on animals",
    "Con Plant"= "Contaminant on plants",
    "Par Plant"= "Parasites on plants",
    "Seed"     = "Seed contaminant",
    "TT"       = "Timber trade",
    "THM"      = "Transportation of habitat material",

    # 4. Stowaway
    "Ang"      = "Angling / fishing equipment",
    "Container"= "Container / bulk",
    "Air"      = "Hitchhikers on airplane",
    "Ship"     = "Hitchhikers on ship / boat",
    "Mach"     = "Machinery / equipment",
    "Lug"      = "People and luggage / equipment",
    "Org"      = "Organic packing material",
    "Ballast"  = "Ship / boat ballast water",
    "Hull"     = "Ship / boat hull fouling",
    "Veh"      = "Vehicles",
    "Other Transport" = "Other means of transport",

    # 5. Corridor
    "Water" = "Interconnected waterways",
    "Tun"   = "Water tunnels and bridges",

    # 6. Unaided
    "Nat"   = "Natural dispersal across borders",

    # 7. Unknown
    "Unknown" = "Unknown"
  )
  dat_frw2$Subcategory.for.pathways.of.introduction <- stringr::str_replace_all(
    dat_frw2$Subcategory.for.pathways.of.introduction,
    replacements_detailed_pathways
  )
  dat_frw2$RecipientRange <- ""


  #Mix both
  mixed <- rbind(dat_frw, dat_frw2)

  #Save
  write.xlsx(mixed, "InputFiles/freshwatersubset_oficialdegui_et_al_2023.xlsx")

}
