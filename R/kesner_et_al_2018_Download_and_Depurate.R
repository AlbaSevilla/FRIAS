kesner_et_al_2018_Download_and_Depurate <- function(){
  #Download
  #manually
  texto <- "Species	EICAT maximum impact	Mechanism(s)	EICAT confidence	EICAT no. publications	SEICAT maximum impact	Constituent(s) of human well-being	SEICAT confidence	SEICAT no. publications
              Helisoma duryi	MR	Competition	Low	12	DD
              Oxychilus draparnaudi	MR	Predation	Low	6	DD
              Tarebia granifera	MR	Competition	Low	36	MN	Material and immaterial goods for good life	Low	2
              Theba pisana	MR	Competition	Low	7	MO	Material and immaterial goods for good life	Low	24
              Deroceras laeve	MO	Grazing/herbivory/browsing	Low	8	MN	Health; Material and immaterial goods for good life; Social, spiritual and cultural relations	High	9
              Deroceras panormitanum	MO	Grazing/herbivory/browsing; Predation	Low	5	MN	Material and immaterial goods for good life	High	10
              Lehmannia nyctelia	MO	Grazing/herbivory/browsing	Low	3	DD
              Limacus flavus	MO	Grazing/herbivory/browsing	Low	3	MN	Material and immaterial goods for good life	Medium	5
              Limax maximus	MO	Grazing/herbivory/browsing; Competition	Low	7	MN	Health; Material and immaterial goods for good life; Social, spiritual and cultural relations	Low	9
              Milax gagates	MO	Grazing/herbivory/browsing; Competition	Low	6	MN	Material and immaterial goods for good life	High	12
              Oxychilus alliarius	MO	Predation	Low	9	MN	Health	Low	1
              Oxychilus cellarius	MO	Predation	Low	4	DD
              Aplexa marmorata	MN	Competition	Low	1	MN	Health	Low	1
              Arion intermedius	MN	Grazing/herbivory/browsing; Competition	Low	3	MN	Material and immaterial goods for good life	Medium	5
              Bradybaena similaris	MN	Transmission of diseases to native species	Low	3	MN	Health; Material and immaterial goods for good life	High	18
              Cochlicella barbara	MN	Grazing/herbivory/browsing	Low	5	MO	Material and immaterial goods for good life	Medium	12
              Cornu aspersum	MN	Transmission of diseases to native species	Low	3	MN	Health; Material and immaterial goods for good life; Social, spiritual and cultural relations	High	11
              Deroceras reticulatum	MN	Grazing/herbivory/browsing; Transmission of diseases to native species; Predation	Low	9	MN	Health; Material and immaterial goods for good life; Social, spiritual and cultural relations	High	35
              Lehmannia valentiana	MN	Transmission of diseases to native species; Predation	Medium	3	MN	Health; Material and immaterial goods for good life	Medium	7
              Littorina saxatilis	MN	Predation	Low	1	DD
              Lymnaea columella	MN	Competition	Low	2	MN	Material and immaterial goods for good life	Low	9
              Physa acuta	MN	Transmission of diseases to native species; Competition	Low	3	MN	Health; Material and immaterial goods for good life	Low	3
              Radix rubiginosa	MN	Transmission of diseases to native species	Low	2	MC	Social, spiritual and cultural relations	Low	1
              Vallonia costata	MN	Transmission of diseases to native species	Low	1	DD
              Vallonia pulchella	MC	Transmission of diseases to native species	Low	3	MC	Material and immaterial goods for good life	Low	2
              Zonitoides arboreus	MC	Transmission of diseases to native species	Low	2	MN	Health; Material and immaterial goods for good life; Social, spiritual and cultural relations	High	11
              Arion hortensis	DD				MN	Material and immaterial goods for good life	High	15
              Eobania vermiculata	DD				MO	Material and immaterial goods for good life	Medium	7
              Gyraulus chinensis	DD				DD
              Discus rotundatus	DD				DD
              Thais blanfordi	DD				DD
              Lauria cylindracea	DD				DD
              Cochlicopa cf. lubricella	DD				DD
              Cochlicopa cf. lubrica	DD				DD
              "
  table <- read.table(text = texto, header = TRUE, sep = "\t", stringsAsFactors = FALSE, fill = TRUE)
  write.xlsx(table, "InputFiles/originaldatabase_kesner_et_al_2018.xlsx")

  #Depurate
  dat <- read.xlsx("InputFiles/originaldatabase_kesner_et_al_2018.xlsx")
  datNR <- read_docx("InputFiles/originaldatabase_kesner_et_al_2018_nativeranges.docx")
  content <- docx_summary(datNR)
  paragraphs <- content %>%
    filter(content_type == "table cell") %>%
    select(text)
  dataset <- as.data.frame(paragraphs, stringsAsFactors = FALSE)
  dataset <- dataset %>%
    mutate(col_block = sub("\\..*", "", row.names(dataset)))
  dataset <- dataset %>%
    group_by(col_block) %>%
    mutate(row_id = row_number()) %>%
    ungroup()
  dataset_wide <- dataset %>%
    pivot_wider(names_from = col_block, values_from = text)
  colnames(dataset_wide) <- dataset_wide[1,]
  dataset_wide <- dataset_wide[-1,]
  dataset_wide <- as.data.frame(dataset_wide)
  dataset_wide <- dataset_wide[,c(2:3)]
  merged <- merge(dat, dataset_wide, by="Species", all=TRUE)
  merged <- merged %>% filter(Species != "")
  merged <- merged %>%
    mutate(
      EICAT_by_mechanism = if_else(
        !is.na(EICAT.maximum.impact) & EICAT.maximum.impact != "" &
          !is.na(Mechanism.s.) & Mechanism.s. != "",
        str_c(EICAT.maximum.impact, Mechanism.s., sep = "-"),
        NA_character_
      )
    )

  noduplicates_merged <- noduplicates(merged, "Species")
  dataset <- noduplicates_merged
  names <- dataset$Species
  acceptedname <- name_backbone_checklist(names)$canonicalName
  dat_hab <- check_habitat(acceptedname, dataset)
  dat_frw <- dat_hab %>% filter(grepl("FRESHWATER", Habitat))
  dat_frw$RecipientRange <- "South Africa"
  names <- dat_frw$Species
  dat_frw$kingdom <- name_backbone_checklist(names)$kingdom
  dat_frw$order <- name_backbone_checklist(names)$order
  dat_frw$family <- name_backbone_checklist(names)$family
  dat_frw$phylum <- name_backbone_checklist(names)$phylum
  dat_frw$class <- name_backbone_checklist(names)$class

  write.xlsx(dat_frw, "InputFiles/freshwatersubset_kesner_et_al_2018.xlsx")
}
