SInAS_NativeRange <- function(){
  #Download
  url <- "https://zenodo.org/records/17727120/files/SInAS_3.1.1.csv?download=1"
  destfile <- "InputFiles/Native_Range_SInAS.csv"
  download.file(url, destfile, mode = "wb")

  #Depurate
  dat <- read.csv("InputFiles/Native_Range_SInAS.csv", sep = " ") %>%
    filter(establishmentMeans == "native") %>%
    filter(grepl("freshwater", habitat))
  dat_nodup <- noduplicates(dat, "taxon")
  FRIAS_masterlist <- read.xlsx("OutputFiles/Intermediate/step8_additionalNativeRecipientRangeWORRMS_masterlist.xlsx")
  FRIAS_masterlist$NativeRange <- gsub("NA; ", "", FRIAS_masterlist$NativeRange)
  nas_nativeranges<- FRIAS_masterlist %>% filter(NativeRange=="NA")
  names_FRIAS <- nas_nativeranges$AcceptedNameGBIF
  names_SINAS <- dat$taxon

  subset_SINAS <- dat_nodup %>%
    filter(taxon %in% names_FRIAS)
  FRIAS_masterlist_fillgaps <- FRIAS_masterlist %>%
    left_join(
      dat_nodup %>% select(taxon, location),
      by = c("AcceptedNameGBIF" = "taxon")
    ) %>%
    mutate(
      filled_from_location = NativeRange == "NA" & !is.na(location),

      NativeRange = if_else(
        filled_from_location,
        location,
        NativeRange
      ),

      Source_Data = if_else(
        filled_from_location,
        if_else(
          is.na(Source_Data) | Source_Data == "",
          "gómez-suárez_et_al_2025",
          paste(Source_Data, "gómez-suárez_et_al_2025", sep = "; ")
        ),
        Source_Data
      )
    ) %>%
    select(-location, -filled_from_location)


  nas_nativeranges_2<- FRIAS_masterlist_fillgaps %>%
    filter(is.na(NativeRange))
  dim(nas_nativeranges_2)


  #Save
  write.xlsx(FRIAS_masterlist_fillgaps,"OutputFiles/Intermediate/step9_additionalNativeRangeSInAS_masterlist.xlsx")
}
