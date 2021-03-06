library(ALA4R)
library(magrittr)

# lakename = "lakegeorge"
lakename = "lakeeyre"

# Lake George polygon
# wkt <- "POLYGON((149.25 -34.9,149.25 -35.35,149.5 -35.35,149.5 -34.9,149.25 -34.9))"

# Lake Eyre polygon
wkt <- "POLYGON((136.5 -27.7,138.3 -27.7,138.3 -29.6,136.5 -29.6,136.5 -27.7))"

# get list of species in the polygon
spclist <- specieslist(wkt=wkt) %>%
  dplyr::arrange(desc(occurrenceCount)) #%>%
  #dplyr::select(taxonConceptLsid, speciesName, genus, commonName, occurrenceCount)
count = spclist$occurrenceCount

kingdom_animal <- spclist %>% dplyr::filter(kingdom=="ANIMALIA")
kingdom_plant <- spclist %>% dplyr::filter(kingdom=="Plantae")
cat("Total occurrence count", sum(count))

class_birds <- kingdom_animal %>% dplyr::filter(class=="AVES")

#top100spc <- spclist %>% head(100)
# top100spc <- kingdom_plant
# top100spc <- kingdom_animal
top100spc <- class_birds
str(top100spc$taxonConceptLsid)
str(top100spc$genus)
str(top100spc$speciesName)

# obs_records <- occurrences(wkt=wkt, fields=c("id", "latitude", "longitude", "taxon_concept_lsid", "common_name"), download_reason_id="testing")
# summary(obs_records)

# idx <- 2
for (idx in 1:10){
  print(idx)
  name <- top100spc$commonName[idx]
  spc_name <- top100spc$speciesName[idx]
  taxon_id <- top100spc$taxonConceptLsid[idx]
  info <- search_guids(c(taxon_id))
  img <- info$thumbnailUrl
  
  print(sprintf("Searching record of %s (%s) %s", name, spc_name, taxon_id))
  obs <- occurrences(taxon=paste("taxon_concept_lsid:",taxon_id), wkt=wkt, fields=c("id", "latitude", "longitude", "taxon_concept_lsid", "common_name"), download_reason_id="testing")
  record_ids <- obs$data$id
  print(sprintf("Number of records = %d", length(record_ids)))
  
  d_date <- c()
  d_date_str <- c()
  d_log <- c()
  d_lat <- c()
  
  r_idx <- 1
  chunk <- 10
  while (r_idx <= length(record_ids)) {
  #while (r_idx < 10) { #for test
    print(sprintf("Searching record number %d ~ %d", r_idx, min(length(record_ids), (r_idx+chunk))))
    details <- try(occurrence_details(record_ids[r_idx:min(length(record_ids), (r_idx+chunk))]))
    if (class(details) == "try-error") {
      print("SERVER ERROR with the request")
      next;
    }
    for (entity in details) {
      #print(entity$processed$event$eventDate)
      if (is.null(entity$processed$event$eventDate)) {
        print(paste("DATE ERROR invalid date type", entity$processed$event$eventDate))
      } else if (is.na(entity$processed$location$decimalLongitude)){
        print("NO LOCATION ERROR")
      } else {
        eventDate <- entity$processed$event$eventDate
        d_date <- c(d_date, as.integer(gsub("-", "", eventDate)))
        d_date_str <- c(d_date_str, eventDate)
        d_log <- c(d_log, entity$processed$location$decimalLongitude)
        d_lat <- c(d_lat, entity$processed$location$decimalLatitude)
      }
    }
    r_idx <- (r_idx+chunk+1)
  }
  
  spc_data <- data.frame(d_date, d_date_str, d_log, d_lat)
  filename_remove_slash <- tail(strsplit(taxon_id, "/")[[1]], n=1)
  filename_get_id <- tail(strsplit(filename_remove_slash, ":")[[1]], n=1)
  # save(spc_data, file=sprintf("%s/data/%s/plant-%d-%s.Rdata", getwd(), lakename, idx, filename_get_id))
  save(spc_data, file=sprintf("%s/data/%s/bird-%d-%s.Rdata", getwd(), lakename, idx, filename_get_id))
}
