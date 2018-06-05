library(ALA4R)
library(magrittr)

# Lake George polygon 
wkt <- "POLYGON((149.25 -34.9,149.25 -35.35,149.5 -35.35,149.5 -34.9,149.25 -34.9))"

# get list of species in the polygon
spclist <- specieslist(wkt=wkt) %>%
  dplyr::arrange(desc(occurrenceCount)) %>%
  dplyr::select(taxonConceptLsid, speciesName, genus, commonName, occurrenceCount)
count = spclist$occurrenceCount
#print(count)
cat("Total occurrence count", sum(count))

top10spc <- spclist %>% head(100)
str(top10spc$taxonConceptLsid)
str(top10spc$genus)
str(top10spc$speciesName)

obs_records <- occurrences(wkt=wkt, fields=c("id", "latitude", "longitude", "taxon_concept_lsid", "common_name"), download_reason_id="testing")
summary(obs_records)

# get Crimson Rosella (idx=1) for example
idx <- 1

name <- top10spc$commonName[idx]
spc_name <- top10spc$speciesName[idx]
taxon_id <- top10spc$taxonConceptLsid[idx]
info <- search_guids(c(taxon_id))
img <- info$thumbnailUrl

print(sprintf("Searching record of %s (%s) %s", name, spc_name, taxon_id))
obs <- occurrences(taxon=paste("taxon_concept_lsid:",taxon_id), wkt=wkt, fields=c("id", "latitude", "longitude", "taxon_concept_lsid", "common_name"), download_reason_id="testing")
record_ids <- obs$data$id

spc_data <- c()
print(sprintf("Number of records = %d", length(record_ids)))

r_idx <- 1
while (r_idx < length(record_ids)) {
  print(sprintf("Searching record number %d ~ %d", r_idx, (r_idx+10)))
  details <- occurrence_details(record_ids[r_idx:(r_idx+10)])
  for (entity in details) {
    if (is.na(entity)) break
    strdate <- strsplit(entity$raw$event$eventDate, "/")
    d <- strdate[[1]][1]
    eventDate <- as.Date(d, "%Y-%m-%d")
    if (is.na(eventDate)) eventDate <- as.Date(d, "%a %b %d 00:00:00 EST %Y")
    if (is.na(eventDate)) {
      print(paste("ERROR date type", entity$raw$event$eventDate))
    } else {
      #spc_data[[length(spc_data)+1]] <- list(eventDate, name, spc_name, taxon_id, entity$raw$location$decimalLatitude, entity$raw$location$decimalLongitude)
      spc_data[[length(spc_data)+1]] <- eventDate
    }
  }
  r_idx <- (r_idx+11)
}
filename <- sprintf("~/Work/lakegeorge/data/%s.Rdata", tail(strsplit(taxon_id, "/")[[1]], n=1))
save(spc_data, file=filename)


hist(spc_data, breaks=100)

load(file="2899343.Rdata")
hist(spc_data, breaks=100)
