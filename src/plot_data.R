library(ALA4R)
library(magrittr)
library(ggplot2)

# Lake George polygon 
wkt <- "POLYGON((149.25 -34.9,149.25 -35.35,149.5 -35.35,149.5 -34.9,149.25 -34.9))"

# get list of species in the polygon
spclist <- specieslist(wkt=wkt) %>%
  dplyr::arrange(desc(occurrenceCount)) %>%
  dplyr::select(taxonConceptLsid, speciesName, genus, commonName, occurrenceCount)
count = spclist$occurrenceCount

top100spc <- spclist %>% head(100)

d_name_aggr <- c()
d_spc_name_aggr <- c()
d_taxon_id_aggr <- c()
d_date_aggr <- c()
d_date_str_aggr <- c()
d_lon_aggr <- c()
d_lat_aggr <- c()
d_group <- c()

for (idx in 1:20) {
  name <- top100spc$commonName[idx]
  spc_name <- top100spc$speciesName[idx]
  taxon_id <- top100spc$taxonConceptLsid[idx]
  info <- search_guids(c(taxon_id))
  img <- info$thumbnailUrl
  print(sprintf("%s (%s) rank = %d, occurrence = %d", name, spc_name, idx, count[idx]))

  filename_remove_slash <- tail(strsplit(taxon_id, "/")[[1]], n=1)
  filename_get_id <- tail(strsplit(filename_remove_slash, ":")[[1]], n=1)
  load(sprintf("~/Work/lakegeorge/data/%d-%s.Rdata", idx, filename_get_id))
  
  num_rows = nrow(spc_data)
  d_name_aggr <- c(d_name_aggr, rep(name, num_rows))
  d_spc_name_aggr <- c(d_spc_name_aggr, rep(spc_name, num_rows))
  d_taxon_id_aggr <- c(d_taxon_id_aggr, rep(taxon_id, num_rows))
  d_date_aggr <- c(d_date_aggr, spc_data$d_date)
  d_date_str_aggr <- c(d_date_str_aggr, as.character(spc_data$d_date_str))
  d_lon_aggr <- c(d_lon_aggr, spc_data$d_log)
  d_lat_aggr <- c(d_lat_aggr, spc_data$d_lat)

  if (idx %in% c(3,6,9,10,11,16)) { # ducks + swan
    d_group <- c(d_group, rep("ducks", num_rows))
  } else {
    d_group <- c(d_group, rep("flying birds", num_rows))
  }
}

d_year_aggr <- as.integer(substr(d_date_str_aggr, 1, 4))
d_month_aggr <- substr(d_date_str_aggr, 1, 7)
spc_aggr <- data.frame(name=d_name_aggr, spc_name=d_spc_name_aggr, taxon_id=d_taxon_id_aggr,
                       date=d_date_aggr, date_str=d_date_str_aggr, year=d_year_aggr, month=d_month_aggr,
                       longitude=d_lon_aggr, latitude=d_lat_aggr, group=d_group)

month_range <- c()
for (y in 1986:2018) {
  for (m in 1:12) {
    month_range <- c(month_range, sprintf("%d-%02d", y, m))
  }
}

ggplot(spc_aggr, aes(x = month, fill = group))+
  geom_bar( aes(y = ..count..*100/sum(..count..) ), position = 'fill')+
  scale_x_discrete(limits=month_range, breaks=month_range[seq(1, length(month_range), 48)])+
  xlab("Time")+
  ylab("Occurrence percentage")

ggplot(spc_aggr, aes(x = year, fill = group))+
  geom_bar( aes(y = ..count..*100/sum(..count..) ), position = 'fill')+
  xlim(1986,2018)+
  xlab("Time")+
  ylab("Occurrence percentage")

ggplot(spc_aggr, aes(x = year, fill = sprintf("%s (%s)", name, spc_name)))+
  geom_bar( aes(y = ..count..*100/sum(..count..) ), position = 'fill')+
  #geom_bar( aes(y = ..count..*100/sum(..count..) ))+
  #geom_bar( aes(y = ..count.. ))+
  xlim(1986,2018)+
  xlab("Time")+
  ylab("Occurrence percentage")

ggplot(spc_aggr, aes(x=year, fill = sprintf("%s (%s)", name, spc_name))) +
  geom_freqpoly()+
  xlim(1986,2018)+
  xlab("Time")+
  ylab("Occurrence percentage")

