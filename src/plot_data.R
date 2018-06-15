library(ALA4R)
library(magrittr)
library(ggplot2)

# Lake George polygon 
wkt <- "POLYGON((149.25 -34.9,149.25 -35.35,149.5 -35.35,149.5 -34.9,149.25 -34.9))"

# get list of species in the polygon
spclist <- specieslist(wkt=wkt) %>%
  dplyr::arrange(desc(occurrenceCount))
count = spclist$occurrenceCount

# kingdom histogram
barplot(table(spclist$kingdom), main="Lake George Kingdom Distribution")

kingdom_animal <- spclist %>% dplyr::filter(kingdom=="ANIMALIA")
kingdom_plant <- spclist %>% dplyr::filter(kingdom=="Plantae")

# classes histogram
ggplot(kingdom_animal, aes(x = class, fill = class))+
  geom_bar( aes(y = ..count..) )+
  xlab("Classes in ANIMALIA Kingdom")+
  ylab("Number of classes")+
  scale_y_reverse() +
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust=0)) +
  scale_x_discrete(position = "top")

ggplot(kingdom_plant, aes(x = class, fill = class))+
  geom_bar( aes(y = ..count..) )+
  xlab("Classes in Plantae Kingdom")+
  ylab("Number of classes")+
  scale_y_reverse() +
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust=0)) +
  scale_x_discrete(position = "top")


#top100spc <- spclist %>% head(100)
top100spc <- kingdom_plant

d_name_aggr <- c()

d_species <- c()
d_genus <- c()
d_family <- c()
d_order <- c()
d_class <- c()
d_phylum <- c()
d_kingdom <- c()

d_taxon_id_aggr <- c()
d_date_aggr <- c()
d_date_str_aggr <- c()
d_lon_aggr <- c()
d_lat_aggr <- c()
d_group <- c()

for (idx in 1:100) {
  name <- top100spc$commonName[idx]
  species <- top100spc$speciesName[idx]
  genus <- top100spc$genus[idx]
  family <- top100spc$family[idx]
  order <- top100spc$order[idx]
  class <- top100spc$class[idx]
  phylum <- top100spc$phylum[idx]
  kingdom <- top100spc$kingdom[idx]
  
  taxon_id <- top100spc$taxonConceptLsid[idx]
  info <- search_guids(c(taxon_id))
  img <- info$thumbnailUrl
  
  # if (genus_name == "") genus_name <- name
  # if (genus_name == "") genus_name <- spc_name
  # print(sprintf("%s (%s) - %s, rank = %d, occurrence = %d", name, spc_name, genus_name, idx, count[idx]))
  
  filename_remove_slash <- tail(strsplit(taxon_id, "/")[[1]], n=1)
  filename_get_id <- tail(strsplit(filename_remove_slash, ":")[[1]], n=1)
  load(sprintf("%s/data/plant-%d-%s.Rdata", getwd(), idx, filename_get_id))
  
  num_rows = nrow(spc_data)
  d_name_aggr <- c(d_name_aggr, rep(name, num_rows))
  d_taxon_id_aggr <- c(d_taxon_id_aggr, rep(taxon_id, num_rows))
  
  d_species <- c(d_species, rep(species, num_rows))
  d_genus <- c(d_genus, rep(genus, num_rows))
  d_family <- c(d_family, rep(family, num_rows))
  d_order <- c(d_order, rep(order, num_rows))
  d_class <- c(d_class, rep(class, num_rows))
  d_phylum <- c(d_phylum, rep(phylum, num_rows))
  d_kingdom <- c(d_kingdom, rep(kingdom, num_rows))
  
  d_date_aggr <- c(d_date_aggr, spc_data$d_date)
  d_date_str_aggr <- c(d_date_str_aggr, as.character(spc_data$d_date_str))
  d_lon_aggr <- c(d_lon_aggr, spc_data$d_log)
  d_lat_aggr <- c(d_lat_aggr, spc_data$d_lat)

#  if (idx %in% c(3,6,9,10,11,16)) { # ducks + swan
#    d_group <- c(d_group, rep("ducks", num_rows))
#  } else {
#    d_group <- c(d_group, rep("flying birds", num_rows))
#  }
}

d_year_aggr <- as.integer(substr(d_date_str_aggr, 1, 4))
d_month_aggr <- substr(d_date_str_aggr, 1, 7)
spc_aggr <- data.frame(name=d_name_aggr, taxon_id=d_taxon_id_aggr,
                       species=d_species, genus=d_genus, family=d_family, order=d_order, class=d_class, phylum=d_phylum, kingdom=d_kingdom,
                       date=d_date_aggr, date_str=d_date_str_aggr, year=d_year_aggr, month=d_month_aggr,
                       longitude=d_lon_aggr, latitude=d_lat_aggr)

save(spc_aggr, file=sprintf("%s/data/%s.Rdata", getwd(), "plant_aggr"))

month_range <- c()
for (y in 1986:2018) {
  for (m in 1:12) {
    month_range <- c(month_range, sprintf("%d-%02d", y, m))
  }
}

# month plot ducks vs flying birds
ggplot(spc_aggr, aes(x = month, fill = group))+
  geom_bar( aes(y = ..count..*100/sum(..count..) ), position = 'fill')+
  scale_x_discrete(limits=month_range, breaks=month_range[seq(1, length(month_range), 48)])+
  xlab("Time")+
  ylab("Occurrence percentage")

# year plot ducks vs flying birds
ggplot(spc_aggr, aes(x = year, fill = group))+
  geom_bar( aes(y = ..count..*100/sum(..count..) ), position = 'fill')+
  xlim(1986,2018)+
  xlab("Time")+
  ylab("Occurrence percentage")

# year plot species percent
ggplot(spc_aggr, aes(x = year, fill = sprintf("%s (%s)", name, spc_name)))+
  geom_bar( aes(y = ..count..*100/sum(..count..) ), position = 'fill')+
  #geom_bar( aes(y = ..count..*100/sum(..count..) ))+
  #geom_bar( aes(y = ..count.. ))+
  xlim(1986,2018)+
  xlab("Time")+
  ylab("Occurrence percentage")

# year plot genus percent
ggplot(spc_aggr, aes(x = year, fill = genus))+
  geom_bar( aes(y = ..count..*100/sum(..count..) ), position = 'fill')+
  #geom_bar( aes(y = ..count..*100/sum(..count..) ))+
  #geom_bar( aes(y = ..count.. ))+
  xlim(1986,2018)+
  xlab("Time")+
  ylab("Occurrence percentage")

