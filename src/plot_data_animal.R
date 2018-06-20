library(ALA4R)
library(magrittr)
library(ggplot2)

load(sprintf("%s/data/lakegeorge/animal_aggr.Rdata", getwd()))

# 1986 to 2018
lakeg_water <- c(0, 0, 20, 80, 100, 95, 95, 93, 93, 92, 85, 80, 70, 60, 20, 0, 10 , 0, 15, 5, 30, 35, 20, 30, 40, 55, 75, 50, 15, 30, 25, 15, 0)
print(length(lakeg_water))
print(length(c(1986:2018)))

month_range <- c()
for (y in 1986:2018) {
  for (m in 1:12) {
    month_range <- c(month_range, sprintf("%d-%02d", y, m))
  }
}

print(sprintf("Number of unique species: %d", length(unique(spc_aggr$species))))
print(sprintf("Number of unique genus: %d", length(unique(spc_aggr$genus))))
print(sprintf("Number of unique family: %d", length(unique(spc_aggr$family))))
print(sprintf("Number of unique order: %d", length(unique(spc_aggr$order))))
print(sprintf("Number of unique class: %d", length(unique(spc_aggr$class))))

yeardiv = data.frame(table(year=spc_aggr$year))
orderdiv = data.frame(table(year=spc_aggr$year, order=spc_aggr$order))
lakegeorge = data.frame(year=c(1986:2018), water=lakeg_water)

print(unique(spc_aggr$order))
lims <- as.POSIXct(strptime(c("1986-01-01 00:00", "2018-01-01 00:00"), 
                            format = "%Y-%m-%d %H:%M"))

# lake george water area
ggplot()+
  geom_line(data=lakegeorge, aes(x=as.POSIXct(as.character(year), format = "%Y"), y=water))+
  scale_x_datetime(limits = lims)+
  xlab("Time")+
  ylab("Water Area")


# year plot order lines
ggplot()+
  geom_line(data=orderdiv, aes(x=as.POSIXct(as.character(year), format = "%Y"), y=Freq/yeardiv$Freq*100, color=order, group=order))+
  geom_line(data=lakegeorge, aes(x=as.POSIXct(as.character(year), format = "%Y"), y=water)) +
  # geom_line(data=orderdiv[which(orderdiv$order=="Poales"),], aes(x=year, y=Freq/yeardiv$Freq, group=order), position="jitter")+
  scale_x_datetime(limits = lims)+
  scale_y_continuous(sec.axis= sec_axis(~./100, name="Occurence normalized by year"))+
  xlab("Time")+
  ylab("Water")


# year plot order lines
order_filter = "ANSERIFORMES"
selected_order <- spc_aggr %>% dplyr::filter(order==order_filter) %>% droplevels
yeardiv = data.frame(table(year=selected_order$year))
specdiv = data.frame(table(year=selected_order$year, species=selected_order$name))
ggplot()+
  geom_line(data=specdiv, aes(x=as.POSIXct(as.character(year), format = "%Y"), y=Freq/yeardiv$Freq*100, color=species, group=species))+
  geom_line(data=lakegeorge, aes(x=as.POSIXct(as.character(year), format = "%Y"), y=water)) +
  ggtitle(order_filter)+
  scale_x_datetime(limits = lims)+
  scale_y_continuous(sec.axis= sec_axis(~./100, name="Occurence normalized by year"))+
  xlab("Time")+
  ylab("Water")



ggplot(orderdiv, aes(x=as.POSIXct(as.character(year), format = "%Y"), y=Freq/yeardiv$Freq, color=order, group=order))+
  geom_line()+
  scale_x_datetime(limits = lims)+
  xlab("Time")+
  ylab("Occurence normalized by year")

# year plot order stacked percent
order_name = "Apiales"
ggplot(spc_aggr[spc_aggr$order == order_name, ], aes(x=as.POSIXct(as.character(year), format = "%Y"), fill = species))+
  geom_bar( aes(y = ..count.. ))+ 
  ggtitle(sprintf("Order: %s", order_name))+
  scale_x_datetime(limits = lims)+
  xlab("Time")+
  ylab("Occurrence percentage")


# year plot order stacked percent
ggplot(spc_aggr, aes(x=as.POSIXct(as.character(year), format = "%Y"), fill = order))+
  geom_bar( aes(y = ..count..*100/sum(..count..) ))+
  scale_x_datetime(limits = lims)+
  xlab("Time")+
  ylab("Occurrence percentage")

