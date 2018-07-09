library(ALA4R)
library(magrittr)
library(ggplot2)

lakegeorge_water <- c(30, 30, 60, 80, 100, 95, 95, 93, 93, 90, 85, 80, 80, 70, 30, 0, 10, 10, 15, 10, 30, 35, 20, 30, 40, 55, 75, 50, 60, 25, 25, 30, 25)
lakeeyre_water <- c(0, 10, 0, 80, 50, 30, 35, 0, 0, 0, 0, 70, 0, 0, 50, 10, 0, 0, 0, 0, 0, 0, 0, 35, 45, 65, 10, 0, 0, 0, 30, 0, 0)
month_range <- c()
for (y in 1986:2018) {
  for (m in 1:12) {
    month_range <- c(month_range, sprintf("%d-%02d", y, m))
  }
}
lims <- as.POSIXct(strptime(c("1986-01-01 00:00", "2018-01-01 00:00"), format = "%Y-%m-%d %H:%M"))

printStats <- function(data) {
  print(sprintf("Number of unique species: %d", length(unique(spc_aggr$species))))
  print(sprintf("Number of unique genus: %d", length(unique(spc_aggr$genus))))
  print(sprintf("Number of unique family: %d", length(unique(spc_aggr$family))))
  print(sprintf("Number of unique order: %d", length(unique(spc_aggr$order))))
  print(sprintf("Number of unique class: %d", length(unique(spc_aggr$class))))
}

drawWater <- function(lakename) {
  if (lakename == "lakegeorge") {
    lake = data.frame(year=c(1986:2018), water=lakegeorge_water)
  } else if (lakename == "lakeeyre") {
    lake = data.frame(year=c(1986:2018), water=lakeeyre_water)
  }
  ggplot()+
    geom_line(data=lake, aes(x=as.POSIXct(as.character(year), format = "%Y"), y=water))+
    scale_x_datetime(limits = lims)+
    xlab("Time")+
    ylab("Water Area")
}

drawWithWater <- function(lakename, datatype) {
  load(sprintf("%s/data/%s/%s_aggr.Rdata", getwd(), lakename, datatype))
  printStats()

  yeardiv = data.frame(table(year=spc_aggr$year))
  orderdiv = data.frame(table(year=spc_aggr$year, order=spc_aggr$order))

  if (lakename == "lakegeorge") {
    lake = data.frame(year=c(1986:2018), water=lakegeorge_water)
  } else if (lakename == "lakeeyre") {
    lake = data.frame(year=c(1986:2018), water=lakeeyre_water)
  }

  # year plot order lines
  ggplot()+
    geom_line(data=orderdiv, aes(x=as.POSIXct(as.character(year), format = "%Y"), y=Freq/yeardiv$Freq*100, color=order, group=order))+
    geom_line(data=lake, aes(x=as.POSIXct(as.character(year), format = "%Y"), y=water)) +
    # geom_line(data=orderdiv[which(orderdiv$order=="Poales"),], aes(x=year, y=Freq/yeardiv$Freq, group=order), position="jitter")+
    scale_x_datetime(limits = lims)+
    scale_y_continuous(sec.axis= sec_axis(~./100, name="Occurence normalized by year"))+
    xlab("Time")+
    ylab("Water")
}

# drawWithWater("lakegeorge", "animal")
# drawWithWater("lakegeorge", "plant")
drawWithWater("lakeeyre", "plant")
drawWithWater("lakeeyre", "bird")

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

