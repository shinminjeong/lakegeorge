library(ALA4R)
library(magrittr)
library(ggplot2)

load("~/Work/lakegeorge/data/plant_aggr.Rdata")

month_range <- c()
for (y in 1986:2018) {
  for (m in 1:12) {
    month_range <- c(month_range, sprintf("%d-%02d", y, m))
  }
}

print(sprintf("Number of unique species: %d", length(unique(spc_aggr$spc_name))))
print(sprintf("Number of unique genera: %d", length(unique(spc_aggr$genus))))
print(sprintf("Number of unique families: %d", length(unique(spc_aggr$family))))
print(sprintf("Number of unique orders: %d", length(unique(spc_aggr$order))))

yeardiv = data.frame(table(spc_aggr$year))
orderdiv = data.frame(table(spc_aggr$year, spc_aggr$order))

# year plot order lines
ggplot(orderdiv, aes(x=Var1, y=Freq/yeardiv$Freq, color=Var2, group=Var2))+
  # geom_point()+
  geom_line(position="jitter")+
  scale_color_discrete(guide=FALSE)+
  ylim(0,0.3)+
  scale_x_discrete(limits=c(1990:2018))+
  # xlim(1986,2018)+
  xlab("Time")+
  ylab("Occurence normalized by year")

# year plot order stacked percent
ggplot(spc_aggr, aes(x = year, fill = order))+
  geom_bar( aes(y = ..count..*100/sum(..count..) ))+
  xlim(1986,2018)+
  xlab("Time")+
  ylab("Occurrence percentage")

# year plot order stacked percent
ggplot(spc_aggr, aes(x = year, fill = order))+
  geom_bar( aes(y = ..count..*100/sum(..count..) ), position = 'fill')+
  xlim(1986,2018)+
  xlab("Time")+
  ylab("Occurrence percentage")

# year plot family stacked percent
ggplot(spc_aggr, aes(x = year, fill = family))+
  geom_bar( aes(y = ..count..*100/sum(..count..) ), position = 'fill')+
  xlim(1986,2018)+
  xlab("Time")+
  ylab("Occurrence percentage")

# year plot species stacked percent
ggplot(spc_aggr, aes(x = year, fill = sprintf("%s (%s)", name, spc_name)))+
  geom_bar( aes(y = ..count..*100/sum(..count..) ), position = 'fill')+
  xlim(1986,2018)+
  xlab("Time")+
  ylab("Occurrence percentage")

# year plot genus stacked percent
ggplot(spc_aggr, aes(x = year, fill = genus))+
  geom_bar( aes(y = ..count..*100/sum(..count..) ), position = 'fill')+
  xlim(1986,2018)+
  xlab("Time")+
  ylab("Occurrence percentage")


