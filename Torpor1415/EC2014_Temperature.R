## Nov 28, 2015
## Anusha Shankar, Rebecca Shroeder, et al.
## Maqui and Santa Lucia, Ecuador, July 2014 - August 2014
## iButton temperature analyses; data collected by Benjamin Weinstein

library(reshape2)
library(ggplot2)

setwd("C:\\Users\\ANUSHA\\Dropbox\\Hummingbird energetics\\EC_data")
Ta_EC2014 <- read.csv("Ecuador_TempData_corrected.csv")
head(Ta_EC2014)

##m.temp <- melt(Ta_JulAug, id.vars = c("Month", "Day", "Year", "Time", "AM_PM", "iButton_number"), 
##     measure.vars = "Temperature")

#### TODO - have to add AM, PM and check what happened with after 9:40am
Temp_plot <- ggplot(Ta_EC2014, aes(Hour,Temp)) + theme_bw() + geom_point() + 
  scale_color_discrete() + geom_smooth(aes(group=1)) + facet_grid(elevation~Month) + 
  theme(axis.title.x = element_text(size=16, face="bold"), 
        axis.text.x = element_text(size=12, face="bold", angle = 60, vjust=0.2, hjust = 0.4),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=12))
Temp_plot

## Stat summaries

m.temp <- melt(Ta_EC2014, id.vars=c("Date","Hour","elevation"), measure.vars = "Temp" )
m.temp <- m.temp[,c(1,2,3,5)]
colnames(m.temp) <- c("Date", "Hour", "Elevation", "Temp")

dcast(m.temp, formula= Temp ~ Date ~ elevation, mean)

TempSummary <- data.frame(
  aggregate(Temp~Date+Elevation, data=m.temp, min),
  aggregate(Temp~Date+Elevation, data=m.temp, max),
  aggregate(Temp~Date+Elevation, data=m.temp, mean))

TempSummary <- TempSummary[,c(1,2,3,6,9)]

colnames(TempSummary) <- c("Date","Elevation", "min", "max", "mean")
head(TempSummary)

write.csv(TempSummary,"Ecuador2014_Temp_summary.csv")
