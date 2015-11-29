## Nov 28, 2015
## Anusha Shankar, Rebecca Shroeder, et al.
## Maqui and Santa Lucia, Ecuador, July 2014 - August 2014
## iButton temperature analyses; data collected by Benjamin Weinstein

library(reshape)
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

