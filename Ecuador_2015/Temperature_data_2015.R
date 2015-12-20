## August 16, 2015; updated Nov 28, 2015
## Anusha Shankar
## El Gullan, Ecuador, June 2015 - January 2016
## iButton temperature analyses

library(reshape)
library(ggplot2)

setwd("C:\\Users\\ANUSHA\\Dropbox\\Data 2015\\Temperature\\iButton_Ta")
Ta_TillNov <- read.csv("CompiledTemp_Nov28_2015.csv")
head(Ta_TillNov)

##m.temp <- melt(Ta_JulAug, id.vars = c("Month", "Day", "Year", "Time", "AM_PM", "iButton_number"), 
##     measure.vars = "Temperature")

#### TODO - have to add AM, PM and check what happened with after 9:40am
Temp_plot <- ggplot(Ta_TillNov, aes(Time,Temperature)) + theme_bw() + geom_point() + 
  scale_color_discrete() + geom_smooth(aes(group=1)) + facet_grid(~Month) + 
  theme(axis.title.x = element_text(size=16, face="bold"), 
        axis.text.x = element_text(size=12, face="bold", angle = 60, vjust=0.2, hjust = 0.4),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=12))
Temp_plot
max(Ta_TillNov$Temperature)

