## August 16, 2015; updated Feb 5, 2016
## Anusha Shankar
## El Gullan, Ecuador, June 2015 - January 2016
## iButton temperature analyses

library(reshape)
library(ggplot2)
library(maptools)

setwd("C:\\Users\\ANUSHA\\Dropbox\\Data 2015\\Temperature\\iButton_Ta")
Ta_JulAug <- read.csv("CompiledTemp_July-Aug16_2015.csv")
Ta_TillNov <- read.csv("CompiledTemp_Nov28_2015.csv")
Ta_all <- read.csv("CompiledTemp_All.csv")
head(Ta_all)
## Weird order of times in the middle of the file, Dec 1 - Dec 9. Took those dates out
#samp <- read.csv("Sample_ToPlot.csv")
#samp$hms <- format(samp$Time, format = "%H:%M:%S")
#samp$hms <- as.POSIXct(samp$hms, format = "%H:%M:%S")
#head(samp)
samp <- read.csv("Sample2_ToPlot.csv")

##m.temp <- melt(Ta_JulAug, id.vars = c("Month", "Day", "Year", "Time", "AM_PM", "iButton_number"), 
##     measure.vars = "Temperature")

Ta.xlab <- expression(atop(paste("Ambient Temperature (", degree,"C)")))

OneDay_samp <- Ta_all[Ta_all$Day=="2"&Ta_all$Month=="1",]

#### TODO - have to add AM, PM and check what happened with after 9:40am
Temp_aug <- ggplot(Ta_JulAug, aes(Time,Temperature)) + theme_classic(base_size = 30) + 
  geom_boxplot() + scale_color_discrete() + #geom_smooth(aes(group=1)) + facet_grid(~Month) + 
  theme(axis.title.x = element_text(size=16, face="bold"), 
        axis.text.x = element_text(size=12, face="bold", angle = 60, vjust=0.2, hjust = 0.4),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=12))
Temp_aug

max(Ta_TillNov$Temperature)

#### TODO - have to add AM, PM and check what happened with after 9:40am
Temp_plot <- ggplot(Ta_TillNov, aes(Time,Temperature)) + theme_bw() + geom_point() + 
  scale_color_discrete() + geom_smooth(aes(group=1)) + facet_grid(~Month) + 
  theme(axis.title.x = element_text(size=16, face="bold"), 
        axis.text.x = element_text(size=12, face="bold", angle = 60, vjust=0.2, hjust = 0.4),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=12))
Temp_plot
max(Ta_TillNov$Temperature)

## Trying newly formatted, one day sample from all data
Temp_plot <- ggplot(OneDay_samp, aes(Time,Temperature)) + theme_bw() + 
  geom_point(aes(col=as.factor(iButton_number))) + scale_color_discrete() + #geom_smooth(aes(group=1)) + 
  facet_grid(~Month) + theme(axis.title.x = element_text(size=16, face="bold"), 
        axis.text.x = element_text(size=12, face="bold", angle = 60, vjust=0.2, hjust = 0.4),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=12))
Temp_plot

## Summary stats for temperature during torpor
## example with character variables and NAs
testDF <- data.frame(v1 = c(1,3,5,7,8,3,5,NA,4,5,7,9),
                     v2 = c(11,33,55,77,88,33,55,NA,44,55,77,99) )
by1 <- c("red", "blue", 1, 2, NA, "big", 1, 2, "red", 1, NA, 12)
by2 <- c("wet", "dry", 99, 95, NA, "damp", 95, 99, "red", 99, NA, NA)
aggregate(x = testDF, by = list(by1, by2), FUN = "mean")

Ta_mean_daily <- aggregate(Ta_all$Temperature, by=list(Ta_all$Month, Ta_all$Day), FUN="mean")
Ta_min_daily <- aggregate(Ta_all$Temperature, by=list(Ta_all$Month, Ta_all$Day), FUN="min")
Ta_max_daily <- aggregate(Ta_all$Temperature, by=list(Ta_all$Month, Ta_all$Day), FUN="max")

Ta_all$daynight[Ta_all$Hour==c(7,8,9,10,11)&Ta_all$am_pm=="PM"] <- "Night"
Ta_all$daynight[Ta_all$Hour==c(12,1,2,3,4,5)&Ta_all$am_pm=="AM"] <- "Night"
Ta_all$daynight[is.na(Ta_all$daynight)]<- "Day"

write.csv(Ta_all, file = "Ta_all_daynight.csv")
