## August 16, 2015; updated Feb 5, 2016
## Anusha Shankar
## El Gullan, Ecuador, June 2015 - January 2016
## iButton temperature analyses

library(reshape)
library(ggplot2)
library(maptools)
library(scales)

setwd("C:\\Users\\ANUSHA\\Dropbox\\Data 2015\\Temperature\\TempSummaries\\")
Ta_JulAug <- read.csv("CompiledTemp_July-Aug16_2015.csv")
Ta_TillNov <- read.csv("CompiledTemp_Nov28_2015.csv")
Ta_all <- read.csv("CompiledTemp_All.csv")
Tc <- read.csv("CompiledTc_All.csv")

### TO DO - Make sure to copy and sort out TO DOOOOOOOOO ###
### Temperature_TillNov15_TorporChamber_MFRCage_For_HEVI_Nov11_12.csv

head(Ta_all)

samp <- read.csv("Sample2_ToPlot.csv")

## Processing file to include day/night category column for time of day (7pm-6am is night) 
Ta_all$daynight[Ta_all$Hour %in% c(7,8,9,10,11)&Ta_all$am_pm=="PM"] <- "Night"
Ta_all$daynight[Ta_all$Hour %in% c(12,1,2,3,4,5)&Ta_all$am_pm=="AM"] <- "Night"
Ta_all$daynight[is.na(Ta_all$daynight)]<- "Day"

## Processing chamber file to include day/night (7pm-6am is night)
Tc$daynight[Tc$Hour %in% c(7,8,9,10,11)&Tc$am_pm=="PM"] <- "Night"
Tc$daynight[Tc$Hour %in% c(12,1,2,3,4,5)&Tc$am_pm=="AM"] <- "Night"
Tc$daynight[is.na(Tc$daynight)]<- "Day"

## Writing the daynight file to csv
write.csv(Ta_all, file = "Ta_all_daynight.csv")
write.csv(Tc, file = "Tc_daynight.csv")

## Summary stats for temperature during torpor
Ta_daily_mean <- aggregate(Ta_all$Temperature, 
                   by=list(Ta_all$Year, Ta_all$Month, Ta_all$Day, Ta_all$daynight, Ta_all$Hour, 
                   Ta_all$am_pm), FUN="mean")
Ta_daily_min <- aggregate(Ta_all$Temperature, 
                   by=list(Ta_all$Year, Ta_all$Month, Ta_all$Day, Ta_all$daynight, Ta_all$Hour,
                   Ta_all$am_pm), FUN="min")
Ta_daily_max <- aggregate(Ta_all$Temperature, 
                   by=list(Ta_all$Year, Ta_all$Month, Ta_all$Day, Ta_all$daynight, Ta_all$Hour, 
                   Ta_all$am_pm), FUN="max")

Ta_daily_summ <- merge(Ta_daily_mean, Ta_daily_min, by=c("Group.1", "Group.2", "Group.3", "Group.4",
                                                         "Group.5", "Group.6"))
Ta_daily_summ <- merge(Ta_daily_summ, Ta_daily_max, by=c("Group.1", "Group.2", "Group.3", "Group.4",
                                                         "Group.5", "Group.6"))

names(Ta_daily_summ) <- c("Year", "Month", "Day", "daynight", "Hour", "am_pm", "Mean_Ta", 
                          "Min_Ta", "Max_Ta")

## Writing the summary ambient temperatures file to csv
write.csv(Ta_daily_summ, file = "Ta_summary_2015.csv")

Tc_mean <- aggregate(Tc$Temperature, 
                           by=list(Tc$Expt, Tc$Year, Tc$Month, Tc$Day, Tc$daynight), FUN="mean")
Tc_min <- aggregate(Tc$Temperature, 
                          by=list(Tc$Expt, Tc$Year, Tc$Month, Tc$Day, Tc$daynight), FUN="min")
Tc_max <- aggregate(Tc$Temperature, 
                          by=list(Tc$Expt, Tc$Year, Tc$Month, Tc$Day,Tc$daynight), FUN="max")

Tc_summ <- merge(Tc_mean, Tc_min, by=c("Group.1", "Group.2", "Group.3", "Group.4", "Group.5"))
Tc_summ <- merge(Tc_summ, Tc_max, by=c("Group.1", "Group.2", "Group.3", "Group.4", "Group.5"))

names(Tc_summ) <- c("Expt", "Year", "Month", "Day", "daynight", "Mean_Tc", "Min_Tc", "Max_Tc")
head(Tc_summ)
## Writing the summary temperatures file to csv
write.csv(Tc_summ, file = "Tc_summary_2015.csv")

##### Plots #######
## Creating an object for x axis label to be Ta
Ta.xlab <- expression(atop(paste("Ambient Temperature (", degree,"C)")))

## making a separate object to be able to plot single day's temperature
OneDay_samp <- Ta_all[Ta_all$Day=="2"&Ta_all$Month=="1",]

#### This used to give me some AM/PM problems. And weird after 9:40am
Temp_aug <- ggplot(Ta_JulAug, aes(Time,Temperature)) + theme_classic(base_size = 30) + 
  geom_boxplot() + scale_color_discrete() + #geom_smooth(aes(group=1)) + facet_grid(~Month) + 
  theme(axis.title.x = element_text(size=16, face="bold"), 
        axis.text.x = element_text(size=12, face="bold", angle = 60, vjust=0.2, hjust = 0.4),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=12))
Temp_aug

#### This used to give me some AM/PM problems. And weird after 9:40am
Temp_plot <- ggplot(Ta_TillNov, aes(Time,Temperature)) + theme_bw() + geom_point() + 
  scale_color_discrete() + geom_smooth(aes(group=1)) + facet_grid(~Month) + 
  theme(axis.title.x = element_text(size=16, face="bold"), 
        axis.text.x = element_text(size=12, face="bold", angle = 60, vjust=0.2, hjust = 0.4),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=12))
Temp_plot
max(Ta_TillNov$Temperature)

## combining Month and Day into same column
Ta_daily_summ <- transform(Ta_daily_summ,Monthday=interaction(Month, Day, sep='-'))

Temp_plot <- ggplot(Ta_daily_summ, aes(Monthday,Max_Ta)) + theme_classic(base_size = 30) + 
  geom_point(aes(col=daynight), size=3) + 
  theme(axis.text.x = element_text(size=10, angle=30, vjust=0.2, hjust = 0.4))
Temp_plot
