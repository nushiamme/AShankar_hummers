## August 16, 2015; updated Feb 5, 2016
## Anusha Shankar
## El Gullan, Ecuador, June 2015 - January 2016
## iButton temperature analyses

library(reshape)
library(ggplot2)
library(maptools)
library(scales)

setwd("C:\\Users\\ANUSHA\\Dropbox\\Data 2015\\Temperature\\TempSummaries\\")
setwd("C:\\Users\\ANUSHA\\Dropbox\\Data 2015\\Data 2016 season\\Temperature_2016\\iButton_Ta_2016\\")
setwd("C:\\Users\\ANUSHA\\Dropbox\\Hummingbird energetics\\EC_data")

Ta_JulAug <- read.csv("CompiledTemp_July-Aug16_2015.csv")
Ta_TillNov <- read.csv("CompiledTemp_Nov28_2015.csv")
Ta_all <- read.csv("CompiledTemp_All.csv")
Tc <- read.csv("CompiledTc_All.csv")
EC_Ta <- read.csv("Ecuador_TempData_corrected.csv")
Mar2016_Ta <- read.csv("Mar15_compiled.csv")

test <- 
  read.csv("C://Users//ANUSHA//Dropbox//Data 2015//Data 2016 season//Temperature_2016//Experiment//Mar_16_combined.csv")

Tbs_test <- 
  read.csv("C://Users//ANUSHA//Dropbox//Data 2015//Data 2016 season//Torpor_2016//Body_temp//EG16_0710_HEVI_Tc_plot.csv")

Tbs_test2 <- 
  read.csv("C://Users//ANUSHA//Dropbox//Data 2015//Data 2016 season//Torpor_2016//Body_temp//EG16_0718_METY_Tc_plot.csv")

m.ta_all <- melt(Ta_all, id.vars = c("Month", "Day", "Year", "Hour"), measure.vars="Temperature")

## Experiment to check whether iButtons are working as expected and not heating things up
my_theme <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(vjust = 2),
        panel.border = element_rect(colour = "black", fill=NA))

OneDay <- Mar2016_Ta[Mar2016_Ta$Date=="15/03/16",]
OneDay$Time2 <- factor(OneDay$Time, levels = OneDay$Time)
m.OneDay <- melt(OneDay, id.vars = c("Date", "am_pm", "Time2"), measure.vars = "Temp")

ggplot(OneDay, aes(Time, Temp)) + geom_point(aes(col=ID), size=2) +
  my_theme + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplot(Mar2016_Ta, aes(Date_Time, Temp)) + geom_line(aes(group=ID, col=ID)) + my_theme
### TO DO - Make sure to copy and sort out TO DOOOOOOOOO ###
### Temperature_TillNov15_TorporChamber_MFRCage_For_HEVI_Nov11_12.csv

head(Ta_all)

samp <- read.csv("Sample2_ToPlot.csv")

## For AM/PM format
## Processing file to include day/night category column for time of day (7pm-6am is night) 
Ta_all$daynight[Ta_all$Hour %in% c(7,8,9,10,11)&Ta_all$am_pm=="PM"] <- "Night"
Ta_all$daynight[Ta_all$Hour %in% c(12,1,2,3,4,5)&Ta_all$am_pm=="AM"] <- "Night"
Ta_all$daynight[is.na(Ta_all$daynight)]<- "Day"

## For 24h format of hour
## Processing file to include day/night category column for time of day (7pm-6am is night) 
EC_Ta$daynight[6>EC_Ta$Hour&EC_Ta$Hour>18] <- "Night"
EC_Ta$daynight[5<EC_Ta$Hour&EC_Ta$Hour<19]<- "Day"
head(EC_Ta)

## Site column for MQ and SL
EC_Ta$Site[1900>EC_Ta$elevation] <- "MQ"
EC_Ta$Site[1800<EC_Ta$elevation] <- "SL"

## Processing chamber file to include day/night (7pm-6am is night)
Tc$daynight[Tc$Hour %in% c(7,8,9,10,11)&Tc$am_pm=="PM"] <- "Night"
Tc$daynight[Tc$Hour %in% c(12,1,2,3,4,5)&Tc$am_pm=="AM"] <- "Night"
Tc$daynight[is.na(Tc$daynight)]<- "Day"

## Writing the daynight file to csv
write.csv(EC_Ta, file = "EC_Ta_daynight.csv")
write.csv(Tc, file = "Tc_daynight.csv")

## Summary stats for temperature during torpor
Ta_daily_mean <- aggregate(EC_Ta$Temp, 
                   by=list(EC_Ta$Site, EC_Ta$Year, EC_Ta$Month, EC_Ta$Day, EC_Ta$daynight), FUN="mean")
Ta_daily_min <- aggregate(EC_Ta$Temp, 
                   by=list(EC_Ta$Site, EC_Ta$Year, EC_Ta$Month, EC_Ta$Day, EC_Ta$daynight), FUN="min")
Ta_daily_max <- aggregate(EC_Ta$Temp, 
                   by=list(EC_Ta$Site, EC_Ta$Year, EC_Ta$Month, EC_Ta$Day, EC_Ta$daynight), FUN="max")

Ta_daily_summ <- merge(Ta_daily_mean, Ta_daily_min, 
                       by=c("Group.1", "Group.2", "Group.3", "Group.4", "Group.5"))
Ta_daily_summ <- merge(Ta_daily_summ, Ta_daily_max, 
                       by=c("Group.1", "Group.2", "Group.3", "Group.4", "Group.5"))

names(Ta_daily_summ) <- c("Site", "Year", "Month", "Day", "daynight", "Mean_Ta", "Min_Ta", "Max_Ta")

## Writing the summary ambient temperatures file to csv
write.csv(Ta_daily_summ, file = "EC_Ta_summary.csv")

## Summaries of Ta_all file
Ta_mean <- aggregate(Ta_all$Temperature, by=list(Ta_all$Hour, Ta_all$am_pm), FUN="mean")
names(Ta_mean) <- c("Hour", "am_pm", "Ta_mean")
Ta_min <- aggregate(Ta_all$Temperature, by=list(Ta_all$Hour, Ta_all$am_pm), FUN="min")
names(Ta_min) <- c("Hour", "am_pm", "Ta_min")
Ta_max <- aggregate(Ta_all$Temperature, by=list(Ta_all$Hour, Ta_all$am_pm), FUN="max")
names(Ta_max) <- c("Hour", "am_pm", "Ta_max")

Ta_all_summ <- merge(Ta_mean, Ta_min, by=c("Hour", "am_pm"))
Ta_all_summ <- merge(Ta_all_summ, Ta_max, by=c("Hour", "am_pm"))
m.ta_all_summ <- melt(Ta_all_summ, id.vars=c("Hour", "am_pm"), 
                      measure.vars=c("Ta_mean", "Ta_min", "Ta_max"))
m.ta_all_summ$Hour_pasted <- paste(m.ta_all_summ$Hour, m.ta_all_summ$am_pm)
m.ta_all_summ$Hour_pasted <- as.factor(m.ta_all_summ$Hour_pasted)
levels(m.ta_all_summ$Hour_pasted) <- c("0100", "1300", "1000", "2200", "1100", "2300", "2400",
                                       "1200", "0200", "1400", "0300", "1500", "0400", "1600",
                                       "0500", "1700", "0600", "1800","0700", "1900","0800",
                                       "2000", "0900", "2100")

m.ta_all_summ$Hour_pasted <- factor(m.ta_all_summ$Hour_pasted, 
                                            levels=c('0500', '0600', '0700','0800', '0900', 
                                                     '1000', '1100','1200', '1300', '1400', 
                                                     '1500', '1600', '1700', '1800', '1900',
                                                     '2000', '2100', '2200','2300', '2400', 
                                                     '0100', '0200', '0300','0400'))

## Doing same summaries for chamber temperature
Tc_mean <- aggregate(Tc$Temperature, 
                           by=list(Tc$Expt, Tc$Year, Tc$Month, Tc$Day, Tc$daynight, Tc$Hour, 
                                   Tc$am_pm), FUN="mean")
Tc_min <- aggregate(Tc$Temperature, 
                          by=list(Tc$Expt, Tc$Year, Tc$Month, Tc$Day, Tc$daynight, Tc$Hour,
                                  Tc$am_pm), FUN="min")
Tc_max <- aggregate(Tc$Temperature, 
                          by=list(Tc$Expt, Tc$Year, Tc$Month, Tc$Day, Tc$daynight, Tc$Hour, 
                                  Tc$am_pm), FUN="max")

Tc_summ <- merge(Tc_mean, Tc_min, by=c("Group.1", "Group.2", "Group.3", "Group.4",
                                                         "Group.5", "Group.6", "Group.7"))
Tc_summ <- merge(Tc_summ, Tc_max, by=c("Group.1", "Group.2", "Group.3", "Group.4",
                                                         "Group.5", "Group.6", "Group.7"))

names(Tc_summ) <- c("Expt", "Year", "Month", "Day", "daynight", "Hour", "am_pm", "Mean_Tc", 
                          "Min_Tc", "Max_Tc")
head(Tc_summ)
## Writing the summary temperatures file to csv
write.csv(Tc_summ, file = "Tc_summary_2015_2.csv")

## For overall torpor table- chamber temp summaries of the whole night
Tc_mean <- aggregate(Tc$Temperature, 
                     by=list(Tc$Expt, Tc$Year, Tc$Month, Tc$Day, Tc$daynight), FUN="mean")
Tc_min <- aggregate(Tc$Temperature, 
                    by=list(Tc$Expt, Tc$Year, Tc$Month, Tc$Day, Tc$daynight), FUN="min")
Tc_max <- aggregate(Tc$Temperature, 
                    by=list(Tc$Expt, Tc$Year, Tc$Month, Tc$Day, Tc$daynight), FUN="max")

Tc_summ_night <- merge(Tc_mean, Tc_min, by=c("Group.1", "Group.2", "Group.3", "Group.4", "Group.5"))
Tc_summ_night <- merge(Tc_summ_night, Tc_max, 
                       by=c("Group.1", "Group.2", "Group.3", "Group.4", "Group.5"))

names(Tc_summ_night) <- c("Expt", "Year", "Month", "Day", "daynight", "Mean_Tc", "Min_Tc", "Max_Tc")
head(Tc_summ_night)
## Writing the summary temperatures file to csv
write.csv(Tc_summ_night, file = "Tc_summary_notHourly_2015.csv")
head(Tc_summ_night)

##### Plots #######
## Creating an object for x axis label to be Ta
Ta.xlab <- expression(atop(paste("Ambient Temperature (", degree,"C)")))

## Plotting summary of Ta_all at La Paz
ggplot(m.ta_all_summ, aes(Hour_pasted, value)) + my_theme +
  geom_line(aes(group=variable, col=variable), size=1.5) +
  scale_color_manual(values=c("Black", "Blue", "Red")) +
  scale_alpha_manual(values = c(1, 0.5, 0.5)) + 
  theme(axis.text.x = element_text(angle = 60, size=12, hjust=1)) +
  xlab("Hour") + ylab(Ta.lab)

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
