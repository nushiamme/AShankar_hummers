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
head(Ta_TillNov)
## Weird order of times in the middle of the file, Dec 1 - Dec 9. Took those dates out
#samp <- read.csv("Sample_ToPlot.csv")
#samp$hms <- format(samp$Time, format = "%H:%M:%S")
#samp$hms <- as.POSIXct(samp$hms, format = "%H:%M:%S")
#head(samp)
samp <- read.csv("Sample2_ToPlot.csv")

##m.temp <- melt(Ta_JulAug, id.vars = c("Month", "Day", "Year", "Time", "AM_PM", "iButton_number"), 
##     measure.vars = "Temperature")

Ta.xlab <- expression(atop(paste("Ambient Temperature (", degree,"C)")))

OneDay_samp <- samp[samp$Date=="12/15/2015",]

### This is a horribly manual method, but just getting it done for now
samp$Time2 <- factor(samp$Time2, 
                     levels = c("0:26","0:56", "1:26","1:56", "2:26", "2:56", "3:26","3:56", "4:26",
                                "4:56", "5:26", "5:56", "6:26", "6:56","7:26", "7:56", "8:26", "8:56",
                                "9:26", "9:56", "10:26","10:56", "11:26", "11:56", "12:26", "12:56",
                                "13:26","13:56", "14:26", "14:56", "15:26", "15:56", "16:26","16:56",
                                "17:26", "17:56", "18:26", "18:56", "19:26","19:56", "20:26","20:56",
                                "21:26","21:56", "22:26","22:56", "23:26", "23:56"))

samp_plot <- ggplot(samp, aes(Time2,Temp_C)) + theme_classic(base_size = 35) + 
  geom_point(size=3) + #geom_text(vjust = 0, nudge_y = 0.5) +
  scale_color_discrete() + geom_smooth(aes(group=1)) + #facet_grid(~Month) + 
  theme(axis.title.x = element_text(face="bold"), 
        axis.text.x = element_text(face="bold", angle=60, vjust=0.4, hjust = 0.4),
        axis.title.y = element_text(face="bold", vjust=-2), axis.text.y = element_text(size=35)) +
  xlab("Time of Day") + ylab(Ta.xlab) + 
  scale_x_discrete(breaks = c("0:26","1:56","3:26","4:56","6:26","7:56","9:26","10:56", "12:26",
                              "13:56","15:26","16:56","18:26","19:56","21:26","22:56"))
samp_plot


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
