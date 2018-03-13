
## Read in packages
library(ggplot2)
library(reshape)
## Set wd
setwd("C:/Users/Erich Eberts/ERICH/18Spring/CODE/TINT/EyetempsCODE/actual")
setwd("C:\\Users\\ANUSHA\\Desktop\\Experiment_grant_Erich\\")

my_theme <- theme_classic(base_size = 32) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

# Load data. 
all.data <- read.csv("all.data.2017.csv")
head(all.data)

## Making the eye tep column a numeric column
all.data$eye_temp <- as.numeric(as.character(all.data$eye_temp))
all.data$amb_temp <- as.numeric(as.character(all.data$amb_temp))
## subsetting out, excluding NA's from eye temp
all.data.no_na <- subset(all.data, (!is.na(all.data$eye_temp)))

## Delta Eye - Amb
all.data.no_na$Delta <- all.data.no_na$eye_temp - all.data.no_na$amb_temp

## Aggregating by min, max, mean eye temp
agg.eye <- as.data.frame(as.list(aggregate(all.data.no_na$eye_temp,
                                by=list(all.data.no_na$night_ID), 
                                FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(agg.eye) <- c("Night_ID", "Min", "Mean", "Max")

## Aggregate by hour and night
agg.eye.hour <- as.data.frame(as.list(aggregate(all.data.no_na$eye_temp,
                                                by=list(all.data.no_na$night_num, all.data.no_na$nest_num, all.data.no_na$army_hour),
                                                FUN=function(x) c(mi=min(x), mn = mean(x), mx = max(x)))))
names(agg.eye.hour) <- c("Night_num", "Nest", "Hour", "Eye_Min", "Eye_Mean", "Eye_Max")

agg.delta.hour <- as.data.frame(as.list(aggregate(all.data.no_na$Delta,
                                                by=list(all.data.no_na$night_num, all.data.no_na$nest_num, all.data.no_na$army_hour),
                                                FUN=function(x) c(mi=min(x), mn = mean(x), mx = max(x)))))
names(agg.delta.hour) <- c("Night_num", "Nest", "Hour", "Delta_Min", "Delta_Mean", "Delta_Max")
agg.delta.hour$Hour2 <- factor(agg.delta.hour$Hour, levels= c("16", "17", "18", "19", "20", "21", "22", "23", "12",
                                            "1", "2", "3", "4", "5", "6"), ordered=T)
m.delta <- melt(agg.delta.hour, id.vars=c("Night_num","Nest","Hour2"), measure.vars = c("Delta_Min", "Delta_Mean", "Delta_Max"))

agg.amb.hour <- as.data.frame(as.list(aggregate(all.data.no_na$amb_temp,
                                                by=list(all.data.no_na$night_num, all.data.no_na$nest_num, all.data.no_na$army_hour),
                                                FUN=function(x) c(mi=min(x), mn = mean(x), mx = max(x)))))
names(agg.amb.hour) <- c("Night_num", "Nest", "Hour", "Amb_Min", "Amb_Mean", "Amb_Max")

merge_hour_temps <- merge(agg.amb.hour, agg.eye.hour, by = c("Night_num", "Nest", "Hour"))

m.all <- melt(merge_hour_temps, id.vars=c("Night_num", "Nest", "Hour"), measure.vars = c("Amb_Min", "Amb_Mean", "Amb_Max", 
                                                                                "Eye_Min", "Eye_Mean", "Eye_Max"))

m.all$Hour2 <- factor(m.all$Hour, levels= c("16", "17", "18", "19", "20", "21", "22", "23", "12",
                                "1", "2", "3", "4", "5", "6"), ordered=T)

## melting aggregated eye temp data frame
m.eye <- melt(agg.eye, id.vars="Night_ID", measure.vars = c("Min", "Mean", "Max"))

## Plot eye temps, summaries per night
ggplot(m.eye, aes(Night_ID, value, group=variable)) + geom_point(aes(col=variable)) + my_theme + geom_line(aes(col=variable)) +
  scale_color_manual(values=c("blue", "black", "red")) + xlab("Night ID") + ylab("Eye Temperature C") +
  theme(axis.text.x = element_text(angle=30, size=10), legend.key.height = unit(3, 'lines'))

ggplot(m.delta[m.delta$Nest=="7",], aes(Hour2, value)) + my_theme + facet_grid(Nest~Night_num) +
  geom_point(aes(col=variable)) + geom_line(aes(group=variable, col=variable)) +
  #scale_color_manual(values=c("blue", "black", "red")) +
  xlab("Hour") + ylab("Temperature C") +
  theme(axis.text.x = element_text(angle=30, size=10), legend.key.height = unit(3, 'lines'),
        panel.grid.major.y = element_line(color='grey'),
        panel.grid.minor.y = element_line(color='grey'))
