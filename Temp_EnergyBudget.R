## Analyzing and plotting ambient and chamber temperature data for BBLH energy budget paper
## To make a thermoregulatory model for HC and SC
## Script by Anusha Shankar
## Script started on: November 2, 2016

library(reshape)
library(ggplot2)

#### Reading in files and reshaping ####
## Set wd
setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget")

## Read in file with temperature from each sensor per hour per site (hence temp "details")
temp_details <- read.csv("BBLH_temperatures_compiled.csv")

#### Rename and reshape ####
# See original csv for original sensor names. That's why I'm changing names here and not in source file
names(temp_details) <- c("Site", "Day", "Month", "Year", "Time", "Hour", "HC_te1", "HC_te2", "Mixed_te1", "Mixed_te2",
                         "Te_max", "Te_min", "Te_mean", "Te_sd", "SC_ta1", "SC_ta2", "SC_ta3", "Mixed_ta1", "Mixed_ta2",
                         "Mixed_ta3", "Mixed_ta4", "Mixed_ta5", "Mixed_ta6", "Mixed_ta7", "Mixed_ta8", "Mixed_ta9", 
                         "Mixed_ta10", "Mixed_ta11", "Mixed_ta12", "Mixed_ta13", "Ta_min", "Ta_max", "Ta_mean", "Ta_sd")

## Melt to compile Te-'s and Ta's separately. Then rename columns to sensible names
m.te_det <- melt(temp_details, id.vars=c("Site", "Day", "Month", "Year", "Hour"), 
               measure.vars=c("HC_te1", "HC_te2", "Mixed_te1", "Mixed_te2"), na.rm=T)
m.te_det$DayMonth <- paste0(m.te_det$Day, ",", m.te_det$Month)
names(m.te_det) <- c("Site", "Day", "Month", "Year", "Hour", "Sensor", "Te")

m.ta_det <- melt(temp_details, id.vars=c("Site", "Day", "Month", "Year", "Hour"), 
               measure.vars=c("SC_ta1", "SC_ta2", "SC_ta3", "Mixed_ta1", 
                              "Mixed_ta2", "Mixed_ta3", "Mixed_ta4", "Mixed_ta5", "Mixed_ta6", "Mixed_ta7", "Mixed_ta8",
                              "Mixed_ta9", "Mixed_ta10", "Mixed_ta11", "Mixed_ta12", "Mixed_ta13"), na.rm=T)
m.ta_det$DayMonth <- paste0(m.ta_det$Day, ",", m.ta_det$Month)
names(m.ta_det) <- c("Site", "Day", "Month", "Year", "Hour", "Sensor", "Ta")

#### General functions ####
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

#### Plots #####
m.te_hour <- m.te_det[m.te_det$Hour==700,]
ggplot(m.te_hour, aes(Sensor, Te)) + geom_point() + my_theme #+ facet_grid(DayMonth~.)


ggplot(m.temp[m.temp$Site=="HC",], aes(Sensor, Temp)) + geom_point() + my_theme 
  facet_grid(Hour~.)
  
