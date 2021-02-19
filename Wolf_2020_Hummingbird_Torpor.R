## Plotting all data from Wolf et al. 2020 paper on hummingbird torpor
## https://royalsocietypublishing.org/doi/abs/10.1098/rsbl.2020.0428

## Questions - dataset dates are not all accurate
## Some dates are entered as 2011 but the paper says data are from 2015 March
## Some birds have dates for the second hald of the night entered wrong

## Packages
library(lubridate)
library(tidyr)
library(dplyr)
library(chron)
library(ggplot2)
library(viridis)
library(randomcoloR)


## Read in file
torpor <- read.csv("C://Users/nushi/Desktop/Wolf_etal_BiolLett_HummerTorpor.csv")

## Format
torpor$Date_False <- paste(torpor$Day_False, torpor$Month, torpor$Year, sep="/")
torpor$DateTime_F <- paste(torpor$Date_False, torpor$Time, sep=" ")
torpor$DateTime<- as.POSIXct(torpor$DateTime_F, format = "%d/%m/%Y %H:%M")

torpor$Date2 <- paste(torpor$Day, torpor$Month, torpor$Year, sep="/")
torpor$DateTime_T <- paste(torpor$Date2, torpor$Time, sep=" ")
torpor$DateTime2<- as.POSIXct(torpor$DateTime_T, format = "%d/%m/%Y %H:%M")


#torpor$Time2 <- format(strptime(torpor$DateTime, "%d/%m/%Y %H:%M"), "%H:%M")
#torpor$HMS <- format(torpor$DateTime, format="%H:%M")

## Generic plot theme
my_theme <- theme_classic(base_size = 20) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

torpor$Indiv <- factor(torpor$Indiv, levels= c("1", "2", "3", "4", "5", "6", "7", "8", "9a", "9b", "9c", "10", "11",
                                               "12", "13", "14a", "14b", "15a", "15b", "15c", "16a", "16b", "17", "18a", "18b",
                                               "18c", "19", "20", "21", "22", "23a", "23b", "24", "25", "26"))

## plot
## False date
col_vector <- unname(distinctColorPalette(length(unique(torpor$Indiv)))) 
ggplot(torpor[torpor$Indiv != "19",], aes(DateTime, Tb)) + facet_wrap(.~Species, scales = "free_x",) + my_theme +
  geom_line(aes(col=Indiv)) + #scale_color_manual(values=col_vector)
  scale_colour_viridis_d(alpha = 1, begin = 1, end = 0, direction = 1,
                                                     option = "plasma") #+ theme(axis.text.x = element_text(angle=40))

## Real date
ggplot(torpor, aes(DateTime2, Tb)) + facet_wrap(.~Indiv, scales = "free_x") + my_theme +
  geom_line(aes(col=Species), size=1.2) +  geom_line(aes(y=Tair))  #+ theme(axis.text.x = element_text(angle=40))

