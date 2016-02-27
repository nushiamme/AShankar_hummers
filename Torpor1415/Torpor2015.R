## Processing torpor data from 2015 field season to incorporate with previous torpor data
## Anusha Shankar
## Started February 22, 2016

##Packages
library(ggplot2)
library(reshape)
library(gridExtra)
library(grid)
library(wq)
library(gam)
library(foreign)
library(MASS)

## Set working directory and read in .csv file
wdMS <- setwd("C:\\Users\\ANUSHA\\Dropbox\\Hummingbird energetics\\Tables_for_paper")
wdMS
torpor2015 <- read.csv("Torpor2015.csv")

m.nee <- melt(torpor2015, id.vars = c("Species", "Time", "Day", "Month"), measure.vars = "NEE_kJ") 

## Function to return sample sizes
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

## Subsetting files
agcu <- torpor2015[torpor2015$Species=="AGCU",]
mety <- torpor2015[torpor2015$Species=="METY",]
mety$Time <- factor(mety$Time, levels=mety$Time)

mety_indiv <- torpor2015[torpor2015$BirdID=="EG15_1028_METY",]
mety_indiv$Time <- factor(mety_indiv$Time, levels=mety_indiv$Time)

##METY days - 0910, 1028, 1130, 1209, 1211, 1212, 1219
##AGCU days - 0826, 1023, 1220, 1223, 0104

agcu_indiv <- torpor2015[torpor2015$BirdID=="EG15_0104_AGCU",]
agcu_indiv$Time <- factor(agcu_indiv$Time, levels=agcu_indiv$Time)

#Plot EE over night for agcu
energy15_agcu <- ggplot(na.omit(agcu_indiv[, c("Time", "EE_J", "BirdID")]), aes(Time, EE_J)) +
  theme_bw(base_size=30) +  geom_line(aes(group=BirdID, col=BirdID), size=2) +
  scale_color_manual(values="purple") +
  ylab("Hourly energy expenditure (J)")
energy15_agcu

#Plot EE over night for mety
energy15_mety <- ggplot(na.omit(mety_indiv[, c("Time", "EE_J", "BirdID")]), aes(Time, EE_J)) + 
  theme_bw(base_size=30) +  geom_line(aes(group=BirdID, col = BirdID), size=2) + 
  ylab("Hourly energy expenditure (J)")
energy15_mety

## Plot NEE
energy_plot <- ggplot(m.nee, aes(Species, value)) +  theme_bw(base_size = 30) +
  geom_boxplot(size=2) + geom_point(aes(col=Species), size=6) +
  ylab("Nighttime energy expenditure (kJ)")
energy_plot



###### For later #######
## Adding column dividing NEE by 2/3*Mass to correct for mass with allometric scaling
torpor$NEE_MassCorrected<- torpor$NEE_kJ/((2/3)*torpor$Mass)

## Adding columns to correct for mass in Avg EE normo, Min EE normo, torpid, etc.
torpor$AvgEE_normo_MassCorrected <- torpor$Avg_EE_hourly_normo/((2/3)*torpor$Mass)
torpor$MinEE_normo_MassCorrected <- as.numeric(torpor$Min_EE_normo)/((2/3)*torpor$Mass)
torpor$AvgEE_torpid_MassCorrected <- torpor$Avg_EE_hourly_torpid/((2/3)*torpor$Mass)
torpor$MinEE_torpid_MassCorrected <- as.numeric(torpor$Min_EE_torpid)/((2/3)*torpor$Mass)