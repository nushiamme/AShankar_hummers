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
library(devtools)
library(plotflow)

## Set working directory and read in .csv file
wdMS <- setwd("C:\\Users\\ANUSHA\\Dropbox\\Hummingbird energetics\\Tables_for_paper")
wdMS
torpor2015 <- read.csv("Torpor2015.csv")

## Function to return sample sizes
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

## Subsetting files
#agcu <- torpor2015[torpor2015$Species=="AGCU",]
#mety <- torpor2015[torpor2015$Species=="METY",]
tor_sub <- torpor2015[torpor2015$Species=="AGCU" | torpor2015$Species=="METY",]

## Set time as a factor
#agcu_indiv <- torpor2015[torpor2015$BirdID=="EG15_0104_AGCU",]
#agcu_indiv$Time <- factor(agcu_indiv$Time, levels=agcu_indiv$Time)

#mety$Time <- factor(mety$Time, levels=mety$Time)
#mety_indiv <- torpor2015[torpor2015$BirdID=="EG15_1028_METY",]
#mety_indiv$Time <- factor(mety_indiv$Time, levels=mety_indiv$Time)

##METY days - 0910, 1028, 1130, 1209, 1211, 1212, 1219
##AGCU days - 0826, 1023, 1220, 1223, 0104

o.tor_sub <- na.omit(tor_sub[, c("Hourly", "Time", "EE_J", "BirdID","Species", "Ta_day_min", 
                                 "Ta_day_avg", "Ta_day_max", "Ta_night_min", "Tc_avg", "Tc_min")])
o.tor_sub$Hourly <- factor(o.tor_sub$Hourly, levels=o.tor_sub$Hourly)

o.tor_sub$BirdID <- factor(o.tor_sub$BirdID, 
                           levels = c("EG15_0826_AGCU", "EG15_0910_METY", "EG15_1023_AGCU",
                                      "EG15_1028_METY", "EG15_1130_METY", "EG15_1209_METY",
                                      "EG15_1211_METY","EG15_1212_METY", "EG15_1219_METY",
                                      "EG15_1220_AGCU", "EG15_1223_AGCU", "EG15_0104_AGCU"))


energy_metyagcu <- ggplot(o.tor_sub, aes(Hourly, EE_J)) + theme_bw(base_size=18) +
  geom_line(aes(group=BirdID, col=Species), size=1.5) + facet_wrap(~BirdID, scales="free_x") +
  geom_point() + geom_text(aes(label=Tc_min), vjust=-1) + 
  #annotate("text", x=7, y=2100, label= paste("Ta daytime min = ", o.tor_sub$Ta_day_min)) + 
  ylab("Hourly energy expenditure (J)") + scale_color_manual(values=c("#000080", "#ff0000")) +
  scale_y_continuous(breaks=c(0,100,200,300,500,1000,1500,2000))+
  theme(axis.text.x = element_text(angle=30, hjust=1), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size=.1, color="grey"),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) +
  xlab("Hour step (Birdno_ArmyTime)") + scale_x_discrete(labels=o.tor_sub$Time)
energy_metyagcu

#Plot EE over night for agcu
energy15_agcu <- ggplot(na.omit(agcu_indiv[, c("Time", "EE_J", "BirdID")]),aes(Time, EE_J)) +
  theme_bw(base_size=30) + geom_line(aes(group=BirdID, col=BirdID), size=2) + 
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