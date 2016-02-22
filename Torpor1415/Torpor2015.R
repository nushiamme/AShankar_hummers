## Processing torpor data from 2015 field season to incorporate with previous torpor data
## Anusha Shankar
## Started February 22, 2016

library(ggplot2)
library(reshape)
library(gridExtra)
library(grid)
library(wq)
library(gam)
library(foreign)
library(MASS)

wdMS <- setwd("C:\\Users\\ANUSHA\\Dropbox\\Hummingbird energetics\\Tables_for_paper")
wdMS
torpor2015 <- read.csv("Torpor2015.csv")
melt()


###### For later #######
## Adding column dividing NEE by 2/3*Mass to correct for mass with allometric scaling
torpor$NEE_MassCorrected<- torpor$NEE_kJ/((2/3)*torpor$Mass)

## Adding columns to correct for mass in Avg EE normo, Min EE normo, torpid, etc.
torpor$AvgEE_normo_MassCorrected <- torpor$Avg_EE_hourly_normo/((2/3)*torpor$Mass)
torpor$MinEE_normo_MassCorrected <- as.numeric(torpor$Min_EE_normo)/((2/3)*torpor$Mass)
torpor$AvgEE_torpid_MassCorrected <- torpor$Avg_EE_hourly_torpid/((2/3)*torpor$Mass)
torpor$MinEE_torpid_MassCorrected <- as.numeric(torpor$Min_EE_torpid)/((2/3)*torpor$Mass)