## Below all for broad-billed hummingbirds
## Anusha Shankar January 21, 2015

## Thermoregulatory equations
## tre_h = 0.0144*Te - 0.2655; intercept got by substituting BMR~BBLH~ 
## in equation with Costa's slope, and UCT = 35&deg;C
## tre_l = 0.9530 - 0.0223*Te, assuming LCT is 32&deg;C
## Tb from the tre_l equation   

library(reshape)

## Set wd and read in file
setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha_personal\\Thesis proposal\\R_csv\\AZ")
sc_temp <- read.csv("SonoitaCreek_Temperatures_S1.csv")

## Melt
m.sc <- melt(sc_temp, id.vars = c("Time", "Mean_Ta"), measure.vars = "MR_ml.h")
m.sc <- m.sc[,c(1,2,4)]
names(m.sc) <- c("Time", "Mean_Ta", "MR_ml_h" )

## Metabolic rates in ml O~2~/h (bmr calculated in "costa_MR_temperature.R")
bmr <- 0.2385*60
rmr <- 1.5*bmr
hmr <- 10.3*bmr
flmr <- 0.5*hmr

## TRE_H (i.e. MR measured above 35&deg;C) from SC daytime temperature data and broad-bill equation
tre_h <- sum(m.sc$MR_ml_h[m.sc$Mean_Ta > 35 & 4 < m.sc$Time & m.sc$Time < 20])

## From SC daytime temperature data and broad-bill equation (in csv)
tre_l <- sum(m.sc$MR_ml_h[m.sc$Mean_Ta < 32 & 4 < m.sc$Time & m.sc$Time < 20])

t_bmr <- sum(m.sc$MR_ml_h[32 < m.sc$Mean_Ta & m.sc$Mean_Ta < 35 & 4 < m.sc$Time & m.sc$Time < 20])
tre_total <- (tre_h + tre_l + t_bmr)

## NEE in ml O2/h
nee <- sum(m.sc$MR_ml_h[m.sc$Time < 5 | m.sc$Time > 18])

## ACT = 70% resting + 15% hovering + 15% flying; assuming 14 daylight hours, in ml O~2~/h
ACT <- (0.7*14*(rmr-bmr)) + (0.15*14*(hmr-bmr)) + (0.15*14*(flmr-bmr))
ACT

DEE_model <- ACT + nee + tre_total
DEE_model

## To get a per hour CO2 estimate, multiply by RQ and divide by 24
DEE_model_hr <- DEE_model*0.85/24
DEE_model_hr

## Measured estimate of DEE from DLW
dlw <- 51.3

## Percentage the model is off from the mean DLW estimate
per.off <- ((dlw - DEE_model_hr)/dlw)*100
per.off
