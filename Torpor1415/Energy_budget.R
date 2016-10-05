## Below all for broad-billed hummingbirds
## Anusha Shankar January 21, 2015

## Thermoregulatory equations
## tre_h = 0.0144*Te - 0.2655; intercept got by substituting BMR~BBLH~ 
## in equation with Costa's slope, and UCT = 35&deg;C
## tre_l = 0.9530 - 0.0223*Te, assuming LCT is 32&deg;C
## Tb from the tre_l equation   

library(reshape)

## Set wd and read in file
setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget")

sc_temp <- read.csv("SonoitaCreek_Temperatures_S1.csv")
bblh_tatc <- read.csv("BBLH_TcTa_2013.csv")
bblh_tnz <- read.csv("C:\\Users\\ANUSHA\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Energy budget data\\BroadBill.csv")

bblh_tnz$N_T <- factor(bblh_tnz$N_T, levels=c('T', 'N', 'N?'))

bblh_tatc$Hour_rounded <- factor(bblh_tatc$Hour_rounded, 
                            levels= c("1900", "1930", "2000", "2030", "2100", "2130", "2200", "2230", "2300", "2330", "2400",
                                      "2430", "100", "130", "200", "230", "300", "330", "400", "430", "500", "530",
                                      "600", "630", "700"), ordered=T)
#tatc$Hour2 <- factor(tatc$Hour2, levels= c("19", "20", "21", "22", "23", "24", "1", "2", "3", "4", "5", "6", "7"), ordered=T)

Hour_labels <- c("1900", "2000", "2100", "2200","2300", "2400", "100", "200", "300", "400", "500", "600", "700")

## Melt
m.sc <- melt(sc_temp, id.vars = c("Time", "Mean_Ta"), measure.vars = "MR_ml.h")
m.sc <- m.sc[,c(1,2,4)]
names(m.sc) <- c("Time", "Mean_Ta", "MR_ml_h" )

## Metabolic rates in ml O~2~/h (bmr calculated in "costa_MR_temperature.R")
bmr <- 0.2385*60
rmr <- 1.5*bmr
hmr <- 10.3*bmr
flmr <- 0.5*hmr

## Ambient temperatures - Weird, says 2500 as time - check
AmbTemp <- ggplot(bblh_tatc, aes(Hour_rounded, Ta_Mean)) + facet_grid(.~Site) +  my_theme +
  geom_point(size=1.5) +
  #geom_line(size=1.5) +
  #scale_color_manual(values=c("Black", "Blue", "Red")) +
  #scale_alpha_manual(values = c(1, 0.5, 0.5)) +
  theme(axis.text.x = element_text(angle = 90, size=15), legend.position="none", plot.title = element_text(size = 20),
        panel.grid.major.y = element_line(size=.1, color="grey75")) +
  xlab("Hour") + ylab(Ta.lab) + ggtitle("Sites")
  #scale_x_discrete(labels=Hour_labels)
AmbTemp


## TRE_H (i.e. MR measured above 35&deg;C) from SC daytime temperature data and broad-bill equation
tre_h <- sum(m.sc$MR_ml_h[m.sc$Mean_Ta > 35 & 4 < m.sc$Time & m.sc$Time < 20])

## BBLH below LCT equation
bblh_LCT_eqn <- lm(bblh_tnz$Normothermic~bblh_tnz$Temp_C)
bblh_LCT_eqn

## Thermoregulatory costs if mean ambient temperature is <= 32 deg C, and between 5am and 8pm
bblh_tatc$thermo_mean[bblh_tatc$Ta_Mean<=32 & 500 < bblh_tatc$Hour_rounded & bblh_tatc$Hour_rounded < 2000] <- bblh_LCT_eqn$coefficients[[1]] + 
  bblh_tatc$Ta_Mean[bblh_tatc$Ta_Mean<=32 & 500 < bblh_tatc$Hour_rounded & bblh_tatc$Hour_rounded < 2000]*bblh_LCT_eqn$coefficients[[2]]


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
