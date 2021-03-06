## Below all for broad-billed hummingbirds
## Anusha Shankar January 21, 2015

## Thermoregulatory equations
## tre_h = 0.0144*Te - 0.2655; intercept got by substituting BMR~BBLH~ 
## in equation with Costa's slope, and UCT = 35&deg;C
## tre_l = 0.9530 - 0.0223*Te, assuming LCT is 32&deg;C
## Tb from the tre_l equation   

library(reshape)
library(ggplot2)

#### Reading in files and reshaping ####
## Set wd and read in file
setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget")

sc_temp <- read.csv("SonoitaCreek_Temperatures_S1.csv")
bblh_tatc <- read.csv("BBLH_TcTa_2013.csv")
bblh_tnz <- read.csv("Energy budget data\\BroadBill.csv")
bblh_dlw <- read.csv("DLW_summary.csv")
costa <- read.csv("Costa1986_Don.csv")
costaVO2 <- read.csv("Costa1986_DonVO2.csv")
torpor <- read.csv("C:\\Users\\ANUSHA\\Dropbox\\Hummingbird energetics\\Tables_for_paper\\Torpor_table_plot_Mar26.csv")
torpor$AvgEE_normo_MassCorrected <- torpor$Avg_EE_hourly_normo/(torpor$Mass^(2/3))
torpor$AvgEE_torpid_MassCorrected <- torpor$Avg_EE_hourly_torpid/(torpor$Mass^(2/3))
BBLH_torpor <- subset(torpor, Species=="BBLH")

## Reading in merged NEE and DEE dataset including only pre-monsoon DEE data. For all DEE data, use BBLH_merged_summ.csv
bblh_merged <- read.csv("BBLH_merged_premonsoon.csv")
m.bblh <- melt(bblh_merged, id.vars="Site", measure.vars = c("NEE_kJ", "DEE_kJ"))
## Average of DEE and NEE for HC and SC, only pre-monsoon
mean_dee_nee <- aggregate(m.bblh$value, by=list(m.bblh$variable, m.bblh$Site), FUN=mean, na.rm=T)
names(mean_dee_nee) <- c("variable", "Site", "value")
## Melt temp files
m.sc <- melt(sc_temp, id.vars = c("Time", "Mean_Ta"), measure.vars = "MR_ml.h")
m.sc <- m.sc[,c(1,2,4)]
names(m.sc) <- c("Time", "Mean_Ta", "MR_ml_h" )

#### General functions and ordering factors within columns ####
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))
Ta.lab <- expression(atop(paste("Ambient Temperature (", degree,"C)")))

bblh_tnz$N_T <- factor(bblh_tnz$N_T, levels=c('T', 'N', 'N?'))

#bblh_tatc$Hour_rounded <- as.numeric(as.character(bblh_tatc$Hour_rounded))
bblh_tatc$Hour2 <- factor(bblh_tatc$Hour_rounded, 
                            levels= c("530", "600", "630", "700", "730", "800", "830", "900", "1000", "1100", "1200", "1300", "1400", "1500",
                                      "1600", "1700", "1800", "1900", "1930", "2000", "2030", "2100", "2130", "2200", "2230", "2300", "2330", 
                                      "2400", "2430", "100", "130", "200", "230", "300", "330", "400", "430", "500"), ordered=T)
#tatc$Hour2 <- factor(tatc$Hour2, levels= c("19", "20", "21", "22", "23", "24", "1", "2", "3", "4", "5", "6", "7"), ordered=T)

Hour_labels <- c("1900", "2000", "2100", "2200","2300", "2400", "100", "200", "300", "400", "500", "600", "700")

bblh_tatc$mmdd <- paste(bblh_tatc$Month, bblh_tatc$Day, sep='/') 

## Metabolic rates in ml O~2~/h (bmr calculated in "costa_MR_temperature.R")
bmr <- 0.2385*60
rmr <- 1.5*bmr
hmr <- 10.3*bmr
flmr <- 0.5*hmr

## Ambient temperatures - Weird, says 2500 as time - check
AmbTemp <- ggplot(bblh_tatc, aes(Hour2, Ta_Mean)) + facet_grid(.~Site) +  my_theme +
  geom_point(size=1.5) +
  theme(axis.text.x = element_text(angle = 90, size=15), legend.position="none", plot.title = element_text(size = 20),
        panel.grid.major.y = element_line(size=.1, color="grey75")) +
  xlab("Hour") #+ ylab(Ta.lab) + ggtitle("Sites")
AmbTemp

## TRE_H (i.e. MR measured above 35&deg;C) from SC daytime temperature data and broad-bill equation
## MR~H~ (mL O~2~/min) = 0.214 (T~e~) - 7.2515
#### WHEN TRYING WITH VO2 above UCT rather than with MR, equation I get is:
## MR~H~ (mL O~2~/min) = 0.0106*(T~e~) + 0.132381

## Old
#tre_h <- sum(m.sc$MR_ml_h[m.sc$Mean_Ta > 35 & 4 < m.sc$Time & m.sc$Time < 20])

## Above UCT, since we don't have BBLH measurements, we use the slope from Costa's, and find out the y intercept BBLH would have with that slope, 
## knowing that the UCT line would pass through BMR 
lm.abovecosta <- lm(costaVO2$AboveVO2~costaVO2$Temperature)
lm.abovecosta # Just use slope from here
bmr_permin <- 0.23846
yinterceptBBLH <- (lm.abovecosta$coefficients[2]*35)-bmr_permin # for intercept

###What Don got for Costa's above UCT
# MRH (mL O2/min) = 0.0144 (Te) - 0.3623

bblh_tatc$thermo_mlO2_tamean <- NA

## BBLH below LCT equation
bblh_LCT_eqn <- lm(bblh_tnz$Normothermic~bblh_tnz$Temp_C)
bblh_LCT_eqn

for(i in 1:nrow(bblh_tatc)) {
  if(bblh_tatc$Ta_Mean[i]>=35) { # What Don got for Costa's above UCT
    bblh_tatc$thermo_mlO2_tamean[i] <-  bblh_tatc$Ta_Mean[i]*lm.abovecosta$coefficients[2] - yinterceptBBLH
  }   
  if(bblh_tatc$Ta_Mean[i]<=32) { # Thermoregulatory costs if mean ambient temperature is <= 32 deg C, and between 5am and 8pm
    bblh_tatc$thermo_mlO2_tamean[i] <- bblh_LCT_eqn$coefficients[[1]] + 
      bblh_tatc$Ta_Mean[i]*bblh_LCT_eqn$coefficients[[2]]
  }
  if(bblh_tatc$Ta_Mean[i]>32 & bblh_tatc$Ta_Mean[i]<35) { # For within BMR
    bblh_tatc$thermo_mlO2_tamean[i] <- 0.2385
  }    
}
head(bblh_tatc, n=20)

## Same with Ta min to see how different MR's are if birds were in the coldest parts of the habitat
bblh_tatc$thermo_mlO2_tamin <- NA
for(i in 1:nrow(bblh_tatc)) {
  if(bblh_tatc$Ta_min[i]<=32) {
    bblh_tatc$thermo_mlO2_tamin[i] <- bblh_LCT_eqn$coefficients[[1]] + bblh_tatc$Ta_min[i]*bblh_LCT_eqn$coefficients[[2]]
  }    
}

#& bblh_tatc$Hour_rounded[i] > 500 & bblh_tatc$Hour_rounded[i] < 2000
ggplot(bblh_tatc, aes(Ta_Mean, thermo_mlO2_tamean)) + my_theme +
  geom_point(col="black", size=2) + ylab("Thermoregulatory costs in O2 ml/min") + xlab(Ta.lab)

ggplot(bblh_tatc[!is.na(bblh_tatc$Hour2),], aes(Hour2, thermo_mlO2_tamean)) + my_theme +
  geom_point(col="black")

ggplot(bblh_tatc[!is.na(bblh_tatc$Hour2),], aes(Hour2, thermo_mlO2_tamean)) + my_theme + facet_grid(~mmdd) +
  geom_point(aes(col=Site), size=2) + theme(axis.text.x = element_text(angle = 90, size=15)) +
  scale_colour_brewer(palette = "Paired")

## Old
## From SC daytime temperature data and broad-bill equation (in csv)
#tre_l <- sum(m.sc$MR_ml_h[m.sc$Mean_Ta < 32 & 4 < m.sc$Time & m.sc$Time < 20])

t_bmr <- sum(m.sc$MR_ml_h[32 < m.sc$Mean_Ta & m.sc$Mean_Ta < 35 & 4 < m.sc$Time & m.sc$Time < 20])
tre_total <- (tre_h + tre_l + t_bmr)

## NEE in ml O2/h
nee <- sum(m.sc$MR_ml_h[m.sc$Time < 5 | m.sc$Time > 18])

## ACT = 70% resting + 15% hovering + 15% flying; assuming 15 daylight hours, in ml O~2~/h
ACT <- (0.7*15*(rmr-bmr)) + (0.15*15*(hmr-bmr)) + (0.15*15*(flmr-bmr))
ACT

DEE_model <- ACT + nee + tre_total
DEE_model

## To get a per hour CO2 estimate, multiply by RQ and divide by 24
DEE_model_hr <- DEE_model*0.85/24
DEE_model_hr

## Measured estimate of DEE from DLW in kJ, converted into O2 mL/min
dlwhc_1306 <- (mean(bblh_dlw$kJ_day[bblh_dlw$Day==13]))*1000/20.5 # Only one sample from this day
dlwhc_2706 <- (mean(bblh_dlw$kJ_day[bblh_dlw$Day==27]))*1000/20.5 # n = 10
dlwhc_min_2706 <- (min(bblh_dlw$kJ_day[bblh_dlw$Day==27]))*1000/20.5
dlwhc_max_2706 <- (max(bblh_dlw$kJ_day[bblh_dlw$Day==27]))*1000/20.5
dlwsc_0207 <- mean(bblh_dlw$kJ_day[bblh_dlw$Day==2])*1000/20.5 # n = 6

## OLD - Measured estimate of DEE from DLW in CO2
dlw <- 51.3

## Percentage the model is off from the mean DLW estimate
per.off <- ((dlw - DEE_model_hr)/dlw)*100
per.off

### Let's build the models and see what we get!
## In O2 ml/min
tre_hc_pre <- sum(bblh_tatc$thermo_mlO2_tamean[bblh_tatc$mmdd=="6/15" & 
                                                 bblh_tatc$Hour_rounded <2000 & bblh_tatc$Hour_rounded > 500])

tre_sc_pre <- sum(bblh_tatc$thermo_mlO2_tamean[bblh_tatc$mmdd=="6/24" & 
                                                 bblh_tatc$Hour_rounded <2000 & bblh_tatc$Hour_rounded > 500])

## Convert to ml/h
tre_hc_pre <- tre_hc_pre*60
tre_hc_pre
tre_sc_pre <- tre_sc_pre*60
tre_sc_pre

nee_hc_pre <- mean(torpor$NEE_kJ[torpor$Site=="HC"])*1000/20.5 ## TO convert kJ to ml O2/min
nee_hc_pre
nee_sc_pre <- mean(torpor$NEE_kJ[torpor$Site=="SC"])*1000/20.5
nee_sc_pre

## Remove later- duplicate of above; moved for temporary convenience
#ACT <- (0.4*15*(rmr-bmr)) + (0.4*15*(hmr-bmr)) + (0.3*15*(flmr-bmr))
#ACT

bud_hc_pre <- ACT + nee_hc_pre + tre_hc_pre ### HAVE TO ADD BMR
bud_hc_pre

bud_hc_post <- ACT + nee_hc_post + tre_hc_post ### HAVE TO ADD BMR

bud_sc_pre <- ACT + nee_sc_pre + tre_sc_pre ### HAVE TO ADD BMR
bud_sc_pre

bud_sc_post <- ACT + nee_sc_post + tre_sc_post ### HAVE TO ADD BMR

per.off_hc_pre <- ((dlw - DEE_model_hr)/dlw)*100