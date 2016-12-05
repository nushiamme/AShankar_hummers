## Energy budget model variants
## Anusha Shankar
## November 14, 2016

## Read in bblh_tatc, torpor, bblh_dlw csv's

## Measured values in ml O~2~/h (bmr calculated in "costa_MR_temperature.R")
bmr <- 0.2385*60 #in ml O~2~/h
rmr <- 1.5*bmr #in ml O~2~/h
hmr <- 10.3*bmr #in ml O~2~/h
flmr <- 0.5*hmr #in ml O~2~/h

## Traditional activity budget
## ACT = 70% resting + 15% hovering + 15% flying; assuming 15 daylight hours, in ml O~2~/h
ACT <- (0.7*15*(rmr-bmr)) + (0.15*15*(hmr-bmr)) + (0.15*15*(flmr-bmr))
ACT # in ml O~2~/day

## Slightly modified act budget
## ACT = 70% resting + 15% hovering + 15% flying; assuming 15 daylight hours, in ml O~2~/h
ACT <- (0.6*15*(rmr-bmr)) + (0.2*15*(hmr-bmr)) + (0.2*15*(flmr-bmr))
ACT # in ml O~2~/day

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

## Temporarily manually plugging in fron Temp_EnergyBudget.R
tre_hc_1306 <- 295 # Assuming 15 hour daytime normothermy, in mL O2 consumed/day
tre_hc_2706 <- 302 # Assuming 15 hour daytime normothermy, in mL O2 consumed/day
tre_sc_0207 <- 283 # Assuming 15 hour daytime normothermy, in mL O2 consumed/day

tremin_hc_1306 <- Results$MinTemp_thermo_day[1]
tremin_hc_2706 <- Results$MinTemp_thermo_day[2]
tremin_sc_0207 <- Results$MinTemp_thermo_day[4]

tremax_hc_1306 <- Results$MaxTemp_thermo_day[1]
tremax_hc_2706 <- Results$MaxTemp_thermo_day[2]
tremax_sc_0207 <- Results$MaxTemp_thermo_day[4]

## In ml O2 consumed/day
Results$DEE_randTemp[1] <- ACT + nee_hc_pre + tre_hc_1306
Results$DEE_minTemp[1] <- ACT + nee_hc_pre + tremin_hc_1306
Results$DEE_maxTemp[1] <- ACT + nee_hc_pre + tremax_hc_1306
Results$DEE_randTemp[2] <- ACT + nee_hc_pre + tre_hc_2706
Results$DEE_minTemp[2] <- ACT + nee_hc_pre + tremin_hc_2706
Results$DEE_maxTemp[2] <- ACT + nee_hc_pre + tremax_hc_2706
Results$DEE_randTemp[4] <- ACT + nee_sc_pre + tre_sc_0207
Results$DEE_minTemp[4] <- ACT + nee_sc_pre + tremin_sc_0207
Results$DEE_maxTemp[4] <- ACT + nee_sc_pre + tremax_sc_0207
Results$ACT[1]

## To get a per hour CO2 estimate, multiply by RQ and divide by 24
DEE_model_hr <- DEE_model*0.85/24
DEE_model_hr


## Measured estimate of DEE from DLW in kJ, converted into mL O2 consumed
Results$DLW_mean[1] <- (mean(bblh_dlw$kJ_day[bblh_dlw$Day==13]))*1000/20.5 # Only one sample from this day
Results$DLW_mean[2] <- (mean(bblh_dlw$kJ_day[bblh_dlw$Day==27]))*1000/20.5 # n = 10
Results$DLW_min[2] <- (min(bblh_dlw$kJ_day[bblh_dlw$Day==27]))*1000/20.5
Results$DLW_max[2] <- (max(bblh_dlw$kJ_day[bblh_dlw$Day==27]))*1000/20.5
Results$DLW_mean[4] <- mean(bblh_dlw$kJ_day[bblh_dlw$Day==2])*1000/20.5 # n = 6
Results$DLW_min[4] <- (min(bblh_dlw$kJ_day[bblh_dlw$Day==2]))*1000/20.5
Results$DLW_max[4] <- (max(bblh_dlw$kJ_day[bblh_dlw$Day==2]))*1000/20.5

Results

melt
ggplot(Results, aes())
#write.csv(Results, "Summary_minmaxTemps_prelim.csv")

## OLD - Measured estimate of DEE from DLW in CO2
dlw <- 51.3

## Percentage the model is off from the mean DLW estimate
per.off <- ((dlw - DEE_model_hr)/dlw)*100
per.off
