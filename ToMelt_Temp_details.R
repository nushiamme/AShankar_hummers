## Melting and adding thermoregulatory cost calculations to data from BBLH_temperatures_compiled.csv file.
## For details on temperature data from each sensor for each hour, and thermoregulatory costs at each of those temperature values
## Author: Anusha Shankar, with loop help from Giovanni Rapacciuolo
## November 10, 2016

library(reshape)

#### Reading in files ####
## Set wd
setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget")

## Read in file with temperature from each sensor per hour per site (hence temp "details")
temp_details <- read.csv("BBLH_temperatures_compiled.csv")
## Read in files for thermoregulatory costs from costas and BBLH
costaVO2 <- read.csv("Costa1986_DonVO2.csv")
bblh_tnz <- read.csv("Energy budget data\\BroadBill.csv")

## Rename and reshape
# See "BBLH_temperatures_compiled.csv" for original sensor names; i.e. changing names here and not in source file
names(temp_details) <- c("Site", "Day", "Month", "Year", "Time", "Hour", "HC_te1", "HC_te2", "Mixed_te1", "Mixed_te2",
                         "Te_max", "Te_min", "Te_mean", "Te_sd", "SC_ta1", "SC_ta2", "SC_ta3", "Mixed_ta1", "Mixed_ta2",
                         "Mixed_ta3", "Mixed_ta4", "Mixed_ta5", "Mixed_ta6", "Mixed_ta7", "Mixed_ta8", "Mixed_ta9", 
                         "Mixed_ta10", "Mixed_ta11", "Mixed_ta12", "Mixed_ta13", "Ta_min", "Ta_max", "Ta_mean", "Ta_sd")

## Melt to compile Te-'s and Ta's separately. Then rename columns to sensible names
m.te_det <- melt(temp_details, id.vars=c("Site", "Day", "Month", "Year", "Hour"), 
                 measure.vars=c("HC_te1", "HC_te2", "Mixed_te1", "Mixed_te2"), na.rm=T)
m.te_det$DayMonth <- paste0(m.te_det$Day, ",", m.te_det$Month)
names(m.te_det) <- c("Site", "Day", "Month", "Year", "Hour", "Sensor", "Te", "DayMonth")
m.te_det$Hour <- as.factor(m.te_det$Hour)

m.ta_det <- melt(temp_details, id.vars=c("Site", "Day", "Month", "Year", "Hour"), 
                 measure.vars=c("SC_ta1", "SC_ta2", "SC_ta3", "Mixed_ta1", 
                                "Mixed_ta2", "Mixed_ta3", "Mixed_ta4", "Mixed_ta5", "Mixed_ta6", "Mixed_ta7", "Mixed_ta8",
                                "Mixed_ta9", "Mixed_ta10", "Mixed_ta11", "Mixed_ta12", "Mixed_ta13"), na.rm=T)
m.ta_det$DayMonth <- paste0(m.ta_det$Day, ",", m.ta_det$Month)
names(m.ta_det) <- c("Site", "Day", "Month", "Year", "Hour", "Sensor", "Ta", "DayMonth")
m.ta_det$Hour <- as.factor(m.ta_det$Hour)

## Making a thermoregulatory costs column in the temp dataframe
## Add columns to melted Te and Ta dataframes for thermoregulatory costs
m.te_det$thermo_mlO2 <- NA
m.ta_det$thermo_mlO2 <- NA

## Above UCT, since we don't have BBLH measurements, we use the slope from Costa's, and find out the y intercept BBLH would have with that slope, 
## knowing that the UCT line would pass through BMR 
lm.abovecosta <- lm(costaVO2$AboveVO2~costaVO2$Temperature)
lm.abovecosta # use only slope from here; we get the intercept from BBLH TNZ data
bmr_permin <- 0.23846
yinterceptBBLH <- (lm.abovecosta$coefficients[2]*35)-bmr_permin # for intercept

## Below LCT equation from BBLH TNZ measurement data
bblh_LCT_eqn <- lm(bblh_tnz$Normothermic~bblh_tnz$Temp_C)
bblh_LCT_eqn

## For Te
for(i in 1:nrow(m.te_det)) {
  if(m.te_det$Te[i]>=35) { ## Above UCT for Te
    m.te_det$thermo_mlO2[i] <-  m.te_det$Te[i]*lm.abovecosta$coefficients[2] - yinterceptBBLH
  }    
  if(m.te_det$Te[i]<=32) { # Below LCT Thermoregulatory costs if mean Te is <= 32 deg C
    m.te_det$thermo_mlO2[i] <- bblh_LCT_eqn$coefficients[[1]] + 
      m.te_det$Te[i]*bblh_LCT_eqn$coefficients[[2]]
  }    
  if(m.te_det$Te[i]>32 & m.te_det$Te[i]<35) {  # Within TNZ - BMR
    m.te_det$thermo_mlO2[i] <- 0.2385
  }  
}

## For Ta
for(i in 1:nrow(m.ta_det)) { 
  if(m.ta_det$Ta[i]>=35) { ## Above UCT for Ta
    m.ta_det$thermo_mlO2[i] <-  m.ta_det$Ta[i]*lm.abovecosta$coefficients[2] - yinterceptBBLH
  }    
  if(m.ta_det$Ta[i]<=32) { ## Below LCT Thermoregulatory costs if mean Ta is <= 32 deg C
    m.ta_det$thermo_mlO2[i] <- bblh_LCT_eqn$coefficients[[1]] + 
      m.ta_det$Ta[i]*bblh_LCT_eqn$coefficients[[2]]
  }   
  if(m.ta_det$Ta[i]>32 & m.ta_det$Ta[i]<35) { ## For within BMR
    m.ta_det$thermo_mlO2[i] <- 0.2385
  }    
}
  

write.csv(m.te_det, "Melted_Te_thermo.csv")
write.csv(m.ta_det, "Melted_Ta_thermo.csv")
