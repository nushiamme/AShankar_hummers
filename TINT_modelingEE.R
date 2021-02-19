# Trying out a model of energy expenditure in nesting female hummingbirds

# Reading in packages
library(ggplot2)
library(here)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)


## read in files
nest <- read.csv("DPowers_Nest_data.csv") #Nest insulation and convection measurements
tint <- read.csv("final_data_Apr2020.csv") #LA nest measurements
model <- read.csv("TINT_Model_template.csv") #model columns
nest_long <- read.csv("nest_longform.csv")
tnz <- read.csv("JAvBiol_BroadBill.csv")


## General functions
my_theme <- theme_classic(base_size = 15) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))


## Organize data frames
morpho <- melt(nest, id.vars="Nest_ID", 
               measure.vars=c("Wall_AC_outer_diameter_mm","Wall_BD_outer_diameter_mm",
                              "Wall_AC_cup_diameter_mm", "A_cup_height_mm"))

m.long <- melt(nest_long, id.vars=c("Nest_ID", "Species", "Variable", "Side"),
               measure.vars=c("Temp_05C", "Temp_10C", "Temp_15C", "Temp_20C",
                              "Temp_25C", "Temp_30C", "Temp_35C", "Temp_40C"))

m.long <- m.long %>% 
  rename(TempVar = variable)

m.long <- m.long %>% separate(TempVar, c(NA, "Temp", NA), sep = "([_ C])")

m.long$Temp <- as.numeric(as.character(m.long$Temp))

#Adding "Day" column to TINT data frame
tint$Date <- tint$Date_Time
tint <- tint %>% separate(Date, c(NA, "Day", NA), sep = "/")
tint$Day <- as.numeric(as.character(tint$Day))
tint$DayMonth <- paste0(tint$Day, "/", tint$Month)

## Get average hourly Amb and eye temp per night
tint_hourly <- as.data.frame(tint %>%
  group_by(Night_ID, Nest_ID, Hour_Elapsed) %>%
  summarize(AmbTemp = mean(Amb_Temp_FINAL, na.rm = T),
            EyeTemp = mean(Peye_Temp_FINAL, na.rm=T)))

## Extract just Hours and RER columns
#m.model <- model %>%
 # select(Hours_elapsed_sunset, RER)

tint_hourly$RER <- ifelse(tint_hourly$Hour_Elapsed<3,1,0.71)

## Adding in MR columns
## Calculate Lower TNZ slope equation for broad-bills
tnz_eqn <- lm(VO2_Normothermic~Temp_C, tnz)

## intercept: tnz_eqn$coefficients[1]
## slope: tnz_eqn$coefficients[2]

## Calculate VO2 given ambient temp is amb temp
tint_hourly$VO2_amb <- tnz_eqn$coefficients[1] + (tnz_eqn$coefficients[2]*tint_hourly$AmbTemp)

## Convert O2 ml/min to kJ/hour
tint_hourly$kJ_amb <- tint_hourly$VO2_amb*((16 + (5.164*tint_hourly$RER))/1000)*60

NestTs_eqn <- lm(deltas$value[deltas$Side=="NestTs_Amb" & deltas$Species=="BBLH"]~deltas$value[deltas$Side=="Amb" & deltas$Species=="BBLH"])
## Nest Ts = 8.4325 - 0.2334(Ta) for BBLH

tint_hourly$NestTs <- NestTs_eqn$coefficients[1] + (NestTs_eqn$coefficients[2]*tint_hourly$AmbTemp)

## Calculate VO2 given ambient temp is nest surface temp
tint_hourly$VO2_NestTs <- tnz_eqn$coefficients[1] + (tnz_eqn$coefficients[2]*tint_hourly$NestTs)

## Convert O2 ml/min to kJ/hour
tint_hourly$kJ_NestTs <- tint_hourly$VO2_NestTs*((16 + (5.164*tint_hourly$RER))/1000)*60

Tcup_eqn <- lm(deltas$value[deltas$Side=="Tcup_Amb" & deltas$Species=="BBLH"]~deltas$value[deltas$Side=="Amb"& deltas$Species=="BBLH"])
## Tcup = 50.425 - 1.166(Ta) for BBLH

tint_hourly$TempCup <- Tcup_eqn$coefficients[1] + (Tcup_eqn$coefficients[2]*tint_hourly$AmbTemp)

## Calculate VO2 given ambient temp is nest surface temp
tint_hourly$VO2_Cup <- tnz_eqn$coefficients[1] + (tnz_eqn$coefficients[2]*tint_hourly$TempCup)

## Convert O2 ml/min to kJ/hour
tint_hourly$kJ_Cup <- tint_hourly$VO2_Cup*((16 + (5.164*tint_hourly$RER))/1000)*60

### Torpor calculations
tor_int <- 0.46
tor_slope <- 0.053
tor2 <- 0.0016

## Create columns to add torpor MR measurements
tint_hourly$TorporAmb <- 0
tint_hourly$TorporNestTs <- 0
tint_hourly$TorporCup <- 0

## Calculating torpor MR in kJ for each type of temperature measurement
for(i in 1:nrow(tint_hourly)) {
  if(tint_hourly$Hour_Elapsed[i]<14 & tint_hourly$Hour_Elapsed[i]>6) {
    tint_hourly$TorporAmb[i] <- (tor_int - tor_slope*(tint_hourly$AmbTemp[i]) + tor2*((tint_hourly$AmbTemp[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
    tint_hourly$TorporNestTs[i] <- (tor_int - tor_slope*(tint_hourly$NestTs[i]) + tor2*((tint_hourly$NestTs[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
    tint_hourly$TorporCup[i] <- (tor_int - tor_slope*(tint_hourly$TempCup[i]) + tor2*((tint_hourly$TempCup[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
  } 
  #if(tint_hourly$Hour_Elapsed[i]<14 & tint_hourly$Hour_Elapsed[i]>6 & tint_hourly$TempCup[i] <31) {
   # tint_hourly$TorporCup[i] <- (tor_int - tor_slope*(tint_hourly$TempCup[i]) + tor2*((tint_hourly$TempCup[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
  #}
}

## Melt metabolic rates in kJ
m.tint <- melt(tint_hourly,id.vars = c("Night_ID", "Nest_ID", "Hour_Elapsed"), 
               measure.vars = c("kJ_amb", "kJ_NestTs", "kJ_Cup",
                                "TorporAmb", "TorporNestTs", "TorporCup"))

## Column to specific normo vs. torpor
m.tint$Tornor <- ifelse(m.tint$variable=="kJ_amb"|m.tint$variable=="kJ_NestTs"|m.tint$variable=="kJ_Cup","N","T")

## Making column with hourly MR assuming 7 hours of torpor use, and the rest normo
tint_hourly$Tor_Nor_merged_Amb <- tint_hourly$TorporAmb
for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor_Nor_merged_Amb[i]==0) {
  tint_hourly$Tor_Nor_merged_Amb[i] <- tint_hourly$kJ_amb[i]}
}

tint_hourly$Tor_Nor_merged_NestTs <- tint_hourly$TorporNestTs
for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor_Nor_merged_NestTs[i]==0) {
  tint_hourly$Tor_Nor_merged_NestTs[i] <- tint_hourly$kJ_NestTs[i]}
}

tint_hourly$Tor_Nor_merged_Cup <- tint_hourly$TorporCup
for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor_Nor_merged_Cup[i]==0) {
  tint_hourly$Tor_Nor_merged_Cup[i] <- tint_hourly$kJ_Cup[i]}
}


## Calculating NEE, first three normo, and next 3 cols with 7 hours of torpor
tint_nee <- as.data.frame(tint_hourly %>%
                            group_by(Night_ID, Nest_ID) %>%
                            summarize(Nor_kJ_Amb = sum(kJ_amb, na.rm = T),
                                      Nor_kJ_NestTs = sum(kJ_NestTs, na.rm = T),
                                      Nor_kJ_Cup = sum(kJ_Cup, na.rm = T),
                                      Tor_kJ_Amb = sum(Tor_Nor_merged_Amb, na.rm = T),
                                      Tor_kJ_NestTs = sum(Tor_Nor_merged_NestTs, na.rm = T),
                                      Tor_kJ_Cup = sum(Tor_Nor_merged_Cup, na.rm = T)))

m.nee <- melt(tint_nee, id.vars = c("Night_ID", "Nest_ID"), 
              measure.vars = c("Nor_kJ_Amb", "Nor_kJ_NestTs", "Nor_kJ_Cup",
                               "Tor_kJ_Amb", "Tor_kJ_NestTs", "Tor_kJ_Cup"))

## Plots
ggplot(morpho, aes(variable, value)) + my_theme + 
  geom_line(aes(group=Nest_ID, col=Nest_ID))

ggplot(nest, aes(Wall_AC_outer_diameter_mm,Wall_AC_cup_diameter_mm)) + my_theme + 
  geom_point() + geom_smooth(method = "lm", stat="smooth")

## Temperature at warmed equilibrium when heated sphere was put into the nest
ggplot(m.long[m.long$Variable=="EqbTemp" & m.long$Side=="Cup",], aes(Temp,value)) + my_theme + 
  geom_point(aes(col=Species))

ggplot(m.long[m.long$Variable=="EqbTemp",], aes(Temp,value)) + my_theme + 
  geom_point(aes(col=Side)) + facet_grid(.~Species) +
  geom_smooth(method="lm", stat="smooth", aes(group=Side, col=Side)) +
  scale_color_viridis_d()

## Temperature at cooled equilibrium when heated sphere was removed from nest
ggplot(m.long[m.long$Variable=="Cooled_EqbTemp",], aes(Temp,value)) + my_theme + 
  geom_point(aes(col=Side)) + facet_grid(.~Species) +
  geom_smooth(method="lm", stat="smooth", aes(group=Side, col=Side)) +
  scale_color_viridis_d()


convec <- m.long %>% filter(
  Variable %in% "Convec_EqbTemp", 
  !Side %in% c("NestTs-Amb", "Tcup-Amb")
)

ggplot(convec, aes(Temp,value)) + my_theme + 
  geom_point(aes(col=Side)) + facet_grid(.~Species) +
  geom_smooth(method="lm", stat="smooth", aes(group=Side, col=Side)) +
  scale_color_viridis_d()


## Time to warmed equilibrium when heated sphere was put into the nest
## Equilibrium time was variable, usually less than 30 minutes
ggplot(m.long[m.long$Variable=="EqbTime",], aes(Temp, value)) + my_theme + 
  geom_point(aes(col=Side)) + facet_grid(.~Species) +
  #geom_smooth(method="lm", stat="smooth", aes(group=Side, col=Side)) +
  scale_color_viridis_d()

## Time to cooled equilibrium when heated sphere was removed
ggplot(m.long[m.long$Variable=="Cooled_EqbTime",], aes(Temp, value)) + my_theme + 
  geom_point(aes(col=Side)) + facet_grid(.~Species) +
  #geom_smooth(method="lm", stat="smooth", aes(group=Side, col=Side)) +
  scale_color_viridis_d()

deltas <- m.long %>% filter(
  Variable %in% "Convec_EqbTemp", 
  Side %in% c("NestTs_Amb", "Tcup_Amb", "Amb")
)

NestSurfTemp <- m.long %>% filter(
  Variable %in% "Convec_EqbTemp", 
  Side %in% c("Nest_Ts")
)

ggplot(NestSurfTemp, aes(Temp,value)) + my_theme + 
  geom_point() + #facet_grid(.~Species) +
  geom_smooth(method="lm", stat="smooth") +
  scale_color_viridis_d()

tint[tint$Amb_Temp_FINAL==min(tint$Amb_Temp_FINAL),]
tint[tint$Amb_Temp_FINAL==max(tint$Amb_Temp_FINAL),]
ggplot(tint[tint$Day==20 & tint$Month==2,], aes(Hour_Elapsed,Amb_Temp_FINAL)) + my_theme + 
  geom_point(aes(col=Nest_ID)) + geom_line(aes(group=Nest_ID, col=Nest_ID))

ggplot(tint[tint$Month==4 & (tint$Day==20|tint$Day==21),], aes(Hour_Elapsed,Amb_Temp_FINAL)) + my_theme + 
  #geom_point(aes(col=Nest_ID)) + 
  geom_line(aes(group=Nest_ID, col=Nest_ID))

ggplot(tint[tint$Day==20 & tint$Month==4,], aes(Hour_Elapsed,Amb_Temp_FINAL)) + my_theme + 
  geom_point(aes(col=Nest_ID)) + geom_line(aes(group=Nest_ID, col=Nest_ID))

ggplot(tint[tint$Day==20 & tint$Month==4,], aes(Hour_Elapsed,Nest_ID)) + my_theme + 
  geom_tile(aes(fill=Amb_Temp_FINAL)) +
  scale_fill_viridis()

# Hourly ambient temps per nest and night, heat map
ggplot(tint_hourly, aes(Hour_Elapsed, Night_ID)) + my_theme + 
  geom_tile(aes(fill=AmbTemp)) + scale_x_continuous(breaks =seq(0,15,1)) +
  scale_fill_viridis()

# Hourly eye temps per nest and night, heat map. Why so similar to Amb plot?
ggplot(tint_hourly, aes(Hour_Elapsed, Night_ID)) + my_theme +
  geom_tile(aes(fill=EyeTemp)) + scale_x_continuous(breaks =seq(0,15,1)) +
  scale_fill_viridis()

ggplot(data=tint[tint$Night_ID=="N3_10.2.2017",], aes(Hour_Elapsed, Amb_Temp_FINAL)) + my_theme + 
  geom_smooth(aes(x=Hour_Elapsed, y=Amb_Temp_FINAL), col="black") +
  geom_smooth(aes(x=Hour_Elapsed, y=Peye_Temp_FINAL), col="red")

ggplot(data=tint, aes(Hour_Elapsed, Amb_Temp_FINAL)) + my_theme + 
  geom_smooth(aes(x=Hour_Elapsed, y=Amb_Temp_FINAL, col=Night_ID,), linetype="dashed", method="loess") +
  geom_smooth(aes(x=Hour_Elapsed, y=Peye_Temp_FINAL, col=Night_ID), method="loess") +
  scale_color_viridis(discrete = F)

ggplot(data=tint, aes(Hour_Elapsed, Amb_Temp_FINAL)) + my_theme + facet_wrap(.~Nest_ID) +
  geom_smooth(aes(x=Hour_Elapsed, y=Amb_Temp_FINAL, col=Nest_ID,), linetype="dashed", method="loess") +
  geom_smooth(aes(x=Hour_Elapsed, y=Peye_Temp_FINAL, col=Nest_ID), method="loess") +
  scale_color_viridis(discrete = T)

## Metabolic rates, per hour, under different temperature regimes
ggplot(data=m.tint[m.tint$variable != "AmbTemp",], aes(Hour_Elapsed, value)) + my_theme +
  geom_smooth(aes(col=variable), method="loess") + ylab("kJ_hour") + facet_wrap(.~Night_ID)

## Hourly Metabolic rates, under different temperature regimes
ggplot(data=m.tint[m.tint$variable != "AmbTemp",], aes(variable, value)) + my_theme +
  geom_boxplot(aes(col=variable)) + ylab("kJ_hour")

## Total NEE, just normo
ggplot(data=m.nee_normo, aes(variable, value)) + my_theme +
  geom_boxplot(aes(col=variable)) + ylab ("kJ/hour")

## Total NEE, comparing normo and max torpor
ggplot(data=m.nee, aes(variable, value)) + my_theme +
  geom_boxplot(aes(col=variable)) + ylab ("kJ/hour") +
  scale_color_viridis_d()




lm(deltas$value[deltas$Side=="NestTs_Amb" & deltas$Species=="BBLH"]~deltas$value[deltas$Side=="Amb" & deltas$Species=="BBLH"])
## Nest Ts = 8.4325 - 0.2334(Ta) for BBLH

lm(deltas$value[deltas$Side=="Tcup_Amb" & deltas$Species=="BBLH"]~deltas$value[deltas$Side=="Amb"& deltas$Species=="BBLH"])
## Tcup = 50.425 - 1.166(Ta) for BBLH

lm(deltas$value[deltas$Side=="NestTs_Amb" & deltas$Species=="BCHU"]~deltas$value[deltas$Side=="Amb" & deltas$Species=="BCHU"])
## Nest Ts = 7.5263 - 0.2094(Ta) for BCHU

lm(deltas$value[deltas$Side=="Tcup_Amb" & deltas$Species=="BCHU"]~deltas$value[deltas$Side=="Amb"& deltas$Species=="BCHU"])
## Tcup Ts = 38.7361 - 0.9016(Ta) for BCHU


cor(deltas$value[deltas$Side=="NestTs_Amb" & deltas$Species=="BBLH"], deltas$value[deltas$Side=="Amb" & deltas$Species=="BBLH"])

cor(deltas$value[deltas$Side=="NestTs_Amb" & deltas$Species=="BCHU"], deltas$value[deltas$Side=="Amb" & deltas$Species=="BCHU"])

cor(deltas$value[deltas$Side=="Tcup_Amb" & deltas$Species=="BBLH"], deltas$value[deltas$Side=="Amb"& deltas$Species=="BBLH"])

cor(deltas$value[deltas$Side=="Tcup_Amb" & deltas$Species=="BCHU"], deltas$value[deltas$Side=="Amb"& deltas$Species=="BCHU"])
