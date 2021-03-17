# Trying out a model of energy expenditure in nesting female hummingbirds

# Reading in packages
library(ggplot2)
library(here)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)
library(mgcv) ## To fit gam to costas tnz data
library(segmented) # To fit piecewise, segmented, regression lines to TNZ data
library(tibble) # to use add_column
library(gridExtra) ## To put NEE and temp plots side by side

## read in files
nest <- read.csv("DPowers_Nest_data.csv") #Nest insulation and convection measurements
tint <- read.csv("final_data_Apr2020.csv") #LA nest measurements
#model <- read.csv("TINT_Model_template.csv") #model columns
nest_long <- read.csv("nest_longform.csv")
tnz <- read.csv("JAvBiol_BroadBill.csv") ## For TNZ modelling
#costas <- read.csv("FuncEcol_Costa1986_DonVO2.csv")  ## For above UCT TNZ modelling

## Subset BBLH nest data from all of Don's nest temperature measurements
nest_bblh <- nest_long[nest_long$Species=="BBLH",]

## General functions
my_theme <- theme_classic(base_size = 15) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_gradient <- c("#372772", "#4CB5AE","#F2B880","#b7adcf","#db2763","#588157","#69306d","#4f646f")

## Axis labels
Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))
Percival.lab <- expression(atop(paste("Percival Temperature (", degree,"C)")))
Thermocouple.lab <- expression(atop(paste("Thermocouple Temperature (", degree,"C)")))

## Organize data frames
# morpho <- melt(nest, id.vars="Nest_ID", 
#                measure.vars=c("Wall_AC_outer_diameter_mm","Wall_BD_outer_diameter_mm",
#                               "Wall_AC_cup_diameter_mm", "A_cup_height_mm"))

m.long <- melt(nest_bblh, id.vars=c("Nest_ID", "Measure", "Side"),
               measure.vars=c("Temp_05C", "Temp_10C", "Temp_15C", "Temp_20C",
                              "Temp_25C", "Temp_30C", "Temp_35C", "Temp_40C"))

m.long <- m.long %>% 
  dplyr::rename(TempVar = variable)

m.long <- m.long %>% separate(TempVar, c(NA, "Temp", NA), sep = "([_ C])")

m.long$Temp <- as.numeric(as.character(m.long$Temp))


## Making new data frames for convection, split by sides differently
temp_ABCD <- m.long %>% filter(
  Measure %in% "EqbTemp", 
  Side %in% c("A", "B", "C", "D")
)

# avgs <- as.data.frame(temp_ABCD %>% 
#                         dplyr::group_by(Nest_ID, Measure, Temp) %>%
#     dplyr::summarize(value = mean(value)))
# Side <- "SideAvg"
# avgs <- add_column(avgs, Side, .after = "Measure")

deltas <- m.long %>% filter(
  Measure %in% "EqbTemp", 
  Side %in% c("A", "B", "C", "D")
)
#deltas <- rbind(deltas, avgs)

avgs <- as.data.frame(deltas %>% 
                        dplyr::group_by(Nest_ID, Measure, Temp) %>%
                        dplyr::summarize(value = mean(value, na.rm=T)))
Side <- "SideAvg"
avgs <- add_column(avgs, Side, .after = "Measure")

deltas <- rbind(deltas, avgs, m.long %>% filter(
  Measure %in% "Convec_EqbTemp", 
  Side %in% c("Amb", "Nest_Ts")
))

deltas$Side <- factor(deltas$Side, levels = c("Amb", "A", "B", "C", "D", "SideAvg", "Nest_Ts"))

ggplot(deltas, aes(Temp, value)) + geom_smooth(aes(group=Side, col=Side), method="lm") + 
  geom_point() + my_theme + geom_abline(slope=1, intercept=0) + 
  ylab("Thermocouple measurements") + scale_color_viridis_d()

# ggplot(deltas[deltas$Side==c("A", "SideAvg", "D"),], aes(Temp, value)) + geom_smooth(aes(group=Side, col=Side), method = "lm") + geom_point() + my_theme +
#   geom_abline(slope=1, intercept=0)

#ggplot(m.long[m.long$Measure=="Convec_EqbTemp",], aes(Temp, value)) + geom_smooth(aes(group=Side, col=Side)) + geom_point() + my_theme +
 # geom_abline(slope=1)

# NestSurfTemp <- m.long %>% filter(
#   Measure %in% "Convec_EqbTemp",
#   Side %in% c("Nest_Ts")
# )

# convec <- m.long %>% filter(
#   Measure %in% "Convec_EqbTemp", 
#   !Side %in% c("NestTs-Amb", "Tcup-Amb")
# )

#Adding "Day" column to TINT data frame
tint$Date <- tint$Date_Time
tint <- tint %>% separate(Date, c(NA, "Day", NA), sep = "/")
tint$Day <- as.numeric(as.character(tint$Day))
tint$DayMonth <- paste0(tint$Day, "/", tint$Month)

## Adding in MR columns
## Calculate Lower TNZ slope equation for broad-bills
tnz_eqn <- lm(VO2_Normothermic~Temp_C, tnz)

## Costas and broadbills TNZ merged
# m.tnz_costas <- melt(costas, id.vars = "Temperature", measure.vars="VO2_ml.g.h")
# m.tnz_costas$Species <- "costas"
# m.tnz_costas$N_T <- "N"

m.tnz_bblh <- melt(tnz, id.vars = c("Temp_C", "N_T"), measure.vars = "VO2_all")
m.tnz_bblh$Species <- "bblh"


# m.tnz_costas <- m.tnz_costas %>% 
#   rename(
#     Temp_C = Temperature
#     )
# 
# gam(VO2_all~s(Temp_C) + s())
# 
# m.tnz <- rbind(m.tnz_bblh, m.tnz_costas)
# m.tnz$Species_NT <- paste0(m.tnz$Species, "_", m.tnz$N_T) 

## Get average hourly Amb and eye temp per night
tint_hourly <- as.data.frame(tint %>%
                               dplyr::group_by(Night_ID, Nest_ID, Hour_Elapsed) %>%
                               dplyr::summarize(AmbTemp = mean(Amb_Temp_FINAL, na.rm = T),
                                         EyeTemp = mean(Peye_Temp_FINAL, na.rm=T)))


## Extract just Hours and RER columns
#m.model <- model %>%
# select(Hours_elapsed_sunset, RER)
tint_hourly$RER <- 0
tint_hourly$RER <- ifelse(tint_hourly$Hour_Elapsed<3,1,0.71)

## intercept: tnz_eqn$coefficients[1]
## slope: tnz_eqn$coefficients[2]

## Calculate VO2 given ambient temp is amb temp
tint_hourly$VO2_amb <- tnz_eqn$coefficients[1] + (tnz_eqn$coefficients[2]*tint_hourly$AmbTemp)

## Convert O2 ml/min to kJ/hour
tint_hourly$kJ_amb <- tint_hourly$VO2_amb*((16 + (5.164*tint_hourly$RER))/1000)*60


## Equations for Side A, D, NestTs, and Cup

### For Side A
SideA_eqn <- lm(deltas$value[deltas$Side=="A"]~deltas$Temp[deltas$Side=="A"])
tint_hourly$SideA <- SideA_eqn$coefficients[1] + (SideA_eqn$coefficients[2]*tint_hourly$AmbTemp)
## Calculate VO2 given ambient temp is nest surface temp
tint_hourly$VO2_SideA <- tnz_eqn$coefficients[1] + (tnz_eqn$coefficients[2]*tint_hourly$SideA)
## Convert O2 ml/min to kJ/hour
tint_hourly$kJ_SideA <- tint_hourly$VO2_SideA*((16 + (5.164*tint_hourly$RER))/1000)*60

### For Side B
SideB_eqn <- lm(deltas$value[deltas$Side=="B"]~deltas$Temp[deltas$Side=="B"])
tint_hourly$SideB <- SideB_eqn$coefficients[1] + (SideB_eqn$coefficients[2]*tint_hourly$AmbTemp)
## Calculate VO2 given ambient temp is nest surface temp
tint_hourly$VO2_SideB <- tnz_eqn$coefficients[1] + (tnz_eqn$coefficients[2]*tint_hourly$SideB)
## Convert O2 ml/min to kJ/hour
tint_hourly$kJ_SideB <- tint_hourly$VO2_SideB*((16 + (5.164*tint_hourly$RER))/1000)*60

### For Side C
SideC_eqn <- lm(deltas$value[deltas$Side=="C"]~deltas$Temp[deltas$Side=="C"])
tint_hourly$SideC <- SideC_eqn$coefficients[1] + (SideC_eqn$coefficients[2]*tint_hourly$AmbTemp)
## Calculate VO2 given ambient temp is nest surface temp
tint_hourly$VO2_SideC <- tnz_eqn$coefficients[1] + (tnz_eqn$coefficients[2]*tint_hourly$SideC)
## Convert O2 ml/min to kJ/hour
tint_hourly$kJ_SideC <- tint_hourly$VO2_SideC*((16 + (5.164*tint_hourly$RER))/1000)*60


### For Side D
SideD_eqn <- lm(deltas$value[deltas$Side=="D"]~deltas$Temp[deltas$Side=="D"])
tint_hourly$SideD <- SideD_eqn$coefficients[1] + (SideD_eqn$coefficients[2]*tint_hourly$AmbTemp)
## Calculate VO2 given ambient temp is nest surface temp
tint_hourly$VO2_SideD <- tnz_eqn$coefficients[1] + (tnz_eqn$coefficients[2]*tint_hourly$SideD)
## Convert O2 ml/min to kJ/hour
tint_hourly$kJ_SideD <- tint_hourly$VO2_SideD*((16 + (5.164*tint_hourly$RER))/1000)*60


## For average of A, B, C D
SideAvg_eqn <- lm(deltas$value[deltas$Side=="SideAvg"]~deltas$Temp[deltas$Side=="SideAvg"])
tint_hourly$SideAvg <- SideAvg_eqn$coefficients[1] + (SideAvg_eqn$coefficients[2]*tint_hourly$AmbTemp)
## Calculate VO2 given ambient temp is nest surface temp
tint_hourly$VO2_SideAvg <- tnz_eqn$coefficients[1] + (tnz_eqn$coefficients[2]*tint_hourly$SideAvg)
## Convert O2 ml/min to kJ/hour
tint_hourly$kJ_SideAvg <- tint_hourly$VO2_SideAvg*((16 + (5.164*tint_hourly$RER))/1000)*60


### For Nest Ts
NestTs_eqn <- lm(deltas$value[deltas$Side=="Nest_Ts"]~deltas$Temp[deltas$Side=="Nest_Ts"])
## Nest Ts = 8.4325 - 0.2334(Ta) for BBLH
tint_hourly$NestTs <- NestTs_eqn$coefficients[1] + (NestTs_eqn$coefficients[2]*tint_hourly$AmbTemp)
## Calculate VO2 given ambient temp is nest surface temp
tint_hourly$VO2_NestTs <- tnz_eqn$coefficients[1] + (tnz_eqn$coefficients[2]*tint_hourly$NestTs)
## Convert O2 ml/min to kJ/hour
tint_hourly$kJ_NestTs <- tint_hourly$VO2_NestTs*((16 + (5.164*tint_hourly$RER))/1000)*60


### Torpor calculations
tor_int <- 0.46 ## from BBLH(?), torpor intercept
tor_slope <- 0.053 ##  from BBLH(?), torpor slope
tor2 <- 0.0016  ## from BBLH(?), for quadratic

## Create columns to add torpor MR measurements
tint_hourly$TorporAmb <- 0
tint_hourly$TorporSideA <- 0
tint_hourly$TorporSideB <- 0
tint_hourly$TorporSideC <- 0
tint_hourly$TorporSideD <- 0
tint_hourly$TorporSideAvg <- 0
tint_hourly$TorporNestTs <- 0
#tint_hourly$TorporCup <- 0

## Calculating torpor MR in kJ for each type of temperature measurement
for(i in 1:nrow(tint_hourly)) {
  if(tint_hourly$Hour_Elapsed[i]<14 & tint_hourly$Hour_Elapsed[i]>6) {
    tint_hourly$TorporAmb[i] <- (tor_int - tor_slope*(tint_hourly$AmbTemp[i]) + tor2*((tint_hourly$AmbTemp[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
    tint_hourly$TorporSideA[i] <- (tor_int - tor_slope*(tint_hourly$SideA[i]) + tor2*((tint_hourly$SideA[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
    tint_hourly$TorporSideB[i] <- (tor_int - tor_slope*(tint_hourly$SideB[i]) + tor2*((tint_hourly$SideB[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
    tint_hourly$TorporSideC[i] <- (tor_int - tor_slope*(tint_hourly$SideC[i]) + tor2*((tint_hourly$SideC[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
    tint_hourly$TorporSideD[i] <- (tor_int - tor_slope*(tint_hourly$SideD[i]) + tor2*((tint_hourly$SideD[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
    tint_hourly$TorporSideAvg[i] <- (tor_int - tor_slope*(tint_hourly$SideAvg[i]) + tor2*((tint_hourly$SideAvg[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
    tint_hourly$TorporNestTs[i] <- (tor_int - tor_slope*(tint_hourly$NestTs[i]) + tor2*((tint_hourly$NestTs[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
    #tint_hourly$TorporCup[i] <- (tor_int - tor_slope*(tint_hourly$TempCup[i]) + tor2*((tint_hourly$TempCup[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
  } 
  #if(tint_hourly$Hour_Elapsed[i]<14 & tint_hourly$Hour_Elapsed[i]>6 & tint_hourly$TempCup[i] <31) {
   # tint_hourly$TorporCup[i] <- (tor_int - tor_slope*(tint_hourly$TempCup[i]) + tor2*((tint_hourly$TempCup[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
  #}
}

## Melt metabolic rates in kJ
m.tint <- melt(tint_hourly,id.vars = c("Night_ID", "Nest_ID", "Hour_Elapsed"), 
               measure.vars = c("kJ_amb", "kJ_SideA", "kJ_SideB","kJ_SideC", "kJ_SideD", "kJ_SideAvg",  "kJ_NestTs",
                                "TorporAmb","TorporSideA", "TorporSideB", "TorporSideC", "TorporSideD", "TorporSideAvg", "TorporNestTs"))

## Column to specify normo vs. torpor
#m.tint$Tornor <- ifelse(m.tint$variable=="kJ_amb"|m.tint$variable=="kJ_NestTs"|m.tint$variable=="kJ_Cup","N","T")

## Making column with hourly MR assuming 7 hours of torpor use, and the rest normo
tint_hourly$Tor_Nor_merged_Amb <- tint_hourly$TorporAmb
for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor_Nor_merged_Amb[i]==0) {
  tint_hourly$Tor_Nor_merged_Amb[i] <- tint_hourly$kJ_amb[i]}
}

tint_hourly$Tor_Nor_merged_SideA <- tint_hourly$TorporSideA
for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor_Nor_merged_SideA[i]==0) {
  tint_hourly$Tor_Nor_merged_SideA[i] <- tint_hourly$kJ_SideA[i]}
}

tint_hourly$Tor_Nor_merged_SideB <- tint_hourly$TorporSideB
for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor_Nor_merged_SideB[i]==0) {
  tint_hourly$Tor_Nor_merged_SideB[i] <- tint_hourly$kJ_SideB[i]}
}

tint_hourly$Tor_Nor_merged_SideC <- tint_hourly$TorporSideC
for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor_Nor_merged_SideC[i]==0) {
  tint_hourly$Tor_Nor_merged_SideC[i] <- tint_hourly$kJ_SideC[i]}
}

tint_hourly$Tor_Nor_merged_SideD <- tint_hourly$TorporSideD
for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor_Nor_merged_SideD[i]==0) {
  tint_hourly$Tor_Nor_merged_SideD[i] <- tint_hourly$kJ_SideD[i]}
}

tint_hourly$Tor_Nor_merged_SideAvg <- tint_hourly$TorporSideAvg
for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor_Nor_merged_SideAvg[i]==0) {
  tint_hourly$Tor_Nor_merged_SideAvg[i] <- tint_hourly$kJ_SideAvg[i]}
}

tint_hourly$Tor_Nor_merged_NestTs <- tint_hourly$TorporNestTs
for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor_Nor_merged_NestTs[i]==0) {
  tint_hourly$Tor_Nor_merged_NestTs[i] <- tint_hourly$kJ_NestTs[i]}
}
 
# tint_hourly$Tor_Nor_merged_Cup <- tint_hourly$TorporCup
# for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor_Nor_merged_Cup[i]==0) {
#   tint_hourly$Tor_Nor_merged_Cup[i] <- tint_hourly$kJ_Cup[i]}
# }

## Temperatures from TINT data
m.tint_temp <- melt(tint_hourly, id.vars = c("Night_ID", "Nest_ID"), 
              measure.vars = c("AmbTemp", "SideA", "SideB", "SideC", "SideD", "SideAvg", "NestTs"))
## Rename columns
names(m.tint_temp) <- c("Night_ID", "Nest_ID", "Side", "value")

## Calculating NEE, first three normo, and next 3 cols with 7 hours of torpor
tint_nee <- as.data.frame(tint_hourly %>%
                            dplyr::group_by(Night_ID, Nest_ID) %>%
                            dplyr::summarize(Nor_kJ_Amb = sum(kJ_amb, na.rm = T),
                                      Nor_kJ_SideA = sum(kJ_SideA, na.rm = T),
                                      Nor_kJ_SideB = sum(kJ_SideB, na.rm = T),
                                      Nor_kJ_SideC = sum(kJ_SideC, na.rm = T),
                                      Nor_kJ_SideD = sum(kJ_SideD, na.rm = T),
                                      Nor_kJ_SideAvg = sum(kJ_SideAvg, na.rm = T),
                                      Nor_kJ_NestTs = sum(kJ_NestTs, na.rm = T),
                                      #Nor_kJ_Cup = sum(kJ_Cup, na.rm = T),
                                      Tor_kJ_Amb = sum(Tor_Nor_merged_Amb, na.rm = T),
                                      Tor_kJ_SideA = sum(Tor_Nor_merged_SideA, na.rm = T),
                                      Tor_kJ_SideB = sum(Tor_Nor_merged_SideB, na.rm = T),
                                      Tor_kJ_SideC = sum(Tor_Nor_merged_SideC, na.rm = T),
                                      Tor_kJ_SideD = sum(Tor_Nor_merged_SideD, na.rm = T),
                                      Tor_kJ_SideAvg = sum(Tor_Nor_merged_SideAvg, na.rm = T),
                                      Tor_kJ_NestTs = sum(Tor_Nor_merged_NestTs, na.rm = T),
                                      #Tor_kJ_Cup = sum(Tor_Nor_merged_Cup, na.rm = T)
                                      ))

night_length <- as.data.frame(tint_hourly %>%
  group_by(Night_ID, Nest_ID) %>%
  summarise(NightLength = n_distinct(Hour_Elapsed)))
## Adding Night length column to tint_nee data frame
tint_nee <- merge(tint_nee, night_length)


m.nee <- melt(tint_nee, id.vars = c("Night_ID", "Nest_ID", "NightLength"), 
              measure.vars = c("Nor_kJ_Amb", "Nor_kJ_SideA", "Nor_kJ_SideB", "Nor_kJ_SideC", "Nor_kJ_SideD", "Nor_kJ_SideAvg", "Nor_kJ_NestTs",# "Nor_kJ_Cup",
                               "Tor_kJ_Amb", "Tor_kJ_SideA", "Tor_kJ_SideB", "Tor_kJ_SideC", "Tor_kJ_SideD", "Tor_kJ_SideAvg", "Tor_kJ_NestTs" #, "Tor_kJ_Cup"
                               ))

m.nee <- m.nee %>% separate(variable, c("Tornor", NA, "Side"), sep = "_", remove=F)
m.nee$Tornor <- plyr::revalue(m.nee$Tornor, c("Nor"="Normothermic", "Tor"="Torpor_7hr"))

## Order sides
m.nee$Side <- factor(m.nee$Side, levels = c("Amb", "SideA", "SideB", "SideC", "SideD", "SideAvg", "NestTs" #, "Cup"
                                            ))

## Standardize NEE by night length
m.nee$stdNEE <- (m.nee$value/m.nee$NightLength)*12

## Difference in NEE between different sides
m.nee_diff <- data.frame(Night_ID = m.nee$Night_ID[m.nee$Side=='Amb'],
                         Nest_ID = m.nee$Nest_ID[m.nee$Side=='Amb'],
                         variable = m.nee$variable[m.nee$Side=='Amb'],
                         Tornor = m.nee$Tornor[m.nee$Side=='Amb'],
                         Amb_SideA = m.nee$value[m.nee$Side=='Amb'] - m.nee$value[m.nee$Side == 'SideA'],
                         Amb_SideB = m.nee$value[m.nee$Side=='Amb'] - m.nee$value[m.nee$Side == 'SideB'],
                         Amb_SideC = m.nee$value[m.nee$Side=='Amb'] - m.nee$value[m.nee$Side == 'SideC'],
                         Amb_SideD = m.nee$value[m.nee$Side=='Amb'] - m.nee$value[m.nee$Side == 'SideD'],
                         Amb_SideAvg = m.nee$value[m.nee$Side=='Amb'] - m.nee$value[m.nee$Side == 'SideAvg'],
                         Amb_NestTs = m.nee$value[m.nee$Side=='Amb'] - m.nee$value[m.nee$Side == 'NestTs']
)
m.nee_diff <- m.nee_diff %>% separate(variable, c("Tornor", NA, NA), sep = "_", remove=F)
m.nee_diff2 <- melt(m.nee_diff, id.vars = c("Night_ID", "Nest_ID", "Tornor"), measure.vars = c("Amb_SideA", "Amb_SideB",
                                                                                               "Amb_SideC", "Amb_SideD", "Amb_SideAvg",
                                                                                               "Amb_NestTs"))
m.nee_diff2$Tornor <- plyr::revalue(m.nee_diff2$Tornor, c("Nor"="Normothermic", "Tor"="Torpor_7hr"))


# Standardize NEE by nightlength
m.nee_diff_std <- m.nee_diff <- data.frame(Night_ID = m.nee$Night_ID[m.nee$Side=='Amb'],
                                      Nest_ID = m.nee$Nest_ID[m.nee$Side=='Amb'],
                                      variable = m.nee$variable[m.nee$Side=='Amb'],
                                      Tornor = m.nee$Tornor[m.nee$Side=='Amb'],
                                      Amb_SideA = m.nee$stdNEE[m.nee$Side=='Amb'] - m.nee$stdNEE[m.nee$Side == 'SideA'],
                                      Amb_SideB = m.nee$stdNEE[m.nee$Side=='Amb'] - m.nee$stdNEE[m.nee$Side == 'SideB'],
                                      Amb_SideC = m.nee$stdNEE[m.nee$Side=='Amb'] - m.nee$stdNEE[m.nee$Side == 'SideC'],
                                      Amb_SideD = m.nee$stdNEE[m.nee$Side=='Amb'] - m.nee$stdNEE[m.nee$Side == 'SideD'],
                                      Amb_SideAvg = m.nee$stdNEE[m.nee$Side=='Amb'] - m.nee$stdNEE[m.nee$Side == 'SideAvg'],
                                      Amb_NestTs = m.nee$stdNEE[m.nee$Side=='Amb'] - m.nee$stdNEE[m.nee$Side == 'NestTs']
)
m.nee_diff_std <- m.nee_diff_std %>% separate(variable, c("Tornor", NA, NA), sep = "_", remove=F)
m.nee_diff_std <- melt(m.nee_diff_std, id.vars = c("Night_ID", "Nest_ID", "Tornor"), measure.vars = c("Amb_SideA", "Amb_SideB",
                                                                                               "Amb_SideC", "Amb_SideD", "Amb_SideAvg",
                                                                                               "Amb_NestTs"))
m.nee_diff_std$Tornor <- plyr::revalue(m.nee_diff_std$Tornor, c("Nor"="Normothermic", "Tor"="Torpor_7hr"))


## Proportional change in NEE depending on side temperature
m.nee_prop_std <- data.frame(Night_ID = m.nee$Night_ID[m.nee$Side=='Amb'],
                         Nest_ID = m.nee$Nest_ID[m.nee$Side=='Amb'],
                         variable = m.nee$variable[m.nee$Side=='Amb'],
                         Tornor = m.nee$Tornor[m.nee$Side=='Amb'],
                         Amb_SideA = ((m.nee$stdNEE[m.nee$Side=='Amb'] - m.nee$stdNEE[m.nee$Side == 'SideA'])/m.nee$stdNEE[m.nee$Side=='Amb'])*100,
                         Amb_SideB = ((m.nee$stdNEE[m.nee$Side=='Amb'] - m.nee$stdNEE[m.nee$Side == 'SideB'])/m.nee$stdNEE[m.nee$Side=='Amb'])*100,
                         Amb_SideC = ((m.nee$stdNEE[m.nee$Side=='Amb'] - m.nee$stdNEE[m.nee$Side == 'SideC'])/m.nee$stdNEE[m.nee$Side=='Amb'])*100,
                         Amb_SideD = ((m.nee$stdNEE[m.nee$Side=='Amb'] - m.nee$stdNEE[m.nee$Side == 'SideD'])/m.nee$stdNEE[m.nee$Side=='Amb'])*100,
                         Amb_SideAvg = ((m.nee$stdNEE[m.nee$Side=='Amb'] - m.nee$stdNEE[m.nee$Side == 'SideAvg'])/m.nee$stdNEE[m.nee$Side=='Amb'])*100,
                         Amb_NestTs = ((m.nee$stdNEE[m.nee$Side=='Amb'] - m.nee$stdNEE[m.nee$Side == 'NestTs'])/m.nee$stdNEE[m.nee$Side=='Amb'])*100
)
m.nee_prop_std <- m.nee_prop_std %>% separate(variable, c("Tornor", NA, NA), sep = "_", remove=F)
m.nee_prop_std <- melt(m.nee_prop_std, id.vars = c("Night_ID", "Nest_ID", "Tornor"), measure.vars = c("Amb_SideA", "Amb_SideB",
                                                                                               "Amb_SideC", "Amb_SideD", "Amb_SideAvg",
                                                                                               "Amb_NestTs"))
m.nee_prop_std$Tornor <- plyr::revalue(m.nee_prop_std$Tornor, c("Nor"="Normothermic", "Tor"="Torpor_7hr"))


## Difference between normo and torpid NEE 
m.nee_Nor_tor <- data.frame(Night_ID = m.nee$Night_ID[m.nee$Tornor=='Normothermic'],
                         Nest_ID = m.nee$Nest_ID[m.nee$Tornor=='Normothermic'],
                         variable = m.nee$variable[m.nee$Tornor=='Normothermic'],
                         Nor_Tor = m.nee$value[m.nee$Tornor=='Normothermic'] - m.nee$value[m.nee$Tornor == 'Torpor_7hr'],
                         Nor_Tor_std = m.nee$stdNEE[m.nee$Tornor=='Normothermic'] - m.nee$stdNEE[m.nee$Tornor == 'Torpor_7hr']
)
m.nee_Nor_tor <- m.nee_Nor_tor %>% separate(variable, c(NA, NA, "Side"), sep = "_", remove=F)
m.nee_Nor_tor <- melt(m.nee_Nor_tor, id.vars = c("Night_ID", "Nest_ID", "Side"), measure.vars = c("Nor_Tor", "Nor_Tor_std"))
m.nee_Nor_tor$variable <- plyr::revalue(m.nee_Nor_tor$variable, c("Nor_Tor"="Normo-Torpor NEE", "Nor_Tor_std"="Nor-Tor NEE Standardized"))


## Plots
ggplot(morpho, aes(variable, value)) + my_theme + 
  geom_line(aes(group=Nest_ID, col=Nest_ID))

ggplot(nest, aes(Wall_AC_outer_diameter_mm,Wall_AC_cup_diameter_mm)) + my_theme + 
  geom_point() + geom_smooth(method = "lm", stat="smooth")

## Temperature at warmed equilibrium when heated sphere was put into the nest
# ggplot(m.long[m.long$Variable=="EqbTemp" & m.long$Side=="Cup",], aes(Temp,value)) + my_theme + 
#   geom_point(aes(col=Species))

## Old exploratory plot, when all species were included
ggplot(m.long[m.long$Measure=="EqbTemp",], aes(Temp,value)) + my_theme + 
  geom_point(aes(col=Side)) + #facet_grid(.~Species) +
  geom_smooth(method="lm", stat="smooth", aes(group=Side, col=Side)) +
  scale_color_viridis_d()

## Temperature at cooled equilibrium when heated sphere was removed from nest
ggplot(m.long[m.long$Variable=="Cooled_EqbTemp",], aes(Temp,value)) + my_theme + 
  geom_point(aes(col=Side)) + #facet_grid(.~Species) +
  geom_smooth(method="lm", stat="smooth", aes(group=Side, col=Side)) +
  scale_color_viridis_d()

# ## Temperature of each thermocouple plotted against Percival's set temperature (using convection temperature measurements)
# ggplot(convec, aes(Temp,value)) + my_theme + 
#   geom_point(aes(col=Side)) + #facet_grid(.~Species) +
#   geom_smooth(method="lm", stat="smooth", aes(group=Side, col=Side)) +
#   scale_color_viridis_d() + ylab(Thermocouple.lab) + xlab(Percival.lab)


## Checking what convection vs. eqb vs. cooled equilibrium temperatures look like
ggplot(m.long[m.long$Measure==c("Convec_EqbTemp", "Cooled_EqbTemp", "EqbTemp"),], aes(Temp, value)) + my_theme + 
  geom_point(aes(col=Side)) + facet_grid(.~Measure) +
  geom_smooth(method="lm", stat="smooth", aes(group=Side, col=Side)) +
  scale_color_viridis_d() + ylab(Thermocouple.lab) + xlab(Percival.lab)


## Time to warmed equilibrium when heated sphere was put into the nest
## Equilibrium time was variable, usually less than 30 minutes
ggplot(m.long[m.long$Variable=="EqbTime",], aes(Temp, value)) + my_theme + 
  geom_point(aes(col=Side)) + #facet_grid(.~Species) +
  #geom_smooth(method="lm", stat="smooth", aes(group=Side, col=Side)) +
  scale_color_viridis_d()

## Time to cooled equilibrium when heated sphere was removed
ggplot(m.long[m.long$Variable=="Cooled_EqbTime",], aes(Temp, value)) + my_theme + 
  geom_point(aes(col=Side)) + #facet_grid(.~Species) +
  #geom_smooth(method="lm", stat="smooth", aes(group=Side, col=Side)) +
  scale_color_viridis_d()


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
  scale_fill_viridis() + theme(axis.text.y = element_text(size=10))

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

# ## Total NEE, just normo
# ggplot(data=m.nee_normo, aes(variable, value)) + my_theme +
#   geom_boxplot(aes(col=variable)) + ylab ("kJ/hour")

#### Main plots ####
## Total NEE, comparing normo and max torpor
nee.plot <- ggplot(data=m.nee, aes(Side, value)) + my_theme2 + facet_grid(.~Tornor) +
  geom_point(size=2) + geom_boxplot(aes(col=Side), size=1.2, show.legend=F) + ylab ("NEE (kJ)") +
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5)) +
  scale_color_manual(values = my_gradient)
nee.plot

## Temp plot to mirror this NEE plot; from Don's nest data
# temp.plot <- ggplot(data=deltas, aes(Side, value)) + my_theme2 + #facet_grid(.~Tornor) +
#   geom_boxplot(aes(col=Side), size=1.2) + geom_point() + ylab(Temp.lab) +
#   theme(axis.text.x = element_text(angle=90, vjust=0.5, size=15)) +
#   scale_color_manual(values = my_gradient)

## Temp plot to mirror this NEE plot; from TINT data
temp.plot_tint <- ggplot(data=m.tint_temp, aes(Side, value)) + my_theme2 + #facet_grid(.~Tornor) +
  geom_point(size=2) + geom_boxplot(aes(col=Side), size=1.2) + ylab(Temp.lab) +
  theme(axis.text.x = element_text(angle=90, size=15)) +
  scale_color_manual(values = my_gradient)

grid.arrange(nee.plot, temp.plot_tint, nrow=1, ncol=2)

nee.std.plot <- ggplot(data=m.nee, aes(Side, stdNEE)) + my_theme2 + facet_grid(.~Tornor) +
  geom_point(size=2) + geom_boxplot(aes(col=Side), size=1.2, show.legend=F) + ylab ("NEE (kJ) standardized to 12h night") +
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5)) +
  scale_color_manual(values = my_gradient)
nee.std.plot

nee.tor_std.plot <- ggplot(data=m.nee[m.nee$Tornor=="Torpor_7hr",], aes(Side, stdNEE)) + my_theme2 + facet_grid(.~Tornor) +
  geom_boxplot(aes(col=Side), size=1.2) + ylab ("NEE (kJ) standardized to 12h night") +
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5)) +
  geom_point(aes(col=as.factor(Night_ID)), size=2, show.legend = F) +
  scale_color_manual(values=c(my_gradient[2:9], cc))
nee.tor_std.plot 

grid.arrange(nee.std.plot, temp.plot_tint, nrow=1, ncol=2)


## Difference between Ambient and side temperatures
ggplot(data=m.nee_diff2, aes(variable, value)) + my_theme2 + facet_grid(.~Tornor) + 
  geom_point(size=2, show.legend = F) +
    geom_boxplot(aes(col=variable), show.legend = F, size=1.2) +
  theme(axis.text.x = element_text(angle=90, size=15)) +
  scale_color_manual(values=my_gradient[2:9]) + ylab("NEE (kJ) difference Ambient-sides") ## using my_gradient[2:9] to match other graphs

## Difference between Ambient and side temperatures NEE, with standardized night lengths
ggplot(data=m.nee_diff_std, aes(variable, value)) + my_theme2 + facet_grid(.~Tornor) + 
  geom_point(size=2, show.legend = F) +
  geom_boxplot(aes(col=variable), show.legend = F, size=1.2) +
  theme(axis.text.x = element_text(angle=90, size=15)) +
  scale_color_manual(values=my_gradient[2:9]) + ylab("NEE (kJ)/12h difference Ambient-sides")

## Difference between all-Normo and normo+7h torpor NEE, with original and standardized night lengths
ggplot(data=m.nee_Nor_tor, aes(Side, value)) + my_theme2 + facet_grid(.~variable) + 
  geom_point(size=2, show.legend = F) +
  geom_boxplot(aes(col=Side),  show.legend = F, size=1.2) +
  theme(axis.text.x = element_text(angle=90, size=15)) +
  scale_color_manual(values=my_gradient) + ylab("NEE (kJ)/12h difference Normo-Torpor")

## Percent difference between Ambient and side temperatures NEE, with standardized night lengths
cc <- scales::seq_gradient_pal("blue", "yellow", "Lab")(seq(0,1,length.out=length(unique(as.factor(m.nee_prop_std$Night_ID)))))
ggplot(data=m.nee_prop_std, aes(variable, value)) + my_theme2 + facet_grid(.~Tornor) + 
  geom_boxplot(aes(col=variable), show.legend = F, size=1.2) +
  #scale_color_manual(values=my_gradient[2:9]) +
  geom_point(aes(col=as.factor(Night_ID)), size=2, show.legend = F) +
  scale_color_manual(values=c(my_gradient[2:9], cc)) +
  theme(axis.text.x = element_text(angle=90, size=15)) +
  ylab("NEE (kJ) percent difference Ambient-sides")


ggplot(data=m.nee, aes(Side, value)) + my_theme2 + facet_grid(.~Tornor) +
  geom_boxplot(aes(col=Side), size=1.2, show.legend=F) + ylab ("NEE (kJ)") + geom_point() +
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5)) +
  scale_color_manual(values=my_gradient)
range(m.nee_diff2$value[m.nee_diff2$Tornor=="Normothermic"])
range(m.nee_diff2$value[m.nee_diff2$Tornor!="Normothermic"])



# ggplot(data=tint_hourly, aes(AmbTemp, SideA)) + my_theme + #facet_grid(.~Tornor) +
#   geom_point(aes(y=SideA), col="red", size=1.2, show.legend=F) +
#   geom_point(aes(y=SideB), col="black", size=1.2, show.legend=F) +
#   geom_point(aes(y=SideC), col="blue", size=1.2, show.legend=F) +
#   geom_point(aes(y=SideD), col="orange", size=1.2, show.legend=F) +
#   geom_point(aes(y=SideAvg), col="green", size=1.2, show.legend=F) +
#   ylab (Temp.lab) #geom_point(aes(col=Side))


## TNZ's for BBLH and Costas
ggplot(m.tnz, aes(Temp_C, value)) + my_theme + geom_point(aes(col=Species_NT)) +
  geom_smooth(data=m.tnz[m.tnz$Species_NT=="bblh_N",], aes(col=Species_NT), method="lm") +
  geom_smooth(data=m.tnz[m.tnz$Species_NT=="bblh_T"|m.tnz$Species_NT=="costas_N",], aes(col=Species_NT), method="loess") +
  xlab(Temp.lab) + ylab("VO2 ml/min")

ggplot(m.tint[m.tint$Night_ID=="N11_3.3.2017",], aes(Hour_Elapsed, value)) + my_theme + #facet_grid(.~Nest_ID) +
  geom_point(aes(col=variable)) + geom_line(aes(col=variable)) + ylab("kJ/hr") #+ #scale_x_continuous(breaks =seq(0,15,1)) 


## Did these when m.long had both species. Now it just has BBLH

lm(deltas$value[deltas$Side=="NestTs_Amb"]~deltas$value[deltas$Side=="Amb"])
## Nest Ts = 8.4325 - 0.2334(Ta) for BBLH

lm(deltas$value[deltas$Side=="Tcup_Amb"]~deltas$value[deltas$Side=="Amb"& deltas$Species=="BBLH"])
## Tcup = 50.425 - 1.166(Ta) for BBLH

lm(deltas$value[deltas$Side=="NestTs_Amb" & deltas$Species=="BCHU"]~deltas$value[deltas$Side=="Amb" & deltas$Species=="BCHU"])
## Nest Ts = 7.5263 - 0.2094(Ta) for BCHU

lm(deltas$value[deltas$Side=="Tcup_Amb" & deltas$Species=="BCHU"]~deltas$value[deltas$Side=="Amb"& deltas$Species=="BCHU"])
## Tcup Ts = 38.7361 - 0.9016(Ta) for BCHU


cor(deltas$value[deltas$Side=="NestTs_Amb" & deltas$Species=="BBLH"], deltas$value[deltas$Side=="Amb" & deltas$Species=="BBLH"])

cor(deltas$value[deltas$Side=="NestTs_Amb" & deltas$Species=="BCHU"], deltas$value[deltas$Side=="Amb" & deltas$Species=="BCHU"])

cor(deltas$value[deltas$Side=="Tcup_Amb" & deltas$Species=="BBLH"], deltas$value[deltas$Side=="Amb"& deltas$Species=="BBLH"])

cor(deltas$value[deltas$Side=="Tcup_Amb" & deltas$Species=="BCHU"], deltas$value[deltas$Side=="Amb"& deltas$Species=="BCHU"])
