# Trying out a model of energy expenditure in nesting female hummingbirds
# Given two temperature scenarios- either nest temperatures or ambient temperatures
# Anusha Shankar wrote this code, with inputs from Erich Eberts and Glenn Tattersall. Nest temperature data from Don Powers
# Data on temperatures experienced by birds in nests under natural conditions from Erich Eberts and his field team
# Contact Anusha at nushiamme<at>gmail<dot>com, and Erich at ebertser<at>gmail<dot>com


#### Code layout ####
# Read in packages
# Read in files
# General functions, theme
# 

  
#### Reading in packages ####
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
library(rstatix) ## For multiple pairwise comparisons
library(ggpubr) ## For multiple pairwise comparisons plot


#### Read in files ####
# File paths are relative to the directory that the .Rproj file resides in
#nest <- read.csv("DPowers_Nest_data.csv") #Nest insulation and convection measurements
tint <- read.csv("final_data_Apr2020.csv") #LA nest measurements
#model <- read.csv("TINT_Model_template.csv") #model columns
nest_long <- read.csv("nest_longform.csv")
tnz <- read.csv("JAvBiol_BroadBill.csv") ## For TNZ modelling
#costas <- read.csv("FuncEcol_Costa1986_DonVO2.csv")  ## For above UCT TNZ modelling
## Subset just BBLH (broad-billed hummingbird) nest data from all of Don's nest temperature measurements
nest_bblh <- nest_long[nest_long$Species=="BBLH",]


#### General functions ####
my_theme <- theme_classic(base_size = 15) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_gradient <- c("#372772", "#4CB5AE","#F2B880","#b7adcf","#db2763","#588157","#69306d","#4f646f")
erich_col <- c("#23988aff", "#F38BA8", "#440558ff", "#9ed93aff")
torpor_gradient <- c("#615d6c", "#6f8ab7", "#89bbfe", "#acedff", "0cf574", "#cae5ff")

## Axis labels
Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))
Percival.lab <- expression(atop(paste("Percival Temperature (", degree,"C)")))
Thermocouple.lab <- expression(atop(paste("Thermocouple Temperature (", degree,"C)")))


#### Organize data frames ####
# Morhphological data
# morpho <- melt(nest, id.vars="Nest_ID", 
#                measure.vars=c("Wall_AC_outer_diameter_mm","Wall_BD_outer_diameter_mm",
#                               "Wall_AC_cup_diameter_mm", "A_cup_height_mm"))

# Subset out just the necessary columns, and melt them. 
# This includes nest measurements from different sides of the nest, at different temperatures
m.long <- melt(nest_bblh, id.vars=c("Nest_ID", "Measure", "Side"),
               measure.vars=c("Temp_05C", "Temp_10C", "Temp_15C", "Temp_20C",
                              "Temp_25C", "Temp_30C", "Temp_35C", "Temp_40C"))
m.long <- m.long %>% 
  dplyr::rename(TempVar = variable)

m.long <- m.long %>% separate(TempVar, c(NA, "Temp", NA), sep = "([_ C])")

m.long$Temp <- as.numeric(as.character(m.long$Temp))


# Making new data frame for convection measurements, split by sides
nestTemps <- m.long %>% filter(
  Measure %in% "EqbTemp", 
  Side %in% c("A", "B", "C", "D")
)

## Average temperatures measurements from all sides of the nest to make a SideAvg measurement
avgs <- as.data.frame(nestTemps %>% 
                        dplyr::group_by(Nest_ID, Measure, Temp) %>%
                        dplyr::summarize(value = mean(value, na.rm=T)))
Side <- "SideAvg"
avgs <- add_column(avgs, Side, .after = "Measure")

nestTemps <- rbind(nestTemps, avgs, m.long %>% filter(
  Measure %in% "Convec_EqbTemp", 
  Side %in% c("Amb", "Nest_Ts")
))

nestTemps$Side <- factor(nestTemps$Side, levels = c("Amb", "A", "B", "C", "D", "SideAvg", "Nest_Ts"))

ggplot(nestTemps, aes(Temp, value)) + geom_smooth(aes(group=Side, col=Side), method="lm") + 
  geom_point() + my_theme + geom_abline(slope=1, intercept=0) + 
  ylab("Thermocouple measurements") + scale_color_viridis_d()

## Erich wants just Amb and SideAvg so using this from now on. Mar 22, 2021
Temp_Amb_SideAvg <- nestTemps[nestTemps$Side==c("Amb", "SideAvg"),]

#### Format data on the temperatures from the field ####
#Adding "Day" column to TINT data frame
tint$Date <- tint$Date_Time
tint <- tint %>% separate(Date, c(NA, "Day", NA), sep = "/")
tint$Day <- as.numeric(as.character(tint$Day))
tint$DayMonth <- paste0(tint$Day, "/", tint$Month)

## Get average hourly Amb and eye temp per night
tint_hourly <- as.data.frame(tint %>%
                               dplyr::group_by(Night_ID, Nest_ID, Hour_Elapsed) %>%
                               dplyr::summarize(AmbTemp = mean(Amb_Temp_FINAL, na.rm = T),
                                                EyeTemp = mean(Peye_Temp_FINAL, na.rm=T)))


#### Metabolic rate data from BBLH under a range of temperatures ####
## These data are from Shankar et al. J Avian Biology doi/10.1111/jav.02305
## Adding in MR columns
## Calculate Lower TNZ slope equation for broad-bills
tnz_eqn <- lm(VO2_Normothermic~Temp_C, tnz)

m.tnz_bblh <- melt(tnz, id.vars = c("Temp_C", "N_T"), measure.vars = "VO2_all")
m.tnz_bblh$Species <- "bblh"

#### Now putting together the field temperatures and lab-measured metabolic rates ####
## Set RER (Respiratory Exchange Ratio) depending on the hour
tint_hourly$RER <- 0
tint_hourly$RER <- ifelse(tint_hourly$Hour_Elapsed<3,1,0.71)

## intercept: tnz_eqn$coefficients[1]
## slope: tnz_eqn$coefficients[2]

## Calculate VO2 given ambient temp is amb temp
tint_hourly$VO2_amb <- tnz_eqn$coefficients[1] + (tnz_eqn$coefficients[2]*tint_hourly$AmbTemp)

## Convert O2 ml/min to kJ/hour
tint_hourly$kJ_amb <- tint_hourly$VO2_amb*((16 + (5.164*tint_hourly$RER))/1000)*60

## Equations for the relationship between a side's temp and the amb temp. Removed individual sides' and
# NestTs eqns on Mar 22, 2021.
## For average of A, B, C D
SideAvg_eqn <- lm(nestTemps$value[nestTemps$Side=="SideAvg"]~nestTemps$Temp[nestTemps$Side=="SideAvg"])
tint_hourly$SideAvg <- SideAvg_eqn$coefficients[1] + (SideAvg_eqn$coefficients[2]*tint_hourly$AmbTemp)
## Calculate VO2 given ambient temp is nest surface temp
tint_hourly$VO2_SideAvg <- tnz_eqn$coefficients[1] + (tnz_eqn$coefficients[2]*tint_hourly$SideAvg)
## Convert O2 ml/min to kJ/hour
tint_hourly$kJ_SideAvg <- tint_hourly$VO2_SideAvg*((16 + (5.164*tint_hourly$RER))/1000)*60


## Temperatures from TINT data
m.tint_temp <- melt(tint_hourly, id.vars = c("Night_ID", "Nest_ID"), 
                    measure.vars = c("AmbTemp", "SideAvg"))

## Rename columns
names(m.tint_temp) <- c("Night_ID", "Nest_ID", "Side", "value")
m.tint_temp$Side <- plyr::revalue(m.tint_temp$Side, c("AmbTemp"="Ambient", "SideAvg"="Nest"))


#### Model nighttime energy expenditures with varying torpor durations ####
tor_int <- 0.46 ## from BBLH(?), torpor intercept
tor_slope <- 0.053 ##  from BBLH(?), torpor slope
tor2 <- 0.0016  ## from BBLH(?), for quadratic

## Calculating night length
night_length <- as.data.frame(tint_hourly %>%
                                group_by(Night_ID, Nest_ID) %>%
                                summarise(NightLength = max(Hour_Elapsed)))
## Adding night length column to tint_hourly data frame
tint_hourly <- merge(tint_hourly, night_length)

## Check that merge worked well
#ggplot(tint_hourly, aes(Nest_ID, NightLength)) + geom_point() + geom_boxplot() + my_theme

## Torpor MR measurements
## Assuming 0h, 2h, 6h, and 10h of torpor for a 12 hour night. But converting that to proportion of the night
# in torpor, because night length varies 9-14h in this study.
tor_prop_night <- c(0, 2/12, 6/12, 10/12)

## Make a data frame for night lengths and convert proportion of night in torpor to Hour_elapsed per night
## Rounded to nearest whole number
torpor_duration <- night_length
torpor_duration$start_2h <- round(torpor_duration$NightLength-(tor_prop_night[2]*torpor_duration$NightLength)-1)
torpor_duration$stop_2h <- torpor_duration$NightLength-1
torpor_duration$start_6h <- round(torpor_duration$NightLength-(tor_prop_night[3]*torpor_duration$NightLength)-1)
torpor_duration$stop_6h <- torpor_duration$NightLength-1
torpor_duration$start_10h <- round(torpor_duration$NightLength-(tor_prop_night[4]*torpor_duration$NightLength)-1)
torpor_duration$stop_10h <- torpor_duration$NightLength-1
head(torpor_duration)

## Make empty columns in tint_hourly for calculating torpor MR
tint_hourly$TorporAmb_2h <- 0
tint_hourly$TorporSideAvg_2h <- 0

tint_hourly$TorporAmb_6h <- 0
tint_hourly$TorporSideAvg_6h <- 0

tint_hourly$TorporAmb_10h <- 0
tint_hourly$TorporSideAvg_10h <- 0

## Fill those columns in according to proportional torpor duration
## For 2/12 hours of torpor
for(i in 1:nrow(tint_hourly)) {
  categ <- torpor_duration[torpor_duration$Night_ID==tint_hourly$Night_ID[i],]
  if(tint_hourly$Hour_Elapsed[i]> categ$start_2h & tint_hourly$Hour_Elapsed[i]<= categ$stop_2h) {
    tint_hourly$TorporAmb_2h[i] <- (tor_int - tor_slope*(tint_hourly$AmbTemp[i]) + tor2*((tint_hourly$AmbTemp[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
    tint_hourly$TorporSideAvg_2h[i] <- (tor_int - tor_slope*(tint_hourly$SideAvg[i]) + tor2*((tint_hourly$SideAvg[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
  } 
}

## 6/12 hours of torpor
for(i in 1:nrow(tint_hourly)) {
  categ <- torpor_duration[torpor_duration$Night_ID==tint_hourly$Night_ID[i],]
  if(tint_hourly$Hour_Elapsed[i]> categ$start_6h & tint_hourly$Hour_Elapsed[i]<= categ$stop_6h) {
    tint_hourly$TorporAmb_6h[i] <- (tor_int - tor_slope*(tint_hourly$AmbTemp[i]) + tor2*((tint_hourly$AmbTemp[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
    tint_hourly$TorporSideAvg_6h[i] <- (tor_int - tor_slope*(tint_hourly$SideAvg[i]) + tor2*((tint_hourly$SideAvg[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
  }
}

## 10/12 hours of torpor
for(i in 1:nrow(tint_hourly)) {
  categ <- torpor_duration[torpor_duration$Night_ID==tint_hourly$Night_ID[i],]
if(tint_hourly$Hour_Elapsed[i]> categ$start_10h & tint_hourly$Hour_Elapsed[i]<= categ$stop_10h) {
  tint_hourly$TorporAmb_10h[i] <- (tor_int - tor_slope*(tint_hourly$AmbTemp[i]) + tor2*((tint_hourly$AmbTemp[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
  tint_hourly$TorporSideAvg_10h[i] <- (tor_int - tor_slope*(tint_hourly$SideAvg[i]) + tor2*((tint_hourly$SideAvg[i])^2))*((16 + (5.164*tint_hourly$RER[i]))/1000)*60
  }
}

## Making column with hourly MR assuming 2/12 hours of torpor use, and the rest normo
## Under ambient temps
tint_hourly$Tor2h_Nor_merged_Amb <- tint_hourly$TorporAmb_2h
for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor2h_Nor_merged_Amb[i]=="0") {
  tint_hourly$Tor2h_Nor_merged_Amb[i] <- tint_hourly$kJ_amb[i]}
}

## Under nest temps
tint_hourly$Tor2h_Nor_merged_SideAvg <- tint_hourly$TorporSideAvg_2h
for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor2h_Nor_merged_SideAvg[i]=="0") {
  tint_hourly$Tor2h_Nor_merged_SideAvg[i] <- tint_hourly$kJ_SideAvg[i]}
}

## Making column with hourly MR assuming 6/12 hours of torpor use, and the rest normo
## Ambient
tint_hourly$Tor6h_Nor_merged_Amb <- tint_hourly$TorporAmb_6h
for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor6h_Nor_merged_Amb[i]=="0") {
  tint_hourly$Tor6h_Nor_merged_Amb[i] <- tint_hourly$kJ_amb[i]}
}

## Nest temps
tint_hourly$Tor6h_Nor_merged_SideAvg <- tint_hourly$TorporSideAvg_6h
for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor6h_Nor_merged_SideAvg[i]=="0") {
  tint_hourly$Tor6h_Nor_merged_SideAvg[i] <- tint_hourly$kJ_SideAvg[i]}
}

## Making column with hourly MR assuming 10/12 hours of torpor use, and the rest normo
## Ambient temps
tint_hourly$Tor10h_Nor_merged_Amb <- tint_hourly$TorporAmb_10h
for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor10h_Nor_merged_Amb[i]=="0") {
  tint_hourly$Tor10h_Nor_merged_Amb[i] <- tint_hourly$kJ_amb[i]}
}

## Nest temps
tint_hourly$Tor10h_Nor_merged_SideAvg <- tint_hourly$TorporSideAvg_10h
for(i in 1:nrow(tint_hourly)) {if(tint_hourly$Tor10h_Nor_merged_SideAvg[i]=="0") {
  tint_hourly$Tor10h_Nor_merged_SideAvg[i] <- tint_hourly$kJ_SideAvg[i]}
}



#### Calculating NEE, first assuming normothermy (Ambient and SideAvg temps), and next with various proportions of torpor use ####
tint_nee <- as.data.frame(tint_hourly %>%
                            dplyr::group_by(Night_ID, Nest_ID) %>%
                            dplyr::summarize(Nor_kJ_Amb = sum(kJ_amb, na.rm = T),
                                      Nor_kJ_SideAvg = sum(kJ_SideAvg, na.rm = T),
                                      Tor2h_kJ_Amb = sum(Tor2h_Nor_merged_Amb, na.rm = T),
                                      Tor2h_kJ_SideAvg = sum(Tor2h_Nor_merged_SideAvg, na.rm = T),
                                      Tor6h_kJ_Amb = sum(Tor6h_Nor_merged_Amb, na.rm = T),
                                      Tor6h_kJ_SideAvg = sum(Tor6h_Nor_merged_SideAvg, na.rm = T),
                                      Tor10h_kJ_Amb = sum(Tor10h_Nor_merged_Amb, na.rm = T),
                                      Tor10h_kJ_SideAvg = sum(Tor10h_Nor_merged_SideAvg, na.rm = T),
                            ))


## Adding Night length column to tint_nee data frame
tint_nee <- merge(tint_nee, night_length)


# m.nee <- melt(tint_nee, id.vars = c("Night_ID", "Nest_ID", "NightLength"), 
#               measure.vars = c("Nor_kJ_Amb", "Nor_kJ_SideA", "Nor_kJ_SideB", "Nor_kJ_SideC", "Nor_kJ_SideD", "Nor_kJ_SideAvg", "Nor_kJ_NestTs",# "Nor_kJ_Cup",
#                                "Tor_kJ_Amb", "Tor_kJ_SideA", "Tor_kJ_SideB", "Tor_kJ_SideC", "Tor_kJ_SideD", "Tor_kJ_SideAvg", "Tor_kJ_NestTs" #, "Tor_kJ_Cup"
#                                ))

m.nee <- melt(tint_nee, id.vars = c("Night_ID", "Nest_ID", "NightLength"), 
              measure.vars = c("Nor_kJ_Amb", "Nor_kJ_SideAvg",
                               "Tor2h_kJ_Amb", "Tor2h_kJ_SideAvg",
                               "Tor6h_kJ_Amb", "Tor6h_kJ_SideAvg",
                               "Tor10h_kJ_Amb", "Tor10h_kJ_SideAvg"))

m.nee <- m.nee %>% separate(variable, c("Tornor", NA, "Side"), sep = "_", remove=F)
m.nee$Torpor_dur <- plyr::revalue(m.nee$Tornor, c("Nor"="0", "Tor2h"="2", 
                                              "Tor6h"="6", "Tor10h"="10"))
m.nee$Torpor_dur <- factor(m.nee$Torpor_dur, levels = c("0", "2", "6", "10")) ## define order

## Order sides
m.nee$Side <- factor(m.nee$Side, levels = c("Amb", "SideAvg"))
m.nee$Side <- plyr::revalue(m.nee$Side, c("Amb"="Ambient", "SideAvg"="Nest"))


## Standardize NEE by night length
m.nee$stdNEE <- (m.nee$value/m.nee$NightLength)*12

## Difference in NEE between Amb and nest temps across torpor durations
m.nee_diff <- data.frame(Night_ID = m.nee$Night_ID[m.nee$Side=='Ambient'],
                         Nest_ID = m.nee$Nest_ID[m.nee$Side=='Ambient'],
                         variable = m.nee$variable[m.nee$Side=='Ambient'],
                         Torpor_dur = m.nee$Torpor_dur[m.nee$Side=='Ambient'],
                         Amb_Nest_Nor = m.nee$value[m.nee$Side=='Ambient' & m.nee$Torpor_dur=="0"] - 
                           m.nee$value[m.nee$Side == 'Nest'  & m.nee$Torpor_dur=="0"],
                         Amb_Nest_2h = m.nee$value[m.nee$Side=='Ambient' & m.nee$Torpor_dur=="2"] - 
                           m.nee$value[m.nee$Side == 'Nest'  & m.nee$Torpor_dur=="2"],
                         Amb_Nest_6h = m.nee$value[m.nee$Side=='Ambient' & m.nee$Torpor_dur=="6"] - 
                           m.nee$value[m.nee$Side == 'Nest'  & m.nee$Torpor_dur=="6"],
                         Amb_Nest_10h = m.nee$value[m.nee$Side=='Ambient' & m.nee$Torpor_dur=="10"] - 
                           m.nee$value[m.nee$Side == 'Nest'  & m.nee$Torpor_dur=="10"])

m.nee_diff <- melt(m.nee_diff, id.vars = c("Night_ID", "Nest_ID", "Torpor_dur"), 
                   measure.vars = c("Amb_Nest_Nor", "Amb_Nest_2h", "Amb_Nest_6h", "Amb_Nest_10h"))



# Standardize NEE by nightlength
m.nee_diff_std <- data.frame(Night_ID = m.nee$Night_ID[m.nee$Side=='Ambient'],
                                      Nest_ID = m.nee$Nest_ID[m.nee$Side=='Ambient'],
                                      variable = m.nee$variable[m.nee$Side=='Ambient'],
                                      Torpor_dur = m.nee$Torpor_dur[m.nee$Side=='Ambient'],
                                      Amb_Nest_Nor = m.nee$stdNEE[m.nee$Side=='Ambient' & m.nee$Torpor_dur=="0"] - 
                                        m.nee$stdNEE[m.nee$Side == 'Nest'  & m.nee$Torpor_dur=="0"],
                                      Amb_Nest_2h = m.nee$stdNEE[m.nee$Side=='Ambient' & m.nee$Torpor_dur=="2"] - 
                                        m.nee$stdNEE[m.nee$Side == 'Nest'  & m.nee$Torpor_dur=="2"],
                                      Amb_Nest_6h = m.nee$stdNEE[m.nee$Side=='Ambient' & m.nee$Torpor_dur=="6"] - 
                                        m.nee$stdNEE[m.nee$Side == 'Nest'  & m.nee$Torpor_dur=="6"],
                                      Amb_Nest_10h = m.nee$stdNEE[m.nee$Side=='Ambient' & m.nee$Torpor_dur=="10"] - 
                                        m.nee$stdNEE[m.nee$Side == 'Nest'  & m.nee$Torpor_dur=="10"])

m.nee_diff_std <- melt(m.nee_diff_std, id.vars = c("Night_ID", "Nest_ID", "Torpor_dur"), 
                   measure.vars = c("Amb_Nest_Nor", "Amb_Nest_2h", "Amb_Nest_6h", "Amb_Nest_10h"))


## Difference between normo and various torpid NEE 
m.nee_Nor_tor <- data.frame(Night_ID = m.nee$Night_ID[m.nee$Torpor_dur=='0' & m.nee$Side=="Ambient"],
                         Nest_ID = m.nee$Nest_ID[m.nee$Torpor_dur=='0' & m.nee$Side=="Ambient"],
                         variable = m.nee$variable[m.nee$Torpor_dur=='0' & m.nee$Side=="Ambient"],
                         Ambient_Nor_Tor2h_raw = m.nee$value[m.nee$Torpor_dur=='0' & m.nee$Side=="Ambient"] - m.nee$value[m.nee$Torpor_dur == '2' & m.nee$Side=="Ambient"],
                         Ambient_Nor_Tor6h_raw = m.nee$value[m.nee$Torpor_dur=='0' & m.nee$Side=="Ambient"] - m.nee$value[m.nee$Torpor_dur == '6' & m.nee$Side=="Ambient"],
                         Ambient_Nor_Tor10h_raw = m.nee$value[m.nee$Torpor_dur=='0' & m.nee$Side=="Ambient"] - m.nee$value[m.nee$Torpor_dur == '10' & m.nee$Side=="Ambient"],
                         Ambient_Nor_Tor2h_std = m.nee$stdNEE[m.nee$Torpor_dur=='0' & m.nee$Side=="Ambient"] - m.nee$stdNEE[m.nee$Torpor_dur == '2' & m.nee$Side=="Ambient"],
                         Ambient_Nor_Tor6h_std = m.nee$stdNEE[m.nee$Torpor_dur=='0' & m.nee$Side=="Ambient"] - m.nee$stdNEE[m.nee$Torpor_dur == '6' & m.nee$Side=="Ambient"],
                         Ambient_Nor_Tor10h_std = m.nee$stdNEE[m.nee$Torpor_dur=='0' & m.nee$Side=="Ambient"] - m.nee$stdNEE[m.nee$Torpor_dur == '10' & m.nee$Side=="Ambient"],
                         Nest_Nor_Tor2h_raw = m.nee$value[m.nee$Torpor_dur=='0' & m.nee$Side=="Ambient"] - m.nee$value[m.nee$Torpor_dur == '2' & m.nee$Side=="Ambient"],
                         Nest_Nor_Tor6h_raw = m.nee$value[m.nee$Torpor_dur=='0' & m.nee$Side=="Ambient"] - m.nee$value[m.nee$Torpor_dur == '6' & m.nee$Side=="Ambient"],
                         Nest_Nor_Tor10h_raw = m.nee$value[m.nee$Torpor_dur=='0' & m.nee$Side=="Ambient"] - m.nee$value[m.nee$Torpor_dur == '10' & m.nee$Side=="Ambient"],
                         Nest_Nor_Tor2h_std = m.nee$stdNEE[m.nee$Torpor_dur=='0' & m.nee$Side=="Ambient"] - m.nee$stdNEE[m.nee$Torpor_dur == '2' & m.nee$Side=="Ambient"],
                         Nest_Nor_Tor6h_std = m.nee$stdNEE[m.nee$Torpor_dur=='0' & m.nee$Side=="Ambient"] - m.nee$stdNEE[m.nee$Torpor_dur == '6' & m.nee$Side=="Ambient"],
                         Nest_Nor_Tor10h_std = m.nee$stdNEE[m.nee$Torpor_dur=='0' & m.nee$Side=="Ambient"] - m.nee$stdNEE[m.nee$Torpor_dur == '10' & m.nee$Side=="Ambient"]
)
m.nee_Nor_tor <- melt(m.nee_Nor_tor, id.vars = c("Night_ID", "Nest_ID"), 
                      measure.vars = c("Ambient_Nor_Tor2h_raw", "Ambient_Nor_Tor2h_std", "Ambient_Nor_Tor6h_raw", "Ambient_Nor_Tor6h_std",
                                       "Ambient_Nor_Tor10h_raw", "Ambient_Nor_Tor10h_std",
                                       "Nest_Nor_Tor2h_raw", "Nest_Nor_Tor2h_std", "Nest_Nor_Tor6h_raw", "Nest_Nor_Tor6h_std",
                                       "Nest_Nor_Tor10h_raw", "Nest_Nor_Tor10h_std"))
m.nee_Nor_tor <- m.nee_Nor_tor %>% separate(variable, c("Side", NA, "Torpor_dur", "Std"), sep = "_", remove=F)
m.nee_Nor_tor$Torpor_dur <- plyr::revalue(m.nee_Nor_tor$Torpor_dur, c("Tor2h"="2", "Tor6h"="6", "Tor10h"="10"))
m.nee_Nor_tor$Torpor_dur <- factor(m.nee_Nor_tor$Torpor_dur, levels = c("2", "6", "10")) ## define order


### Summarize NEE values in different scenarios
NEE_summ <- data.frame("Torpor_dur" = c("0", "2", "6", "10"),
                       "NEE_Ambient_min" = c(min(m.nee$stdNEE[m.nee$Torpor_dur=="0" & m.nee$Side=="Ambient"]), 
                                             min(m.nee$stdNEE[m.nee$Torpor_dur=="2" & m.nee$Side=="Ambient"]), 
                                             min(m.nee$stdNEE[m.nee$Torpor_dur=="6" & m.nee$Side=="Ambient"]), 
                                             min(m.nee$stdNEE[m.nee$Torpor_dur=="10" & m.nee$Side=="Ambient"])),
                       "NEE_Ambient_mean" = c(mean(m.nee$stdNEE[m.nee$Torpor_dur=="0" & m.nee$Side=="Ambient"]), 
                                             mean(m.nee$stdNEE[m.nee$Torpor_dur=="2" & m.nee$Side=="Ambient"]), 
                                             mean(m.nee$stdNEE[m.nee$Torpor_dur=="6" & m.nee$Side=="Ambient"]), 
                                             mean(m.nee$stdNEE[m.nee$Torpor_dur=="10" & m.nee$Side=="Ambient"])),
                       "NEE_Ambient_max" = c(max(m.nee$stdNEE[m.nee$Torpor_dur=="0" & m.nee$Side=="Ambient"]), 
                                             max(m.nee$stdNEE[m.nee$Torpor_dur=="2" & m.nee$Side=="Ambient"]), 
                                             max(m.nee$stdNEE[m.nee$Torpor_dur=="6" & m.nee$Side=="Ambient"]), 
                                             max(m.nee$stdNEE[m.nee$Torpor_dur=="10" & m.nee$Side=="Ambient"])),
                       "NEE_Ambient_sd" = c(sd(m.nee$stdNEE[m.nee$Torpor_dur=="0" & m.nee$Side=="Ambient"]), 
                                            sd(m.nee$stdNEE[m.nee$Torpor_dur=="2" & m.nee$Side=="Ambient"]), 
                                            sd(m.nee$stdNEE[m.nee$Torpor_dur=="6" & m.nee$Side=="Ambient"]), 
                                            sd(m.nee$stdNEE[m.nee$Torpor_dur=="10" & m.nee$Side=="Ambient"])),
                       "NEE_Nest_min" = c(min(m.nee$stdNEE[m.nee$Torpor_dur=="0" & m.nee$Side=="Nest"]), 
                                          min(m.nee$stdNEE[m.nee$Torpor_dur=="2" & m.nee$Side=="Nest"]), 
                                          min(m.nee$stdNEE[m.nee$Torpor_dur=="6" & m.nee$Side=="Nest"]), 
                                          min(m.nee$stdNEE[m.nee$Torpor_dur=="10" & m.nee$Side=="Nest"])),
                       "NEE_Nest_mean" = c(mean(m.nee$stdNEE[m.nee$Torpor_dur=="0" & m.nee$Side=="Nest"]), 
                                          mean(m.nee$stdNEE[m.nee$Torpor_dur=="2" & m.nee$Side=="Nest"]), 
                                          mean(m.nee$stdNEE[m.nee$Torpor_dur=="6" & m.nee$Side=="Nest"]), 
                                          mean(m.nee$stdNEE[m.nee$Torpor_dur=="10" & m.nee$Side=="Nest"])),
                       "NEE_Nest_max" = c(max(m.nee$stdNEE[m.nee$Torpor_dur=="0" & m.nee$Side=="Nest"]), 
                                          max(m.nee$stdNEE[m.nee$Torpor_dur=="2" & m.nee$Side=="Nest"]), 
                                          max(m.nee$stdNEE[m.nee$Torpor_dur=="6" & m.nee$Side=="Nest"]), 
                                          max(m.nee$stdNEE[m.nee$Torpor_dur=="10" & m.nee$Side=="Nest"])),
                       "NEE_Nest_sd" = c(sd(m.nee$stdNEE[m.nee$Torpor_dur=="0" & m.nee$Side=="Nest"]), 
                                         sd(m.nee$stdNEE[m.nee$Torpor_dur=="2" & m.nee$Side=="Nest"]), 
                                         sd(m.nee$stdNEE[m.nee$Torpor_dur=="6" & m.nee$Side=="Nest"]), 
                                         sd(m.nee$stdNEE[m.nee$Torpor_dur=="10" & m.nee$Side=="Nest"])),
                       "NEE_sample"=c(rep(length(unique(m.nee$Night_ID, 4)))))

## csv with mean, minimum, and maximum per scenario
write.csv(NEE_summ, "TINT_NEE_kJ_Summary.csv")

NEEamb_mean_sd <- data.frame("Nest_Amb"="Ambient", "Torpor_dur"=NEE_summ$Torpor_dur, "sample"=NEE_summ$NEE_sample, "NEE_mean"=NEE_summ$NEE_Ambient_mean, 
                          "NEE_sd"=NEE_summ$NEE_Ambient_sd)
NEEnest_mean_sd <- data.frame("Nest_Amb"="Nest", "Torpor_dur"=NEE_summ$Torpor_dur, "sample"=NEE_summ$NEE_sample,                            
                          "NEE_mean"=NEE_summ$NEE_Nest_mean, "NEE_sd"=NEE_summ$NEE_Nest_sd)
NEE_mean_sd <- rbind(NEEamb_mean_sd, NEEnest_mean_sd)
NEE_mean_sd$Temp_dur <- paste0(NEE_mean_sd$Nest_Amb, "_", NEE_mean_sd$Torpor_dur)

## csv with means and SDs per scenario
write.csv(NEE_mean_sd, "TINT_NEE_meanSD.csv")

#### Main plots ####
## Total NEE, comparing normo and max torpor
breaks_nee <- seq(0,15,1)
labels_nee <- as.character(breaks_nee)
labels_nee[!(breaks_nee %% 2 == 0)] <- ''
#tick.sizes <- rep(.5, length(breaks_nee))
#tick.sizes[(breaks_nee %% 2 == 0)] <- 1
nee.plot <- ggplot(data=m.nee, aes(Torpor_dur, value)) + my_theme2 + facet_grid(.~Side) +
  geom_point(size=2) + geom_boxplot(aes(col=Torpor_dur), size=1.2, show.legend=F) + ylab ("NEE (kJ)") +
  theme(axis.text.x = element_text(size=20, vjust=0.5)) + #ylim(0,14) +
  scale_y_continuous(breaks = breaks_nee, labels = labels_nee) +
  scale_color_manual(values = torpor_gradient) + xlab("Torpor duration (hours)")
nee.plot

## Temp plot to mirror this NEE plot; from TINT data
temp.plot_tint <- ggplot(data=m.tint_temp, aes(Side, value)) + my_theme2 + #facet_grid(.~Torpor_dur) +
  geom_point(size=2) + geom_boxplot(size=1.2) + ylab(Temp.lab) +
  scale_y_continuous(breaks = seq(0,30,5)) +
  theme(axis.text.x = element_text(size=20)) +
  xlab("Temperature")

# Side-by-side plots of NEE and temperatures they were modeled on, but not standardized by night length
grid.arrange(nee.plot, temp.plot_tint, nrow=1, ncol=2, widths = c(2, 1))


## Nighttime energy expenditure (kJ) standardized to 12 hour night length
nee.std.plot <- ggplot(data=m.nee, aes(Torpor_dur, stdNEE)) + my_theme2 + facet_grid(.~Side) +
  geom_point(size=2) + geom_boxplot(aes(col=Torpor_dur), size=1.2, show.legend=F) +
  theme(axis.text.x = element_text(size=20)) + xlab("Torpor duration (hours)") + #ylim(0,15) +
  scale_y_continuous(breaks = breaks_nee, labels = labels_nee) +
  scale_color_manual(values = torpor_gradient) + ylab ("NEE (kJ) standardized to 12h night")
nee.std.plot

## Standardized by night length
grid.arrange(nee.std.plot, temp.plot_tint, nrow=1, ncol=2, widths = c(2, 1))

## Difference between all-Normo and various durations' torpor NEE, with raw night length and standardized night lengths
## No difference between Ambient and Nest here, because this is diff between normo and torpor, so just plotting raw vs. std
ggplot(data=m.nee_Nor_tor[m.nee_Nor_tor$Std=="std",], aes(Torpor_dur, value)) + my_theme2 + facet_grid(.~Side) +
  geom_point(size=2, show.legend = F) +
  geom_boxplot(aes(col=Torpor_dur),  show.legend = F, size=1.2) +
  theme(axis.text.x = element_text(size=20)) + xlab("Torpor duration (hours)") +
  scale_color_manual(values=torpor_gradient[2:5]) + ylab("NEE (kJ)/12h difference Normo-Torpor")


#### Modeling ####
# Multiple pairwise comparisons
#https://www.datanovia.com/en/blog/how-to-perform-t-test-for-multiple-groups-in-r/
#https://www.datanovia.com/en/blog/how-to-add-p-values-to-ggplot-facets/
res.aov <- m.nee %>% anova_test(stdNEE ~ variable)
get_anova_table(res.aov)

## If you use the column name "variable" in the code below it doesn't work, shows a conflict, so
# Renaming 'variable' to 'TorTemp'
m.nee <- m.nee %>%
  rename(TorTemp = variable)

pwc <- m.nee %>%
  pairwise_t_test(stdNEE ~ TorTemp, p.adjust.method = "bonferroni")
write.csv(as.data.frame(pwc), file="Anova_Bonferroni_results.csv")
pwc <- pwc %>% add_xy_position(x = "TorTemp")

## Plot groups with p values on the plot. Basically all pairs are significantly different!
ggboxplot(m.nee, x = "TorTemp", y = "stdNEE") +
  stat_pvalue_manual(pwc, hide.ns = TRUE, label = "p.adj.signif") +
  labs(subtitle = get_test_label(res.aov, detailed = TRUE),
       caption = get_pwc_label(pwc))