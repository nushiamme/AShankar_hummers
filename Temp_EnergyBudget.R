## Analyzing and plotting ambient and chamber temperature data for BBLH energy budget paper
## To make a thermoregulatory model for HC and SC
## Script by Anusha Shankar
## Script started on: November 2, 2016

library(reshape)
library(ggplot2)

#### Reading in files and reshaping ####
## Set wd
setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget")

## Read in file with temperature from each sensor per hour per site (hence temp "details")
temp_details <- read.csv("BBLH_temperatures_compiled.csv")

#### Rename and reshape ####
# See original csv for original sensor names. That's why I'm changing names here and not in source file
names(temp_details) <- c("Site", "Day", "Month", "Year", "Time", "Hour", "HC_te1", "HC_te2", "Mixed_te1", "Mixed_te2",
                         "Te_max", "Te_min", "Te_mean", "Te_sd", "SC_ta1", "SC_ta2", "SC_ta3", "Mixed_ta1", "Mixed_ta2",
                         "Mixed_ta3", "Mixed_ta4", "Mixed_ta5", "Mixed_ta6", "Mixed_ta7", "Mixed_ta8", "Mixed_ta9", 
                         "Mixed_ta10", "Mixed_ta11", "Mixed_ta12", "Mixed_ta13", "Ta_min", "Ta_max", "Ta_mean", "Ta_sd")

## Melt to compile Te-'s and Ta's separately. Then rename columns to sensible names
m.te_det <- melt(temp_details, id.vars=c("Site", "Day", "Month", "Year", "Hour"), 
               measure.vars=c("HC_te1", "HC_te2", "Mixed_te1", "Mixed_te2"), na.rm=T)
m.te_det$DayMonth <- paste0(m.te_det$Day, ",", m.te_det$Month)
names(m.te_det) <- c("Site", "Day", "Month", "Year", "Hour", "Sensor", "Te", "DayMonth")

m.ta_det <- melt(temp_details, id.vars=c("Site", "Day", "Month", "Year", "Hour"), 
               measure.vars=c("SC_ta1", "SC_ta2", "SC_ta3", "Mixed_ta1", 
                              "Mixed_ta2", "Mixed_ta3", "Mixed_ta4", "Mixed_ta5", "Mixed_ta6", "Mixed_ta7", "Mixed_ta8",
                              "Mixed_ta9", "Mixed_ta10", "Mixed_ta11", "Mixed_ta12", "Mixed_ta13"), na.rm=T)
m.ta_det$DayMonth <- paste0(m.ta_det$Day, ",", m.ta_det$Month)
names(m.ta_det) <- c("Site", "Day", "Month", "Year", "Hour", "Sensor", "Ta", "DayMonth")

#### General functions ####
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

Ta.lab <- expression(atop(paste("Ambient Temperature ( ", degree,"C)")))
Te.lab <- expression(atop(paste("Operative Temperature ( ", degree,"C)")))

## Randomly sampling temperatures to get theroregulatory costs
lst <- list()

te_hc <- m.te_det[m.te_det$Site=="HC",]
te_hc$Hour <- as.factor(te_hc$Hour)
te_hc_list <- split(te_hc, as.factor(te_hc$Hour))

te_samp_hc <- lapply(split(te_hc, te_hc$Hour),
              function(subdf) subdf[sample(1:nrow(te_hc), 3),]
)


for(i in nlevels(te_hc$Hour)) {
  for(j in seq_len(5)){
    lst[[j]] <- te_hc[sample(te_hc$Te[te_hc$Hour %in% i], 3, replace = TRUE),]
    lst[[j]]["Sample"] <- j
  }
}
lst

te.hc.samp <- te_hc_list[tapply(1:nrow(te_hc_list), te_hc$, sample, 2),]
sample(te_hc[te_hc$Hour %in% 100,], 1, replace = TRUE)

sample(te_hc$Te[te_hc$Hour=="2400"], 3, replace = TRUE)

#### Plots #####
m.te_hour <- m.te_det[m.te_det$Hour==700 & m.te_det$DayMonth=="8,7" & m.te_det$Site=="HC",]
ggplot(m.te_hour, aes(Sensor, Te)) + geom_point(size=4) + my_theme + 
  ylab(Te.lab) +
  ggtitle("Harshaw July 8, 2016, 7am")

m.ta_hour <- m.ta_det[m.ta_det$Hour==700 & m.ta_det$DayMonth=="8,7" & m.ta_det$Site=="HC",]
ggplot(m.ta_hour, aes(Sensor, Ta)) + geom_point(size=4) + my_theme + 
  ylab(Ta.lab) +
  ggtitle("Harshaw July 8, 2016, 7am")

ggplot(m.temp[m.temp$Site=="HC",], aes(Sensor, Temp)) + geom_point() + my_theme 
  facet_grid(Hour~.)
  
