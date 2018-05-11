## Analyzing and plotting ambient and chamber temperature data for BBLH energy budget paper
## To make a thermoregulatory model for HC and SC
## Script by Anusha Shankar
## Script started on: November 2, 2016

library(reshape)
library(ggplot2)
library(dplyr)
library(data.table)
library(grid)
library(gridExtra)

#### Reading in files and reshaping ####
## Set wd
setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Tables")
## wd at GFU
setwd("/Users/anshankar/Dropbox/Anusha Committee/BBLH_EnergyBudget/Tables")
costas <- read.csv("Costa1986_Don.csv")

ggplot(costas, aes(Temperature, MR)) + geom_point() + geom_smooth(stat='smooth', method='loess', color='black') + my_theme


## Read in file with temperature from each sensor per hour per site (hence temp "details")
temp_details <- read.csv("BBLH_temperatures_compiled.csv")

## Read in premelted dataframes with temperatures and calculated thermoregulatory costs
m.te_det <- read.csv("Melted_Te_thermo.csv")
m.ta_det <- read.csv("Melted_Ta_thermo.csv")

## Can change this depending on Thermoregulatory model- Multiply $thermo (in O2 ml/min) by 15 to get 
#thermoregulatory costs per 15min, 
## assuming the bird is exposed to each temperature sampled for 15 minutes
m.te_det$Hour <- as.factor(m.te_det$Hour)
m.ta_det$Hour <- as.factor(m.ta_det$Hour)
m.te_det$thermo_mlO2_15min <- m.te_det$thermo_mlO2*15
m.ta_det$thermo_mlO2_15min <- m.ta_det$thermo_mlO2*15

#### General functions ####
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

Te.lab <- expression(atop(paste("Operative Temperature ( ", degree,"C)")))
Ta.lab <- expression(atop(paste("Ambient Temperature ( ", degree,"C)")))

## Just plot temps per site to look at the distribution of Ta
temp_details$Day_night <- 0
temp_details$Day_night[600<temp_details$Hour& temp_details$Hour<1900] <- "Day"
temp_details$Day_night[temp_details$Hour<700 | 1800<temp_details$Hour] <- "Night"
temp_details$Day_night <- factor(temp_details$Day_night, levels=c('Night', "Day"))

## Faceted by Day/Night
ggplot(temp_details, aes(Ta_mean, alpha=Site)) + geom_freqpoly(fill="grey35") + xlab(Ta.lab) + 
  coord_flip() + my_theme + facet_grid(~Day_night) + theme(legend.key.height = unit(3, 'lines')) + scale_alpha_manual(values = c(0.2, 0.6), breaks=c("SC", "HC"))


#### Building a model for thermoregulatory costs ####

#### First, subset each site and the dates needed (from DLW data), make it a separate dataframe, 
## then save it as a list where each hour from the day is a separate object ####

te_hc_1306 <- m.te_det[m.te_det$Site=="HC" & m.te_det$DayMonth=="13,6",]
telist_hc_1306 <- split(te_hc_1306, te_hc_1306$Hour)

te_hc_2706 <- m.te_det[m.te_det$Site=="HC" & m.te_det$DayMonth=="27,6",]
telist_hc_2706 <- split(te_hc_2706, te_hc_2706$Hour)

te_hc_1107 <- m.te_det[m.te_det$Site=="HC" & m.te_det$DayMonth=="11,7",]
telist_hc_1107 <- split(te_hc_1107, te_hc_1107$Hour)

ta_hc_1306 <- m.ta_det[m.ta_det$Site=="HC" & m.ta_det$DayMonth=="13,6",]
talist_hc_1306 <- split(ta_hc_1306, ta_hc_1306$Hour)

ta_hc_2706 <- m.ta_det[m.ta_det$Site=="HC" & m.ta_det$DayMonth=="27,6",]
talist_hc_2706 <- split(ta_hc_2706, ta_hc_2706$Hour)

## Don't have Ta data for July 11, so using July 9 data
ta_hc_0907 <- m.ta_det[m.ta_det$Site=="HC" & m.ta_det$DayMonth=="9,7",]
talist_hc_0907 <- split(ta_hc_0907, ta_hc_0907$Hour)

te_sc_0207 <- m.te_det[m.te_det$Site=="SC" & m.te_det$DayMonth=="2,7",]
telist_sc_0207 <- split(te_sc_0207, te_sc_0207$Hour)

te_sc_1607 <- m.te_det[m.te_det$Site=="SC" & m.te_det$DayMonth=="16,7",]
telist_sc_1607 <- split(te_sc_1607, te_sc_1607$Hour)

ta_sc_0207 <- m.ta_det[m.ta_det$Site=="SC" & m.ta_det$DayMonth=="2,7",]
talist_sc_0207 <- split(ta_sc_0207, ta_sc_0207$Hour)

##Don't have 16 July data, so using July 9, which is the last day with >1 sensor
ta_sc_0907 <- m.ta_det[m.ta_det$Site=="SC" & m.ta_det$DayMonth=="9,7",]
talist_sc_0907 <- split(ta_sc_0907, ta_sc_0907$Hour)

## Randomly sampling temperatures to get theroregulatory costs ####
## Using function to pull random values from 4 sensors at a site, with replacement, to represent 
# temperatures in 15 min intervals
rand_therm <- function (list_day) {
  for (i in 1:100){
    iter <- lapply(list_day, function(x) {
      if(as.numeric(as.character(x$Hour[1]))<=1900 & 
           as.numeric(as.character(x$Hour[1])) >=500) {
      temp_rows <- sample_n(x, 4, replace = F) # currently without replacement
      sum(temp_rows$thermo_mlO2_15min)
      }
    })
   test <- do.call(sum, iter)
   # make ddmm variable to call date and month for file name
   ddmm_site <- paste(list_day[[1]][1,3], list_day[[1]][1,4], list_day[[1]][1,2], sep="_") 
   saveRDS(iter, paste("Thermo_iterations//iter//" , ddmm_site, "_iter", i, ".RDS", sep = ""))
   saveRDS(test, paste("Thermo_iterations//test//", ddmm_site, "_test", i, ".RDS", sep = ""))
  }
}

## Applying the function to just ambient temperatures at the sites, on the days for which we have DLW DEE data
rand_therm(talist_hc_1306)
rand_therm(talist_hc_2706)
rand_therm(talist_hc_0907)
rand_therm(talist_sc_0207)
rand_therm(talist_sc_0907)
#rand_therm(talist_hc_1107) # Doesn't work because temp data unavailable
#rand_therm(talist_sc_1607) # Doesn't work because temp data unavailable

## Bind data from all iterations of one day together
compile_iters <- function(x) {
  compiled_daily_thermo <- list.files(path = 'Thermo_iterations\\test\\', pattern = x)
  setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Thermo_iterations\\test")
  dat_list <- lapply(compiled_daily_thermo, function (x) data.table(readRDS(x)))
  daily_thermo_results <- rbindlist(dat_list)
  setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\")
  plot_iter <- ggplot(daily_thermo_results, aes(V1)) + geom_histogram(bins = 20) + my_theme +
    xlab("Daily thermoregulatory costs (mL O2 consumed)") + ggtitle(x)
  plot_iter
  Rand_res <- data.frame(matrix(NA, nrow = 3, ncol = 3))
  names(Rand_res) <- c("RandTemp_median_thermo_day", "RandTemp_min_thermo_day", "RandTemp_max_thermo_day")
  paste("Median daytime thermo cost in mL O2 = ", round(median(daily_thermo_results$V1),2),
        "; Min daytime thermo cost in mL O2 = ", round(min(daily_thermo_results$V1),2), 
        "; Max daytime thermo cost in mL O2 = ", round(max(daily_thermo_results$V1),2))
}

compile_iters('1306_test.*.RDS$')
compile_iters('2706_test.*.RDS$')
compile_iters('9070HC_test.*.RDS$')
compile_iters('207_test.*.RDS$')
compile_iters('9070SC_test.*.RDS$')


## Important: Change the sitedate for object name below depending on what's being run above
plot_hc1306_iters <- ggplot(daily_thermo_results, aes(V1)) + geom_histogram(bins = 20) + my_theme +
  xlab("Daily thermoregulatory costs (mL O2 consumed)") + ggtitle("Harshaw 13 June, 2013")
plot_hc1306_iters

plot_hc2706_iters <- ggplot(daily_thermo_results, aes(V1)) + geom_histogram(bins = 20) + my_theme +
  xlab("Daily thermoregulatory costs (mL O2 consumed)") + ggtitle("Harshaw 27 June, 2013")
plot_hc2706_iters

plot_sc0207_iters <- ggplot(daily_thermo_results, aes(V1)) + geom_histogram(bins = 20) + my_theme +
  xlab("Daily thermoregulatory costs (mL O2 consumed)") + ggtitle("Sonoita 2 July, 2013")
plot_sc0207_iters


## Simmilar function as above, but using lowest 4 thermoreg costs from that hour and day, rather than 
## randomly sampling 4 temperatures across the landscape
##DONT USE
minTemp_therm <- function (list_day) {
  iter <- lapply(list_day, function(x) {
    if(as.numeric(as.character(x$Hour[1]))<=1900 & 
         as.numeric(as.character(x$Hour[1])) >=500) {
      min_rows <- x[which(x$thermo_mlO2_15min <= quantile(x$thermo_mlO2_15min, .2)), ]
      sum(min_rows$thermo_mlO2_15min)
    }
    })
    test <- do.call(sum, iter)
    # make ddmm variable to call date and month for file name
    ddmm <- paste(list_day[[1]][1,3], list_day[[1]][1,4], sep="0") # can use zero to separate because months 
    # in study were single digit (i.e. <10)
    saveRDS(iter, paste("Thermo_iterations//iter_min//" , ddmm, "_itermin", ".RDS", sep = ""))
    saveRDS(test, paste("Thermo_iterations//test_min//", ddmm, "_testmin", ".RDS", sep = ""))
}

minTemp_therm(talist_hc_1306)
minTemp_therm(talist_hc_2706)
minTemp_therm(talist_sc_0207)
#minTemp_therm(talist_hc_1107) # Doesn't work because temp data unavailable
#minTemp_therm(talist_sc_1607) # Doesn't work because temp data unavailable

## Using new method of selecting top four temps, not quantiles
minTemp2_therm <- function (list_day) {
  iter <- lapply(list_day, function(x) {
    if(as.numeric(as.character(x$Hour[1]))<=1900 & 
       as.numeric(as.character(x$Hour[1])) >=500) {
      min_rows <- data.frame(matrix(NA, nrow = 4, ncol = 1))
      min_rows$thermo_mlO2_15min[1] <- sort(x$thermo_mlO2_15min)[1]
      min_rows$thermo_mlO2_15min[2] <- sort(x$thermo_mlO2_15min)[2]
      min_rows$thermo_mlO2_15min[3] <- sort(x$thermo_mlO2_15min)[3]
      min_rows$thermo_mlO2_15min[4] <- sort(x$thermo_mlO2_15min)[4]
      sum(min_rows$thermo_mlO2_15min)
    }
  })
  test <- do.call(sum, iter)
  # make ddmm variable to call date and month for file name
  ddmm_site <- paste(list_day[[1]][1,3], list_day[[1]][1,4], list_day[[1]][1,2], sep="_")
  saveRDS(iter, paste("Thermo_iterations//iter_min2//" , ddmm_site, "_itermin2", ".RDS", sep = ""))
  saveRDS(test, paste("Thermo_iterations//test_min2//", ddmm_site, "_testmin2", ".RDS", sep = ""))
}

minTemp2_therm(talist_hc_1306)
minTemp2_therm(talist_hc_2706)
minTemp2_therm(talist_hc_0907)
minTemp2_therm(talist_sc_0207)
minTemp2_therm(talist_sc_0907)

## Function to calculate thermoregulatory costs if the bird spent its time, every hour, with the 
# highest thermoregulatory costs per hour
#### DONT USE
maxTemp_therm <- function (list_day) {
  iter <- lapply(list_day, function(x) {
    if(as.numeric(as.character(x$Hour[1]))<=1900 & 
         as.numeric(as.character(x$Hour[1])) >=500) {
      max_rows <- x[which(x$thermo_mlO2_15min >= quantile(x$thermo_mlO2_15min, .8)), ]
      sum(max_rows$thermo_mlO2_15min)
    }
  })
  test <- do.call(sum, iter)
  # make ddmm variable to call date and month for file name
  # can use zero to separate because months in study 
  ddmm <- paste(list_day[[1]][1,3], list_day[[1]][1,4], sep="0") 
  # were single digit (i.e. <10)
  saveRDS(iter, paste("Thermo_iterations//iter_max//" , ddmm, "_itermax", ".RDS", sep = ""))
  saveRDS(test, paste("Thermo_iterations//test_max//", ddmm, "_testmax", ".RDS", sep = ""))
}

maxTemp_therm(talist_hc_1306)
maxTemp_therm(talist_hc_2706)
maxTemp_therm(talist_sc_0207)
#maxTemp_therm(talist_hc_1107) # Doesn't work because temp data unavailable
#maxTemp_therm(talist_sc_1607) # Doesn't work because temp data unavailable

# highest thermoregulatory costs per hour
maxTemp2_therm <- function (list_day) {
  iter <- lapply(list_day, function(x) {
    if(as.numeric(as.character(x$Hour[1]))<=1900 & 
       as.numeric(as.character(x$Hour[1])) >=500) {
      n <- length(x$thermo_mlO2_15min)
      max_rows <- data.frame(matrix(NA, nrow = 4, ncol = 1))
      max_rows$thermo_mlO2_15min[1] <- sort(x$thermo_mlO2_15min,partial=n)[n]
      max_rows$thermo_mlO2_15min[2] <- sort(x$thermo_mlO2_15min,partial=n-1)[n-1]
      max_rows$thermo_mlO2_15min[3] <- sort(x$thermo_mlO2_15min,partial=n-2)[n-2]
      max_rows$thermo_mlO2_15min[4] <- sort(x$thermo_mlO2_15min,partial=n-2)[n-3]
      sum(max_rows$thermo_mlO2_15min)
    }
  })
  test <- do.call(sum, iter)
  # make ddmm variable to call date and month for file name
  # can use zero to separate because months in study 
  ddmm_site <- paste(list_day[[1]][1,3], list_day[[1]][1,4], list_day[[1]][1,2], sep="_") 
  # were single digit (i.e. <10)
  saveRDS(iter, paste("Thermo_iterations//iter_max2//" , ddmm_site, "_itermax2", ".RDS", sep = ""))
  saveRDS(test, paste("Thermo_iterations//test_max2//", ddmm_site, "_testmax2", ".RDS", sep = ""))
}

maxTemp2_therm(talist_hc_1306)
maxTemp2_therm(talist_hc_2706)
maxTemp2_therm(talist_hc_0907)
maxTemp2_therm(talist_sc_0207)
maxTemp2_therm(talist_sc_0907)

## Make a table to store results for thermo costs at min and max temperatures
Results <- data.frame(matrix(NA, nrow = 5, ncol = 15))
names(Results) <- c("Site", "Day", "Month", "Year", "RandTemp_median_thermo_day", "RandTemp_min_thermo_day",
                    "RandTemp_max_thermo_day", "MinTemp_thermo_day", "MaxTemp_thermo_day",
                    "DEE_randTemp", "DEE_minTemp","DEE_maxTemp", "DLW_mean", "DLW_min", "DLW_max")
Results$Site <- c("HC", "HC", "HC", "SC", "SC")
Results$Day <- c(13, 27, 9, 2, 9)
Results$Month <- c(6, 6, 7, 7, 7)
Results$Year <- 2013

## For now manually inout from results printed output of the compile_iters() function of random temperatures
Results$RandTemp_median_thermo_day[1] <- 295.83
Results$RandTemp_median_thermo_day[2] <- 302.14
Results$RandTemp_median_thermo_day[3] <- 307.22
Results$RandTemp_median_thermo_day[4] <- 282.54
Results$RandTemp_median_thermo_day[5] <- 304.83
  
Results$RandTemp_min_thermo_day[1] <- 284.67
Results$RandTemp_min_thermo_day[2] <- 291.19
Results$RandTemp_min_thermo_day[3] <- 300.02
Results$RandTemp_min_thermo_day[4] <- 273.92
Results$RandTemp_min_thermo_day[5] <- 295.8

Results$RandTemp_max_thermo_day[1] <- 305.2
Results$RandTemp_max_thermo_day[2] <- 313.21
Results$RandTemp_max_thermo_day[3] <- 318.38
Results$RandTemp_max_thermo_day[4] <- 289.26
Results$RandTemp_max_thermo_day[5] <- 312.76

Results$MinTemp_thermo_day[1] <- readRDS("Thermo_iterations\\test_min2\\1306_testmin2.RDS")
Results$MinTemp_thermo_day[2] <- readRDS("Thermo_iterations\\test_min2\\2706_testmin2.RDS")
Results$MinTemp_thermo_day[3] <- readRDS("Thermo_iterations\\test_min2\\9_7_HC_testmin2.RDS")
Results$MinTemp_thermo_day[4] <- readRDS("Thermo_iterations\\test_min2\\207_testmin2.RDS")
Results$MinTemp_thermo_day[5] <- readRDS("Thermo_iterations\\test_min2\\9_7_SC_testmin2.RDS")

Results$MaxTemp_thermo_day[1] <- readRDS("Thermo_iterations\\test_max2\\1306_testmax2.RDS")
Results$MaxTemp_thermo_day[2] <- readRDS("Thermo_iterations\\test_max2\\2706_testmax2.RDS")
Results$MaxTemp_thermo_day[3] <- readRDS("Thermo_iterations\\test_max2\\9_7_HC_testmax2.RDS")
Results$MaxTemp_thermo_day[4] <- readRDS("Thermo_iterations\\test_max2\\207_testmax2.RDS")
Results$MaxTemp_thermo_day[5] <- readRDS("Thermo_iterations\\test_max2\\9_7_SC_testmax2.RDS")

write.csv(Results, "Summary_minmaxTemps_prelim.csv")

#### Plots #####
#Operative temperature, July 8, 2013, HC
m.te_hour <- m.te_det[m.te_det$Hour==700 & m.te_det$DayMonth=="8,7" & m.te_det$Site=="HC",]
ggplot(m.te_hour, aes(Sensor, Te)) + geom_point(size=4) + my_theme + 
  ylab(Te.lab) +
  ggtitle("Harshaw July 8, 2013, 7am")

#Operative temp 13 June, 2013, HC
ggplot(m.te_det[m.te_det$DayMonth=="13,6" & m.te_det$Site=="HC",], aes(Hour, Te)) + my_theme + 
  geom_point(data=m.te_det[m.te_det$DayMonth=="13,6" & m.ta_det$Site=="HC",], aes(Hour, Te), size=4) + 
  ylab(Te.lab) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(5,55) +
  ggtitle("Harshaw June 13, 2013")

#Ambient temp 13 June, 2013, HC
ggplot(m.ta_det[m.ta_det$DayMonth=="13,6" & m.ta_det$Site=="HC",], aes(Hour, Ta)) + 
  geom_point(size=4) + my_theme + 
  ylab(Ta.lab) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(5,55) +
  ggtitle("Harshaw June 13, 2013")

#Operative and Ambient temp 13 June, 2013, HC
ggplot(NULL, aes(Hour, Te)) + my_theme + 
  geom_point(data=m.te_det[m.te_det$DayMonth=="13,6" & m.te_det$Site=="HC",], aes(Hour, Te), size=4, col="black") + 
  geom_point(data=m.ta_det[m.ta_det$DayMonth=="13,6" & m.ta_det$Site=="HC",], aes(Hour, Ta), col="red") +
  ylab(Te.lab) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(5,55) +
  ggtitle("Harshaw June 13, 2013")

#Operative temp 27 June, 2013, HC
ggplot(m.te_det[m.te_det$DayMonth=="27,6" & m.te_det$Site=="HC",], aes(Hour, Te)) + geom_point(size=4) + my_theme + 
  ylab(Te.lab) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(5,55) +
  ggtitle("Harshaw June 27, 2013")

#Ambient temp 27 June, 2013, HC
ggplot(m.ta_det[m.ta_det$DayMonth=="27,6" & m.ta_det$Site=="HC",], aes(Hour, Ta)) + geom_point(size=4) + my_theme + 
  ylab(Ta.lab) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(5,55) +
  ggtitle("Harshaw June 27, 2013")

# Using these for results section on Ta in paper
min(m.ta_det$Ta[m.ta_det$DayMonth=="27,6" & m.ta_det$Site=="HC"]) #HC pre-monsoon min
max(m.ta_det$Ta[m.ta_det$DayMonth=="27,6" & m.ta_det$Site=="HC"]) #HC pre-monsoon max
min(m.ta_det$Ta[m.ta_det$DayMonth=="27,6" & m.ta_det$Site=="SC"]) #SC pre-monsoon min
max(m.ta_det$Ta[m.ta_det$DayMonth=="27,6" & m.ta_det$Site=="SC"]) #SC pre-monsoon max

min(m.ta_det$Ta[m.ta_det$DayMonth=="9,7" & m.ta_det$Site=="HC"]) #HC post-monsoon min
max(m.ta_det$Ta[m.ta_det$DayMonth=="9,7" & m.ta_det$Site=="HC"]) #HC post-monsoon max
min(m.ta_det$Ta[m.ta_det$DayMonth=="9,7" & m.ta_det$Site=="SC"]) #SC post-monsoon min
max(m.ta_det$Ta[m.ta_det$DayMonth=="9,7" & m.ta_det$Site=="SC"]) #SC post-monsoon max


#Operative and Ambient temp 27 June, 2013, HC
ggplot(NULL, aes(Hour, Te)) + my_theme + 
  geom_point(data=m.te_det[m.te_det$DayMonth=="27,6" & m.te_det$Site=="HC",], aes(Hour, Te), size=4, col="black") + 
  geom_point(data=m.ta_det[m.ta_det$DayMonth=="27,6" & m.ta_det$Site=="HC",], aes(Hour, Ta), col="red") +
  ylab(Te.lab) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(5,55) +
  ggtitle("Harshaw June 27, 2013")

temp_HC_2706.agg <- aggregate(m.ta_det$Ta[m.ta_det$DayMonth=="27,6" & m.ta_det$Site=="HC"], 
                              by=list(as.factor(as.character(
                                m.ta_det[m.ta_det$DayMonth=="27,6" & m.ta_det$Site=="HC",]$Hour))), 
                              FUN="mean", na.rm=T)
names(temp_HC_2706.agg) <- c("Hour", "Ta")
temp_HC_2706.agg$Hour <- as.factor(temp_HC_2706.agg$Hour)

p1 <- ggplot(temp_HC_2706.agg, aes(Hour, Ta)) +
  geom_point(data=m.ta_det[m.ta_det$DayMonth=="27,6" & m.ta_det$Site=="HC",], aes(Hour, Ta),size=4, alpha=0.3) +
  geom_line(data=temp_HC_2706.agg, aes(Hour, Ta, group='Hour'), col='black', size=3) + 
  my_theme +  ylim(5,55) +
  ylab(Ta.lab) + theme(axis.text.x = element_text(angle = 60, hjust = 1, size=15), 
                       plot.title = element_text(hjust = 0.5), axis.title.y = element_text(vjust = -2, size=25)) +
  geom_text(x = 7, y = 50, label = "Harshaw June 27, 2013", size=8)

## Ambient temps, 2 July 2013, HC
ggplot(m.ta_det[m.ta_det$DayMonth=="2,7" & m.ta_det$Site=="HC",], aes(Hour, Ta)) + geom_point(size=4) + my_theme + 
  ylab(Ta.lab) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(5,55) +
  ggtitle("Harshaw July 2, 2013")


## Ambient and Operative, 2 July 2013, HC
ggplot(NULL, aes(Hour, Ta)) + my_theme + 
  geom_point(data=m.te_det[m.te_det$DayMonth=="2,7" & m.te_det$Site=="HC",], aes(Hour, Te, col=Te), col="black", size=4) +
  geom_point(data=m.ta_det[m.ta_det$DayMonth=="2,7" & m.ta_det$Site=="HC",], aes(Hour, Ta, col=Ta), col="red") +
  ylab(Ta.lab) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(5,55) +
  ggtitle("Harshaw July 2, 2013")

## Putting one day each of pre- and post- monsoon for HC on the same plot
ggplot(m.ta_det[m.ta_det$Site=="HC",], aes(Hour, Ta)) + my_theme + 
  geom_point(data=m.ta_det[m.ta_det$DayMonth=="27,6",], aes(Hour, Ta, col=DayMonth), col="black", size=4) +
  geom_point(data=m.ta_det[m.ta_det$DayMonth=="2,7",], aes(Hour, Ta, col=DayMonth), col="red") +
  ylab(Ta.lab) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=20), 
                       plot.title = element_text(size=25, hjust=0.5)) + 
  ylim(5,55) + ggtitle("Harshaw pre- and post-monsoon, 2013")

#Ambient temp 9 July, 2013, HC
ggplot(m.ta_det[m.ta_det$DayMonth=="9,7" & m.ta_det$Site=="HC",], aes(Hour, Ta)) + geom_point(size=4) + my_theme + 
  ylab(Ta.lab) + theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                       plot.title = element_text(hjust = 0.5)) + ylim(5,55) +
  ggtitle("Harshaw July 9, 2013")

temp_HC_0907.agg <- aggregate(m.ta_det$Ta[m.ta_det$DayMonth=="9,7" & m.ta_det$Site=="HC"], 
                              by=list(as.factor(as.character(
                                m.ta_det[m.ta_det$DayMonth=="9,7" & m.ta_det$Site=="HC",]$Hour))), 
                              FUN="mean", na.rm=T)
names(temp_HC_0907.agg) <- c("Hour", "Ta")
temp_HC_0907.agg$Hour <- as.factor(temp_HC_0907.agg$Hour)

p2 <- ggplot(temp_HC_0907.agg, aes(Hour, Ta)) +
  geom_point(data=m.ta_det[m.ta_det$DayMonth=="9,7" & m.ta_det$Site=="HC",], aes(Hour, Ta),size=4, alpha=0.3) +
  geom_line(data=temp_HC_0907.agg, aes(Hour, Ta, group='Hour'), col='black', size=3) + 
  my_theme + ylim(5,55) +
  ylab(Ta.lab) + theme(axis.text.x = element_text(angle = 60, hjust = 1, size=15), 
                       plot.title = element_text(hjust = 0.5), axis.title.y = element_text(vjust = -2, size=25)) + 
  geom_text(x = 7, y = 50, label = "Harshaw July 9, 2013", size=8)

#Operative temp 2 July, 2013, SC
ggplot(m.te_det[m.te_det$DayMonth=="2,7" & m.te_det$Site=="SC",], aes(Hour, Te)) + geom_point(size=4) + my_theme + 
  ylab(Te.lab) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(5,55) +
  ggtitle("Sonoita July 2, 2013")

#Ambient temp 2 July, 2013, SC
ggplot(m.ta_det[m.ta_det$DayMonth=="2,6" & m.ta_det$Site=="SC",], aes(Hour, Ta)) + geom_point(size=4) + my_theme + 
  ylab(Ta.lab) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(5,55) +
  ggtitle("Sonoita July 2, 2013")

#Operative and ambient temps 2 July, 2013, SC
ggplot(NULL, aes(Hour, Ta)) + my_theme + 
  geom_point(data=m.te_det[m.te_det$DayMonth=="2,7" & m.te_det$Site=="SC",], aes(Hour, Te), col="black", size=4) +
  geom_point(data=m.ta_det[m.ta_det$DayMonth=="2,7" & m.ta_det$Site=="SC",], aes(Hour, Ta), col="red") + 
  ylab(Ta.lab) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(5,55) +
  ggtitle("Sonoita July 2, 2013")


temp_SC_0207.agg <- aggregate(m.ta_det$Ta[m.ta_det$DayMonth=="2,7" & m.ta_det$Site=="SC"], 
                              by=list(as.factor(as.character(
                                m.ta_det[m.ta_det$DayMonth=="2,7" & m.ta_det$Site=="SC",]$Hour))), 
                              FUN="mean", na.rm=T)
names(temp_SC_0207.agg) <- c("Hour", "Ta")
temp_SC_0207.agg$Hour <- as.factor(temp_SC_0207.agg$Hour)

p3 <- ggplot(temp_SC_0207.agg, aes(Hour, Ta)) +
  geom_point(data=m.ta_det[m.ta_det$DayMonth=="2,7" & m.ta_det$Site=="SC",], aes(Hour, Ta),size=4, alpha=0.3) +
  geom_line(data=temp_SC_0207.agg, aes(Hour, Ta, group='Hour'), col='black', size=3) + 
  my_theme + ylim(5,55) +
  ylab(Ta.lab) + theme(axis.text.x = element_text(angle = 60, hjust = 1, size=15), 
                       plot.title = element_text(hjust = 0.5), axis.title.y = element_text(vjust = -2, size=25)) + 
  geom_text(x = 7, y = 50, label = "Sonoita July 2, 2013", size=8)

#Ambient temp 9 July, 2013, SC
temp_SC_0907.agg <- aggregate(m.ta_det$Ta[m.ta_det$DayMonth=="9,7" & m.ta_det$Site=="SC"], 
                       by=list(as.factor(as.character(
                         m.ta_det[m.ta_det$DayMonth=="9,7" & m.ta_det$Site=="SC",]$Hour))), 
                       FUN="mean", na.rm=T)
names(temp_SC_0907.agg) <- c("Hour", "Ta")
temp_SC_0907.agg$Hour <- as.factor(temp_SC_0907.agg$Hour)

p4 <- ggplot(temp_SC_0907.agg, aes(Hour, Ta)) +
  geom_point(data=m.ta_det[m.ta_det$DayMonth=="9,7" & m.ta_det$Site=="SC",], aes(Hour, Ta),size=4, alpha=0.3) +
  geom_line(data=temp_SC_0907.agg, aes(Hour, Ta, group='Hour'), col='black', size=3) + 
  my_theme + ylim(5,55) +
  ylab(Ta.lab) + theme(axis.text.x = element_text(angle = 60, hjust = 1, size=15), 
                       plot.title = element_text(hjust = 0.5), axis.title.y = element_text(vjust = -2, size=25)) + 
  geom_text(x = 7, y = 50, label = "Sonoita July 9, 2013", size=8)
p4

grid.arrange(p1, p3, p2, p4, nrow=2, ncol=2)

  
