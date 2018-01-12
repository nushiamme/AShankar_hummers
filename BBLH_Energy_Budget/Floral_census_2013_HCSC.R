## Floral census data 2013 HC/SC analyses
## Anusha Shankar
## Started April 27, 2017
## For BBLH energy budget paper

## Reading in packages
require(ggplot2)
require(reshape)
require(plyr)
require(dplyr)

## Set working directory
setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Tables")
## wd at GFU
setwd("/Users/anshankar/Dropbox/Anusha Committee/BBLH_EnergyBudget/Tables")

## Read in file
floralsumm <- read.csv("HC_SCSNA_ANUSHA_e_summaries.csv")
floralsumm2 <- read.csv("FloralCensus_new.csv")
scrop <- read.csv("StandingCropData_new.csv")
floralsumm$Site <- revalue(floralsumm$Site, c("HC"="Harshaw", "PLSC"="Sonoita"))
#floralsumm2$Site <- revalue(floralsumm2$Site, c("HC"="Harshaw", "PLSC"="Sonoita"))
scrop$Site <- revalue(scrop$Site, c("HC"="Harshaw", "PL/SC"="Sonoita"))
floral <- read.csv("FloralCensus_cropped_commas.csv")
floral$Site_monsoon <- paste(floral$Site,floral$Pre_post_monsoon, sep = "_")
floral$Pre_post_monsoon <- factor(floral$Pre_post_monsoon, levels = c("Pre", "Post"))
head(floral)

#### New columns/dataframes ####
flo <- group_by(floralsumm, Site, Transect, Month)
dflo <- summarize(flo, Flowers = sum(Flowers, na.rm = T))
dhumm <- summarize(flo, Hummcount = sum(HummSp, na.rm=T))
dfru <- summarize(flo, Fruits = sum(Fruits, na.rm=T))
dbud <- summarize(flo, Buds = sum(Buds, na.rm=T))

#### New columns/dataframes, Jan 12 ####
crop <- group_by(scrop, Site, Year, Pre_post, Transect)
dcal <- summarize(crop, Calories = sum(Calories, na.rm = T))
dhumm <- summarize(flo, Hummcount = sum(HummSp, na.rm=T))
dfru <- summarize(flo, TotalFruits = sum(TotalFruits, na.rm=T))
dbud <- summarize(flo, TotalBuds = sum(TotalBuds, na.rm=T))

#### General functions ####
## Saving standard theme  
my_theme <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(color = "black", vjust = 2),
        panel.border = element_rect(colour = "black", fill=NA))

## Theme with slightly smaller font
my_theme2 <- my_theme + theme_classic(base_size = 15)

## Give sample size
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}


#### Plots ####
## From what Susan sent by Claudia
##Summary stats of phenology, comparing sites
sum(floralsumm2$TotalFlowers[floralsumm2$Site=="HC"])
sum(floralsumm2$TotalFlowers[floralsumm2$Site=="PLSC"])

##Using database Susan sent in Oct 2017
sum(scrop$Calories[scrop$Site=="Sonoita" & scrop$Month==6])
sum(floralsumm2$TotalFlowers[floralsumm2$Site=="PLSC"])

## Plot sum of calories by site
ggplot(dcal[dcal$Site %in% c("Harshaw", "Sonoita"),], aes(Site, Calories)) + geom_bar(stat='identity') + my_theme + facet_grid(.~Pre_post)

## Plot sum of flowers by site
ggplot(dflo, aes(Site, Flowers)) + geom_bar(stat='identity') + my_theme
## Sum of flowers per transect
flo_plot_t <- ggplot(dflo, aes(Transect, log(Flowers))) + 
  geom_bar(stat='identity') + my_theme + 
  facet_grid(~Site, scales='free') +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=60, size=15)) +
  ylim(0,12)

ggplot(dflo, aes(Month, log(Flowers))) + 
  geom_point(size=3) + my_theme + 
  facet_grid(~Site, scales='free') +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=60, size=15)) +
  ylim(0,12)

## Fruits
fru_plot_t <- ggplot(dfru, aes(Transect, log(Fruits))) + geom_bar(stat='identity') + my_theme +
  facet_grid(~Site, scales='free') +
  theme(axis.text.x = element_text(angle=60, size=15)) +
  ylim(0,12)

##Buds
buds_plot_t <- ggplot(dbud, aes(Transect, log(Buds))) + geom_bar(stat='identity') + my_theme +
  facet_grid(~Site, scales='free') +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=60, size=15)) +
  ylim(0,12)

## Hummingbird detections
humm_plot_t <- ggplot(dhumm, aes(Month, Hummcount)) + geom_bar(stat='identity') + my_theme +
  facet_grid(~Site, scales='free') +
  theme(axis.text.x = element_text(angle=60, size=15)) +
  ylab("Hummingbird \n detections") 

grid.arrange(flo_plot_t, buds_plot_t, fru_plot_t, humm_plot_t, ncol=2, nrow=2)

## Plotting flowers and hummingbird detections on same plot- doesn't work too well
ggplot(NULL, aes(Transect, log(Flowers))) + 
  my_theme +
  geom_bar(data=dflo, aes(Transect, log(Flowers)), stat='identity') + 
  geom_point(data=dhumm, aes(Transect, Hummcount), col='red', size=3) +
  facet_grid(~Site, scales='free') +
  theme(axis.text.x = element_text(angle=60, size=15))

## From old Access database
## plot of flowers per site and month
flor_sum <- ddply(floral,~Site_monsoon,summarise,
                  scaled_flowers=sum(TotalFlowers)/(length(unique(Date))))
flor_sum$Site_monsoon <- factor(flor_sum$Site_monsoon, 
                                   levels = c("HC_Pre", "PL/SC_Pre", "HC_Post", "PL/SC_Post"))

ggplot(flor_sum, aes(Site_monsoon, scaled_flowers)) + 
  geom_bar(stat = "identity", width=0.5) + my_theme

ggplot(floral, aes(Site,(TotalFlowers))) + 
  geom_bar(stat="identity", width=0.5) + 
  my_theme + facet_grid(.~Pre_post_monsoon)
