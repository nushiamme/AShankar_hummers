## Floral census data 2013 HC/SC analyses
## Anusha Shankar
## Started April 27, 2017
## For BBLH energy budget paper

## Reading in packages
require(ggplot2)
require(reshape)
require(plyr)
require(dplyr)
require(ggthemes) ## Trying out Tufteboxplot

## Set working directory
#setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Tables")
## wd at GFU
setwd("/Users/anshankar/Dropbox/Anusha Committee/BBLH_EnergyBudget/Tables")

## Read in file
floralsumm <- read.csv("HC_SCSNA_ANUSHA_e_summaries.csv") #ver2
floralsumm2 <- read.csv("FloralCensusData2013.csv") #ver3 
scrop <- read.csv("StandingCropData_new.csv") #ver3
floralsumm$Site <- revalue(floralsumm$Site, c("HC"="Harshaw", "PLSC"="Sonoita"))
scrop$Site <- revalue(scrop$Site, c("HC"="Harshaw", "PL/SC"="Sonoita")) #ver3
floral <- read.csv("FloralCensus_cropped_commas.csv") #ver2
floral$Site_monsoon <- paste(floral$Site,floral$Pre_post_monsoon, sep = "_")
floral$Pre_post_monsoon <- factor(floral$Pre_post_monsoon, levels = c("Pre", "Post"))
head(floral)

#### New columns/dataframes ####
flo <- group_by(floralsumm2, Site, Transect, Pre_post)
dflo <- summarize(flo, Flowers = sum(TotalFlowers, na.rm = T))
dhumm <- summarize(flo, Hummcount = sum(HummSp, na.rm=T))
dfru <- summarize(flo, Fruits = sum(Fruits, na.rm=T))
dbud <- summarize(flo, Buds = sum(Buds, na.rm=T))

#### New columns/dataframes, Jan 12 ####
dcrop <- group_by(scrop, Site, Year, Pre_post, Transect)
dcrop_summ <- summarize(dcrop, Calories = sum(Calories, na.rm = T))

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
## From #ver3 (November 2017) Standing Crop data
ggplot(dcrop_summ[dcrop_summ$Site %in% c("Harshaw", "Sonoita"),], aes(Transect, 4.184*Calories, label=round(4.184*Calories, digits = 0))) + 
  geom_point(aes(color=Pre_post, size=Calories)) + facet_grid(~Site, scales='free_x', space='free') +
  geom_text(hjust=0, nudge_x = 0.2, nudge_y=0.3, size=5) +
  scale_color_manual(values = c('red', 'black'), labels=c("Post-monsoon", "Pre-monsoon"), name="Monsoon status") + 
  ylab("kiloJoules") + guides(size=F) +
  #geom_bar(stat="identity", aes(fill=Pre_post), size=4) + 
  #scale_fill_manual(values = c('red', 'black')) + scale_shape_manual(values=c(20,3)) +
  my_theme +  theme(axis.text.x = element_text(size=15, angle=30, vjust=0.95, hjust=1), legend.key.height = unit(3, 'lines'))

## Number of flowers
dflo$Site_Pre_post <- paste(dflo$Site, dflo$Pre_post, sep="_")
ggplot(dflo[dflo$Site %in% c("Harshaw", "Sonoita"),], aes(Transect, log(Flowers))) + facet_grid(~Site, scales="free_x") +
  geom_bar(stat="identity", aes(fill=Pre_post), size=4) + 
  #scale_fill_manual(values = c('red', 'black')) +
  my_theme +  theme(axis.text.x = element_text(size=15, angle=90), legend.key.height = unit(3, 'lines'))

## Good plot of flowers at HC and SC
ggplot(dflo[dflo$Site %in% c("Harshaw", "Sonoita"),], aes(Transect, log(Flowers), label=Flowers)) + 
  facet_grid(~Site, scales="free_x", space='free') + #coord_flip() +
  #geom_tufteboxplot() +
  geom_point(aes(color=Pre_post, size=Flowers)) +
  geom_text(hjust=0, nudge_x = 0.1, nudge_y=0.3, size=5) +
  scale_color_manual(values = c('red', 'black'), labels=c("Post-monsoon", "Pre-monsoon"), name="Monsoon status") + 
  guides(colour = guide_legend(override.aes = list(size=4)), size=F) +
  my_theme +  theme(axis.text.x = element_text(size=15, angle=30, vjust=0.9, hjust=1), legend.key.height = unit(3, 'lines'))

## From what Susan sent by Claudia
##Summary stats of phenology, comparing sites
sum(floralsumm2$TotalFlowers[floralsumm2$Site=="HC"])
sum(floralsumm2$TotalFlowers[floralsumm2$Site=="PLSC"])

##Using database Susan sent in Oct 2017
sum(scrop$Calories[scrop$Site=="Sonoita" & scrop$Month==6])
sum(floralsumm2$TotalFlowers[floralsumm2$Site=="PLSC"])

## Plot sum of calories by site
ggplot(dcal[dcal$Site %in% c("Harshaw", "Sonoita") & dcal$Year==2013,], aes(Transect, (Calories*4.18))) + 
  geom_bar(aes(fill=Site), stat='identity') + my_theme + theme(axis.text.x = element_text(angle=60, size=15, vjust=0.5)) +
         facet_grid(.~Pre_post, scales='free') + ylab("kiloJoules")

ggplot(dcal, aes(Site, (Calories))) + geom_bar(stat='identity') + my_theme + facet_grid(.~Pre_post) 

## Plot sum of flowers by site
ggplot(dflo[dflo$Site %in% c("Harshaw", "Sonoita"),], aes(Transect, Flowers)) + 
  geom_bar(aes(fill=Site), stat='identity') + facet_grid(~Pre_post) + my_theme + 
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=60, size=15, vjust=0.5))

## Plot sum of flowers by site
ggplot(dflo[dflo$Site %in% c("Harshaw", "Sonoita"),], aes(Transect, log(Flowers))) + 
  geom_bar(aes(fill=Site), stat='identity') + 
  facet_grid(~Pre_post, scales='free') + my_theme + 
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=60, size=15, vjust=0.5))


## Sum of flowers per transect
flo_plot_t <- ggplot(dflo, aes(Transect, log(Flowers))) + 
  geom_bar(stat='identity') + my_theme + 
  facet_grid(Month~Site, scales='free') +
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
