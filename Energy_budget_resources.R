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
setwd("C:\\Users\\nushi\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Tables")
## wd at GFU
#setwd("/Users/anshankar/Dropbox/Anusha Committee/BBLH_EnergyBudget/Tables")

## Read in file
floral <- read.csv("FloralCensusData2013.csv") #ver3 
floralsumm <- floral[floral$Site %in% c("Harshaw", "Sonoita"),]
scrop <- read.csv("StandingCropData_new.csv") #ver3
scrop$Site <- revalue(scrop$Site, c("HC"="Harshaw", "PL/SC"="Sonoita")) #ver3

#### New columns/dataframes ####
colnames(floralsumm)[colnames(floralsumm)=="Pre_post"] <- "Season"
floralsumm$Season<- revalue(floralsumm$Season, c("Pre"="Dry", "Post"="Early-wet"))
flo <- group_by(floralsumm, Site, Transect, Season)
dflo <- summarize(flo, Flowers = sum(TotalFlowers, na.rm = T))
#dhumm <- summarize(flo, Hummcount = sum(HummSp, na.rm=T))
#dfru <- summarize(flo, Fruits = sum(Fruits, na.rm=T))
#dbud <- summarize(flo, Buds = sum(Buds, na.rm=T))
dflo$Site_Season <- paste(dflo$Site, dflo$Season, sep="_")
dflo$Site_Season <- factor(dflo$Season, levels = c("Harshaw_Dry", "Harshaw_Early-wet", 
                                                       "Sonoita_Dry", "Sonoita_Early-wet"))

#### New columns/dataframes, Jan 12 ####
colnames(scrop)[colnames(scrop)=="Pre_post"] <- "Season"
scrop$Season<- revalue(scrop$Season, c("Pre"="Dry", "Post"="Early-wet"))
dcrop <- group_by(scrop, Site, Year, Season, Transect)
dcrop_summ <- summarize(dcrop, Calories = sum(Calories, na.rm = T))

## New dataframes May 14, 2018
flo.agg <- aggregate(floralsumm$TotalFlowers, 
                     by=list(floralsumm$PlantSpecies, floralsumm$Month), FUN="mean", na.rm=T)
names(flo.agg) <- c("Species", "Month", "Avg_flowers")

## Quick plot of total flowers across months
ggplot(flo.agg, aes(Species, log(Avg_flowers))) + facet_grid(~Month, scales='free') + geom_point() + 
  my_theme + theme(axis.text.x = element_text(angle=30, size=10))

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
dflo$Pre_post <- factor(dflo$Season, levels = c("Dry", "Early-wet"))
dcrop$Pre_post <- factor(dcrop$Season, levels = c("Dry", "Early-wet"))

ggplot(dcrop_summ[dcrop_summ$Site %in% c("Harshaw", "Sonoita"),], 
       aes(Transect, 4.184*Calories, label=round(4.184*Calories, digits = 0))) + 
  geom_point(aes(color=Season, size=Calories)) + facet_grid(~Site, space='free') +
  geom_text(hjust=0, nudge_x = 0.2, nudge_y=0.3, size=5) +
  scale_color_manual(values = c('red', 'black')) + 
  ylab("kiloJoules") + guides(size=F) +
  #geom_bar(stat="identity", aes(fill=Pre_post), size=4) + 
  #scale_fill_manual(values = c('red', 'black')) + scale_shape_manual(values=c(20,3)) +
  my_theme +  theme(axis.text.x = element_text(size=15, angle=30, vjust=0.95, hjust=1), 
                    legend.key.height = unit(3, 'lines'))

## YES!! Good plot of resources at Hawshaw vs Sonoita, Pre- vs early-monsoon. May 14, 2018
ggplot(dflo[dflo$Flowers>0,], aes(Pre_post, log(Flowers))) + #facet_grid(~Site, scales="free_x") +
  geom_boxplot(aes(fill=Site), position="dodge") + 
  geom_point(aes(x=Pre_post), size=3, alpha=0.8) + facet_grid(~Site) +
  geom_text(aes(label=Flowers), hjust=-0.3, size=6, position=position_jitter(width=0.3)) +
  scale_fill_manual(values = c('grey', 'red')) +
  my_theme + theme(legend.position = "none") +
  xlab("Season")

## Good plot of flowers at HC and SC
ggplot(dflo[dflo$Site %in% c("Harshaw", "Sonoita"),], aes(Site_Season, log(Flowers), label=Flowers)) + 
  facet_grid(~Site, scales="free_x", space='free') + #coord_flip() +
  #geom_tufteboxplot() +
  geom_boxplot(aes(fill=Season, size=Flowers)) +
  geom_point() +
  geom_text(hjust=0, nudge_x = 0.1, nudge_y=0.3, size=7) +
  #scale_color_manual(values = c('red', 'black'), 
  #labels=c("Post-monsoon", "Pre-monsoon"), name="Monsoon status") + 
  guides(colour = guide_legend(override.aes = list(size=4)), size=F) +
  my_theme +  theme(axis.text.x = element_text(size=15, angle=30, vjust=0.9, hjust=1), 
                    legend.key.height = unit(3, 'lines'))

## Good plot of flowers at HC and SC
ggplot(dflo[dflo$Site %in% c("Harshaw", "Sonoita"),], aes(Transect, log(Flowers), label=Flowers)) + 
  facet_grid(~Site, scales="free_x", space='free') + #coord_flip() +
  #geom_tufteboxplot() +
  geom_point(aes(col=Pre_post, size=Flowers), stat="identity") +
  geom_text(hjust=0, nudge_x = 0.1, nudge_y=0.3, size=5) +
  scale_color_manual(values = c('red', 'black'), 
                     labels=c("Post-monsoon", "Pre-monsoon"), name="Monsoon status") + 
  guides(colour = guide_legend(override.aes = list(size=4)), size=F) +
  my_theme +  theme(axis.text.x = element_text(size=15, angle=30, vjust=0.9, hjust=1), 
                    legend.key.height = unit(3, 'lines'))

