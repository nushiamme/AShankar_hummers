library(reshape)

## Set working dir
setwd("C://Users//ANUSHA//Desktop//AZ_Resources//")

## Read in phenology dataset- subset of original dataset- 
## for just HC and PL/Sc one week before and after torpor sampling period. Between 6/8/2013 and 7/8/2013
phen <- read.csv("PhenologyData_Torpor.csv", stringsAsFactors=FALSE)

## Melt to get species richness
sp.phen <- melt(phen, id.vars = c("Month", "Day", "Site"), 
     measure.vars = c("PlantSpecies"), na.rm=T)

m.phen <- melt(phen, id.vars = c("Month", "Day", "Site"), 
               measure.vars = c("TotalFlowers", "TotalBuds", "TotalFruits"), na.rm=T)

HC_sp.richness <- length(unique(sp.phen$value[m.phen$Site=="HC"]))
PLSC_sp.richness <- length(unique(sp.phen$value[m.phen$Site=="PL/SC"]))
HC_sp.richness
PLSC_sp.richness

average(m.phen$value

