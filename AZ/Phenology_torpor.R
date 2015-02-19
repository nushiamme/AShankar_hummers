library(reshape)

## Set working dir
setwd("C://Users//ANUSHA//Desktop//AZ_Resources//")

## Read in phenology dataset- subset of original dataset- 
## for just HC and PL/Sc one week before and after torpor sampling period. Between 6/8/2013 and 7/8/2013
phen <- read.csv("PhenologyData_Torpor.csv", stringsAsFactors=FALSE)
## Across all dates from May - July
phen2 <- read.csv("PhenologyData.csv", stringsAsFactors=FALSE)
## To calculate total plant species richness
treeshrub <- read.csv("Tree_Shrub.csv")

## Melt to get species richness
sp.phen <- melt(phen, id.vars = c("Month", "Day", "Site"), 
     measure.vars = c("PlantSpecies"), na.rm=T)

## For total species richness
m.treeshrub <- melt(treeshrub, id.vars = c("Site","Transect"), measure.vars = "Species")

## Melt to get phenology
m.phen <- melt(phen, id.vars = c("Month", "Day", "Site"), 
               measure.vars = c("TotalFlowers", "TotalBuds", "TotalFruits"), na.rm=T)

## Calculate hummingbird-visited-species richness for each site
HC_sp.richness <- length(unique(sp.phen$value[m.phen$Site=="HC"]))
PLSC_sp.richness <- length(unique(sp.phen$value[m.phen$Site=="PL/SC"]))
HC_sp.richness
PLSC_sp.richness

## Calculate TOTAL species richness- for determining vegetations structure
n_HC_treeshrub <- length(unique(m.treeshrub$Transect[m.treeshrub$Site=="HC"]))
n_PLSC_treeshrub <- length(unique(m.treeshrub$Transect[m.treeshrub$Site=="PL/SC"]))
HC_treeshrub <- (length(unique(m.treeshrub$value[m.treeshrub$Site=="HC"])))/n_HC_treeshrub
PLSC_treeshrub <- (length(unique(m.treeshrub$value[m.treeshrub$Site=="PL/SC"])))/n_PLSC_treeshrub
HC_treeshrub
PLSC_treeshrub

## Calculate phenological variables for HC
HC_flowers <- mean(m.phen$value[m.phen$variable=="TotalFlowers" & m.phen$Site=="HC"])
HC_buds <- sum(m.phen$value[m.phen$variable=="TotalBuds" & m.phen$Site=="HC"])
HC_fruits <- sum(m.phen$value[m.phen$variable=="TotalFruits" & m.phen$Site=="HC"])

## HC Flowers/Fruits ratio
HC_flowers/HC_fruits

## Calculate phenological variables for PL/SC
PLSC_flowers <- mean(m.phen$value[m.phen$variable=="TotalFlowers" & m.phen$Site=="PL/SC"])
PLSC_buds <- sum(m.phen$value[m.phen$variable=="TotalBuds" & m.phen$Site=="PL/SC"])
PLSC_fruits <- sum(m.phen$value[m.phen$variable=="TotalFruits" & m.phen$Site=="PL/SC"])

## Flowers in HC and PL/SC in the torpor measurement period per transect
n_HC_flowersobs <- length((m.phen$value[m.phen$variable=="TotalFlowers" & m.phen$Site=="HC"])) 
HC_flowers/n_HC_flowerobs
PLSC_flowers

## Flowers/Fruits ratio
HC_flowers/HC_fruits
PLSC_flowers/PLSC_fruits