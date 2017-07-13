## Analyzing masses in Santa Lucia, Ecuador banding data for Lewis and Clark report
## July 12, 2017
## Code Author: Anusha Shankar
## Contact: anusha<dot>shankar<at>stonybrook<dot>edu
## Data collected by: Ana Morales, Gabriela Cordova, Rosalee Elting

library(ggplot2)
library(reshape)

setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha_personal\\Thesis_proposal\\Summer 2015")
band_masses <- read.csv("SL_2016_banding_data.csv")
band_lapaz <- read.csv("LaPaz_2016_banding_data.csv")

band_masses$Weight[band_masses$Weight=="-"] <- NA
band_masses$Weight <- as.numeric(as.character((band_masses$Weight)))

band_lapaz$Initial_mass_g[band_lapaz$Initial_mass_g=="-"] <- NA
band_lapaz$Initial_mass_g <- as.numeric(as.character((band_lapaz$Initial_mass_g)))

m.mass <- melt(band_masses, id.vars="Species", measure.vars="Weight")
ggplot(m.mass, aes(Species, value)) + geom_point() + theme_bw()
mass.summ <- aggregate(na.omit(m.mass$value), by=list(m.mass$Species[!is.na(m.mass$value)]), 
          FUN = function(x) c(Min = min(x), Mean = mean(x), Max = max(x)))
mass.summ$Min<- mass.summ$x[,1]
mass.summ$Mean<- mass.summ$x[,2]
mass.summ$Max <- mass.summ$x[,3]
names(mass.summ)[names(mass.summ)=="Group.1"] <- "Species"
mass.summ <- subset(mass.summ, select = -x)


m.mass_lapaz <- melt(band_lapaz, id.vars="Species", measure.vars="Initial_mass_g")
ggplot(m.mass_lapaz, aes(Species, value)) + geom_point() + theme_bw()
massla.summ <- aggregate(na.omit(m.mass_lapaz$value), 
                       by=list(m.mass_lapaz$Species[!is.na(m.mass_lapaz$value)]), 
                       FUN = function(x) c(Min = min(x), Mean = mean(x), Max = max(x)))
massla.summ$Min<- massla.summ$x[,1]
massla.summ$Mean<- massla.summ$x[,2]
massla.summ$Max <- massla.summ$x[,3]
names(massla.summ)[names(massla.summ)=="Group.1"] <- "Species"
massla.summ <- subset(massla.summ, select = -x)
