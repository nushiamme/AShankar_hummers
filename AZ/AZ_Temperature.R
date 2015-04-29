## Analyzing and plotting AZ ambient temperature data
## For torpor manuscript- Rebecca Schroeder*, Anusha Shankar*, Joseph Canepa, Catherine Graham, Don Powers

require(ggplot)
require(reshape)

## Set wd and read in files
setwd("C://Users//ANUSHA//Dropbox//Hummingbird energetics//AZ temperature 2")

## Ambient temperature for Harshaw Creek, 6/27/13 - 6/28/13 and Sonoita Creek 7/2/2013 - 7/3/2013
AZ_ta <- read.csv("AZ_Temp_toPlot.csv")

##Melt AZ_ta
m.AZta <- melt(AZ_ta, id.vars = c("Site", "Date", "Time"), measure.vars = c("Min_Ta", "Max_Ta", "Average"))

m.Mean <- m.AZta[m.AZta$variable=="Average",]
m.Max <- m.AZta[m.AZta$variable=="Max_Ta",]
m.Min <- m.AZta[m.AZta$variable=="Min_Ta",]

## Plots
AZ_TaPlot <- ggplot(m.Max, aes(as.numeric(Time), value, col=Date)) + stat_smooth() + 
  geom_point() + theme_bw() + facet_grid(.~Site)
AZ_TaPlot

#geom_smooth(model=lm, aes(group=1))