## Analyzing and plotting AZ ambient temperature data
## For torpor manuscript- Rebecca Schroeder*, Anusha Shankar*, Joseph Canepa, Catherine Graham, Don Powers

require(ggplot)
require(reshape)

## Set wd and read in files
setwd("C://Users//ANUSHA//Dropbox//Hummingbird energetics//AZ temperature 2")

## Ambient temperature for Harshaw Creek, 6/27/13 - 6/28/13 and Sonoita Creek 7/2/2013 - 7/3/2013
AZ_ta <- read.csv("AZ_Temp_toPlot.csv")

