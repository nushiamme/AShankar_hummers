## Analyzing and plotting AZ ambient temperature data
## For torpor manuscript- Rebecca Schroeder*, Anusha Shankar*, Catherine Graham, Don Powers

require(ggplot2)
require(reshape)

## Set wd and read in files
setwd("C://Users//ANUSHA//Dropbox//Hummingbird energetics//AZ temperature 2")

## Ambient temperature for Harshaw Creek, 6/27/13 - 6/28/13 and Sonoita Creek 7/2/2013 - 7/3/2013
AZ_ta <- read.csv("AZ_Temp_toPlot.csv")

my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

##Melt AZ_ta
m.AZta <- melt(AZ_ta, id.vars = c("Site", "Date", "Time"), 
               measure.vars = c("Min_Ta", "Max_Ta", "Average"))

m.Mean <- m.AZta[m.AZta$variable=="Average",]
m.Max <- m.AZta[m.AZta$variable=="Max_Ta",]
m.Min <- m.AZta[m.AZta$variable=="Min_Ta",]

time <- c("00:00", "05:00", "10:00", "15:00", "20:00")
## Plots
AZ_TaPlot <- ggplot(m.Mean, aes(as.numeric(Time), value)) + stat_smooth(aes(col=Site)) + my_theme +
  geom_point() + xlab ("Time") + 
  scale_x_discrete(breaks=c("0", "500", "1000", "1500", "2000"),
                   labels=c("00:00", "05:00", "10:00", "15:00", "20:00")) + 
  facet_wrap(Date~Site)
AZ_TaPlot

blank_data <- data.frame(group = c("A", "A", "B", "B", "C", "C"), x = 0, y = c(0, 8, 0, 40, 0, 50))


Ta_HC <- ggplot(m.Mean[m.Mean$Date==c("6/27/2013","6/28/2013"),], 
                aes(as.numeric(Time), value)) + stat_smooth() + geom_point(colour="red", size=3) + xlab ("Time") + 
  scale_x_discrete(breaks=c("0", "500", "1000", "1500", "2000"),
                   labels=c("00:00", "05:00", "10:00", "15:00", "20:00")) + 
  theme_bw(base_size=12) + facet_grid(Site~Date) + theme(legend.position = "none")

Ta_HC

Ta_SC <- ggplot(m.Mean[m.Mean$Date==c("7/2/2013","7/3/2013"),], 
                aes(as.numeric(Time), value)) + stat_smooth() + geom_point(colour="black",size=3) + xlab ("Time") + 
  scale_x_discrete(breaks=c("0", "500", "1000", "1500", "2000"),
                   labels=c("00:00", "05:00", "10:00", "15:00", "20:00")) + 
  theme_bw() + facet_grid(Site~Date) + theme(legend.position = "none")
Ta_SC

grid.arrange(Ta_HC, Ta_SC, nrow=2)
#geom_smooth(model=lm, aes(group=1))
