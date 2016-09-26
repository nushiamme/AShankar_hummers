## Analyzing and plotting ambient and chamber temperature data for torpor paper
## Anusha Shankar*, Rebecca Schroeder*, Catherine Graham, Don Powers
## Script started on: Sept 24, 2016

require(ggplot2)
require(reshape)

## Set wd and read in files
setwd("C://Users//ANUSHA//Dropbox//Hummingbird energetics//Tables_for_paper/")

## Read in files
tatc <- read.csv("TempSummary_AllSites.csv")

## General functions
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

pd <- position_dodge(0.1) # move them .05 to the left and right

tatc$Hour_rounded <- factor(tatc$Hour_rounded, 
                            levels= c("1900", "1930", "2000", "2030", "2130", "2200", "2230", "2300", "2330", "2400",
                                      "2430", "100", "130", "200", "230", "300", "330", "400", "430", "500", "530",
                                      "600", "630", "700"), ordered=T)
## Aggregate means, min and max
Tc_mean <- aggregate(tatc$Tc_Mean, 
                           by=list(tatc$Site, tatc$Hour_rounded), FUN="mean")
Tc_min <- aggregate(tatc$Tc_min, 
                              by=list(tatc$Site, tatc$Hour_rounded), FUN="min")
Tc_max <- aggregate(tatc$Tc_max, 
                              by=list(tatc$Site, tatc$Hour_rounded), FUN="max")

tatc_summ <- merge(Tc_mean, Tc_min, 
                       by=c("Group.1", "Group.2"))
tatc_summ <- merge(tatc_summ, Tc_max, 
                       by=c("Group.1", "Group.2"))

names(tatc_summ) <- c("Site", "Hour_rounded", "Mean_Tc", "Min_Tc", "Max_Tc")


## Plots
ggplot(tatc_summ, aes(Hour_rounded,Mean_Tc)) + my_theme + 
  #geom_point(aes(col=Site)) + 
  geom_line(aes(col=Site, group=Site), size=3) +
  geom_errorbar(aes(ymin= Min_Tc, ymax= Max_Tc, col=Site), width=.1, position=pd) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Mean chamber temperature")

ggplot(tatc, aes(Hour,Ta_Mean)) + my_theme + 
  geom_point(aes(col=Site)) + geom_line(aes(col=Site, group=Site)) +
  geom_errorbar(aes(ymin=Ta_min, ymax=Ta_max), width=.1, position=pd) +
  theme(axis.text.x = element_text(angle = 90))
