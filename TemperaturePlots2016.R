## Analyzing and plotting ambient and chamber temperature data for torpor paper
## Anusha Shankar*, Rebecca Schroeder*, Catherine Graham, Don Powers
## Script started on: Sept 24, 2016

require(ggplot2)
require(reshape)

## Set wd and read in files
setwd("C://Users//ANUSHA//Dropbox//Hummingbird energetics//Tables_for_paper/")

## Read in files
tatc <- read.csv("TempSummary_AllSites.csv")
## Made this in R with the aggregating chunk, so avoid that if reading this in
tc_summ <- read.csv("Tc_AllSites_summ.csv")

## General functions, adding columns, ordering factors
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

pd <- position_dodge(0.1) # move them .05 to the left and right

tatc$Hour_rounded <- factor(tatc$Hour_rounded, 
                            levels= c("1900", "1930", "2000", "2030", "2100", "2130", "2200", "2230", "2300", "2330", "2400",
                                      "2430", "100", "130", "200", "230", "300", "330", "400", "430", "500", "530",
                                      "600", "630", "700"), ordered=T)

tatc$Hour2 <- factor(tatc$Hour2, levels= c("19", "20", "21", "22", "23", "24", "1", "2", "3", "4", "5", "6", "7"), ordered=T)

Hour_labels <- c("1900", "2000", "2100", "2200","2300", "2400", "100", "200", "300", "400", "500", "600", "700")

tc_summ$Hour2 <- factor(tc_summ$Hour2, levels= c("19", "20", "21", "22", "23", "24", "1", "2", "3", "4", "5", "6", "7"), ordered=T)
tc_summ$Site <- factor(tc_summ$Site, levels=c('HC','SC','SWRS','MQ','SL'))

Tc.lab <- expression(atop(paste("Chamber Temperature ( ", degree,"C)")))
Ta.lab <- expression(atop(paste("Mean Ambient Temperature (", degree,"C)")))

#### Aggregating ####
## Aggregate means, min and max
Ta_mean <- aggregate(tatc$Ta_Mean, 
                           by=list(tatc$Site, tatc$Hour2), FUN="mean")
Ta_min <- aggregate(tatc$Ta_min, 
                              by=list(tatc$Site, tatc$Hour2), FUN="min")
Ta_max <- aggregate(tatc$Ta_max, 
                              by=list(tatc$Site, tatc$Hour2), FUN="max")

ta_summ <- merge(Ta_mean, Ta_min, 
                       by=c("Group.1", "Group.2"))
ta_summ <- merge(ta_summ, Ta_max, 
                       by=c("Group.1", "Group.2"))

names(ta_summ) <- c("Site", "Hour2", "Mean_Ta", "Min_Ta", "Max_Ta")

Tc_mean <- aggregate(tatc$Tc_Mean, 
                     by=list(tatc$Site, tatc$Hour2), FUN="mean")
Tc_min <- aggregate(tatc$Tc_min, 
                    by=list(tatc$Site, tatc$Hour2), FUN="min")
Tc_max <- aggregate(tatc$Tc_max, 
                    by=list(tatc$Site, tatc$Hour2), FUN="max")

tc_summ <- merge(Tc_mean, Tc_min, 
                 by=c("Group.1", "Group.2"))
tc_summ <- merge(tc_summ, Tc_max, 
                 by=c("Group.1", "Group.2"))

names(tc_summ) <- c("Site", "Hour2", "Mean_Tc", "Min_Tc", "Max_Tc")

Tc_min_HC <- aggregate(tatc$Tc_Mean[tatc$Site=="HC"], 
                    by=list(tatc$Site[tatc$Site=="HC"], tatc$Hour2[tatc$Site=="HC"]), FUN="min")
Tc_max_HC <- aggregate(tatc$Tc_Mean[tatc$Site=="HC"], 
                    by=list(tatc$Site[tatc$Site=="HC"], tatc$Hour2[tatc$Site=="HC"]), FUN="max")

Tc_min_SC <- aggregate(tatc$Tc_Mean[tatc$Site=="SC"], 
                       by=list(tatc$Site[tatc$Site=="SC"], tatc$Hour2[tatc$Site=="SC"]), FUN="min")
Tc_max_SC <- aggregate(tatc$Tc_Mean[tatc$Site=="SC"], 
                       by=list(tatc$Site[tatc$Site=="SC"], tatc$Hour2[tatc$Site=="SC"]), FUN="max")

tc_HC <- merge(Tc_min_HC, Tc_max_HC, by=c("Group.1", "Group.2"))
tc_SC <- merge(Tc_min_SC, Tc_max_SC, by=c("Group.1", "Group.2"))
names(tc_HC) <- c("Site", "Hour2", "Min_Tc", "Max_Tc")
names(tc_SC) <- c("Site", "Hour2", "Min_Tc", "Max_Tc")

tc_summ$Min_Tc[is.na(tc_summ$Min_Tc) | tc_summ$Site=="HC"] <- tc_HC$Min_Tc
tc_summ$Min_Tc[is.na(tc_summ$Min_Tc) | tc_summ$Site=="SC"] <- tc_SC$Min_Tc
tc_summ$Max_Tc[is.na(tc_summ$Max_Tc) | tc_summ$Site=="HC"] <- tc_HC$Max_Tc
tc_summ$Max_Tc[is.na(tc_summ$Max_Tc) | tc_summ$Site=="SC"] <- tc_SC$Max_Tc

write.csv(tc_summ, "Tc_AllSites_summ.csv")

tatc_summ <- merge(ta_summ, tc_summ, by=c("Site", "Hour2"))
tatc_summ$Site <- factor(tatc_summ$Site, levels=c('HC','SC','SWRS','MQ','SL'))
tc_summ$Site <- factor(tc_summ$Site, levels=c('HC','SC','SWRS','MQ','SL'))

#### Plots ####
## Chamber Temp plots by hour, per site
ChambTemp <- ggplot(tc_summ, aes(Hour2,Mean_Tc)) + my_theme + facet_grid(.~Site) +  
  geom_point(aes(group=Site), size=1.5) +
  geom_line(aes(group=Site), ) +
  geom_errorbar(aes(ymin= Min_Tc, ymax= Max_Tc), alpha=0.4, width=.1, position=pd) +
  theme(axis.text.x = element_text(angle = 90, size=15), legend.position="none") +
  xlab("Hour") + ylab(Tc.lab) + ggtitle("Sites") + theme(plot.title = element_text(size = 20)) +
  scale_x_discrete(labels=Hour_labels)
ChambTemp

## Ambient temp
AmbTemp <- ggplot(tatc_summ, aes(Hour_rounded,Mean_Ta)) + my_theme + 
  geom_line(aes(col=Site, group=Site), size=2) +
  geom_errorbar(aes(ymin= Min_Ta, ymax= Max_Ta, col=Site), width=.1, position=pd) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Hour") + ylab(Ta.lab)
AmbTemp
