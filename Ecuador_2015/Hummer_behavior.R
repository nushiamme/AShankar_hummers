## August 13, 2015
## Anusha Shankar
## El Gullan, Ecuador, June 2015 - January 2016
## Hummingbird behavior analyses

library(reshape)
library(ggplot2)

setwd("C:\\Users\\ANUSHA\\Dropbox\\Data 2015\\Behavior")
beh <- read.csv("Beh_for_analysis_Nov2015.csv")
beh_aug <- read.csv("Beh_for_analysis.csv")
head(beh_aug)

m.beh <- melt(beh, id.vars = c("Observer", "Time", "Species", "Month"), 
              measure.vars = c("FL", "Hov", "Perch"), na.rm = T)

## Checking old file, because things are weird
m.beh_aug <- melt(beh_aug, id.vars = c("Observer", "Time", "Species", "Month"), 
              measure.vars = c("FL", "Hov", "Perch"), na.rm = T)

m.beh_mainsp <- m.beh[m.beh$Species==c("AGCU","COIR","METY","HEVI"),]

##Checking old file
# Total_FL_aug <- sum(as.numeric(m.beh_aug$value[m.beh_aug$variable=="FL"])) # adding total spent flying
# Total_Hov_aug <- sum(as.numeric(m.beh_aug$value[m.beh_aug$variable=="Hov"]))
# Total_perch_aug <- sum(as.numeric(m.beh_aug$value[m.beh_aug$variable=="Perch"]))

Total_FL <- sum(as.numeric(m.beh$value[m.beh$variable=="FL"])) # adding total spent flying
Total_Hov <- sum(as.numeric(m.beh$value[m.beh$variable=="Hov"]))
Total_perch <- sum(as.numeric(m.beh$value[m.beh$variable=="Perch"]))

total_beh_time <- sum(Total_FL,Total_Hov,Total_perch)
Total_FL_perc <- (Total_FL/total_beh_time)*100
Total_Hov_perc <- (Total_Hov/total_beh_time)*100
Total_perch_perc <- (Total_perch/total_beh_time)*100
total_beh_time
Total_FL_perc
Total_Hov_perc
Total_perch_perc

Fl_not_1 <- m.beh$variable=="FL"[m.beh$Beh_no!=1&m.beh$variable=="FL"]
length(Fl_not_1)
Hov_not_1 <- m.beh$variable=="Hov"[m.beh$Beh_no!=1]
length(Hov_not_1)

## Plots
beh_by_month <- ggplot(m.beh, aes(variable, value)) + geom_bar(stat="identity") + theme_bw() +
  facet_grid(~Month) + xlab("Behavior") + ylab("Time (seconds)")
beh_by_month

