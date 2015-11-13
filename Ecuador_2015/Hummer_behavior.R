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

##Checking old file
Total_FL_aug <- sum(m.beh_aug$value[m.beh_aug$variable=="FL"]) # adding total spent flying
Total_Hov_aug <- sum(m.beh_aug$value[m.beh_aug$variable=="Hov"])
Total_perch_aug <- sum(m.beh_aug$value[m.beh_aug$variable=="Perch"])


Total_FL <- sum(as.numeric(m.beh$value[m.beh$variable=="FL"])) # adding total spent flying
Total_Hov <- sum(as.numeric(m.beh$value[m.beh$variable=="Hov"]))
Total_perch <- sum(as.numeric(m.beh$value[m.beh$variable=="Perch"]))

Fl_not_1 <- m.beh$variable=="FL"[m.beh$Beh_no!=1&m.beh$variable=="FL"]
length(Fl_not_1)
Hov_not_1 <- m.beh$variable=="Hov"[m.beh$Beh_no!=1]
length(Hov_not_1)

## Plots
ggplot(m.beh_aug, aes(variable, value)) + geom_bar(stat="identity") + theme_bw() ##+ 
#  facet_grid(Observer~Month)
