## August 13, 2015
## Anusha Shankar
## El Gullan, Ecuador, June 2015 - January 2016
## Hummingbird behavior analyses

library(reshape)

setwd("C:\\Users\\ANUSHA\\Dropbox\\Data 2015\\Behavior")
beh <- read.csv("Beh_for_analysis.csv")
head(beh)

m.beh <- melt(beh, id.vars = c("Observer", "Time", "Species", "Beh_no"), 
              measure.vars = c("FL", "Hov", "Perch"), na.rm = T)

Total_FL <- sum(as.numeric(m.beh$value[m.beh$variable=="FL"]))
Total_Hov <- sum(as.numeric(m.beh$value[m.beh$variable=="Hov"]))
Total_perch <- sum(as.numeric(m.beh$value[m.beh$variable=="Perch"]))

Fl_not_1 <- m.beh$variable=="FL"[m.beh$Beh_no!=1&m.beh$variable=="FL"]
length(Fl_not_1)
Hov_not_1 <- m.beh$variable=="Hov"[m.beh$Beh_no!=1]
length(Hov_not_1)
