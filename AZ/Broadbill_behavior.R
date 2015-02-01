## Set working directory
setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha_personal\\Thesis proposal\\R_csv\\AZ")

## Read in the file
bblh <- read.csv("Broadbill_behavior.csv")

## Melt to get Site, sex, behavior
m.bblh <- melt(bblh,id.vars = c("Month","Day","Site","Sex"),measure.vars = "Behavior")

## Subset out just site Harshaw Creek
bblhHC<- m.bblh[m.bblh$Site=="HC",]

## Subset out site Patagonia Lake/Sonoita Creek
bblhSC <- m.bblh[m.bblh$Site=="PL/SC",]

## Calculate percentage time spent on different activities in a site. (Refer to BehaviorKey.csv 
## for behavior codes)
perch <- (sum(m.bblh$value=="P") + sum(m.bblh$value=="V"))*100/nrow(m.bblh)
## Check that it's working
perch

repro <- (sum(m.bblh$value=="CM") + sum(m.bblh$value=="D") + 
  sum(m.bblh$value=="N") + sum(m.bblh$value=="NB"))*100/nrow(m.bblh)

flight <- (sum(m.bblh$value=="F1") + sum(m.bblh$value=="FO"))*100/nrow(m.bblh)

feed <- (sum(m.bblh$value=="HG") + sum(m.bblh$value=="HH") + sum(m.bblh$value=="JE") + sum(m.bblh$value=="MF"))*100/nrow(m.bblh)


