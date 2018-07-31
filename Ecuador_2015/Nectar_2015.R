## October 19, 2015
## Anusha Shankar
## El Gullan, Ecuador, June 2015 - January 2016
## Nectar data analyses

library(reshape)
library(ggplot2)
library(pastecs)

orgr <- read.csv("C:\\Users\\nushi\\Dropbox\\Anusha_personal\\Thesis_proposal\\R_csv\\Nectar_ORGR_Oct19_2015_without55plus.csv")

nectar.orgr <- melt(orgr, id.vars = c("TimeSlot", "Day", "Month"), measure.vars = "Nectar_L_mm")

BRIX.orgr <- melt(orgr, id.vars = c("TimeSlot", "Day", "Month"), measure.vars = "BRIX", na.omit=T)


plot.nec.orgr <- ggplot(nectar.orgr, aes(TimeSlot, value)) + theme_bw() + geom_boxplot() + 
  ylab(label = "Nectar length") + scale_x_discrete(limits=c("6 - 8 am", "9 - 11 am", "12 - 2 pm",
                                                            "3 - 5 pm", "6 - 7 pm"))
plot.nec.orgr

plot.BRIX.orgr <- ggplot(BRIX.orgr, aes(TimeSlot, value)) + theme_bw() + geom_boxplot() + 
  ylab(label = "BRIX") + scale_x_discrete(limits=c("6 - 8 am", "9 - 11 am", "12 - 2 pm",
                                                   "3 - 5 pm", "6 - 7 pm"))
plot.BRIX.orgr

## Basic stats using pastecs package
stat.desc(orgr, basic=F)
mean(orgr$Nectar_L_mm)
