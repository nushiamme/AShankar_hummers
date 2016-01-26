## Anusha Shankar
## January 26, 2016
## Torpor analysis graphs for retreat talk

library(ggplot2)
library(reshape)
library(gridExtra)

setwd("C://Users//ANUSHA//Dropbox//Anusha_personal//Thesis_proposal//Presentations//")
METY_torpid <- read.csv("EG15_1219_METY_0400_analyzed2.csv")
METY_normo <- read.csv("EG15_1219_METY_0545_analyzed.csv")

m_METY_torpid <- melt(METY_torpid, id.vars = "Elapsed_sec", measure.vars = c("CO2", "VO2_ml_min"))
m_METY_normo <- melt(METY_normo, id.vars = "Elapsed_sec", measure.vars = c("CO2", "VO2_ml_min"))

torpid <- ggplot(m_METY_torpid, aes(Elapsed_sec, value)) + theme_bw() + geom_line() + 
  facet_grid(variable~., scales = "free_y") + 
  ylab(expression(paste(CO[2]," exhaled or ",~O[2]," inhaled"))) + 
  xlab("Time (s)") 
torpid

normo <- ggplot(m_METY_normo, aes(Elapsed_sec, value)) + theme_bw() + 
  facet_grid(variable~., scales = "free_y") + geom_line() + 
  ylab(expression(paste(CO[2]," exhaled or ",~O[2]," inhaled"))) + 
  xlab("Time (s)") 
normo

grid.arrange(normo, torpid, nrow=1, ncol=2)
