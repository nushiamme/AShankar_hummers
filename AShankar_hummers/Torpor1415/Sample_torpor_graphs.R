## Anusha Shankar
## January 26, 2016
## Torpor analysis graphs for retreat talk

library(ggplot2)
library(reshape)
library(gridExtra)

setwd("C://Users//ANUSHA//Dropbox//Anusha_personal//Thesis_proposal//Presentations//")
METY_torpid <- read.csv("EG15_1219_METY_0400_analyzed2.csv")
METY_normo <- read.csv("EG15_1219_METY_0545_analyzed.csv")

METY_torpid$State <- 'torpid'
METY_normo$State <- 'normothermic'

m_METY_torpid <- melt(METY_torpid, id.vars = c("Elapsed_sec", "State"), measure.vars = "VO2_ml_min")
m_METY_normo <- melt(METY_normo, id.vars = c("Elapsed_sec", "State"), measure.vars = "VO2_ml_min")

METY_bound <- rbind(m_METY_torpid, m_METY_normo)

bound_plot <- ggplot(METY_bound, aes(Elapsed_sec, value, group=State)) + theme_classic(base_size=30) + 
  geom_line(aes(col=State)) +  scale_color_manual(values=c("#0099ff", "#ff0000")) +
  ylab(expression(paste(O[2]," consumed, ml/min"))) + xlab("Time (s)") +
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"), legend.key.height=unit(3,"line"), 
        legend.text=element_text(size=30)) + guides(colour = guide_legend(override.aes = list(size=3)))
  #ylab(expression(paste(CO[2]," exhaled or ",~O[2]," exhaled"))) + 
bound_plot

normo <- ggplot(m_METY_normo, aes(Elapsed_sec, value)) + theme_bw(base_size=30) + 
  facet_grid(variable~., scales = "free_y") + geom_line() + 
  ylab(expression(paste(O[2]," inhaled"))) + 
  #ylab(expression(paste(CO[2]," exhaled or ",~O[2]," inhaled"))) + 
  xlab("Time (s)") 
normo

lay_out(list(normo, 1, 1), 
        list(torpid, 1, 2))