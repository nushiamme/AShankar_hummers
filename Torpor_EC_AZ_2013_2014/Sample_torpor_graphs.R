## Anusha Shankar
## January 26, 2016
## Torpor analysis graphs for retreat talk

library(ggplot2)
library(reshape)
library(gridExtra)

setwd("C://Users//ANUSHA//Dropbox//Anusha_personal//Thesis_proposal//Presentations//")
METY_torpid <- read.csv("EG15_1219_METY_0400_analyzed2.csv")
METY_normo <- read.csv("EG15_1219_METY_0545_analyzed.csv")

## To arrange graphs
lay_out = function(...) {    
  x <- list(...)
  n <- max(sapply(x, function(x) max(x[[2]])))
  p <- max(sapply(x, function(x) max(x[[3]])))
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))    
  
  for (i in seq_len(length(x))) {
    print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]], 
                                           layout.pos.col = x[[i]][[3]]))
  }
} 

METY_torpid$type <- 'torpor'
METY_normo$type <- 'normothermy'

m_METY_torpid <- melt(METY_torpid, id.vars = c("Elapsed_sec", "type"), measure.vars = "VO2_ml_min")
m_METY_normo <- melt(METY_normo, id.vars = c("Elapsed_sec", "type"), measure.vars = "VO2_ml_min")

METY_bound <- rbind(m_METY_torpid, m_METY_normo)

bound_plot <- ggplot(METY_bound, aes(Elapsed_sec, value, group=type)) + theme_bw(base_size=30) + 
  geom_line(aes(col=type)) +  facet_grid(variable~., scales = "free_y") + 
  scale_color_manual(values=c("#0099ff", "#ff0000")) +
  ylab(expression(paste(O[2]," inhaled"))) + xlab("Time (s)") +
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"), legend.key.height=unit(3,"line"), 
        legend.text=element_text(size=30)) + guides(colour = guide_legend(override.aes = list(size=3)))
  #ylab(expression(paste(CO[2]," exhaled or ",~O[2]," inhaled"))) + 
bound_plot

normo <- ggplot(m_METY_normo, aes(Elapsed_sec, value)) + theme_bw(base_size=30) + 
  facet_grid(variable~., scales = "free_y") + geom_line() + 
  ylab(expression(paste(O[2]," inhaled"))) + 
  #ylab(expression(paste(CO[2]," exhaled or ",~O[2]," inhaled"))) + 
  xlab("Time (s)") 
normo

lay_out(list(normo, 1, 1), 
        list(torpid, 1, 2))