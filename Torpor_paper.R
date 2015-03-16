## March 16, 2015
## Torpor paper, A. Shankar, R. Schroeder et al.
library(ggplot2)

data <- data.frame (id=1:3, weight=c(20,27,24))
torpor <- data.frame(hc=c(2,2,4,5,6,6,6,NA))
torpor$sc <- c(0,0,0,1,1,2,4,4)

m.tor <- melt(torpor, measure.vars = c("hc", "sc"))
names(m.tor) <- c("Site", "Hours")
ggplot(m.tor, aes(x = Site, y = Hours)) + theme_bw() + geom_boxplot() + geom_point(aes(col = Site)) + 
  scale_fill_brewer(type = "qual",palette = 3)
