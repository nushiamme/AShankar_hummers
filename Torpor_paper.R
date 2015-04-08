## March 16, 2015
## Torpor paper, A. Shankar, R. Schroeder et al.
library(ggplot2)
library(reshape)

## setwd and read in file
setwd("C:\\Users\\ANUSHA\\Dropbox\\Hummingbird energetics\\Arizona Torpor")
torpor <- read.csv("Torpor_summary.csv")

## Rename columns, remove spaces
names(torpor) <- c("Site", "Bird_no", "Nighttime_Energy", "Energy", "Hours_torpor")

tor <- data.frame(torpor$Site, torpor$Energy, torpor$Hours_torpor)
names(tor) <- c("Site", "Energy", "Hours_torpor")

energy_plot <- ggplot(tor, aes(Site,Energy)) + theme_bw() + geom_boxplot() + 
  geom_point(aes(col=Site, size=2)) + scale_size(guide = 'none') + scale_shape_identity() +
  scale_color_manual(values = c("grey60", "black"), guide = FALSE) + 
  ylab("Nighttime energy expenditure (kJ)") + 
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14))
hours_torpor_plot <- ggplot(tor, aes(Site, Hours_torpor)) + theme_bw() + geom_boxplot() +
  geom_point(aes(col=Site, size=2)) + scale_size(guide = 'none') + scale_shape_identity() +
  scale_color_manual(values = c("grey60", "black"), guide = FALSE) + 
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14))
grid.arrange(energy_plot, hours_torpor_plot, nrow=1, ncol=2)

## Melt
#m.tor <- melt(torpor, id.vars = "Site", measure.vars = c("Energy", "Hours_torpor"))
#ggtor<- ggplot(m.tor, aes(Site, as.numeric(value))) + theme_bw() + facet_grid(~variable) + 
#  geom_boxplot() + geom_point(aes(col = Site, pch = 18)) + scale_shape_identity()


