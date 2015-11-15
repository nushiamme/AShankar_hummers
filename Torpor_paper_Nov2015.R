## Started fresh, 15 November, 2015
## Torpor paper, A. Shankar, R. Schroeder et al.

library(ggplot2)
library(reshape)
library(gridExtra)

## setwd and read in file
setwd("C:\\Users\\ANUSHA\\Dropbox\\Hummingbird energetics\\Tables_for_paper")
torpor <- read.csv("Torpor_table_plot.csv")
names(torpor)

m.tor <- melt(torpor, id.vars = c("Site","Species","Day","Month","Year","Daytime_Ta_mean_C"), 
              measure.vars = "NEE_kJ")
##"Time_of_entry","Hours_torpid","Hours_normo","Nighttime_Ta_mean_C","Tc_mean_C",
##"Min_EE_torpid","Min_EE_normo",

energy_plot <- ggplot(m.tor, aes(Species,value)) + 
  theme_bw() + geom_boxplot() +   geom_point(aes(col=Site, size=2)) + scale_size(guide = 'none') + 
  scale_shape_identity() +  #scale_color_manual(values = c("grey60", "black"), guide = FALSE) + 
  ylab("Nighttime energy expenditure (kJ)") + 
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14))
energy_plot
hours_torpor_plot <- ggplot(tor, aes(Site, Hours_torpor)) + theme_bw() + geom_boxplot() +
  geom_point(aes(col=Site, size=2)) + scale_size(guide = 'none') + scale_shape_identity() +
  scale_color_manual(values = c("grey60", "black"), guide = FALSE) + 
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14))
grid.arrange(energy_plot, hours_torpor_plot, nrow=1, ncol=2)